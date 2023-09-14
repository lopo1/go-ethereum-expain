// Copyright 2014 The go-ethereum Authors
// This file is part of the go-ethereum library.
//
// The go-ethereum library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The go-ethereum library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the go-ethereum library. If not, see <http://www.gnu.org/licenses/>.
// 类型缓存，记录了类型->内容(编码器/解码器)

package rlp

import (
	"fmt"
	"reflect"
	"sync"
	"sync/atomic"

	"github.com/ethereum/go-ethereum/rlp/internal/rlpstruct"
)

// typeinfo is an entry in the type cache.
// 存储对应的编码器和解码器函数
type typeinfo struct {
	decoder    decoder
	decoderErr error // error from makeDecoder
	writer     writer
	writerErr  error // error from makeWriter
}

// typekey is the key of a type in typeCache. It includes the struct tags because
// they might generate a different decoder.
// 类型
type typekey struct {
	reflect.Type
	rlpstruct.Tags
}

type decoder func(*Stream, reflect.Value) error

type writer func(reflect.Value, *encBuffer) error

var theTC = newTypeCache()

type typeCache struct {
	cur atomic.Value

	// This lock synchronizes writers.
	// 多线程中保护typeCache
	mu   sync.Mutex
	next map[typekey]*typeinfo
}

// 生成类型结构
func newTypeCache() *typeCache {
	c := new(typeCache)
	c.cur.Store(make(map[typekey]*typeinfo))
	return c
}

func cachedDecoder(typ reflect.Type) (decoder, error) {
	info := theTC.info(typ)
	return info.decoder, info.decoderErr
}

func cachedWriter(typ reflect.Type) (writer, error) {
	info := theTC.info(typ)
	return info.writer, info.writerErr
}

// 传入类型，返回改类型的编码器或者解码器函数
func (c *typeCache) info(typ reflect.Type) *typeinfo {
	key := typekey{Type: typ}
	// 获取函数信息
	if info := c.cur.Load().(map[typekey]*typeinfo)[key]; info != nil {
		return info
	}

	// Not in the cache, need to generate info for this type.
	return c.generate(typ, rlpstruct.Tags{})
}

func (c *typeCache) generate(typ reflect.Type, tags rlpstruct.Tags) *typeinfo {
	c.mu.Lock()
	defer c.mu.Unlock()

	cur := c.cur.Load().(map[typekey]*typeinfo)
	if info := cur[typekey{typ, tags}]; info != nil {
		return info
	}

	// Copy cur to next.
	c.next = make(map[typekey]*typeinfo, len(cur)+1)
	for k, v := range cur {
		c.next[k] = v
	}

	// Generate.
	info := c.infoWhileGenerating(typ, tags)

	// next -> cur
	c.cur.Store(c.next)
	c.next = nil
	return info
}

func (c *typeCache) infoWhileGenerating(typ reflect.Type, tags rlpstruct.Tags) *typeinfo {
	key := typekey{typ, tags}
	if info := c.next[key]; info != nil {
		return info
	}
	// Put a dummy value into the cache before generating.
	// If the generator tries to lookup itself, it will get
	// the dummy value and won't call itself recursively.
	// 没找到
	// 创建一个填充类型的位置
	info := new(typeinfo)
	c.next[key] = info
	// 生成对应类型的编码和解码器
	info.generate(typ, tags)
	return info
}

type field struct {
	index    int
	info     *typeinfo
	optional bool
}

// structFields resolves the typeinfo of all public fields in a struct type.
// 结构体字段
func structFields(typ reflect.Type) (fields []field, err error) {
	// Convert fields to rlpstruct.Field.
	var allStructFields []rlpstruct.Field
	// 遍历所有的结构体字段
	for i := 0; i < typ.NumField(); i++ {
		rf := typ.Field(i)
		allStructFields = append(allStructFields, rlpstruct.Field{
			Name:     rf.Name,
			Index:    i,
			Exported: rf.PkgPath == "", // 是否是导出字段，指的是首字母大写
			Tag:      string(rf.Tag),
			Type:     *rtypeToStructType(rf.Type, nil),
		})
	}

	// Filter/validate fields.
	structFields, structTags, err := rlpstruct.ProcessFields(allStructFields)
	if err != nil {
		if tagErr, ok := err.(rlpstruct.TagError); ok {
			tagErr.StructType = typ.String()
			return nil, tagErr
		}
		return nil, err
	}

	// Resolve typeinfo.
	for i, sf := range structFields {
		typ := typ.Field(sf.Index).Type
		tags := structTags[i]
		info := theTC.infoWhileGenerating(typ, tags)
		fields = append(fields, field{sf.Index, info, tags.Optional})
	}
	return fields, nil
}

// firstOptionalField returns the index of the first field with "optional" tag.
func firstOptionalField(fields []field) int {
	for i, f := range fields {
		if f.optional {
			return i
		}
	}
	return len(fields)
}

type structFieldError struct {
	typ   reflect.Type
	field int
	err   error
}

func (e structFieldError) Error() string {
	return fmt.Sprintf("%v (struct field %v.%s)", e.err, e.typ, e.typ.Field(e.field).Name)
}

// 生成对应类型的编码/解码函数
func (i *typeinfo) generate(typ reflect.Type, tags rlpstruct.Tags) {
	i.decoder, i.decoderErr = makeDecoder(typ, tags)
	i.writer, i.writerErr = makeWriter(typ, tags)
}

// rtypeToStructType converts typ to rlpstruct.Type.
func rtypeToStructType(typ reflect.Type, rec map[reflect.Type]*rlpstruct.Type) *rlpstruct.Type {
	k := typ.Kind()
	if k == reflect.Invalid {
		panic("invalid kind")
	}

	if prev := rec[typ]; prev != nil {
		return prev // short-circuit for recursive types
	}
	if rec == nil {
		rec = make(map[reflect.Type]*rlpstruct.Type)
	}

	t := &rlpstruct.Type{
		Name:      typ.String(),
		Kind:      k,
		IsEncoder: typ.Implements(encoderInterface),
		IsDecoder: typ.Implements(decoderInterface),
	}
	rec[typ] = t
	if k == reflect.Array || k == reflect.Slice || k == reflect.Ptr {
		t.Elem = rtypeToStructType(typ.Elem(), rec)
	}
	return t
}

// typeNilKind gives the RLP value kind for nil pointers to 'typ'.
func typeNilKind(typ reflect.Type, tags rlpstruct.Tags) Kind {
	styp := rtypeToStructType(typ, nil)

	var nk rlpstruct.NilKind
	if tags.NilOK {
		nk = tags.NilKind
	} else {
		nk = styp.DefaultNilValue()
	}
	switch nk {
	case rlpstruct.NilKindString:
		return String
	case rlpstruct.NilKindList:
		return List
	default:
		panic("invalid nil kind value")
	}
}

func isUint(k reflect.Kind) bool {
	return k >= reflect.Uint && k <= reflect.Uintptr
}

func isByte(typ reflect.Type) bool {
	return typ.Kind() == reflect.Uint8 && !typ.Implements(encoderInterface)
}
