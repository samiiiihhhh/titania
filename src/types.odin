package titania

import "core:mem/virtual"

Type_Kind :: enum u8 {
	Invalid,

	Bool,
	Char,
	Int,
	Real,
	Byte,

	Set,

	Pointer,
	Array,
	Record,
	Proc,
}

Type :: struct {
	kind:   Type_Kind,
	size:   i32,
	align:  i32,

	variant: union {
		^Type_Pointer,
		^Type_Array,
		^Type_Record,
		^Type_Proc,
	},
}

Type_Pointer :: struct {
	using base: Type,
	elem: ^Type,
}

Type_Array :: struct {
	using base: Type,
	counts: []i64, // 'nil' if a procedure parameter
	elem:   ^Type,
}

Type_Record :: struct {
	using base: Type,
	scope:   ^Scope,
	entity:  ^Entity,
	subtype: ^Type_Record,
	fields:  []Type_Field,
}

Type_Field :: struct {
	entity: ^Entity,
	offset: i32,
}


Type_Proc :: struct {
	using base: Type,
	parameters: []Type_Proc_Param,
}

Type_Proc_Param :: struct {
	is_var: bool, // by-reference semantics
	name:   ^Ast_Ident,
	type:   ^Ast_Type,
}


t_invalid := &Type{kind = .Invalid, size = 0, align = 1}
t_bool    := &Type{kind = .Bool,    size = 1, align = 1}
t_char    := &Type{kind = .Char,    size = 1, align = 1}
t_int     := &Type{kind = .Int,     size = 8, align = 8}
t_real    := &Type{kind = .Real,    size = 8, align = 8}
t_byte    := &Type{kind = .Byte,    size = 1, align = 1}
t_set     := &Type{kind = .Set,     size = 8, align = 8}


align_forward_i32 :: proc(x, y: i32) -> i32 {
	assert(y > 0)
	assert(y & (y-1) == 0)
	return x + (y-1) &~ (y-1)
}

type_init_offsets_for_record :: proc(record: ^Type_Record) {
	if record.size == 0 || record.align == 0 {
		max_alignment := i32(1)
		offset := i32(0)
		for &field in record.fields {
			al := type_align_of(field.entity.type)
			max_alignment = max(max_alignment, al)

			offset = align_forward_i32(offset, al)
			field.offset = offset

			sz := type_size_of(field.entity.type)
			offset += sz
		}
		size := align_forward_i32(offset, max_alignment)
		record.size = size
		record.align = max_alignment
	}
}

type_size_of :: proc(t: ^Type) -> i32 {
	switch t.kind {
	case: fallthrough
	case .Invalid:
		return 0
	case .Bool, .Char, .Int, .Real, .Byte, .Set:
		return t.size
	case .Array:
		array := t.variant.(^Type_Array)
		sz := i64(type_size_of(array.elem))
		for count in array.counts {
			sz *= count
		}
		return i32(sz)
	case .Record:
		record := t.variant.(^Type_Record)

		type_init_offsets_for_record(record)
		return t.size
	case .Proc, .Pointer:
		// pointer size is 8
		return 8
	}
}

type_align_of :: proc(t: ^Type) -> i32 {
	if record, ok := t.variant.(^Type_Record); ok {
		type_init_offsets_for_record(record)
	}
	assert(t.align > 0)
	return t.align
}



@(require_results)
type_new :: proc(arena: ^virtual.Arena, kind: Type_Kind, $T: typeid) -> ^T {
	t, err := virtual.new(arena, T)
	t.variant = t
	_ = err
	t.kind  = kind
	return t
}
