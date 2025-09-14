package titania

import "core:fmt"
import "core:mem/virtual"

Addressing_Mode :: enum {
	Invalid,
	No_Value,
	Nil,
	RValue,
	LValue,
	Const,
	Type,
	Builtin,
}

Operand :: struct {
	expr:  ^Ast_Expr,
	type:  ^Type,
	mode:  Addressing_Mode,
	value: Const_Value,
}

Checker_Info :: struct {
	arena: virtual.Arena,
	builtin_scope: ^Scope,
}

Checker_Context :: struct {
	info: ^Checker_Info,

	// current stuff
	arena:  ^virtual.Arena,
	module: ^Module,
	scope:  ^Scope,
	mode:   Addressing_Mode,
	value:  Const_Value,
}

Module :: struct {
	arena: virtual.Arena `fmt:"-"`,

	filename: string,

	tok:  Token,
	name: Token,

	imports: [dynamic]^Ast_Import,

	decls: [dynamic]^Ast_Decl,

	entry: ^Ast_Stmt_Sequence,

	scope: ^Scope,

	info: ^Checker_Info,
}


Entity_Kind :: enum u32 {
	Invalid,

	Nil,

	Const,
	Type,
	Var,
	Proc,
	Import,
	Builtin,
}

@(rodata)
entity_kind_string := [Entity_Kind]string{
	.Invalid = "invalid",
	.Nil     = "nil",
	.Const   = "const",
	.Type    = "type",
	.Var     = "var",
	.Proc    = "proc",
	.Import  = "import name",
	.Builtin = "builtin proc",
}

Entity_Flags :: distinct bit_set[Entity_Flag; u32]
Entity_Flag :: enum {
	By_Value, // procedure parameter
}

Entity :: struct {
	kind:   Entity_Kind,
	flags:  Entity_Flags,
	scope:  ^Scope,
	module: ^Module,

	pos:  Pos,
	name: string,
	type: ^Type,

	decl: ^Ast_Decl,

	import_module: ^Module,
	value:  Const_Value,
}

Scope :: struct {
	parent:    ^Scope,
	elements:  map[string]^Entity,
	module:    ^Module,
	procedure: Maybe(^Entity),
}

Const_Value :: union {
	i64,
	f64,
	string,
	bool,
}

@(require_results)
entity_new :: proc(arena: ^virtual.Arena, kind: Entity_Kind, name: string, type: ^Type, scope: ^Scope) -> ^Entity {
	e, err := virtual.new(arena, Entity)
	_ = err
	e.kind  = kind
	e.name  = name
	e.type  = type
	e.scope = scope
	return e
}

@(require_results)
scope_new :: proc(m: ^Module, parent: ^Scope) -> ^Scope {
	s, err := virtual.new(&m.arena, Scope)
	s.module = m
	s.parent = parent
	return s
}

@(require_results)
scope_lookup_current :: proc(s: ^Scope, name: string) -> (e: ^Entity, ok: bool) {
	return s.elements[name]
}

@(require_results)
scope_lookup :: proc(s: ^Scope, name: string) -> (e: ^Entity, ok: bool) {
	s := s
	for s != nil {
		e, ok = scope_lookup_current(s, name)
		if ok {
			return
		}
		s = s.parent
	}
	return
}

scope_push :: proc(c: ^Checker_Context, s: ^Scope) {
	c.scope = s
}
scope_pop :: proc(c: ^Checker_Context) -> ^Scope {
	s := c.scope
	c.scope = s.parent
	return s
}

scope_insert_entity :: proc(s: ^Scope, e: ^Entity) -> bool {
	assert(e.name  != "")
	assert(e.scope != nil)
	if e.name in s.elements {
		if e.module != nil {
			error(e.module, e.pos, "'%s' has already been defined in this scope", e.name)
		}
		return false
	}
	s.elements[e.name] = e
	return true
}

checker_info_init :: proc(info: ^Checker_Info) {
	info.builtin_scope, _ = virtual.new(&info.arena, Scope)
	s := info.builtin_scope

	scope_insert_entity(s, entity_new(&info.arena, .Type, "invalid", t_invalid, s))
	scope_insert_entity(s, entity_new(&info.arena, .Type, "bool",    t_bool,    s))
	scope_insert_entity(s, entity_new(&info.arena, .Type, "char",    t_char,    s))
	scope_insert_entity(s, entity_new(&info.arena, .Type, "int",     t_int,     s))
	scope_insert_entity(s, entity_new(&info.arena, .Type, "real",    t_real,    s))
	scope_insert_entity(s, entity_new(&info.arena, .Type, "byte",    t_byte,    s))
	scope_insert_entity(s, entity_new(&info.arena, .Type, "set",     t_set,     s))


}

check_module :: proc(info: ^Checker_Info, m: ^Module) {
	m.info = info
	m.scope = scope_new(m, info.builtin_scope)


	c := &Checker_Context{}
	c.module = m
	c.scope = m.scope
	c.arena = &info.arena

	for decl in m.decls {
		switch v in decl.variant {
		case ^Ast_Const_Decl:
			check_const_decl(c, v)
		case ^Ast_Type_Decl:
			check_type_decl(c, v)
		case ^Ast_Var_Decl:
			check_var_decl(c, v)
		case ^Ast_Proc_Decl:
			check_proc_decl(c, v)
		}
	}
}

