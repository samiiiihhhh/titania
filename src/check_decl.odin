package titania

import "core:mem/virtual"

check_decl :: proc(c: ^Checker_Context, decl: ^Ast_Decl) {
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

check_const_decl :: proc(c: ^Checker_Context, decl: ^Ast_Const_Decl) {
	name := decl.name.tok.text
	entity := entity_new(c.arena, .Const, name, t_invalid, c.scope)
	entity.decl = decl

	o: Operand
	check_expr(c, &o, decl.expr)
	if o.mode != .Const {
		error(c, decl.expr.pos, "expected a constant value for a constant declaration")
	} else {
		entity.value = o.value
		entity.type  = o.type
	}

	scope_insert_entity(c.scope, entity)
}

check_var_decl :: proc(c: ^Checker_Context, decl: ^Ast_Var_Decl) {
	type := check_type(c, decl.type)
	for name in decl.names {
		vname := name.tok.text
		found, ok := scope_lookup_current(c.scope, vname)
		if ok {
			error(c, name.pos, "'%s' has been previously declared in this scope", vname)
			continue
		}
		e := entity_new(c.arena, .Var, vname, type, c.scope)
		scope_insert_entity(c.scope, e)
	}
}

check_type_decl :: proc(c: ^Checker_Context, decl: ^Ast_Type_Decl) {
	name := decl.name.tok.text
	entity := entity_new(c.arena, .Type, name, t_invalid, c.scope)
	entity.decl = decl


	switch dt in decl.type {
	case ^Ast_Ident:
		scope_insert_entity(c.scope, entity)

		ident := dt.tok.text
		found, ok := scope_lookup(c.scope, ident)
		if !ok {
			error(c, decl.pos, "'%s' has not been declared", ident)
			return
		}
		if found.kind != .Type {
			error(c, decl.pos, "'%s' is not a type, got %s", ident, entity_kind_string[found.kind])
			return
		}
		entity.type = found.type

	case ^Ast_Qual_Ident:
		lhs := dt.lhs.text
		rhs := dt.rhs.text
		module, module_ok := scope_lookup(c.scope, lhs)
		if !module_ok {
			error(c, decl.pos, "'%s' has not been declared", lhs)
			return
		}
		if module.kind != .Import {
			error(c, decl.pos, "'%s' is not an import name, got %s", entity_kind_string[module.kind])
			return
		}
		found, found_ok := scope_lookup(module.scope, rhs)
		if !found_ok {
			error(c, decl.pos, "'%s' does not exist in the module '%s'", rhs, lhs)
			return
		}
		if found.kind != .Type {
			error(c, decl.pos, "'%s.%s' is not a type, got %s", lhs, rhs, entity_kind_string[found.kind])
		}
		entity.type = found.type

		scope_insert_entity(c.scope, entity)

	case ^Ast_Structured_Type:
		switch v in dt.variant {
		case ^Ast_Array_Type:
			t := type_new(c.arena, .Array, Type_Array)
			t.elem = check_type(c, v.elem)
			entity.type = t

			if v.counts != nil {
				counts, _ := virtual.make(c.arena, []i64, len(v.counts))
				index := 0
				for expr in v.counts {
					o: Operand
					check_expr(c, &o, expr)
					if o.mode != .Const {
						error(c, expr.pos, "array counts must be constant")
						continue
					}
					cv, ok := o.value.(i64)
					if !ok {
						error(c, expr.pos, "array counts must be constant integers")
						continue
					}
					if  cv < 0 {
						error(c, expr.pos, "array counts must not be negative, got %d", cv)
						continue
					}

					counts[index] = cv
					index += 1
				}
				t.counts = counts[:index]
			} else {
				error(c, v.pos, "array type declaration without any count specified")
			}

			scope_insert_entity(c.scope, entity)

		case ^Ast_Pointer_Type:
			t := type_new(c.arena, .Pointer, Type_Pointer)
			t.elem = check_type(c, v.elem)

			scope_insert_entity(c.scope, entity)

		case ^Ast_Record_Type:
			t := type_new(c.arena, .Record, Type_Record)
			t.scope = scope_new(c.module, c.scope)

			scope_insert_entity(t.scope, entity)

			scope_push(c, t.scope)
			defer scope_pop(c)

			t.entity = entity
			entity.type = t
			field_count := 0
			for field in v.fields {
				field_count += len(field.names)
			}

			t.fields, _ = virtual.make(c.arena, []Type_Field, field_count)
			index := 0
			for field in v.fields {
				type := check_type(c, field.type)

				for f, i in field.names {
					fname := f.tok.text
					if prev, ok := scope_lookup_current(c.scope, fname); ok {
						error(c, f.tok.pos, "'%s' has been previously defined in this record type", fname)
					} else {
						e := entity_new(c.arena, .Var, fname, type, c.scope)
						scope_insert_entity(c.scope, e)
						t.fields[index] = {e, 0}
						index += 1
					}
				}
			}
			t.fields = t.fields[:index]

		case ^Ast_Proc_Type:
			scope_push(c, c.scope)
			defer scope_pop(c
			                )
			t := check_proc_type(c, v.parameters[:])
			entity.type = t
			entity.scope = c.scope
		}
	}

	return
}

check_proc_decl :: proc(c: ^Checker_Context, decl: ^Ast_Proc_Decl) {
	name := decl.name.tok.text
	entity := entity_new(c.arena, .Proc, name, t_invalid, c.scope)
	entity.decl = decl
	scope_insert_entity(c.scope, entity)

	scope_push(c, c.scope)
	defer scope_pop(c)

	entity.type = check_proc_type(c, decl.parameters[:])

	// body
	for decl in decl.decls {
		check_decl(c, decl)
	}

	check_stmt_sequence(c, decl.body)

	if re, ok := decl.return_expr.?; ok {
		ret: Operand
		check_expr(c, &ret, re)
	}
}


