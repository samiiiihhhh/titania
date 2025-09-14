package titania

import "core:fmt"
import "core:strconv"
import "core:mem/virtual"

check_is_boolean :: proc(c: ^Checker_Context, o: ^Operand) {
	assert(o.type != nil)
	if o.type.kind != .Bool {
		if o.expr != nil {
			error(c, o.expr.pos, "expected a boolean expression")
		}
	}
}

check_is_integer :: proc(c: ^Checker_Context, o: ^Operand) {
	assert(o.type != nil)

	#partial switch o.type.kind {
	case .Invalid:
		return
	case .Char, .Int, .Byte:
		return
	}

	if o.expr != nil {
		error(c, o.expr.pos, "expected an integer-like expression")
	}
}



assign_entity_to_operand :: proc(c: ^Checker_Context, e: ^Entity, o: ^Operand, expr: ^Ast_Expr) {
	switch e.kind {
	case .Invalid:
		error(c, e.pos, "invalid entity")
		o.mode  = .Invalid
		o.type  = t_invalid
		o.value = nil
	case .Nil:
		o.mode  = .Nil
		o.expr  = expr
		o.value = nil
	case .Const:
		o.mode  = .Const
		o.type  = e.type
		o.expr  = expr
		o.value = e.value
	case .Type:
		o.mode  = .Type
		o.type  = e.type
		o.expr  = expr
		o.value = nil
	case .Var:
		o.mode  = .LValue
		o.type  = e.type
		o.expr  = expr
		o.value = nil

	case .Proc:
		o.mode  = .RValue
		o.type  = e.type
		o.expr  = expr
		o.value = nil

	case .Import:
		o.mode  = .Invalid
		o.type  = t_invalid
		o.expr  = expr
		o.value = nil

	case .Builtin:
		o.type = t_invalid
		o.mode = .Builtin
		o.expr = expr
		o.builtin_id = e.builtin_id
	}
}

check_expr :: proc(c: ^Checker_Context, o: ^Operand, expr: ^Ast_Expr) {
	defer if o.mode == .Const {
		expr.value = o.value
		expr.type  = o.type
	}

	switch e in expr.variant {
	case ^Ast_Bad_Expr:
		error(c, e.pos, "bad expression found")

	case ^Ast_Ident:
		found, ok := scope_lookup(c.scope, e.tok.text)
		if !ok {
			error(c, e.pos, "'%s' has not been declared in scope", e.tok.text)
			o.type = t_invalid
			o.mode = .Invalid
			return
		}
		assign_entity_to_operand(c, found, o, expr)


	case ^Ast_Literal:
		#partial switch e.tok.kind {
		case .Integer:
			o.mode  = .Const
			o.expr  = expr
			o.type  = t_int
			o.value, _ = strconv.parse_i64(e.tok.text)
		case .Real:
			o.mode  = .Const
			o.expr  = expr
			o.type  = t_real
			o.value, _ = strconv.parse_f64(e.tok.text)
		case .String:
			allocator := virtual.arena_allocator(c.arena)
			str, _, ok := strconv.unquote_string(e.tok.text, allocator)
			if !ok {
				error(c, e.tok.pos, "unable to unquote the following string: %s", e.tok.text)
			}

			o.mode  = .Const
			o.expr  = expr
			o.value = str
			o.type  = type_new_string(c.arena, len(str))
		case .Nil:
			o.mode = .Nil
			o.expr = expr
		case .True:
			o.mode  = .Const
			o.expr  = expr
			o.value = true
		case .False:
			o.mode  = .Const
			o.expr  = expr
			o.value = false
		}
	case ^Ast_Qual_Ident:
		lhs := e.lhs.text
		rhs := e.rhs.text
		module, module_ok := scope_lookup(c.scope, lhs)
		if !module_ok {
			error(c, e.pos, "'%s' has not been declared in scope", lhs)
			o.type = t_invalid
			o.mode = .Invalid
			return
		}
		if module.kind != .Import {
			error(c, e.pos, "'%s' is not an import name, got %s", entity_kind_string[module.kind])
			o.type = t_invalid
			o.mode = .Invalid
			return
		}
		found, found_ok := scope_lookup(module.import_scope, rhs)
		if !found_ok {
			error(c, e.pos, "'%s' does not exist in the module '%s'", rhs, lhs)
			o.type = t_invalid
			o.mode = .Invalid
			return
		}
		assign_entity_to_operand(c, found, o, expr)

	case ^Ast_Unary_Expr:
		#partial switch e.op.kind {
		case .Add, .Sub:
			check_expr(c, o, e.expr)
			if o.type != nil {
				#partial switch o.type.kind {
				case .Int, .Byte, .Char, .Real:
					// okay
				case:
					error(c, e.op.pos, "%s is only supported for numeric types", e.op.text)
				}
			}

			if e.op.kind == .Sub && o.mode == .Const {
				switch v in o.value {
				case i64: o.value = -v
				case f64: o.value = -v
				case string: // ignore
				case bool:   // ignore
				}
			}
		case:
			error(c, e.op.pos, "invalid unary operator, got '%s'", e.op.text)
		}

	case ^Ast_Binary_Expr:
		lhs, rhs: Operand

		check_expr(c, &lhs, e.lhs)
		check_expr(c, &rhs, e.rhs)

		o.value = nil
		o.mode = .RValue
		o.type = lhs.type
		if is_relation(e.op.kind) {
			o.type = t_bool
		}

		if !types_equal(lhs.type, rhs.type) {
			error(c, e.op.pos, "mismatching types")
		} else if lhs.mode == .Const && rhs.mode == .Const {
			o.value = check_const_binary_expr(c, lhs.value, e.op.kind, rhs.value)
		}


	case ^Ast_Deref_Expr:
		check_expr(c, o, e.expr)
		if o.type.kind != .Pointer {
			error(c, e.expr.pos, "expected a pointer expression to dereference")
			o.mode = .Invalid
		} else {
			o.type = type_deref(o.type)
			o.mode = .RValue
			o.value = nil
		}

	case ^Ast_Selector_Expr:
		// check_expr(c, o, e.lhs)
		panic("Ast_Selector_Expr")

	case ^Ast_Paren_Expr:
		check_expr(c, o, e.expr)

	case ^Ast_Set_Expr:
		panic("Ast_Set_Expr")

	case ^Ast_Index_Expr:
		check_expr(c, o, e.expr)
		for index in e.indices {
			i: Operand
			check_expr(c, &i, index)
			check_is_integer(c, &i)
		}
		if o.type.kind != .Array {
			error(c, e.expr.pos, "cannot index a non-array type")
			o.mode = .Invalid
			o.type = t_invalid
			o.value = nil
		} else {
			if o.mode != .LValue {
				error(c, e.expr.pos, "cannot index a non l-value")
			}
			o.mode = .LValue
			o.type = o.type.variant.(^Type_Array).elem
			o.value = nil
		}

	case ^Ast_Call_Expr:
		check_expr(c, o, e.call)
		#partial switch o.mode {
		case .Builtin:
			check_builtin(c, o, e.parameters[:])
		case .Type:
			o.mode = .RValue
			o.type = o.type
			if len(e.parameters) != 1 {
				error(c, e.call.pos, "expect only 1 parameter for a type conversion, got %d", len(e.parameters))
			}
			if len(e.parameters) > 1 {
				p: Operand
				check_expr(c, &p, e.parameters[0])

				dst := o.type.kind
				src := p.type.kind
				if dst == src {
					return
				}

				#partial switch src {
				case .Bool:
					#partial switch dst {
					case .Int, .Byte:
						// okay
						if b, ok := p.value.(bool); ok {
							o.value = i64(1 if b else 0)
							o.mode = .Const
						}
					case:
						error(c, p.expr.pos, "cannot cast from %s to %s", type_to_string(o.type), type_to_string(p.type))
					}
				case .Char:
					#partial switch dst {
					case .Int, .Byte:
						if i, ok := p.value.(i64); ok {
							o.value = i64(i)
							o.mode = .Const
						}
					case:
						error(c, p.expr.pos, "cannot cast from %s to %s", type_to_string(o.type), type_to_string(p.type))
					}
				case .Int:
					#partial switch dst {
					case .Real:
						if i, ok := p.value.(i64); ok {
							o.value = f64(i)
							o.mode = .Const
						}
					case .Char, .Byte:
						if i, ok := p.value.(i64); ok {
							o.value = i64(u64(i) & 0xff)
							o.mode = .Const
						}
					case:
						error(c, p.expr.pos, "cannot cast from %s to %s", type_to_string(o.type), type_to_string(p.type))
					}
				case .Real:
					#partial switch dst {
					case .Int:
						if i, ok := p.value.(f64); ok {
							o.value = i64(i)
							o.mode = .Const
						}
					case .Byte:
						if i, ok := p.value.(f64); ok {
							o.value = i64(u64(i) & 0xff)
							o.mode = .Const
						}
					case:
						error(c, p.expr.pos, "cannot cast from %s to %s", type_to_string(o.type), type_to_string(p.type))
					}
				case .Byte:
					#partial switch dst {
					case .Int, .Char:
						if i, ok := p.value.(i64); ok {
							o.value = i64(i)
							o.mode = .Const
						}

					case .Real:
						if i, ok := p.value.(i64); ok {
							o.value = f64(i)
							o.mode = .Const
						}
					case:
						error(c, p.expr.pos, "cannot cast from %s to %s", type_to_string(o.type), type_to_string(p.type))
					}
				case .Pointer:
					#partial switch dst {
					case .Int, .Pointer:
						// okay
					case:
						error(c, p.expr.pos, "cannot cast from %s to %s", type_to_string(o.type), type_to_string(p.type))
					}
				}

			}

			if o.mode != .Const {
				o.value = nil
			}

		case .RValue, .LValue:
			if o.type.kind != .Proc {
				error(c, e.call.pos, "expected a procedure value, got %s", type_to_string(o.type))
				o.mode = .No_Value
				o.type = t_invalid
				return
			}

			o.mode = .No_Value
			o.type = t_invalid
		case:
			o.mode = .No_Value
			o.type = t_invalid
		}
	}
}


check_const_binary_expr :: proc(c: ^Checker_Context, lhs: Const_Value, op: Token_Kind, rhs: Const_Value) -> (res: Const_Value) {
	return
}
