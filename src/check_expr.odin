package titania

import "core:strconv"

check_expr :: proc(c: ^Checker_Context, o: ^Operand, expr: ^Ast_Expr) {
	switch e in expr.variant {
	case ^Ast_Bad_Expr:
		error(c, e.pos, "bad expression found")

	case ^Ast_Ident:
		found, ok := scope_lookup(c.scope, e.tok.text)
		if !ok {
			error(c, e.pos, "'%s' has not been declared in scope", e.tok.text)
			return
		}
		switch found.kind {
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
			o.type  = found.type
			o.expr  = expr
			o.value = found.value
		case .Type:
			o.mode  = .Type
			o.type  = found.type
			o.expr  = expr
			o.value = nil
		case .Var:
			o.mode  = .LValue
			if .By_Value in found.flags {
				o.mode = .RValue
			}
			o.type  = found.type
			o.expr  = expr
			o.value = nil

		case .Proc:
			o.mode  = .RValue
			o.type  = found.type
			o.expr  = expr
			o.value = nil

		case .Import:
			error(c, e.pos, "illegal use of an import name")
			o.mode  = .Invalid
			o.type  = t_invalid
			o.expr  = expr
			o.value = nil

		case .Builtin:
			panic("TODO(bill): builtin")
		}


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
			o.mode  = .Const
			o.expr  = expr
			o.value = e.tok.text
			// TODO(bill): unescape the string
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

	case ^Ast_Unary_Expr:

	case ^Ast_Binary_Expr:

	case ^Ast_Deref_Expr:

	case ^Ast_Selector_Expr:

	case ^Ast_Paren_Expr:

	case ^Ast_Set_Expr:

	case ^Ast_Index_Expr:

	case ^Ast_Call_Expr:

	}
}
