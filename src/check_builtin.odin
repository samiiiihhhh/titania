package titania

import "core:fmt"
import "core:unicode/utf8"

Builtin_Id :: enum {
	Invalid,

	abs, // absolute value

	lsh,
	ash,
	ror,

	chr,
	ord,

	inc,
	dec,

	incl,
	excl,

	odd,

	floor,
	ceil,

	assert,
	new,

	addr,
	size_of,
	align_of,

	copy,

	print,
	println,
}


@(rodata)
builtin_strings := [Builtin_Id]string {
	.Invalid  = "",

	.abs      = "abs",

	.lsh      = "lsh",
	.ash      = "ash",
	.ror      = "ror",

	.chr      = "chr",
	.ord      = "ord",

	.inc      = "inc",
	.dec      = "dec",

	.incl     = "incl",
	.excl     = "excl",

	.odd      = "odd",

	.floor    = "floor",
	.ceil     = "ceil",

	.assert   = "assert",
	.new      = "new",

	.addr     = "addr",
	.size_of  = "size_of",
	.align_of = "align_of",

	.copy     = "copy",

	.print    = "print",
	.println  = "println",
}


check_builtin :: proc(c: ^Checker_Context, o: ^Operand, parameters: []^Ast_Expr) {

	o.type = t_invalid

	id := o.builtin_id
	name := builtin_strings[id]

	switch id {
	case .Invalid:
		return
	case .abs:
		panic("TODO(bill): builtin.abs")

	case .lsh:
		panic("TODO(bill): builtin.lsh")

	case .ash:
		panic("TODO(bill): builtin.ash")

	case .ror:
		panic("TODO(bill): builtin.ror")

	case .chr:
		if len(parameters) != 1 {
			error(c, o.expr.pos, "expected 1 parameter to '%s', got %d", name, len(parameters))
		}
		if len(parameters) > 0 {
			p: Operand
			check_expr(c, &p, parameters[0])
			if !types_equal(p.type, t_int) {
				error(c, o.expr.pos, "expected an int parameter to '%s', got %s", name, type_to_string(p.type))
			}
		}

		o.type = t_char
		o.mode = .RValue

	case .ord:
		if len(parameters) != 1 {
			error(c, o.expr.pos, "expected 1 parameter to '%s', got %d", name, len(parameters))
		}
		o.mode = .RValue

		if len(parameters) > 0 {
			p: Operand
			check_expr(c, &p, parameters[0])
			if p.mode == .Const {
				v, ok := p.value.(string)
				if !ok {
					error(c, o.expr.pos, "expected a constant string parameter to '%s", name)
				} else {
					r, _ := utf8.decode_rune_in_string(v)
					o.value = i64(r)
					o.mode = .Const
				}
			} else {
				error(c, o.expr.pos, "expected a constant string parameter to '%s', got %s", name, type_to_string(p.type))
			}
		}

		o.type = t_int

	case .inc, .dec:
		if len(parameters) != 1 {
			error(c, o.expr.pos, "expected 1 parameter to '%s', got %d", name, len(parameters))
		}
		o.mode = .No_Value
		o.type = t_invalid
		if len(parameters) > 0 {
			p: Operand
			check_expr(c, &p, parameters[0])
			if p.mode == .LValue && type_is_numeric(p.type) {
				// Okay
			} else {
				error(c, o.expr.pos, "expected an addressable numeric value to '%s'", name)
			}
		}

	case .incl, .excl:
		if len(parameters) != 2 {
			error(c, o.expr.pos, "expected 1 parameter to '%s', got %d", name, len(parameters))
		}
		o.mode = .No_Value
		o.type = t_invalid
		if len(parameters) > 1 {
			dst: Operand
			val: Operand
			check_expr(c, &dst, parameters[0])
			check_expr(c, &val, parameters[1])
			if !(dst.mode == .LValue && dst.type.kind == .Set) {
				error(c, o.expr.pos, "expected an addressable set to '%s'", name)
			}
			if !type_is_integer_like(val.type) {
				error(c, o.expr.pos, "expected an integer-like type set to '%s', got %s", name, type_to_string(val.type))
			}
		}

	case .odd:
		panic("TODO(bill): builtin.odd")

	case .floor:
		panic("TODO(bill): builtin.floor")

	case .ceil:
		panic("TODO(bill): builtin.ceil")

	case .assert:
		panic("TODO(bill): builtin.assert")

	case .new:
		panic("TODO(bill): builtin.new")

	case .addr:
		panic("TODO(bill): builtin.addr")

	case .size_of:
		panic("TODO(bill): builtin.size_of")

	case .align_of:
		panic("TODO(bill): builtin.align_of")

	case .copy:
		panic("TODO(bill): builtin.copy")

	case .print:
		panic("TODO(bill): builtin.print")

	case .println:
		panic("TODO(bill): builtin.println")

	}
}
