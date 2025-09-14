package titania

import "core:fmt"

syntax_error :: proc(t: ^Tokenizer, pos: Pos, format: string, args: ..any) {
	fmt.eprintf("%s(%d:%d) Syntax Error: ", t.filename, pos.line, pos.column)
	fmt.eprintfln(format, args=args)
	t.error_count += 1
}

error_module :: proc(m: ^Module, pos: Pos, format: string, args: ..any) {
	fmt.eprintf("%s(%d:%d) Error: ", m.filename, pos.line, pos.column)
	fmt.eprintfln(format, args=args)
	m.error_count += 1
}

error_checker :: proc(c: ^Checker_Context, pos: Pos, format: string, args: ..any) {
	error_module(c.module, pos, format, ..args)
}

error :: proc{
	error_module,
	error_checker,
}