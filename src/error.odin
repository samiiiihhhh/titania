package titania

import "core:fmt"

syntax_error :: proc(t: ^Tokenizer, pos: Pos, format: string, args: ..any) {
	fmt.eprintf("%s(%d:%d) Syntax Error: ", t.filename, pos.line, pos.column)
	fmt.eprintfln(format, args=args)
}

error :: proc(pos: Pos, format: string, args: ..any) {
	fmt.eprintf("<file>(%d:%d) Error: ", pos.line, pos.column)
	fmt.eprintfln(format, args=args)
}