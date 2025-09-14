package titania

import "core:os"
import "core:fmt"

Parser :: struct {
	tok: Tokenizer,

	file_data: []byte,

	prev_token: Token,
	curr_token: Token,
}

parser_init :: proc(p: ^Parser, filename: string) -> bool {
	p^ = {}

	data, ok := os.read_entire_file(filename)
	if !ok {
		fmt.println("Failed to read file: ", filename)
		return false
	}

	p.file_data = data
	tokenizer_init(&p.tok, filename, string(data))

	advance_token(p)

	return true
}

parser_fini :: proc(p: ^Parser) {
	delete(p.file_data)
}

advance_token :: proc(p: ^Parser) -> Token {
	p.prev_token = p.curr_token
	p.curr_token = get_token(&p.tok)
	return p.prev_token
}

allow_token :: proc(p: ^Parser, kind: Token_Kind) -> bool {
	if p.curr_token.kind == kind {
		advance_token(p)
		return true
	}
	return false
}

peek_token :: proc(p: ^Parser, kind: Token_Kind) -> bool {
	if p.curr_token.kind == kind {
		return true
	}
	return false
}

expect_token :: proc(p: ^Parser, kind: Token_Kind) -> (Token, bool) {
	prev := p.curr_token
	advance_token(p)
	if prev.kind == kind {
		return prev, true
	}
	syntax_error(&p.tok, prev.pos, "expected %s, got %s", token_kind_string[kind], token_kind_string[prev.kind])
	return prev, false
}

parse :: proc(p: ^Parser) -> bool {
	return parse_module(p)
}

// module = "module" ident ";" [import_list] decl_sequence
//          ["begin" stmt_sequence] "end"
parse_module :: proc(p: ^Parser) -> bool {
	expect_token(p, .Module) or_return
	name := expect_token(p, .Ident)  or_return

	expect_token(p, .Semicolon)  or_return

	if peek_token(p, .Import) {
		parse_import_list(p)
	}

	parse_decl_sequence(p)

	if allow_token(p, .Begin) {
		parse_stmt_sequence(p)
	}
	expect_token(p, .End) or_return
	return true
}

// import_list = "import" import_decl {"," import_decl} ";"
parse_import_list :: proc(p: ^Parser) {
	expect_token(p, .Import)
	parse_import_decl(p)
	for allow_token(p, .Comma) {
		parse_import_decl(p)
	}
	expect_token(p, .Semicolon)
}

// import_decl = ident [":=" ident]
parse_import_decl :: proc(p: ^Parser) {
	expect_token(p, .Ident)
	if allow_token(p, .Assign) {
		expect_token(p, .Ident)
	}
}

// decl_sequence = ["const" {const_decl ";"}]
//                 ["type"  {type_decl  ";"}]
//                 ["var"   {var_decl   ";"}]
//                 [{proc_de  cl        ";"}]
parse_decl_sequence :: proc(p: ^Parser) {
	if allow_token(p, .Const) {
		for peek_token(p, .Ident) {
			parse_const_decl(p)
			expect_token(p, .Semicolon)
		}
	}
	if allow_token(p, .Type) {
		for peek_token(p, .Ident) {
			parse_type_decl(p)
			expect_token(p, .Semicolon)
		}
	}
	if allow_token(p, .Var) {
		for peek_token(p, .Ident) {
			parse_var_decl(p)
			expect_token(p, .Semicolon)
		}
	}
	for peek_token(p, .Proc) {
		parse_proc_decl(p)
		expect_token(p, .Semicolon)
	}
}

// const_decl = ident "=" const_expr
parse_const_decl :: proc(p: ^Parser) {
	expect_token(p, .Ident)
	expect_token(p, .Equal)
	parse_const_expr(p)
}
// type_decl = ident "="" struct_type
parse_type_decl :: proc(p: ^Parser) {
	expect_token(p, .Ident)
	expect_token(p, .Equal)
	parse_struct_type(p)
}
// var_decl = ident_list ":" type
parse_var_decl :: proc(p: ^Parser) {
	parse_ident_list(p)
	expect_token(p, .Colon)
	parse_type(p)
}
// proc_decl = proc_heading ";" proc_body
parse_proc_decl :: proc(p: ^Parser) {
	parse_proc_heading(p)
	expect_token(p, .Semicolon)
	parse_proc_body(p)
}

// proc_heading = "proc" ident [formal_parameters]
parse_proc_heading :: proc(p: ^Parser) {
	expect_token(p, .Proc)
	expect_token(p, .Ident)
	if peek_token(p, .Paren_Open) {
		parse_formal_parameters(p)
	}
}

// proc_body = decl_sequence ["begin" stmt_sequence] ["return" expr] "end"
parse_proc_body :: proc(p: ^Parser) {
	parse_decl_sequence(p)
	if allow_token(p, .Begin) {
		parse_stmt_sequence(p)
	}
	if allow_token(p, .Return) {
		parse_expr(p)
	}
	expect_token(p, .End)
}

// const_expr = expr
parse_const_expr :: proc(p: ^Parser) {
	parse_expr(p)
}

// expr = simple_expr {relation simple_expr}
parse_expr :: proc(p: ^Parser) {
	parse_simple_expr(p)
	for is_relation(p.curr_token.kind) {
		advance_token(p)
		parse_simple_expr(p)
	}
}

is_relation :: proc(kind: Token_Kind) -> bool {
	#partial switch kind {
	case .Equal, .Not_Equal, .Less_Than, .Less_Than_Equal, .Greater_Than, .Greater_Than_Equal, .In, .Is:
		return true
	}
	return false
}

// simple_expr = ["+" | "-"] term {add_operator term}
parse_simple_expr :: proc(p: ^Parser) {
	if peek_token(p, .Add) || peek_token(p, .Sub) {
		advance_token(p)
	}
	parse_term(p)
	for is_add_operator(p.curr_token.kind) {
		advance_token(p)
		parse_term(p)
	}

}

// term = factor {mul_operator factor}
parse_term :: proc(p: ^Parser) {
	parse_factor(p)
	for is_mul_operator(p.curr_token.kind) {
		advance_token(p)
		parse_factor(p)
	}
}

// factor = integer | real | string | nil | true | false | set |
//          "(" expr ")" | "not" expr | designator
parse_factor :: proc(p: ^Parser) {
	#partial switch p.curr_token.kind {
	case .Integer, .Real, .String:
		advance_token(p)
	case .Nil, .True, .False:
		advance_token(p)
	case .Brace_Open: // set = "{" [element {"," element} [","]} "}"
		expect_token(p, .Bracket_Open)
		if !peek_token(p, .Bracket_Close) {
			parse_element(p)
			for !allow_token(p, .Comma) {
				parse_element(p)
			}
			allow_token(p, .Comma)
		}
		expect_token(p, .Bracket_Close)

	case .Paren_Open:
		expect_token(p, .Paren_Open)
		parse_expr(p)
		expect_token(p, .Paren_Close)
	case .Not:
		expect_token(p, .Not)
		parse_factor(p)
	case:
		parse_designator(p)
	}
}

// element = expr [".." expr]
parse_element :: proc(p: ^Parser) {
	parse_expr(p)
	if allow_token(p, .Ellipsis) {
		parse_expr(p)
	}
}

is_add_operator :: proc(kind: Token_Kind) -> bool {
	#partial switch kind {
	case .Add, .Sub, .Or:
		return true
	}
	return false
}

is_mul_operator :: proc(kind: Token_Kind) -> bool {
	#partial switch kind {
	case .Mul, .Quo, .Mod, .And:
		return true
	}
	return false
}


// ident_list = ident {"," ident}
parse_ident_list :: proc(p: ^Parser) {
	expect_token(p, .Ident)
	for allow_token(p, .Comma) {
		expect_token(p, .Ident)
	}
}

// type = qual_ident | struct_type
parse_type :: proc(p: ^Parser) {
	if peek_token(p, .Ident) {
		parse_qual_ident(p)
	} else {
		parse_struct_type(p)
	}
}

// qual_ident = [ident "."] ident
parse_qual_ident :: proc(p: ^Parser) {
	expect_token(p, .Ident)
	if allow_token(p, .Dot) {
		expect_token(p, .Ident)
	}
}

// struct_type = array_type | record_type | pointer_type | proc_type
parse_struct_type :: proc(p: ^Parser) {
	if allow_token(p, .Bracket_Open) {
		// array_type = "["" const_expr {"," const_expr} "]" type
		parse_const_expr(p)
		for allow_token(p, .Comma) {
			parse_const_expr(p)
		}
		expect_token(p, .Bracket_Close)
		parse_type(p)
	} else if allow_token(p, .Record) {
		// record_type = "record" ["(" qual_ident ")"] [field_list_sequence] "end"
		if allow_token(p, .Paren_Open) {
			parse_qual_ident(p)
			expect_token(p, .Paren_Close)
		}
		if !peek_token(p, .End) {
			parse_field_list_sequence(p)
		}
		expect_token(p, .End)
	} else if allow_token(p, .Caret) {
		// pointer_type = "^" type
		parse_type(p)
	} else if allow_token(p, .Proc) {
		// proc_type = "proc" formal_parameters
		parse_formal_parameters(p)
	}
}

// field_list_sequence = field_list {";" field_list} ";"
parse_field_list_sequence :: proc(p: ^Parser) {
	for {
		#partial switch p.curr_token.kind {
		case .End, .EOF:
			return
		}
		parse_field_list(p)
		if !allow_token(p, .Semicolon) {
			break
		}
	}
}

// field_list = ident_list ":" type
parse_field_list :: proc(p: ^Parser) {
	parse_ident_list(p)
	expect_token(p, .Colon)
	parse_type(p)
}


// formal_parmeters = "(" [fp_section {";" fp_section}] [";"] ")"
parse_formal_parameters :: proc(p: ^Parser) {
	expect_token(p, .Paren_Open)

	for !peek_token(p, .Paren_Close) && !peek_token(p, .EOF) {
		// fp_section {["var"] ident {"," ident} ":" formal_type}
		allow_token(p, .Var)
		expect_token(p, .Ident)
		for allow_token(p, .Comma) {
			expect_token(p, .Ident)
		}
		expect_token(p, .Colon)
		parse_formal_type(p)

		allow_token(p, .Semicolon)
	}

	expect_token(p, .Paren_Close)
}

// format_type = "[" "]" qual_ident
parse_formal_type :: proc(p: ^Parser) {
	if allow_token(p, .Bracket_Open) {
		expect_token(p, .Bracket_Close)
	}
	parse_qual_ident(p)
}

// stmt_sequence = stmt {";" stmt} [";"]
parse_stmt_sequence :: proc(p: ^Parser) {
	for {
		#partial switch p.curr_token.kind {
		case .EOF, .End, .Until, .Else, .Elseif:
			return
		}

		parse_stmt(p)
		if p.curr_token.kind != .Semicolon {
			break
		}
		allow_token(p, .Semicolon)
	}
}

// stmt = [assignment | proc_call | if_stmt | case_stmt | while_stmt | repeat_stmt | for_stmt ]
parse_stmt :: proc(p: ^Parser) {
	#partial switch p.curr_token.kind {
	case .If:
		parse_if_stmt(p)
	case .Case:
		parse_case_stmt(p)
	case .While:
		parse_while_stmt(p)
	case .Repeat:
		parse_repeat_stmt(p)
	case .For:
		parse_for_stmt(p)
	case .Semicolon:
		expect_token(p, .Semicolon)
	case:
		parse_designator(p)
		// assignment = designator ":=" expr
		if allow_token(p, .Assign) {
			parse_expr(p)
		}
	}
}

// if_stmt = "if" expr "then" stmt_sequence
//           {"elseif" expr "then" stmt_sequence}
//           ["else" stmt_sequence]
//           "end"
parse_if_stmt :: proc(p: ^Parser) {
	expect_token(p, .If)
	parse_expr(p)
	expect_token(p, .Then)
	parse_stmt_sequence(p)
	for allow_token(p, .Elseif) {
		parse_expr(p)
		expect_token(p, .Then)
		parse_stmt_sequence(p)
	}
	if allow_token(p, .Else) {
		parse_stmt_sequence(p)
	}
	expect_token(p, .End)
}

// case_stmt = "case" expr "of" case {"|" case} "end"
parse_case_stmt :: proc(p: ^Parser) {
	expect_token(p, .Case)
	parse_expr(p)
	expect_token(p, .Of)
	pase_case(p)
}

// case = [case_label_list ":" stmt_sequence]
pase_case :: proc(p: ^Parser) {
	parse_case_list(p)
	expect_token(p, .Colon)
	parse_stmt_sequence(p)
}

// case_list = label_range {"," label_range}
parse_case_list :: proc(p: ^Parser) {
	parse_label_range(p)
	for allow_token(p, .Comma) {
		parse_label_range(p)
	}
}

// label_range = label [".." label]
parse_label_range :: proc(p: ^Parser) {
	parse_label(p)
	if allow_token(p, .Ellipsis) {
		parse_label(p)
	}
}

// label = integer | string | qual_ident
parse_label :: proc(p: ^Parser) {
	#partial switch p.curr_token.kind {
	case .Integer: advance_token(p)
	case .String:  advance_token(p)
	case .Ident:   parse_qual_ident(p)
	case: syntax_error(&p.tok, p.curr_token, "expected an integer, string, or identifier as a label")
	}
}



// while_stmt = "while" expr "do" stmt_sequence
//              {"elseif" expr "then" stmt_sequence}
//              "end"
parse_while_stmt :: proc(p: ^Parser) {
	expect_token(p, .While)
	parse_expr(p)
	expect_token(p, .Do)
	parse_stmt_sequence(p)
	for allow_token(p, .Elseif) {
		parse_expr(p)
		expect_token(p, .Then)
		parse_stmt_sequence(p)
	}
	expect_token(p, .End)
}

// repeat_stmt = "repeat" stmt_sequence "until" expr
parse_repeat_stmt :: proc(p: ^Parser) {
	expect_token(p, .Repeat)
	parse_stmt_sequence(p)
	expect_token(p, .Until)
	parse_expr(p)
}

// for_stmt = "for" ident ":=" expr "to" expr ["by" const_expr] "do" stmt_sequence "end"
parse_for_stmt :: proc(p: ^Parser) {
	expect_token(p, .For)
	expect_token(p, .Ident)
	expect_token(p, .Assign)
	parse_expr(p)
	expect_token(p, .To)
	parse_expr(p)
	if allow_token(p, .By) {
		parse_const_expr(p)
	}
	expect_token(p, .Do)
	parse_stmt_sequence(p)
	expect_token(p, .End)
}


// designator = qual_ident {selector}
parse_designator :: proc(p: ^Parser) {
	parse_qual_ident(p)

	for {
		#partial switch p.curr_token.kind {
		case .Dot, .Bracket_Open, .Caret, .Paren_Open:
			parse_selector(p)
		case:
			return
		}
	}
}

// actual_parameters = "(" expr_list ")"
parse_actual_parameters :: proc(p: ^Parser) {
	expect_token(p, .Paren_Open)
	if !peek_token(p, .Paren_Close) {
		parse_expr_list(p)
	}
	expect_token(p, .Paren_Close)
}

// selector = "." ident | "[" expr_list "]" | "^" | "(" qual_ident ")"
parse_selector :: proc(p: ^Parser) {
	switch {
	case allow_token(p, .Dot):
		expect_token(p, .Ident)
	case allow_token(p, .Bracket_Open):
		parse_expr_list(p)
		expect_token(p, .Bracket_Close)

	case allow_token(p, .Caret):
		// okay
	case allow_token(p, .Paren_Open):
		parse_expr_list(p)
		expect_token(p, .Paren_Close)
	}
}

// expr_list = expr {"," expr}
parse_expr_list :: proc(p: ^Parser) {
	parse_expr(p)
	for allow_token(p, .Comma) {
		parse_expr(p)
	}
}