package titania

import "core:path/filepath"

main :: proc() {
	filename, _ := filepath.abs("test.titania")

	p: Parser
	if !parser_init(&p, filename) {
		return
	}
	defer parser_fini(&p)

	parse(&p)
}