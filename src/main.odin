package titania

import "core:path/filepath"
import "core:mem/virtual"
import "core:fmt"

main :: proc() {
	filename, _ := filepath.abs("test.titania")

	p: Parser
	if !parser_init(&p, filename) {
		return
	}
	defer parser_fini(&p)

	module: Module
	parse(&p, &module)

	info: Checker_Info
	checker_info_init(&info)

	check_module(&info, &module)
}