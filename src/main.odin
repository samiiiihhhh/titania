package titania

import "core:path/filepath"
import "core:os"
import "core:fmt"

main :: proc() {
	filename, _ := filepath.abs("test.titania")

	data, _ := os.read_entire_file(filename)
	defer delete(data)

	tok: Tokenizer
	tokenizer_init(&tok, filename, string(data))

	for {
		token := get_token(&tok)
		fmt.printfln("%20s %s", token.kind, token.text)
		if token.kind == .EOF {
			break
		}
	}
}