.import __asm_disassemble
.import __asm_origin
.import __asm_tokenize
.import __asm_tokenize_pass
.import __asm_tokenize_pass1
.import __asm_tokenize_pass2
.import __asm_reset
.import __asm_resetpc

.scope asm
	disassemble = __asm_disassemble
	; assembles the line of code in (>Y/<X)
	origin = __asm_origin
	tokenize = __asm_tokenize
	tokenize_pass = __asm_tokenize_pass
	tokenize_pass1 = __asm_tokenize_pass1
	tokenize_pass2 = __asm_tokenize_pass2
	reset = __asm_reset
	resetpc = __asm_resetpc
.endscope

