.import __asm_disassemble
.import __asm_tokenize
.import __asm_reset
.import __asm_verify

.scope asm
	disassemble = __asm_disassemble
	; assembles the line of code in (>Y/<X)
	tokenize = __asm_tokenize
	reset = __asm_reset
	verify = __asm_verify
.endscope

