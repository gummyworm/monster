.import __asm_addlabel
.import __asm_compile
.import __asm_labelat
.import __asm_reporterr
.import __asm_result
.import __asm_tokenize
.import __asm_setpc
.import __asm_pc
.import __asm_advancepc

.scope asm
	pc = __asm_pc
	addlabel = __asm_addlabel
	labelat = __asm_labelat
	; assembles the line of code in (>Y/<X)
	compile	 = __asm_compile
	reporterr = __asm_reporterr
	result = __asm_result
	setpc = __asm_setpc
	tokenize = __asm_tokenize
	advancepc = __asm_advancepc
.endscope

