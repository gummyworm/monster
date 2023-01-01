.import __asm_addlabel
.import __asm_compile
.import __asm_labelat
.import __asm_reporterr
.import __asm_tokenize

.scope asm
	addlabel = __asm_addlabel
	labelat = __asm_labelat
	; assembles the line of code in (>Y/<X)
	compile	 = __asm_compile
	reporterr = __asm_reporterr
	tokenize = __asm_tokenize
.endscope

