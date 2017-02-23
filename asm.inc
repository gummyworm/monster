.import __asm_compile
.import __asm_reporterr

.scope asm
	; assembles the line of code in (>Y/<X)
	compile	 = __asm_compile
	reporterr = __asm_reporterr
.endscope

