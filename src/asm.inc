.import __asm_disassemble
.import __asm_include
.import __asm_origin
.import __asm_org_string
.import __asm_pcset
.import __asm_reset
.import __asm_resetpc
.import __asm_set_pc
.import __asm_startpass
.import __asm_tokenize
.import __asm_tokenize_pass
.import __asm_tokenize_pass1
.import __asm_tokenize_pass2
.import __asm_top

.scope asm
	disassemble    = __asm_disassemble
	include        = __asm_include
	origin         = __asm_origin
	org_string     = __asm_org_string
	pcset          = __asm_pcset
	reset          = __asm_reset
	resetpc        = __asm_resetpc
	setpc          = __asm_set_pc
	startpass      = __asm_startpass
	tokenize       = __asm_tokenize
	tokenize_pass  = __asm_tokenize_pass
	tokenize_pass1 = __asm_tokenize_pass1
	tokenize_pass2 = __asm_tokenize_pass2
	top            = __asm_top
.endscope

