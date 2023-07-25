.import __asm_addlabel
.import __asm_labelat
.import __asm_tokenize
.import __asm_label_address
.import __asm_reset

.scope asm
	addlabel = __asm_addlabel
	labelat = __asm_labelat
	label_address = __asm_label_address
	; assembles the line of code in (>Y/<X)
	tokenize = __asm_tokenize
	reset = __asm_reset
.endscope

