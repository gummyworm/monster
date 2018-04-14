.include "zeropage.inc"

.import __util_hline
.import __util_hextostr
.import __util_memset
.import __util_memcpy
.import __util_strncmp

.scope util
	hextostr = __util_hextostr
	hline = __util_hline
	memset = __util_memset
	memcpy = __util_memcpy
	strncmp = __util_strncmp
.endscope

; compares the the string in (YX) to the string s.
.macro streq s, len
	stx zp::tmp0
	sty zp::tmp0+1

	lda #<s
	sta zp::tmp2
	lda #>s
	sta zp::tmp2+1

	lda #len
	jsr __util_strncmp
.endmacro
