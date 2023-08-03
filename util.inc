.include "zeropage.inc"

.import __util_chtohex
.import __util_hextostr
.import __util_is_whitespace
.import __util_is_null_return_space_comma_closingparen_newline
.import __util_memset
.import __util_memcpy
.import __util_strncmp
.import __util_strlen
.import __util_todec
.import atoi
.import __util_isoperator
.import __util_is_separator

.scope util
	chtohex = __util_chtohex
	hextostr = __util_hextostr
	memset = __util_memset
	memcpy = __util_memcpy
	strncmp = __util_strncmp
	strlen = __util_strlen
	todec = __util_todec
	is_whitespace = __util_is_whitespace
	is_null_return_space_comma_closingparen_newline = __util_is_null_return_space_comma_closingparen_newline
	isoperator = __util_isoperator
	isseparator = __util_is_separator
.endscope

.macro copy dst, src, len
	sei
	ldxy src
	stxy zp::tmp2
	ldxy dst
	stxy zp::tmp4
	ldxy len
	stxy zp::tmp0
	jsr util::memcpy
.endmacro

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
