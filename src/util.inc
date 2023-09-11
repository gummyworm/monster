.include "zeropage.inc"

.import __util_chtohex
.import __util_hextostr
.import __util_is_whitespace
.import __util_is_null_return_space_comma_closingparen_newline
.import __util_memset
.import __util_memcpy
.import __util_todec
.import atoi
.import __util_isoperator
.import __util_is_separator
.import __util_is_alphanum

.scope util
	chtohex = __util_chtohex
	hextostr = __util_hextostr
	memset = __util_memset
	memcpy = __util_memcpy
	todec = __util_todec
	is_whitespace = __util_is_whitespace
	is_null_return_space_comma_closingparen_newline = __util_is_null_return_space_comma_closingparen_newline
	isoperator = __util_isoperator
	isseparator = __util_is_separator
	isalphanum =  __util_is_alphanum
.endscope

.macro copy dst, src, len
	ldxy src
	stxy zp::tmp2
	ldxy dst
	stxy zp::tmp4
	ldxy len
	stxy zp::tmp0
	jsr util::memcpy
.endmacro
