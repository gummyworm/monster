.import __ctx_push
.import __ctx_pop
.import __ctx_getline
.import __ctx_rewind
.import __ctx_write
.import __ctx_init
.import __ctx_type

; the max number of lines stored in contexts for macros
MAX_CTX_LINES = 16

CTX_REPEAT = 1
CTX_MACRO = 2

CONTEXT_REPEAT_SIZE = 16*40+4

.scope repctx
	base = 0
	iter = 2
	iter_end = 4
	cur = 6 	; ptr to current location being read in (lines)
	param = 8	; 16 bytes
	lines = 22+40*MAX_CTX_LINES
.endscope

.scope ctx
	init = __ctx_init
	push = __ctx_push
	pop = __ctx_pop
	getline = __ctx_getline
	write = __ctx_write
	rewind = __ctx_rewind

	type = __ctx_type
.endscope
