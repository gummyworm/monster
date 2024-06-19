.import __ctx_addparam
.import __ctx_getparams
.import __ctx_push
.import __ctx_pop
.import __ctx_getdata
.import __ctx_getline
.import __ctx_numlines
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
	params = 8	; 16 bytes
	numparams = 10  ; 1 byte
	lines = 11	; rest of buffer ($200-10)
.endscope

.scope macctx
	base = 0
	numparams = 1
	params = 3	; up to 4 params, 16-bit pointers
	cur = 5 	; ptr to current location being read in (lines)
	lines = 13	; rest of buffer ($200-13)
.endscope

.scope ctx
	addparam  = __ctx_addparam
	getparams = __ctx_getparams
	init      = __ctx_init
	numlines  = __ctx_numlines
	push      = __ctx_push
	pop       = __ctx_pop
	getline   = __ctx_getline
	getdata   = __ctx_getdata
	write     = __ctx_write
	rewind    = __ctx_rewind

	type = __ctx_type
.endscope
