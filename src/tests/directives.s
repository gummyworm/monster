.org $7700
.eq CONST 100
	lda #CONST
	sta $1000
.if 1
	.db "hi"
.endif

.if 0
	.db "hiiiiiii"
.endif
