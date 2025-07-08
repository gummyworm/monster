.if 0 ; experimental WIP stuff

	pha
	lda #FINAL_BANK_BUFF
	bne jmp_proc_with_args

;*******************************************************************************
	.byte FINAL_BANK_BUFF

;*******************************************************************************
; JUMP PROC WITH ARGS
; Jumps to the procedure in the word that follows the JSR to this
; procedure.
; e.g.
;   jsr util20::jmp_proc_with_args
;   .word my_proc_target
; Note that although you must JSR to this procedure, it will return
; to the address already in the return spot on the stack before this
; is called.
; The return address from the JSR to this routine will be eaten, in other words
.export jmp_proc_with_args
.proc jmp_proc_with_args
	sta @save_a

	; save current status
	php
	pla

	; eat return address
	tsx
	inx
	inx

	; read the bank to switch to
	inx
	lda $100,x
	sta zp::banktmp

	; eat second return address
	inx
	inx

	; read the target address
	inx
	lda $100,x
	sta @target,x
	lda $101,x
	sta @target,x

	; update stack pointer
	txs

	; restore status and .A
	pha
@save_a=*+1
	lda #$00
	plp

	; execute the target procedure
@target=*+1
	jmp $f00d
.endproc

.endif
