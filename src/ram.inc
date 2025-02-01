.ifndef RAM_INC
.include "zeropage.inc"

;******************************************************************************
; SET SEGMENT
; Equivalent to the `.segment` directive, but also sets a variable so that
; other RAM macros know which bank the calling code resides in.
; It is important NOT to use other segment-setting directives (.segment,
; .CODE, .BSS, .DATA, or .RODATA) in order for this variable to correctly be
; set.
.macro SET_SEGMENT name
	__current_bank .set bank
.endmacro

;;******************************************************************************
;; CALL
;; Calls the given routine.  If the routine is within the same bank as the
;; active segment, will evaluate to a simple JSR.  If not, will execute the
;; platform-specific code to call the routine
;.macro CALL bank, proc
;	.if bank = __current_bank
;		; target procedure is in current bank
;		jsr proc
;	.else
;		; target procedure is in another bank
;	.endif
;.endmacro
;
;;******************************************************************************
;; JUMP
;; Jumps to the given routine.  If the routine is within the same bank as the
;; active segment, will evaluate to a simple JMP.  If not, will execute the
;; platform-specific code to jump to the routine
;.macro JUMP bank, proc
;	.if bank = __current_bank
;		; target procedure is in current bank
;		jmp proc
;	.else
;		; target procedure is in another bank
;	.endif
;.endmacro

.ifdef vic20
.import __final_call
.import __final_store_byte
.import __final_load_byte
.import __final_load_byte_off
.import __final_memcpy
.import __final_copy
.import __final_copy_line
.import __final_bank_store_rel
.endif

.scope ram
	call      = __final_call
	fcopy     = __final_copy
	copy      = __final_memcpy
	copyline  = __final_copy_line
	store     = __final_store_byte
	store_off = __final_bank_store_rel
	load      = __final_load_byte
	load_off  = __final_load_byte_off
.endscope

;******************************************************************************
; LDA24
; Loads the given 24 bit address into .A
.macro lda24 bank, addr, offset
.ifdef vic20
.ifnblank offset
	lda offset
	sta zp::bankval
.endif
.ifnblank addr
	ldxy addr
.endif
	lda bank

.ifblank offset
	jsr __final_load_byte
.else
	lda bank
	jsr __final_load_byte_off
.endif
.endif
.endmacro

;******************************************************************************
; STA24
; Stores the value in .A to the given 24 bit address
.macro sta24 bank, addr
.ifdef vic20
	sta zp::bankval
	ldxy addr
	lda bank
	jsr __final_store_byte
.endif
.endmacro

;******************************************************************************
; MOV
; Moves the value from the given 24-bit source to the given 24-bit destination
.macro mov dst24, src24
	.if (.match (.left (1, {src24}), #))
		; immediate mode, just load .A with the immediate value
		lda src24
	.else
		lda24 src
	.endif
	sta24 dst24
.endmacro
.endif
