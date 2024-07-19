.include "finalex.inc"
.include "macros.inc"
.include "memory.inc"

;******************************************************************************
; LOAD
; Reads a byte from the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
; OUT:
;  - .A: the byte at the physical address
.export __vmem_load
.proc __vmem_load
	jsr __vmem_translate
	jmp fe3::load
.endproc

;******************************************************************************
; LOAD OFF
; Reads a byte from the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
;  - .A: the offset of the virtual address to load
; OUT:
;  - .A: the byte at the physical address
.export __vmem_load_off
.proc __vmem_load_off
	sta zp::bankval
	jsr __vmem_translate
	jmp fe3::load_off
.endproc

;******************************************************************************
; STORE
; Stores a byte at the physical address associated with the given virtual
; address
; IN:
;  - .XY: the virtual address
;  - .A:  the byte to store
.export __vmem_store
.proc __vmem_store
	sta zp::bankval
	jsr __vmem_translate
	jmp fe3::store
.endproc

;******************************************************************************
; STORE OFF
; Stores a byte at the physical address associated with the given virtual
; address offset by the given offset.
; IN:
;  - .XY:         the virtual address
;  - .A:          the offset from the base address
;  - zp::bankval: the value to store
.export __vmem_store_off
.proc __vmem_store_off
	sta zp::bankoffset
	jsr __vmem_translate
	jmp fe3::store_off
.endproc

;******************************************************************************
; TRANSLATE
; Returns the physical address associated with the given virtual address
; IN:
;  - .XY: the virtual address
; OUT:
;  - .XY: the physical address
;  - .A:  the bank number of the physical address
.export __vmem_translate
.proc __vmem_translate
	lda #FINAL_BANK_USER	; default to user's bank
	cmpw #$100
	bcs :+

@00:	; $00-$100 is stored in the prog00 buffer
	add16 #(mem::prog00-$00)
	lda #FINAL_BANK_MAIN
	rts

:	cmpw #$1000
	bcc @done
	cmpw #$1100
	bcs :+

@1000:	; $1000-$1100 is stored in the prog1000 buffer
	add16 #(mem::prog1000-$1000)
	lda #FINAL_BANK_MAIN
	rts

:	cmpw #$2000
	bcs :+

@1100:	; $1100-$2000 is stored in the "fast copy" bank
	add16 #($a000-$1100)
	lda #FINAL_BANK_FASTCOPY
	rts

:	cpy #>$9000
	bcc @done
	cpx #<$9010
	bcs :+

@9000:	; $9000-$9010 is stored in the prog9000 buffer
	add16 #(mem::prog9000-$9000)
	lda #FINAL_BANK_MAIN
	rts

:	cpy #>$9400
	bne @done

@9400:	; $9400-$9500 is stored in the prog9400 buffer
	add16 #(mem::prog9400-$9400)
	lda #FINAL_BANK_MAIN
	rts

@done:	; everything else is stored at its unaltered address in the
	; USER bank
	lda #FINAL_BANK_USER
	rts
.endproc
