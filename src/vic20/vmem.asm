.include "finalex.inc"
.include "ram.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../nmi.inc"

.CODE

;*******************************************************************************
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
	jsr nmi::off
	jsr fe3::load
	jmp nmi::on
.endproc

;*******************************************************************************
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
	jsr nmi::off
	jsr fe3::load_off
	jmp nmi::on
.endproc

;*******************************************************************************
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
	jsr nmi::off
	jsr fe3::store
	jmp nmi::on
.endproc

;*******************************************************************************
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

;*******************************************************************************
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
	cpy #>$0400
	bcs :+

@00:	; $00-$400 is stored in the prog00 buffer
	add16 #(mem::prog00-$00)
	lda #FINAL_BANK_MAIN
	rts

:	cpy #>$1000
	bcc @done
	cpy #>$1100
	bcs :+

@1000:	; $1000-$1100 is stored in the prog1000 buffer
	add16 #(mem::prog1000-$1000)
	lda #FINAL_BANK_MAIN
	rts

:	cpy #>$2000
	bcs :+

@1100:	; $1100-$2000 is stored in the "fast copy" bank
	add16 #($a000-$1100)
	lda #FINAL_BANK_FASTCOPY
	rts

:	cpy #>$9000
	bne :+
	cpx #<$9010
	bcs @done		; $9010-$9100 is not buffered anywhere

@9000:	; $9000-$9010 is stored in the prog9000 buffer
	add16 #(mem::prog9000-$9000)
	lda #FINAL_BANK_MAIN
	rts

:	cpy #>$9400
	bne @done

@9400:	; $9400-$94f0 is stored in the prog9400 buffer
	cpx #$f0
	bcs @done			; if addr > $94ef, not virtual
	add16 #(mem::prog9400-$9400)
	lda #FINAL_BANK_MAIN
	rts

@done:	; everything else is stored at its unaltered address in the
	; USER bank
	lda #FINAL_BANK_USER
	rts
.endproc

;*******************************************************************************
; WRITABLE
; Checks if the given address is within the valid writable range.
; This includes the addresses [$00, $8000) and [$a000, $c000)
; IN:
;   - .XY: the address to check for writability
; OUT:
;   - .C: set if the address is NOT writable
.export __vmem_writable
.proc __vmem_writable
	cmpw #$8000
	bcc @done	; [$00, $8000) -> writable
	cmpw #$c000
	bcs @done	; [$c000, $ffff) -> not writable
	cmpw #$a000
	bcs @writable
	sec		; [$8000, $a000) -> not writable
	rts
@writable:
	clc
@done:	rts
.endproc
