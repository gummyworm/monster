.include "cursor.inc"
.include "finalex.inc"
.include "macros.inc"
.include "zeropage.inc"

;******************************************************************************
; MODULE addresses
; Modules use 24 bit addresses, the high byte directly corresponds to the
; bank selection register in the final expansion, so $a5 means bank 5
UDGEDITOR_ADDR = $a5a000

.CODE
;******************************************************************************
; ENTER
; Calls the module that is stored in the given bank
; .A the ID of the module to execute
.export __mod_enter
.proc __mod_enter
	pushcur
	tax
	lda banks,x
	sta zp::banktmp
	lda addrslo,x
	sta zp::bankjmpvec
	lda addrshi,x
	sta zp::bankjmpvec+1
	jsr fe3::call
	popcur
	rts
.endproc

.segment "SETUP"
;******************************************************************************
; LOAD
; Loads the given module name into the designated module location
; NOTE: only available during init
; IN:
;  - .A:  the module ID to load
;  - .XY: the filename to source the module binary from
.export __mod_load
.proc __mod_load
@filename=zp::tmp0
	stxy @filename

	; get the addresses
	tax
	lda banks,x
	sta $9c02	; set bank

	ldy #$00
@l0:	lda (@filename),y
	beq :+
	iny
	bne @l0
	beq @err	; unterminated filename

:	tya
	ldxy @filename
	jsr $ffbd	; SETNAM
	lda #$01
	ldx $ba		; last used device
	bne :+
	ldx #$0a	; default to #10
:	ldy #$01	; load to address stored in file
	jsr $ffba	; SETFLS

	lda #$00	; load (not verify)
	jsr $ffd5	; LOAD
	bcs @err

	lda #$80
	sta $9c02	; restore bank
	rts

@err:	inc $900f
	jmp @err
.endproc

.DATA
;******************************************************************************
; MODULE ADDRESSES
.define addrs UDGEDITOR_ADDR
addrslo: .lobytes addrs
addrshi: .hibytes addrs
banks:   .bankbytes addrs
