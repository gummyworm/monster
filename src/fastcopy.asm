.include "bitmap.inc"
.include "finalex.inc"
.include "macros.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS

; $2000	- gencode start address
; 6     - sizeof(lda abs) + sizeof(sta abs)
; $ef0  - number of pixels to copy
GENCODE_END = ($2000 + 6*$ef0)

.SEGMENT "FASTCOPY"

;******************************************************************************
; SCREEN
; Copies the screen ($1100) to ($a000)
; this is the address that the generated unrolled loop
; will reside in
.export __fastcopy_save
__fastcopy_save = $2100

.export __fastcopy_restore
__fastcopy_restore = $2100 + $2f00

.export __fast_clr
__fast_clr = $2100

.segment "SETUP"
;******************************************************************************
; INIT
; Initializes the fast copy routine to the given bank
; IN:
;  - .A: the bank to store the fast copy code to
.export __fastcopy_init
.proc __fastcopy_init
@src=zp::tmp1
@dst=zp::tmp3
@cnt=zp::tmp5
@addr=zp::tmp7
@i=zp::tmp9
@bank=zp::tmpa
	sta @bank

	ldxy #BITMAP_ADDR
	stxy @src	; source for backup
	ldxy #$a000
	stxy @dst	; destination for backup
	ldxy #$f00/2
	stxy @cnt	; bytes to copy
	ldxy #__fastcopy_save
	stxy @addr

;--------------------------------------
; assemble the following code
;COPY:
; LDA $1100,x
; STA $a000,x
; LDA $1102,x
; STA $a002,x
; ...
; LDA $1ff0-1,x
; STA $aff0-1,x
; INX
; CPX #2
; BEQ DONE
; JMP COPY
;DONE:
; RTS
;--------------------------------------
; LDX #$00
	lda #$00
	sta @i
@prefix0:
	ldx @i
	lda @prefix,x
	sta zp::bankval
	ldxy @addr
	lda @bank
	jsr fe3::store
	incw @addr
	inc @i
	lda @i
	cmp #2
	bcc @prefix0

; COPY LDA ABS,X; STA ABS,X, ....
@copy_save:
	lda #$00
	sta @i
	lda @src
	sta @loadstore+1
	lda @src+1
	sta @loadstore+2

	lda @dst
	sta @loadstore+4
	lda @dst+1
	sta @loadstore+5

@l0:	ldx @i
	lda @loadstore,x
	sta zp::bankval
	ldxy @addr
	lda @bank
	jsr fe3::store
	incw @addr
	inc @i
	lda @i
	cmp #6
	bcc @l0

	incw @src
	incw @src
	incw @dst
	incw @dst
	decw @cnt
	ldxy @cnt
	cmpw #0
	bne @copy_save

; copy the suffix
	lda #$00
	sta @i
@save_addendum:
	ldx @i
	lda @save_suffix,x
	sta zp::bankval
	ldxy @addr
	lda @bank
	jsr fe3::store
	incw @addr
	inc @i
	lda @i
	cmp #9
	bcc @save_addendum

@savescr_done:
;--------------------------------------
; assemble the following code
; LDA $a000
; STA $1100
; ...
; LDA $aeff
; STA $1fff
; RTS
;--------------------------------------
	ldxy #$a000
	stxy @src	; source for restore
	ldxy #BITMAP_ADDR
	stxy @dst	; destination for restore
	ldxy #$f00/2
	stxy @cnt	; bytes to copy
	ldxy #__fastcopy_restore
	stxy @addr

	lda #$00
	sta @i
@prefix1:
	ldx @i
	lda @prefix,x
	sta zp::bankval
	ldxy @addr
	lda @bank
	jsr fe3::store
	incw @addr
	inc @i
	lda @i
	cmp #2
	bcc @prefix1

; COPY LDA ABS,X; STA ABS,X, ....
@copy_restore:
	lda #$00
	sta @i
	lda @src
	sta @loadstore+1
	lda @src+1
	sta @loadstore+2

	lda @dst
	sta @loadstore+4
	lda @dst+1
	sta @loadstore+5

@l1:	ldx @i
	lda @loadstore,x
	sta zp::bankval
	ldxy @addr
	lda @bank
	jsr fe3::store
	incw @addr
	inc @i
	lda @i
	cmp #6
	bcc @l1

	incw @src
	incw @src
	incw @dst
	incw @dst
	decw @cnt
	ldxy @cnt
	cmpw #0
	bne @copy_restore

; copy the suffix
	lda #$00
	sta @i
@restore_addendum:
	ldx @i
	lda @restore_suffix,x
	sta zp::bankval
	ldxy @addr
	lda @bank
	jsr fe3::store
	incw @addr
	inc @i
	lda @i
	cmp #9
	bcc @restore_addendum

	ldxy @addr
	lda @bank
	jsr fe3::store

	jmp gen_bm_clr

@prefix:
	ldx #$00

@loadstore:
	lda $f00d,x
	sta $f00d,x

@save_suffix:
	inx
	cpx #2
	beq :+
	jmp $2102
:	rts

@restore_suffix:
	inx
	cpx #2
	beq :+
	jmp $5002
:	rts
.endproc

;******************************************************************************
.proc gen_bm_clr
@addr=zp::tmp0
@target=zp::tmp2
	lda #$a9	; LDA #
	sta zp::bankval
	ldxy #__fast_clr
	lda #FINAL_BANK_FAST
	jsr fe3::store

	lda #$00
	sta zp::bankval
	ldxy #__fast_clr+1
	lda #FINAL_BANK_FAST
	jsr fe3::store

	ldxy #__fast_clr+2
	stxy @addr
	ldxy #$1100
	stxy @target
@gen_sta:
	lda #$8d	; STA ABS
	sta zp::bankval
	ldxy @addr
	lda #FINAL_BANK_FAST
	jsr fe3::store

	incw @addr
	lda @target
	sta zp::bankval
	ldxy @addr
	lda #FINAL_BANK_FAST
	jsr fe3::store

	incw @addr
	lda @target+1
	sta zp::bankval
	ldxy @addr
	lda #FINAL_BANK_FAST
	jsr fe3::store

	incw @target

	incw @addr
	ldxy @addr
	cmpw #__fast_clr+($f00*3)+2	; sizeof(lda #$00)+$ef0*sizeof(sta abs)
	bne @gen_sta

	lda #$60			; RTS
	sta zp::bankval
	ldxy @addr
	lda #FINAL_BANK_FAST
	jsr fe3::store
	rts
.endproc
