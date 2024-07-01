.include "bitmap.inc"
.include "finalex.inc"
.include "macros.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS

.SEGMENT "FASTCOPY"

; number of iterations to roll the copy/store routines
NUM_ITERATIONS = 4

.segment "VSCREEN"
vscreen:
	.res $1000

;******************************************************************************
.export __fast_clr
__fast_clr = $a000	; fast clear entrypoint
fast_clr2 = $727f	; clear last 6 columns

.export __fastcopy_save
__fastcopy_save = $727f-$168a

.export __fastcopy_restore
__fastcopy_restore = $727f-$168a-$168a

; from $2000 to fastcopy_save ($456b) is used for the screen

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
	ldxy #$f00/NUM_ITERATIONS
	stxy @cnt	; bytes to copy
	ldxy #__fastcopy_save
	stxy @addr

;--------------------------------------
; assemble the following code
;COPY:
; LDA $1100,x
; STA $a000,x
; LDA $1104,x
; STA $a004,x
; ...
; LDA $1ff0-3,x
; STA $aff0-3,x
; INX
; CPX #NUM_ITERATIONS
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

	lda @src
	clc
	adc #NUM_ITERATIONS
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #NUM_ITERATIONS
	sta @dst
	bcc :+
	inc @dst+1

:	decw @cnt
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
	ldxy #$f00/NUM_ITERATIONS
	stxy @cnt	; bytes to copy
	ldxy #__fastcopy_restore
	stxy @addr

	lda #$00
	sta @i

; copy LDX #$00
@prefix1:
	ldx @i
	lda @prefix,x		; LDX #$00
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
	lda @src		; get LSB of source address to copy
	sta @loadstore+1
	lda @src+1		; and MSB
	sta @loadstore+2

	lda @dst		; get LSB of dest address
	sta @loadstore+4
	lda @dst+1		; and MSB
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
	cmp #6			; sizeof(LDA abs)+sizeof(STA abs)
	bcc @l1

	lda @src
	clc
	adc #NUM_ITERATIONS
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #NUM_ITERATIONS
	sta @dst
	bcc :+
	inc @dst+1

:	decw @cnt
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
	cpx #NUM_ITERATIONS
	beq :+
	jmp __fastcopy_save+2
:	rts

@restore_suffix:
	inx
	cpx #NUM_ITERATIONS
	beq :+
	jmp __fastcopy_restore+2
:	rts
.endproc

;******************************************************************************
; GEN_BM_CLR
; Generates the code to clear the bitmap
; The entrypoint is at __fast_clr.
; The routine jumps to another block of RAM when the current one is full
.proc gen_bm_clr
@addr=r0
@target=r2
@row=r4
	bank_store_byte #FINAL_BANK_FAST, #__fast_clr, #$a9	; LDA #

	lda #$00
	sta @target
	bank_store_byte #FINAL_BANK_FAST, #__fast_clr+1 	; 0

	ldxy #__fast_clr+2
	stxy @addr

	lda #$11
	sta @target+1
@gen_sta:
	bank_store_byte #FINAL_BANK_FAST, @addr, #$8d		; STA Abs
	incw @addr

	bank_store_byte #FINAL_BANK_FAST, @addr, @target	; LSB
	incw @addr

	bank_store_byte #FINAL_BANK_FAST, @addr, @target+1	; MSB
	incw @addr

	incw @target

	ldxy @addr
	cmpw #fast_clr2+(192*6*3)	; sizeof(sta abs)*192*6
	beq @done
	cmpw #__fast_clr+(192*14*3)+2	; sizeof(lda #$00)+192*14*sizeof(sta abs)
	bne @gen_sta

	bank_store_byte #FINAL_BANK_FAST, @addr, #$4c		; JMP
	incw @addr
	bank_store_byte #FINAL_BANK_FAST, @addr, #<fast_clr2	; LSB
	incw @addr
	bank_store_byte #FINAL_BANK_FAST, @addr, #>fast_clr2	; MSB
	incw @addr

	ldxy #fast_clr2
	stxy @addr
	jmp @gen_sta

@done:	lda #$60			; RTS
	sta zp::bankval
	ldxy @addr
	lda #FINAL_BANK_FAST
	jsr fe3::store
	rts
.endproc
