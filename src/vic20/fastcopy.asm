.include "../macros.inc"
.include "../ram.inc"
.include "../screen.inc"
.include "../zeropage.inc"

;******************************************************************************
; CONSTANTS

.SEGMENT "FASTCOPY"

;******************************************************************************
; SCREEN
; Copies the screen ($1100) to ($a000)
; this is the address that the generated unrolled loop
; will reside in
.export __fastcopy_save
__fastcopy_save = $2000

.export __fastcopy_restore
__fastcopy_restore = $2000 + $2f00

.export __fast_clr
__fast_clr = $a000	; fast clear entrypoint
fast_clr2 = $727f	; clear last 6 columns

.segment "SETUP"
;******************************************************************************
; INIT
; Initializes the fast copy routine to the given bank
; IN:
;  - .A: the bank to store the fast copy code to
.export __fastcopy_init
.proc __fastcopy_init
@src=r1
@dst=r3
@cnt=r5
@addr=r7
@i=r9
@bank=ra
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
	jsr ram::store
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
	jsr ram::store
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
	jsr ram::store
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
	jsr ram::store
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
	jsr ram::store
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
	jsr ram::store
	incw @addr
	inc @i
	lda @i
	cmp #9
	bcc @restore_addendum

	ldxy @addr
	lda @bank
	jsr ram::store

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
	jmp __fastcopy_save+2
:	rts

@restore_suffix:
	inx
	cpx #2
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
	lda #$a9	; LDA #
	sta24 #FINAL_BANK_FAST, #__fast_clr

	lda #$00	; #$00
	sta @target
	sta24 #FINAL_BANK_FAST, #__fast_clr+1

	ldxy #__fast_clr+2
	stxy @addr

	lda #$11
	sta @target+1
@gen_sta:
	lda #$8d	; STA abs
	sta24 #FINAL_BANK_FAST, @addr
	incw @addr

	lda @target	; LSB
	sta24 #FINAL_BANK_FAST, @addr
	incw @addr

	lda @target+1	; MSB
	sta24 #FINAL_BANK_FAST, @addr
	incw @addr

	incw @target

	ldxy @addr
	cmpw #fast_clr2+(192*6*3)	; sizeof(sta abs)*192*6
	beq @done
	cmpw #__fast_clr+(192*14*3)+2	; sizeof(lda #$00)+192*14*sizeof(sta abs)
	bne @gen_sta

	lda #$4c	; JMP
	sta24 #FINAL_BANK_FAST, @addr
	incw @addr
	lda #<fast_clr2	; LSB
	sta24 #FINAL_BANK_FAST, @addr
	incw @addr
	lda #>fast_clr2	; MSB
	sta24 #FINAL_BANK_FAST, @addr
	incw @addr

	ldxy #fast_clr2
	stxy @addr
	jmp @gen_sta

@done:	lda #$60			; RTS
	sta24 #FINAL_BANK_FAST, @addr
	rts
.endproc
