.include "settings.inc"
.include "../screen.inc"
.include "../irq.inc"
.include "../memory.inc"

NUM_PALETTES = 2
SCREEN_ROWS = 24

.export __prefs_reverse_color
.export __prefs_text_color
.export __prefs_normal_color


.DATA
__prefs_reverse_color: .byte DEFAULT_RVS
__prefs_text_color:    .byte TEXT_COLOR
__prefs_normal_color:  .byte DEFAULT_900F


pal_num: .byte 0

prev_reverse_color: .byte DEFAULT_RVS
prev_normal_color:  .byte DEFAULT_900F

.RODATA
palettes:
; default, white-on-black
text_colors:    .byte DEFAULT_RVS,  $01
normal_colors:  .byte TEXT_COLOR,   $08
reverse_colors: .byte DEFAULT_900F, $00

.CODE

;*******************************************************************************
; NEXT PAL
; Cycles to the next palette definition
.export __prefs_next_pal
.proc __prefs_next_pal
	ldx pal_num
	inx
	cpx #NUM_PALETTES
	bcc :+
	ldx #$00
:	bpl set_pal
.endproc

;*******************************************************************************
; PREV PAL
; Cycles to the previous palette definition
.export __prefs_prev_pal
.proc __prefs_prev_pal
	ldx pal_num
	dex
	bpl set_pal
	ldx #NUM_PALETTES-1

	; fall through to set_pal
.endproc

;*******************************************************************************
; SET PAL
; Sets the palette to the one of the given id
; IN:
;   - .X: the id of the palette to switch to
.proc set_pal
	jsr irq::off
	inc $900f
	stx pal_num

	lda text_colors,x
	sta __prefs_text_color

	lda normal_colors,x
	sta __prefs_normal_color
	sta $900f

	lda reverse_colors,x
	sta __prefs_reverse_color

	; set the row colors to their new proper colors
	ldx #SCREEN_ROWS-1
@l0:	lda mem::rowcolors,x
	cmp prev_normal_color
	bne :+
	lda __prefs_normal_color
	sta mem::rowcolors,x
	jmp :++
:	cmp prev_reverse_color
	bne :+
	lda __prefs_reverse_color
	sta mem::rowcolors,x
:	dex
	bpl @l0

	jsr scr::clrcolor
	jmp irq::on
.endproc
