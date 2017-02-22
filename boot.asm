.include "asm.inc"
.include "bitmap.inc"
.include "irq.inc"
.include "text.inc"
.include "zeropage.inc"
.include "memory.inc"
.include "key.inc"

;------------------------------------------------------------------------------
.segment "SETUP"
.word head 
head: .word @Next 
.word .version
.byte $9e 
.asciiz "4621"
@Next: .word 0 
;------------------------------------------------------------------------------
start:
        ldx #<irq_handler
        ldy #>irq_handler 
        lda #$20
        jsr irq::raster
        jmp enter
;------------------------------------------------------------------------------
.CODE
enter:
        jsr bm::init
        jsr bm::clr
	lda #$00
	sta zp::curx
	sta zp::cury
main:
        lda #$05
        cmp $9004
        bne *-3

	jsr key::getch
	cmp #$00
	beq main

	cmp #$0d
	bne :+
	pha
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr asm::compile
	pla

:	jsr text::putch
	jsr text::update
	jsr text::status
	
	ldx #<mem::filebuffer
	ldy #>mem::filebuffer
	lda #10
	jsr text::print
	
        jmp main

irq_handler:
        jmp $eabf

