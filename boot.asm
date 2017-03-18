.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
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
	bne @putc
	pha
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr asm::compile

@chklbl:
	cmp #ASM_LABEL
	bne @chkop
	jsr text::fmtlabel
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda zp::cury
	jsr text::puts
	jmp @cont

@chkop: 
	cmp #ASM_OPCODE
	bne @cont
	jsr text::fmtopcode
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda #$00
	sta text::colstart
	lda zp::cury
	jsr text::puts
	jmp @cont

@cont:	pla
@putc:
	jsr text::putch
	jsr text::update
	jsr text::status

        jmp main

irq_handler:
        jmp $eabf

