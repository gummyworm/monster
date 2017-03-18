.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
.include "irq.inc"
.include "text.inc"
.include "zeropage.inc"
.include "memory.inc"
.include "key.inc"
.include "util.inc"

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
	jmp @newl

@chkop: 
	cmp #ASM_OPCODE
	bne @err
	jsr text::fmtopcode
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda #$00
	sta text::colstart
	lda zp::cury
	jsr text::puts
	jmp @newl

@err:	lda #$00
	sta zp::curx
	jmp @txtdone
	
@newl:	inc zp::cury
	lda #$00
	sta zp::curx
	lda #39
	sta zp::tmp0
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	lda #' '
	jsr util::memset
	jmp @txtdone

@cont:	pla
@putc:
	jsr text::putch
@txtdone:
	jsr text::update
	jsr text::status

        jmp main

irq_handler:
        jmp $eabf

