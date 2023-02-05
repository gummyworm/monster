.include "edit.inc"
.include "irq.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "zeropage.inc"

;#######################################
.segment "SETUP"
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0

;--------------------------------------
start:
        ldx #<irq_handler
        ldy #>irq_handler
        lda #$20
        jsr irq::raster
	jsr src::new
	lda #<brkhandler
	sta $0316
	lda #>brkhandler
	sta $0317
	lda #<nmihandler
	sta $0318
	lda #>nmihandler
	sta $0319
        jmp enter

;#######################################
.CODE

;--------------------------------------
.proc brkhandler
	inc $900f
	jmp *-3
	jmp reenter
.endproc

;--------------------------------------
.proc nmihandler
	jmp $eabf
.endproc

;--------------------------------------
irq_handler:
	jmp $eabf

;--------------------------------------
enter:
	; set address to assemble to
	ldxy #mem::program
	stxy zp::asmresult

	jsr src::new

reenter:
	ldx #$ff
	txs
	jsr edit::init
	jmp edit::run
