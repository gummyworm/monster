.include "asm.inc"
.include "edit.inc"
.include "irq.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "zeropage.inc"

.import __BSS_LOAD__
.import __BSS_SIZE__

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
	sei
	ldxy #__BSS_LOAD__
	stxy zp::tmp0
@zeromem:
	ldy #$00
	sta (zp::tmp0),y
	incw zp::tmp0
	ldxy zp::tmp0
	cmpw #(__BSS_LOAD__+__BSS_SIZE__)
	bne @zeromem

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
	jsr $ffe7	; CLALL (close all files)
	lda #9
	sta zp::device

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
	inc $900f
	jmp $eb15	; ack timer and rti
.endproc

;--------------------------------------
irq_handler:
	jmp $eb15	; ack timer and rti

;--------------------------------------
enter:
	jsr asm::reset
	jsr src::new

;--------------------------------------
reenter:
	ldx #$ff
	txs
	jsr edit::init
	jmp edit::run
