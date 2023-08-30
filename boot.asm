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

.segment "SETUP"
;******************************************************************************
; BASIC header: SYS 4621
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0
	jmp start

.CODE
;******************************************************************************
; START
; Entrypoint to program
start:
	sei
	ldxy #__BSS_LOAD__
	stxy zp::tmp0
@zeromem:
	ldy #$00
	tya
	sty zp::gendebuginfo
	sta (zp::tmp0),y
	incw zp::tmp0
	ldxy zp::tmp0
	cmpw #(__BSS_LOAD__+__BSS_SIZE__)
	bne @zeromem
@zerozp:
	sta $00,x
	dex
	bne @zerozp

; initialize the JMP vector
	lda #$4c	; JMP
	sta $00

        ldx #<irq_handler
        ldy #>irq_handler
        lda #$20
        jsr irq::raster
	jsr src::new
	lda #<start
	sta $0316
	lda #>start
	sta $0317
	lda #<start
	sta $0318
	lda #>start
	sta $0319
	jsr $ffe7	; CLALL (close all files)
	lda #9
	sta zp::device

        jmp enter

; disassembly test TODO: delete
	ldxy #$100
	stxy zp::tmp0
	ldxy #test
	jsr asm::disassemble
	jmp *
test:	clc

;******************************************************************************
; IRQHANDLER
irq_handler:
	jmp $eb15	; ack timer and rti

;******************************************************************************
; ENTER
; Entrypoint after initialization, from here on we're safe to use the bitmap
; address space ($1000-$2000) as a bitmap
enter:
	ldx #$ff
	txs
	jsr asm::reset
	jsr src::new
	jsr edit::init
	jmp edit::run
