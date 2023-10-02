.include "asm.inc"
.include "debug.inc"
.include "edit.inc"
.include "fastcopy.inc"
.include "finalex.inc"
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

	; restore default KERNAL vectors
	jsr $fd52

	; print loading message
	ldx #$00
:	lda @loading,x
	jsr $ffd2
	inx
	cpx #@loadinglen
	bne :-

	; install dummmy IRQ
	ldxy #$eb15
	stxy $0314

	jsr fe3::init
	lda #FINAL_BANK_FASTCOPY
	jsr fcpy::init
	lda #FINAL_BANK_FASTCOPY2
	jsr fcpy::init

	ldxy #__BSS_LOAD__
	stxy zp::tmp0
@zeromem:
	ldy #$00
	tya
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
	sta zp::jmpaddr

	ldx #<irq::sys_update
        ldy #>irq::sys_update
        lda #$20
        jsr irq::raster
	lda #<start
	sta $0316		; BRK
	sta $0318		; NMI
	lda #>start
	sta $0317		; BRK
	sta $0319		; NMI
	jsr $ffe7	; CLALL (close all files)
	lda #$a
	sta zp::device

	; save current screen for debugger
	jsr dbg::save_progstate

	lda #$80
	sta $9c02	; enable 35K of RAM for final expansion

	jmp enter

@loading: .byte "initializing..."
@loadinglen=*-@loading

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
