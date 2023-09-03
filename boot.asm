.include "asm.inc"
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
	sta $0316		; BRK
	sta $0318		; NMI
	lda #>start
	sta $0317		; BRK
	sta $0319		; NMI
	jsr $ffe7	; CLALL (close all files)
	lda #9
	sta zp::device

	lda #$80
	sta $9c02	; enable 35K of RAM for final expansion

        jmp enter
@loading: .byte "initializing..."
@loadinglen=*-@loading

; disassembly test TODO: delete
	ldxy #$100
	stxy zp::tmp0
	ldxy #test
	jsr asm::disassemble
	jmp *
test:	jsr $e5b5


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
