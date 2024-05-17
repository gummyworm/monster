.include "asm.inc"
.include "bitmap.inc"
.include "debug.inc"
.include "edit.inc"
.include "fastcopy.inc"
.include "finalex.inc"
.include "irq.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "module.inc"
.include "source.inc"
.include "vmem.inc"
.include "zeropage.inc"

.import __BSS_LOAD__
.import __BSS_SIZE__

.import __DATA_LOAD__
.import __DATA_RUN__
.import __DATA_SIZE__

.import __FASTTEXT_LOAD__
.import __FASTTEXT_SIZE__
.import __FASTTEXT_RUN__

.import __MACROCODE_LOAD__
.import __MACROCODE_RUN__
.import __MACROCODE_SIZE__

.import __IRQ_LOAD__
.import __IRQ_RUN__
.import __IRQ_SIZE__

;******************************************************************************
.macro relocate src, dst, dstbank, size
	ldxy src
	stxy r0
	ldxy dst
	stxy r2
	lda dstbank
	sta r4
	ldxy size
	stxy r6
	jsr reloc
.endmacro

.segment "SETUP"
;******************************************************************************
; BASIC header: SYS 4621
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0

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

	; install dummy IRQ
	ldxy #$eb15
	stxy $0314

	jsr fe3::init
	lda #FINAL_BANK_FASTCOPY
	jsr fcpy::init
	lda #FINAL_BANK_FASTCOPY2
	jsr fcpy::init

	ldxy #__BSS_LOAD__
	stxy zp::tmp0
@zerobss:
	ldy #$00
	sty mem::drive_err	; clear the drive error
	tya
	sta (zp::tmp0),y
	incw zp::tmp0
	ldxy zp::tmp0
	cmpw #(__BSS_LOAD__+__BSS_SIZE__)
	bne @zerobss

; relocate segments that need to be moved
; DATA
	ldxy #__DATA_LOAD__
	stxy zp::tmp0
	ldxy #__DATA_RUN__
	stxy zp::tmp2
@reloc:
	ldy #$00
	lda (zp::tmp0),y
	sta (zp::tmp2),y
	incw zp::tmp0
	incw zp::tmp2
	ldxy zp::tmp0
	cmpw #(__DATA_LOAD__+__DATA_SIZE__)
	bne @reloc

; FASTTEXT
; copy the fast text code to its bank
	ldxy #__FASTTEXT_LOAD__
	stxy zp::tmp0
	ldxy #__FASTTEXT_RUN__
	stxy zp::tmp2
@fasttxt:
	ldy #$00
	lda (zp::tmp0),y
	bank_store_byte #FINAL_BANK_FASTTEXT, zp::tmp2
	incw zp::tmp0
	incw zp::tmp2
	ldxy zp::tmp0
	cmpw #__FASTTEXT_LOAD__+__FASTTEXT_SIZE__
	bne @fasttxt

; MACRO
; copy the macro code to its bank
	relocate #__MACROCODE_LOAD__, #__MACROCODE_RUN__, #FINAL_BANK_MACROS, #__MACROCODE_SIZE__

	relocate #__IRQ_LOAD__, #__IRQ_RUN__, #FINAL_BANK_MAIN, #__IRQ_SIZE__

; load modules from disk to their designated bank
	jsr loadmods

; initialize the JMP vector
	lda #$4c		; JMP
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

	jsr $ffe7		; CLALL (close all files)

	; save current screen for debugger
	jsr dbg::save_progstate

	; TODO: enable write-protection for the $2000-$8000 blocks when
	; all SMC is removed from the segments in that range
	lda #$80
	sta $9c02	; enable 35K of RAM for final expansion

	lda #$80
	sta $028a	; repeat all characters
	sta $0291	; don't swap charset on C= + SHIFT

	jmp enter
@loading: .byte "initializing.."
@loadinglen=*-@loading

;******************************************************************************
; RELOC
; relocates code from 1 address to another
; IN:
;  - r0r1: source address
;  - r2r3: dest address
;  - r4:   dest bank
;  - r6:   number of bytes to copy
.proc reloc
@copy:	ldy #$00
	lda (r0),y
	bank_store_byte r4,r2
	incw r0
	incw r2
	decw r6
	ldxy r6
	cmpw #$00
	bne @copy
	rts
.endproc

;******************************************************************************
; LOADMODS
; Loads all modules into their designated locations in RAM
.proc loadmods
	inc $900f
	ldxy #@module_udgedit
	lda #MOD_UDGEDIT
	jsr mod::load
	dec $900f
	rts
@module_udgedit: .byte "udgedit.prg",0
.endproc


.CODE
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
