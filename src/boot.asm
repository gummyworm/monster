.include "asm.inc"
.include "bitmap.inc"
.include "config.inc"
.include "debug.inc"
.include "debuginfo.inc"
.include "draw.inc"
.include "edit.inc"
.include "fastcopy.inc"
.include "fasttext.inc"
.include "finalex.inc"
.include "irq.inc"
.include "labels.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "vmem.inc"
.include "zeropage.inc"

.import __SETUP_LOAD__
.import __SETUP_RUN__
.import __SETUP_SIZE__

.import __BANKCODE_LOAD__
.import __BANKCODE_RUN__
.import __BANKCODE_SIZE__

.import __BANKCODE2_LOAD__
.import __BANKCODE2_RUN__
.import __BANKCODE2_SIZE__

.import __BSS_LOAD__
.import __BSS_SIZE__

.import __DATA_LOAD__
.import __DATA_RUN__
.import __DATA_SIZE__

.import __DEBUGINFO_CODE_LOAD__
.import __DEBUGINFO_CODE_RUN__
.import __DEBUGINFO_CODE_SIZE__

.import __FASTTEXT_LOAD__
.import __FASTTEXT_SIZE__
.import __FASTTEXT_RUN__

.import __MACROCODE_LOAD__
.import __MACROCODE_RUN__
.import __MACROCODE_SIZE__

.import __VSCREEN_LOAD__
.import __VSCREEN_RUN__
.import __VSCREEN_SIZE__

.import __IRQ_LOAD__
.import __IRQ_RUN__
.import __IRQ_SIZE__

.import __LINKER_LOAD__
.import __LINKER_RUN__
.import __LINKER_SIZE__

.import __LABELS_LOAD__
.import __LABELS_RUN__
.import __LABELS_SIZE__

.import __UDGEDIT_LOAD__
.import __UDGEDIT_RUN__
.import __UDGEDIT_SIZE__

.import __CONSOLE_LOAD__
.import __CONSOLE_RUN__
.import __CONSOLE_SIZE__

.import __COPYBUFF_LOAD__
.import __COPYBUFF_RUN__
.import __COPYBUFF_SIZE__

.import __RODATA_LOAD__
.import __RODATA_RUN__
.import __RODATA_SIZE__

TOTAL_SIZE = __SETUP_SIZE__+__BANKCODE_SIZE__+__BANKCODE2_SIZE__+__DATA_SIZE__+__FASTTEXT_SIZE__+__MACROCODE_SIZE__+__VSCREEN_SIZE__+__IRQ_SIZE__+__LINKER_SIZE__+__LABELS_SIZE__+__UDGEDIT_SIZE__+__CONSOLE_SIZE__+__COPYBUFF_SIZE__+__RODATA_SIZE__+__DEBUGINFO_CODE_SIZE__

;******************************************************************************
; RELOC
; relocates code from 1 address to another
; IN:
;  - .A: destination bank
;  - r0r1: source address
;  - r2r3: dest address
;  - r4:   number of bytes to copy
.macro reloc
@src=r0
@dst=r2
@size=r4
@bank=r6
	sta @bank
	lda @size+1
	beq @lastpage

	ldy #$00
@pageloop:
	lda #$a1
	sta $9c02	; source bank

	lda (@src),y

	ldx @bank
	stx $9c02	; dest bank

	sta (@dst),y
	iny
	bne @pageloop
	inc @src+1
	inc @dst+1
	dec @size+1
	bne @pageloop

@lastpage:
	ldy @size
	dey
:	lda #$a1
	sta $9c02	; source bank
	lda (@src),y
	ldx @bank
	stx $9c02	; dest bank
	sta (@dst),y
	dey
	bne :-

@lastbyte:
	lda #$a1
	sta $9c02
	lda (@src),y
	ldx @bank
	stx $9c02
	sta (@dst),y

	lda #$a1
	sta $9c02
.endmacro

.segment "SETUP"

.ifndef CART
;******************************************************************************
; BASIC header: SYS 4621
.word @head
@head: .word @next
.word .version
.byte $9e
.asciiz "4621"
@next: .word 0
	jmp start

;******************************************************************************
; CART header and boot code
.else ; CART
.segment "CART"
.word START		; Entry point for power up
.word edit::init	; Entry point for warm start (RESTORE)

; cartridge header
.byte "a0",$C3,$C2,$CD	; "A0CBM"

; copy cart binary ($0000-$6000) to RAM
START:
	jsr $fd8d	; RAMTAS
	jsr $fd52	; init vectors
	jsr $fdf9	; init I/O
	jsr $e518	; init I/O 2

	ldx #@end-@unlock-1
:	lda @unlock,x
	sta $200,x
	dex
	bpl :-

	; run the unlock code
	jmp $200

@unlock:
	ldx $a000
	stx $a000
	sta $9c02
	cmp $9c02

	; activate ROM bank 0
	lda #$40
	sta $9c02

	; copy SETUP
	ldxy #$2000
	stxy r0
	ldxy #__SETUP_RUN__
	stxy r2

	ldx #>TOTAL_SIZE+1	; # of pages to copy
	ldy #$00
@reloc:
	lda (r0),y
	sta (r2),y
	iny
	bne @reloc
	inc r0+1
	inc r2+1
	dex			; next page
	bne @reloc

	; set default device number
	lda #DEFAULT_DEVICE
	sta zp::device

	jmp start
@end:
.segment "SETUP"
.endif	; CART

;******************************************************************************
; LOWINIT
; Code that is sensitive to initialization order
; This code loads the app and sets up various banked code.
; Once the initialization in complete, jumps to enter to begin the app
.proc lowinit
	sei
	lda #FINAL_BANK_FASTCOPY
	jsr fcpy::init
	lda #FINAL_BANK_FASTCOPY2
	jsr fcpy::init

	lda #CUR_BLINK_SPEED
	sta zp::curtmr

.ifdef CART
; CART init code; copy the application from ROM bank 1
	; copy the app and enter it
	lda #$41	; ROM 32k page #1
	sta $9c02

	; copy everything from $2000-$8000
	ldxy #$2000
	stxy r0
	ldx #$60	; 96 pages
	ldy #$00
@l0:	lda (r0),y	; reads from ROM in ROM bank 1
	sta (r0),y	; writes go to RAM in RAM bank 1
	iny
	bne @l0
	inc r0+1	; next page
	dex
	bne @l0

	lda #FINAL_BANK_MAIN
	sta $9c02
	jmp enter

.else
; DISK init code; load the application from file
	; load the app and enter it
	lda #FINAL_BANK_MAIN
	sta $9c02
	ldxy #@mainprg
	lda #@mainprg_len
	jsr $ffbd	; SETNAM
	lda #$01
	ldx $ba		; last used device
	bne :+
	ldx #$0a	; default to #10
:	ldy #$01	; load to address stored in file
	jsr $ffba	; SETLFS

	lda #$00	; load (not verify)
	jsr $ffd5	; LOAD
	jmp enter
@mainprg: .byte "masm.prg"
@mainprg_len=*-@mainprg
.endif
.endproc

;******************************************************************************
; START
; Entrypoint to program
start:
	sei

	; enable all memory
	lda #FINAL_BANK_MAIN
	sta $9c02

	; restore default KERNAL vectors
	jsr $fd52

	; install dummmy IRQ
	ldxy #$eb15
	stxy $0314

;--------------------------------------
; zero the BSS segment
	ldxy #__BSS_LOAD__
	stxy r0

	lda #$00
	ldy #$00
@zerobss:
	sta (r0),y
	iny
	bne @zerobss
	inc r0+1
	ldx r0+1
	cpx #>(__BSS_LOAD__+__BSS_SIZE__)
	bne @zerobss

	; TODO: double check this
	ldy #<(__BSS_LOAD__+__BSS_SIZE__)
	beq @zero_bss_done
@zerobss_last_page:
	sta (r0),y
	dey
	bne @zerobss_last_page
	sta (r0),y		; last byte

@zero_bss_done:

;--------------------------------------
; relocate segments that need to be moved
@cnt=r7
@relocs=r8
	sei
	lda #num_relocs
	sta @cnt
	ldxy #relocs
	stxy @relocs
@reloc:	ldy #$00
	lda (@relocs),y
	sta r0
	iny
	lda (@relocs),y
	sta r0+1

	; destination
	iny
	lda (@relocs),y
	sta r2
	iny
	lda (@relocs),y
	sta r2+1

	; size
	iny
	lda (@relocs),y
	sta r4
	iny
	lda (@relocs),y
	sta r4+1

	; bank
	iny
	lda (@relocs),y
	cmp #$ad
	bne :+

:	reloc

	lda @relocs
	clc
	adc #$07
	sta @relocs
	bcc :+
	inc @relocs+1
:
	dec @cnt
	bne @reloc

	lda #FINAL_BANK_FASTTEXT
	sta $9c02
	jsr ftxt::init

	; TODO: enable write-protection for the $2000-$8000 blocks when
	; all SMC is removed from the segments in that range
	lda #$a1
	sta $9c02	; enable 35K of RAM for final expansion

	; initialize the JMP vector
	lda #$4c		; JMP
	sta zp::jmpaddr

	; clean up files
	jsr $ffe7		; CLALL (close all files)

	lda #$80
	sta $028a	; repeat all characters
	sta $0291	; don't swap charset on C= + SHIFT

	; clear row colors
	lda #DEFAULT_900F
	ldx #22
:	sta mem::rowcolors,x
	dex
	bpl :-

	jmp lowinit

;******************************************************************************
; RELOCS
; Table of start and target addresses for segments that need to be relocated
relocs:
; BANK CODE
.word __BANKCODE_LOAD__, __BANKCODE_RUN__, __BANKCODE_SIZE__
.byte FINAL_BANK_MAIN

; BANK CODE 2
.word __BANKCODE2_LOAD__, __BANKCODE2_RUN__, __BANKCODE2_SIZE__
.byte FINAL_BANK_MAIN

; DATA
.word __DATA_LOAD__, __DATA_RUN__, __DATA_SIZE__
.byte FINAL_BANK_MAIN

; DEBUGINFO_CODE
.word __DEBUGINFO_CODE_LOAD__, __DEBUGINFO_CODE_RUN__, __DEBUGINFO_CODE_SIZE__
.byte FINAL_BANK_DEBUG

; FASTTEXT
.word __FASTTEXT_LOAD__, __FASTTEXT_RUN__, __FASTTEXT_SIZE__
.byte FINAL_BANK_FASTTEXT

; MACRO
.word __MACROCODE_LOAD__, __MACROCODE_RUN__, __MACROCODE_SIZE__
.byte FINAL_BANK_MACROS

; IRQ
.word __IRQ_LOAD__, __IRQ_RUN__, __IRQ_SIZE__
.byte FINAL_BANK_MAIN

; SCREEN
.word __VSCREEN_LOAD__, __VSCREEN_RUN__, __VSCREEN_SIZE__
.byte FINAL_BANK_VSCREEN

; LINKER
.word __LINKER_LOAD__, __LINKER_RUN__, __LINKER_SIZE__
.byte FINAL_BANK_LINKER

; LABELS
.word __LABELS_LOAD__, __LABELS_RUN__, __LABELS_SIZE__
.byte FINAL_BANK_SYMBOLS

; UDG EDITOR
.word __UDGEDIT_LOAD__, __UDGEDIT_RUN__, __UDGEDIT_SIZE__
.byte FINAL_BANK_UDGEDIT

; CONSOLE
.word __CONSOLE_LOAD__, __CONSOLE_RUN__, __CONSOLE_SIZE__
.byte FINAL_BANK_CONSOLE

; COPYBUFF
.word __COPYBUFF_LOAD__, __COPYBUFF_RUN__, __COPYBUFF_SIZE__
.byte FINAL_BANK_BUFF

; RODATA
.word __RODATA_LOAD__, __RODATA_RUN__, __RODATA_SIZE__
.byte FINAL_BANK_MAIN

num_relocs=(*-relocs)/7

.CODE
;******************************************************************************
; ENTER
; Entrypoint after initialization, from here on we're safe to use the bitmap
; address space ($1000-$2000) as a bitmap
.export enter
.proc enter
        jsr irq::raster
	sei

	lda #<start
	sta $0316		; BRK
	lda #>start
	sta $0317		; BRK
	cli

	; initialize the status row reverse
	lda #DEFAULT_RVS
	ldx #23
	jsr draw::hline

	jsr asm::reset
	jsr src::new

	; initialize bitmap
	jsr bm::init
	jsr edit::clear

	jsr dbgi::initonce
	jsr asm::reset

.ifndef TEST
	jmp edit::init
.else
	.import testsuite
	jmp testsuite
.endif
.endproc
