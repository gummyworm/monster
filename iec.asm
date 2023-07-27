.include "zeropage.inc"
.include "memory.inc"
.include "layout.inc"

.CODE
;--------------------------------------
; readerr reads the drive's error into $100 (0-terminated)
;
.export __io_readerr
.proc __io_readerr
@ch=zp::tmpf
	lda #$00      ; no filename
	ldx #$00
	ldy #$00
	sta @ch

	jsr $ffbd	; SETNAM
	lda #$0f	; file number 15
	ldx $ba		; last used device number
	bne @skip
	ldx #$08	; default to device 8
@skip:
	ldy #$0f	; secondary address 15 (error channel)
	jsr $ffba	; SETLFS
	jsr $ffc0	; OPEN
	bcs @close	; if carry set, file could not be opened

	ldx #$0f	; filenumber 15
	jsr $ffc6	; CHKIN (file 15 now used as input)

@loop:
	jsr $ffb7	; READST (read status byte)
	bne @eof	; either EOF or read error
	jsr $ffcf	; CHRIN (get a byte from file)
	ldx @ch
	cpx #20		; cap size of string
	bcs @close
	sta $0100,x	; call CHROUT (print byte to screen)
	inc @ch
	bne @loop	; next byte

@eof:
@close:
	lda #$0f	; filenumber 15
	jsr $ffc3	; CLOSE

.ifdef DRAW_DRIVE_ERR
	ldx @ch
	dex
	dex
:	lda $0100,x
	sta mem::statusline+DRIVE_ERR_COL,x

	dex
	bpl :-
.endif
	jmp $ffcc	; CLRCHN
.endproc
