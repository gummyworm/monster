.include "zeropage.inc"
.include "memory.inc"
.include "layout.inc"

.CODE
;******************************************************************************
; READERR
; Reads the drive's error into $100 (0-terminated)
; NOTE: file #15 is opened and should be closed by the caller when done
; OUT:
;  $100: the drive error message
.export __io_readerr
.proc __io_readerr
@ch=zp::tmpf
	lda #$00	; no filename
	sta @ch
	jsr $ffbd	; SETNAM

	lda #$0f	; file number 15
	ldx #$09	; default to device 9
	ldy #$0f	; secondary address 15 (error channel)
	jsr $ffba	; SETLFS
	jsr $ffc0	; OPEN
	bcs @done	; if carry set, file could not be opened

	ldx #$0f	; filenumber 15
	jsr $ffc6	; CHKIN (file 15 now used as input)

@loop:
	jsr $ffb7	; READST (read status byte)
	bne @eof	; either EOF or read error
	jsr $ffcf	; CHRIN (get a byte from file)
	ldx @ch
	cpx #20		; cap size of string
	bcs @done
	sta $0100,x	; call CHROUT (print byte to screen)
	inc @ch
	bne @loop	; next byte

@eof:
@done:
.ifdef DRAW_DRIVE_ERR
	ldx @ch
	dex
	dex
:	lda $0100,x
	sta mem::statusline+DRIVE_ERR_COL,x

	dex
	bpl :-
.endif
	; TODO: why does save fail if this is closed?
	;lda #$0f
	;jsr $ffc3	; CLOSE
	rts
.endproc
