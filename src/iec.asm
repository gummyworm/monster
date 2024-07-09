.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "zeropage.inc"

.CODE

;******************************************************************************
; READERR
; Reads the drive's error into mem::drive_err (0-terminated)
; NOTE: file #15 is opened and should be closed by the caller when done
; OUT:
;  - .X:             the error code
;  - mem::drive_err: the drive error message
.export __io_readerr
.proc __io_readerr
@ch=zp::tmpf
	lda #$00	; no filename
	sta @ch
	jsr $ffbd	; SETNAM

	lda #$0f	; file number 15
	ldx #$0a	; default to device 10
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
	sta mem::drive_err,x
	inc @ch
	bne @loop	; next byte

@eof:
@done:	ldxy #mem::drive_err
	jmp atoi
.endproc
