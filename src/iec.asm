.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "util.inc"
.include "zeropage.inc"

.CODE

;*******************************************************************************
; READERR
; Reads the drive's error into mem::drive_err (0-terminated)
; OUT:
;  - .X:             the error code
;  - mem::drive_err: the drive error message
.export __io_readerr
.proc __io_readerr
@ch=zp::tmpf
	lda #$00	; no filename
	tax
	tay
	sta @ch
	jsr $ffbd	; SETNAM

	lda #$0f	; file number 15
	ldx zp::device
	tay		; secondary address 15 (error channel)
	jsr $ffba	; SETLFS
	jsr $ffc0	; OPEN
	bcc @ok
	rts

@ok:	ldx #$0f	; filenumber 15
	jsr $ffc6	; CHKIN (file 15 now used as input)

	; read the error message to mem::drive_err
@loop:	jsr $ffb7	; READST (read status byte)
	bne @eof	; either EOF or read error

	jsr $ffcf	; CHRIN (get a byte from file)
	cmp #$0d
	beq @eof
	ldx @ch
	cpx #40		; cap size of string
	bcs @done
	sta mem::drive_err,x
	inc @ch
	bne @loop	; next byte

@eof:	ldx @ch
	lda #$00
	sta mem::drive_err,x

@done:	; close the command channel (file 15)
	lda #15		; filenumber 15 (command channel)
	jsr $ffc3	; CLOSE 15
	jsr $ffcc	; CLRCHN

	ldxy #mem::drive_err
	jmp atoi
.endproc
