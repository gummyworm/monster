;******************************************************************************
; INSTALL.ASM
; This file contains the code for the flash installer.
; This program reads the image file to flash to the Final Expansion 3 from disk
; and writes it to the FE3's internal flash memory at blocks 0 and 1.
;******************************************************************************
.include "../macros.inc"
.include "../zeropage.inc"

;******************************************************************************
; Constants
DEVICE = 8	; device ID to read image from

;******************************************************************************
; 29F040 Constants
flash_base = $2000
flash_555  = flash_base + $555
flash_2aa  = flash_base + $2aa

FLASH_ALG_ERROR_BIT = $20

;******************************************************************************
; zeropage
load_ptr = $c3

.CODE

;******************************************************************************
; BASIC header: SYS 4621
	.word @head
@head:  .word @next
	.word .version
	.byte $9e
	.asciiz "4621"
@next:	.word 0

;******************************************************************************
; FLASHER
; Main flasher program
.proc flasher
	sta $a000	; unlock IO
	lda #$20	; Flash Write mode (bank 0)
	sta $9c02

	jsr test_eeprom	; make sure EEPROM is good
	bcs @err

	jsr is_blank	; check if we need to erase
	beq @flash	; if blank, no need to erase

	jsr erase
	bcs @err

	jsr is_blank	; check again if flash is blank
	beq @flash	; if so, continue to flashing procedure

; flash is bad or not blank even after erasing it
@err:	lda #<err_erase_failed
	ldy #>err_erase_failed
	jsr puts
	jmp *

@flash:	jsr FLASH_FIRMWARE
	jsr FlashCodeEndSequ	; RESET

@done:	lda #<done_msg
	ldy #>done_msg
	jsr puts
	lda #$00
	sta $9c02		; put FE3 in "start" mode
	jmp ($fffc)		; reset
.endproc

;******************************************************************************
; TEST EEPROM
; Tests that the EEPROM is the correct type by comparing its vendor ID
; to those supported
.proc test_eeprom
	jsr FlashCodeVendorID
	tya
	pha

	; check the vendor ID
	cpx #$01	; AMD
	beq @ok
	cpx #$20	; AMD clone
	beq @ok
	cpx #$37	; AMIC
	beq @ok
	cpx #$c2	; AMD by MX
	beq @ok

	pla
@err:	lda #<err_badflash_
	ldy #>err_badflash_
	jsr puts
	sec
	rts

@ok:	pla
	tax

	cpx #$e2
	beq @deviceok
	cpx #$86                              ; AMIC A29040B
	beq @deviceok
	cpx #$a4                              ; 29F040
	bne @err

@deviceok:
	clc
	rts
.endproc

;******************************************************************************
; FLASH CODE MAGIC
; Writes the "magic" flash sequence to enable writing a byte to flash
.proc flash_CodeMagic
	lda #$aa
	sta flash_555
	lda #$55
	sta flash_2aa
	rts
.endproc

;******************************************************************************
; FLASH CODE END
; Writes the "magic" end-sequence command to the flash
.proc FlashCodeEndSequ
	lda #$f0
	sta flash_base
	rts
.endproc

;******************************************************************************
; FLASH CODE SECTOR ERASE
; Writes the "magic" sequence to erase flash
.proc flash_CodeSectorErase
	jsr flash_CodeMagic
	lda #$80
	sta flash_555
	jsr flash_CodeMagic
	lda #$30
	sta flash_base
	rts
.endproc

;******************************************************************************
; FLASH CODE CHIP ERASE
; Writes the "magic" sequence to erase the chip
.proc flash_CodeChipErase
	jsr flash_CodeMagic
	lda #$80
	sta flash_555
	jsr flash_CodeMagic
	lda #$10
	sta flash_555
	rts
.endproc

;******************************************************************************
; FLASH CODE WRITE
; Writes the given byte to flash
; IN:
;   - .A
.proc flash_CodeWrite
	pha
	jsr flash_CodeMagic
	lda #$A0
	sta flash_555
	pla
	ldy #0
	sta (load_ptr),y
	rts
.endproc

;******************************************************************************
; FLASH CHECK PROGRESS
; Checks the progress of the flash
; OUT:
;   - .C: clear if OK, set if an error has occurred
.proc flash_CodeCheckProgress                 ; TOGGLE CHECK
	pha
	txa
	pha

	; make sure we can read stable values from flash_base
@l0:	ldx #2
@l1:	lda flash_base
	cmp flash_base
	beq @next

	and #FLASH_ALG_ERROR_BIT
	beq @l0		; if error bit not set, try again

	; try again
	lda flash_base
	cmp flash_base
	beq @ok

	; if retry failed, return an error
	jsr FlashCodeEndSequ
	sec
	bcs @err

@next:	dex
	bne @l1
@ok:	clc

@err:	pla	; restore .X and .A
	tax
	pla
	rts
.endproc

;******************************************************************************
; GET VENDOR AND DEVICE
; Returns the vendor and device ID for the flash
FlashCodeVendorID:
.proc get_vendor_device
; OUT:
;   - .X: the vendor ID
;   - .Y: the device ID
	jsr flash_CodeMagic
	lda #$90
	sta flash_555
	ldx flash_base
	ldy flash_base+1
	jmp FlashCodeEndSequ
.endproc

;******************************************************************************
;ERASE SECTOR
; Sends the code to erase a sector to the flash
.proc FlashCodeSectorErase
	jsr flash_CodeSectorErase
	jmp flash_CodeCheckProgress
.endproc

;******************************************************************************
; PUTS
; Prints the given string
; IN:
;   - .AY: the string to display
.proc puts
	sta r0
	sty r0+1
	ldy #0
:	lda (r0),y
	beq @done
	jsr $ffd2
	iny
	bne :-
@done:	rts
.endproc

;******************************************************************************
; FLASH FIRMWARE
; Flashes the binary image to flash
.proc FLASH_FIRMWARE
	lda #<msgflash_ing
	ldy #>msgflash_ing
	jsr puts

	lda #image_name_size
	ldy #>image_name
	ldx #<image_name
	jsr $ffbd	; SETNAM

	lda #3		; SA
	ldy #3
	ldx #DEVICE
	jsr $ffba	; SETLFS

	jsr $ffc0	; OPEN
	bcs @err

	ldx #3
	jsr $ffc6	; CHKIN
	jsr $ffb7	; READST
	cmp #$00
	beq @ok

@err:	lda #<err_open_failed
	ldy #>err_open_failed
	jsr puts
	jmp *

@ok:	; init load pointer to $2000
	ldxy #$2000
	stxy load_ptr

; read the image and flash it
@l0:
	jsr $ffb7	; READST
	cmp #$00
	bne @cleanup
	inc $900f
	jsr $ffcf	; call CHRIN (get a byte from file)
	dec $900f
	jsr flash_byte
	jmp @l0

@cleanup:
	lda #3
	jsr $ffc3	; CLOSE
	rts
.endproc

;******************************************************************************
; FLASH BYTE
; Writes one byte of the flash image to the flash chip at the current write
; pointer.
; IN:
;   - .A:        the byte to write
;   - WRITE_PTR: the address to write to in the current bank
.proc flash_byte
	jsr flash_CodeWrite
	jsr flash_CodeCheckProgress
	bcs @err

	cmp (load_ptr),y	; make sure byte has been written
	bne @err

	incw load_ptr

	lda load_ptr+1
	cmp #$80	; end of the $2000-$8000 block?
	beq @seta0
	cmp #$c0
	beq @nextbank
	rts

@seta0:	; finished flashing $2000-$8000, bump to $a000
	ldxy #$a000
	stxy load_ptr
	rts

@nextbank:
	inc $9c02	; move to next bank
	ldxy #$2000
	stxy load_ptr	; move back to start of flash in new bank
	rts

@err:	lda #<errflash_
	ldy #>errflash_
	jmp puts
.endproc

;******************************************************************************
; ERASE
; Erases the contents of the flash
.proc erase
	lda #<msg_erasing
	ldy #>msg_erasing
	jsr puts
	jsr FlashCodeSectorErase
	bcs @err
	rts

@err:	lda #<err_erase_failed
	ldy #>err_erase_failed
	jsr puts
	jmp *
.endproc

;******************************************************************************
; BLANK CHECK
; Checks if the Flash is blank (erased)
.proc is_blank
	ldxy #$7000	; address to check
	lda #16		; pages to check
	jsr @check
	bne @done

	ldxy #$a000
	lda #32		; pages
@check:
	stxy r0
	tax
	lda #$ff
:	cmp (r0),y
	bne @done
	iny
	bne :-
	inc r0+1
	dex
	bne :-
@done:	rts
.endproc

;******************************************************************************
.DATA

;******************************************************************************
; filename for image to write to flash
image_name:
.byte "monster.bin"
image_name_size=*-image_name

;******************************************************************************
; error messages
errflash_:        .byte "flash "
err_badflash_:    .byte "bad flash",0
err_erase_failed: .byte "failed to erase",0
err_open_failed:  .byte "failed to open image file",0

;******************************************************************************
; info messages
msgflash_ing:  .byte "flashing ...",13,0
msg_erasing:   .byte "erasing...",13,0
done_msg:      .byte "done",0
