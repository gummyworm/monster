;*******************************************************************************
; REU.ASM
; This file contains C64-specific REU routines
;*******************************************************************************

.export __reu_c64_addr
.export __reu_reu_addr
.export __reu_txlen

__reu_c64_addr = $df02
__reu_reu_addr = $df04
__reu_txlen    = $df07

.include "../errors.inc"
.include "../macros.inc"
.include "../memory.inc"
.include "../zeropage.inc"

.exportzp __reu_move_src
.exportzp __reu_move_dst
.exportzp __reu_move_size

REU_TMP_ADDR            = $ff0000
REU_VMEM_ADDR           = $fe0000
REU_SYMTABLE_ADDRS_ADDR = $fd0000	; label addresses
REU_SYMTABLE_NAMES_ADDR = $fc0000	; label names
REU_SYMTABLE_ANONS_ADDR = $fb0000	; anonymous label addresses

.BSS
;*******************************************************************************
; TABLE STATE
; These parameters contain the properties of the table usedb by the
; tab* procedures
tab_addr:         .res 3
tab_element_size: .byte 0
tab_num_elements: .word 0

.CODE

;*******************************************************************************
; INIT
.export __reu_init
.proc __reu_init
	lda #$00
	sta $df0a	; count UP
	rts
.endproc

;*******************************************************************************
; STORE
; Moves the data from the given source 24-bit address to the given
; destination one.
; IN:
;   - reu::c64_addr: the source address (24 bit)
;   - reu::reu_addr: the destination address (24 bit)
;   - reu::len:      the number of bytes to copy (16-bit)
.export __reu_store
.proc __reu_store
	lda #$00
	sta $df0a
	lda #$90	; transfer from c64 -> REU with immediate execution
	sta $df01	; execute
	rts
.endproc

;*******************************************************************************
; LOAD
; Loads the C64 with data from the given source 24-bit address to the given
; C64 address
; IN:
;   - reu::c64_addr: the source address (24 bit)
;   - reu::reu_addr: the destination address (24 bit)
;   - reu::len:      the number of bytes to copy (16-bit)
.export __reu_load
.proc __reu_load
	lda #$00
	sta $df0a
	lda #$91	; transfer from REU -> c64 with immediate execution
	sta $df01	; execute
	rts
.endproc

;*******************************************************************************
; COMPARE
; Compares the data at reuaddr and c64addr for up to reu::txlen bytes.
; OUT:
;   .Z: set if there are no differences
.export __reu_compare
.proc __reu_compare
	lda $df00	; read status to clear fault bit
	lda #$93|$20	; compare C64 <-> REU
	sta $df01	; execute
	lda $df00
	and #$20	; check fault bit (set if differences found)
	rts
.endproc

;*******************************************************************************
; SWAP
; Swaps the data from the REU at the address in reu::reuaddr with the data
; in the C64 at reu::c64addr.
.export __reu_swap
.proc __reu_swap
	lda #$92	; swap c64 <-> REU with immediate execution
	sta $df01	; execute
	rts
.endproc

;*******************************************************************************
; ZERO
; Zeroes out the number of bytes in txlen at reu::move_dst
.export __reu_zero
.proc __reu_zero
	ldxy #@zero
	stxy __reu_c64_addr
	lda #$80
	sta $df0a		; fix c64 address

	lda #$90
	sta $df01		; transfer c64 -> REU

	lda #$00
	sta $df0a		; unfix c64 address

@zero:	.byte 0
.endproc

;*******************************************************************************
; MOVE
; Moves the given addresses from one part of the REU to another
; This routine first copies the data to the C64 and then stores
; it back to the REU at the destination address
; IN:
;   - reu::move_src: the address of the data to move
;   - reu::move_dst: the destination address in the REU
;   - reu::move_size: # of byte to relocate
__reu_move_src=zp::bank
__reu_move_dst=zp::bank+3
__reu_move_size=zp::bank+6
.export __reu_move
.proc __reu_move
@src=__reu_move_src
@dst=__reu_move_dst
@size=__reu_move_size
@move:	lda @size+2
	beq :+
	jmp *		; oversized move

:	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1

	; backup the C64 memory we will clobber
	ldxy #@end
	stxy __reu_c64_addr
	stxy __reu_reu_addr
	lda #^REU_TMP_ADDR
	sta __reu_reu_addr+2
	jsr __reu_swap

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1
	ldxy #@end
	stxy __reu_c64_addr
	stxy __reu_reu_addr

	; bring in the source data to relocate
	lda @src
	sta __reu_reu_addr
	lda @src+1
	sta __reu_reu_addr+1
	lda @src+2
	sta __reu_reu_addr+2
	jsr __reu_load

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1
	ldxy #@end
	stxy __reu_c64_addr

	; and store it to its relocation address
	lda @dst
	sta __reu_reu_addr
	lda @dst+1
	sta __reu_reu_addr+1
	lda @dst+2
	sta __reu_reu_addr+2
	jsr __reu_store

	lda @size
	sta __reu_txlen
	lda @size+1
	sta __reu_txlen+1


	; finally, restore the C64's memory that we used as an intermediate
	; buffer
	ldxy #@end
	stxy __reu_c64_addr
	stxy __reu_reu_addr
	lda #^REU_TMP_ADDR
	sta __reu_reu_addr+2
	jmp __reu_swap
@end=*
.endproc

;*******************************************************************************
; FIND
; Seeks, page by page, for the given string beginning at the given
; address. If no match is found at the 64k page of the given address,
; returns with the .C flag set.
; IN:
;  - .XY:           the string to look for
;  - .A:            the length of the string
;  - reu::reu_addr: the address to start seeking at
; OUT:
;  - .C: set if the string is not found
;  - .A:  the 64k block of the return address (same as one given)
;  - .XY: the address of the string (if found)
.export __reu_find
.proc __reu_find
@str=zp::bankoffset
@len=zp::bankoffset+2
@tmp=zp::bankoffset+3
@pagebuff=@end
	stxy @str
	sta @len

	ldxy #$100
	stxy __reu_txlen
	ldxy #@pagebuff
	stxy __reu_c64_addr
	stxy __reu_c64_addr

	; read one page for compare
	jsr __reu_load

	; search the page for the string
	ldy #$00
	ldx #$00
@l0:	lda (@str),y
	cmp @pagebuff,y
	beq @next

	; .Y -= .X (backtrack the # of chars we matched)
	stx @tmp
	tya
	sec
	sbc @tmp
	tay
	ldx #$ff		; reset char match count

@next:	inx
	cpx @len
	beq @found
	iny
	bne @l0
	inc __reu_reu_addr+1	; next page
	bne @l0			; repeat until end of 64k block
	sec			; flag not found
	rts

@found:	tya
	clc
	adc __reu_reu_addr
	tax
	lda __reu_reu_addr+1
	adc #$00
	tay
	lda __reu_reu_addr+2
	RETURN_OK
@end:
.endproc

;*******************************************************************************
; DBG
; Copies the contents of REU to $0500
.export __reu_dbg
.proc __reu_dbg
	ldxy #$200
	stxy __reu_txlen
	stx __reu_reu_addr
	stx __reu_reu_addr+1

	lda #^REU_SYMTABLE_NAMES_ADDR
	sta __reu_reu_addr+2

	ldxy #$500
	stxy __reu_c64_addr
	jmp __reu_load
.endproc

;*******************************************************************************
; TAB SETUP
; Sets up the table with the given parameters
; IN:
;   - .A:    the MSB of the REU address of the table
;   - .X:    the size of each element in the table
;            NOTE: 256 % this must be 0
;   - r0/r1: the number of elements in the table
.export __reu_tabsetup
.proc __reu_tabsetup
	sta tab_addr+2
	lda #$00
	sta tab_addr
	sta tab_addr+1

	stx tab_element_size

	lda r0
	sta tab_num_elements
	lda r0+1
	sta tab_num_elements+1

	rts
.endproc

;*******************************************************************************
; TAB FIND
; Finds the given data in the table and returns its address (if found)
; IN:
;   - .XY: address of the data to match
;   - .A:  length of the data to match
; OUT:
;   - .C:   set if the item wasn't found
;   - .XYA: if the data was found, the address of it
.export __reu_tabfind
.proc __reu_tabfind
@cnt=zp::banktmp
	sta __reu_txlen

	; make sure there are elements in our table, return if not
	lda tab_num_elements
	bne :+
	lda tab_num_elements+1
	bne :+
	sec
	rts

:	stxy __reu_c64_addr

	lda #$00
	sta __reu_txlen+1
	sta @cnt
	sta @cnt+1

	lda tab_addr
	sta __reu_reu_addr
	lda tab_addr+1
	sta __reu_reu_addr+1
	lda tab_addr+2
	sta __reu_reu_addr+2

@l0:	jsr __reu_compare
	beq @found

	; move to next entry in the table
	lda __reu_reu_addr
	clc
	adc tab_element_size
	sta __reu_reu_addr
	bcc @next
	inc __reu_reu_addr+1
	bne @next
	inc __reu_reu_addr+2

@next:	inc @cnt
	bne :+
	inc @cnt+1
:	lda @cnt+1
	cmp tab_num_elements+1
	bne @l0
	lda @cnt
	cmp tab_num_elements
	bne @l0
	sec			; not found
	rts

@found:	ldx __reu_reu_addr
	ldy __reu_reu_addr+1
	lda __reu_reu_addr+2
	RETURN_OK
.endproc

;*******************************************************************************
; TAB FIND SORTED
; Finds the given data in the table assuming the table is sorted
; IN:
;   - .XY: the data to find
;   - .A:  the length of the input data
; OUT:
;   - .XY: the index of the matched data (or where it would be if it existed)
;   - .C:  set if the input was not found in the table
.export __reu_tabfind_sorted
.proc __reu_tabfind_sorted
@cnt=zp::banktmp
@src=zp::banktmp+2
@len=zp::banktmp+4
@buff=mem::spare
	sta @len

	lda tab_element_size
	sta __reu_txlen

	lda #$00
	sta __reu_txlen+1
	sta @cnt
	sta @cnt+1

	; make sure there are elements in our table, return if not
	lda tab_num_elements
	bne :+
	lda tab_num_elements+1
	beq @notfound

:	stxy @src
	ldxy #@buff
	stxy __reu_c64_addr

	lda tab_addr
	sta __reu_reu_addr
	lda tab_addr+1
	sta __reu_reu_addr+1
	lda tab_addr+2
	sta __reu_reu_addr+2

@l0:	; load the data to compare
	jsr __reu_load

	; compare the data we're searching for with the loaded table data
	ldy #$00
:	lda (@src),y
	cmp @buff,y
	bcc @notfound
	bne @next
	iny
	cpy @len
	bcc :-

	; make sure the table element is the same size (if not max size)
	cpy tab_element_size
	bcs @found		; max size, don't require null
	lda @buff,y
	beq @found

@next:	; move to next entry in the table
	lda __reu_reu_addr
	clc
	adc tab_element_size
	sta __reu_reu_addr
	bcc :+
	inc __reu_reu_addr+1
	bne :+
	inc __reu_reu_addr+2

:	inc @cnt
	bne :+
	inc @cnt+1
:	lda @cnt+1
	cmp tab_num_elements+1
	bne @l0
	lda @cnt
	cmp tab_num_elements
	bne @l0

@notfound:
	ldxy @cnt
	sec			; not found
	rts

@found:	ldxy @cnt
	RETURN_OK
.endproc
