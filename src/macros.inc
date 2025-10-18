.include "zeropage.inc"

;******************************************************************************
; INCW
; incw increments the 16-bit value at the address w.
.macro incw w
.local @skip
	inc w
	bne @skip
	inc w+1
@skip:
.endmacro

;******************************************************************************
; DECW
; decrements the 16-bit value at the address w.
; clobbers .A
; .Z is set if LSB of w is 0 after decrementing
.macro decw w
.local @skip
	lda w
	bne @skip
	dec w+1
@skip:	dec w
.endmacro

;******************************************************************************
; CMPW
; Compares .XY to the 16-bit argument. The flags are set as they would be
; from a normal cmp instruction
; OUT:
;  - .Z: set if the two are equal
;  - .C: set if .XY is >= to the argument
.macro cmpw arg
.local @skip
	.if .match( .left( 1, {arg} ), # )
		cpy #>(.right( .tcount( {arg} )-1, {arg} ))
		bne @skip
		cpx #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		cpy arg+1
		bne @skip
		cpx arg
	.endif
@skip:
.endmacro

;******************************************************************************
; LDXY
; loads .X with the LSB of arg and .Y with the MSB.
.macro ldxy arg
	.if .match( .left( 1, {arg} ), # )
		ldx #<(.right( .tcount( {arg} )-1, {arg} ))
		ldy #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		ldx arg
		ldy arg+1
	.endif
.endmacro

;******************************************************************************
; STXY
; Stores the contents of the 16-bit value in .XY to the given address
.macro stxy arg
	stx arg
	sty arg+1
.endmacro

;******************************************************************************
; ADD16
.macro add16 arg
	txa
	clc
	.if .match( .left( 1, {arg} ), # )
		adc #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		adc arg
	.endif
	tax
	tya
	.if .match( .left( 1, {arg} ), # )
		adc #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		adc arg+1
	.endif
	tay
.endmacro

;******************************************************************************
; SUB16
.macro sub16 arg
	txa
	sec
	.if .match( .left( 1, {arg} ), # )
		sbc #<(.right( .tcount( {arg} )-1, {arg} ))
	.else
		sbc arg
	.endif
	tax
	tya
	.if .match( .left( 1, {arg} ), # )
		sbc #>(.right( .tcount( {arg} )-1, {arg} ))
	.else
		sbc arg+1
	.endif
	tay
.endmacro

;******************************************************************************
; SKB
; Skips the next byte
.macro skb
	.byte $24
.endmacro

;******************************************************************************
; SKW
; Skips execution of the next word (equivalent to .byte $2C (bit ABS))
.macro skw
	.byte $2c
.endmacro

;******************************************************************************
; ISZERO
; Sets the .Z flag if the operand is 0
; .A is destroyed
.macro iszero arg
	lda arg
	ora arg+1
.endmacro

; TODO: the below macros are Vic-20 specific.
; each target should implement these

;*******************************************************************************
; LOADB Y
; Load a byte from memory with Y offset
; IN:
;  - addr: the address to load from
;  - .Y:   offset to apply to load address
; OUT:
;  - .A: the value that was read
.macro LOADB_Y addr
	lda (addr),y
.endmacro

;*******************************************************************************
; LOADB
; Load a byte from the given address
; IN:
;  - addr: the address to load from
; OUT:
;  - .A: the value that was read
.macro LOADB addr
	ldy #$00
	LOADB_Y addr
.endmacro

;*******************************************************************************
; LOADW Y
; Read a word from the given address + .Y (offset)
; IN:
;   - addr: source address
;   - .Y:    offset
; OUT:
;   - .XY: the value that was read
.macro LOADW_Y addr
	lda (addr),y
	tax
	iny
	lda (addr),y
	tay
.endmacro

;*******************************************************************************
; LOADW
; Read a word from the given address
; IN:
;   - addr: source address
; OUT:
;   - .XY: the value that was read
.macro LOADW addr
	ldy #$00
	LOADW_Y addr
.endmacro

;*******************************************************************************
; STOREB Y
; Store a byte to memory with Y offset
; IN:
;   - .A: byte to write
;   - .Y: offset
;   - addr: address to write to
.macro STOREB_Y addr
	sta (addr),y
.endmacro

;*******************************************************************************
; STOREB
; Stores the value in .A to the target address
; IN:
;   - .A:   value to store
;   - addr: target address
.macro STOREB addr
	ldy #$00
	STOREB_Y addr
.endmacro

;*******************************************************************************
; STOREW_Y
; Write a word to memory at the target address offset by the value in .Y
; IN:
;   - val:  value to write
;   - addr: address to store to
; OUT:
;   - .Y: the input value of .Y + 2
.macro STOREW_Y val, addr
	lda val
	STOREB_Y addr
	iny
	STOREB_Y addr
	iny
.endmacro

;*******************************************************************************
; STOREW
; Write a word to memory
; IN:
;   - .XY:  value to write
;   - addr: address to store to
.macro STOREW addr
	txa
	sta (addr),y
	iny
	tya
	sta (addr),y
	dey
.endmacro

;*******************************************************************************
; SWAPB_Y
; Swaps 2 bytes at the given addresses offset by the value of .Y
; IN:
;   - addr1: one address to swap
;   - addr2: the other address to swap
.macro SWAPB_Y addr1, addr2
	lda (addr1),y
	tax
	lda (addr2),y
	sta (addr1),y
	txa
	sta (addr2),y
.endmacro

;*******************************************************************************
; SWAPW_Y
; Swaps 2 words at the given addresses offset by the value of .Y
; IN:
;   - addr1: one address to swap
;   - addr2: the other address to swap
.macro SWAPW_Y addr1, addr2
	SWAPB_Y
	iny
	SWAPB_Y
.endmacro

;*******************************************************************************
; COPY_Y
; Copy the number of bytes in .Y from src to dst
; IN:
;   - src:  source address
;   - dst:  destination address
;   - .Y:   the number of bytes to copy
.macro COPY_Y src, dst
.local @loop
@loop:	lda (src),y
	sta (dst),y
	dey
	bpl @loop
.endmacro

;*******************************************************************************
; COPYxx
; Copy a chunk of memory (count in Y register, decrementing)
; IN:
;   - src:  source address
;   - dst:  destination address
;   - size: size (0-255)
.macro COPYxx src, dst, size
	ldy #size
	COPY_Y
.endmacro

;*******************************************************************************
; FILL_Y
; Fill memory with a single byte value
; IN:
;   - addr: start address to fill
;   - .A:   byte value
;   - .Y:   number of bytes to fill
.macro FILL_Y addr
.local @loop
@loop:	sta (addr),y
	dey
	bpl @loop
.endmacro
