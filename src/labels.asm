.include "errors.inc"
.include "finalex.inc"
.include "macros.inc"
.include "util.inc"
.include "zeropage.inc"

MAX_LABELS    = 256
MAX_LOCALS    = 32
MAX_LABEL_LEN = 16

.import __BANKCODE_LOAD__
.import __BANKCODE_SIZE__

.BSS
;******************************************************************************
.export __label_num
__label_num:
numlabels: .word 0   	; total number of labels
numlocals: .byte 0	; number of local labels

;******************************************************************************
; LABELS
; Table of label names. Each entry corresponds to an entry in label_addresses,
; which contains the value for the label name.
.export labels
.ifdef USE_FINAL
labels = __BANKCODE_LOAD__+__BANKCODE_SIZE__	; ~$20xx-$8000
.else
labels: .res MAX_LABELS * MAX_LABEL_LEN
.endif

;******************************************************************************
; LABEL_ADDRESSES
; Table of addresses for each label
; The address of a given label id is label_addresses + (id * 2)
.export label_addresses
.ifdef USE_FINAL
label_addresses = $a000
.else
label_addresses: .res 256 * 2
.endif

.CODE
;******************************************************************************

;******************************************************************************
; CLR
; Removes all labels effectively resetting the label state
.export __label_clr
.proc __label_clr
	lda #$00
	sta numlabels
	sta numlabels+1
	sta numlocals
	rts
.endproc

;******************************************************************************
; FIND
; Looks for the ID corresponding to the given label and returns it.
; in:
;  - .XY: the name of the label to look for
; out:
;  - .C: set if label is not found
;  - .A: contains the length of the label because why not
;  - .XY: the id of the label or the id where the label WOULD be if not found
.export __label_find
.proc __label_find
@cnt=zp::tmp6
@search=zp::tmp8
@label=zp::tmpa
@ch=zp::tmpe
@offset=zp::tmpf
@ch2=zp::tmp10
@islocal=zp::tmp11
	stxy @label

	; check (and flag) if the label is local. if it is, we will start
	; searching at the end of the label table, where locals are stored
	jsr __label_is_local
	sta @islocal
	beq :+
	lda numlocals
	bne :+
	ldxy numlabels	; if there are no local labels; this must be the first
	RETURN_ERR ERR_LABEL_UNDEFINED

:	ldx numlabels
	bne :+
	ldy numlabels+1
	bne :+
	RETURN_ERR ERR_LABEL_UNDEFINED ; no labels exist

:	lda #$00
	sta @cnt
	sta @cnt+1
	ldxy #labels
	stxy @search

	lda @islocal
	beq @seek

	; if local; start searching from labels + 16*(numlabels-numlocals)
	; @cnt = (numlabels-numlocals)
	lda numlabels
	sec
	sbc numlocals
	sta @cnt
	lda numlabels+1
	sbc #$00
	sta @cnt+1

	; @cnt *= 16
	lda @cnt
	asl
	rol @cnt+1
	asl
	rol @cnt+1
	asl
	rol @cnt+1
	asl
	rol @cnt+1

	; seek = labels + @cnt
	adc #<labels
	sta @search
	lda @cnt+1
	adc #>labels
	sta @search+1

@seek:	ldy #$00
@l0:	lda (@label),y
	jsr util::isseparator
	beq @chkend

.ifdef USE_FINAL
	sty @offset
	sta @ch
	bank_read_byte_rel #FINAL_BANK_SYMBOLS, @search, @offset
	sta @ch2
	lda @ch		; TODO: could clean this up
	ldy @offset
	cmp @ch2
.else
	cmp (@search),y
.endif

	beq @chmatch
	bcc @notfound	; labels are alphabetical, if our label is not alphabetically greater, we're done
	bne @next  ; if our label IS greater alphabetically, try the next label

@chmatch:
	iny
	cpy #MAX_LABEL_LEN-1
	bcs @found
	bcc @l0
@chkend:
.ifdef USE_FINAL
	sty @offset
	bank_read_byte_rel #FINAL_BANK_SYMBOLS, @search, @offset
	ldy @offset
	cmp #$00
.else
	lda (@search),y
.endif
	beq @found

@next:	lda @search
	clc
	adc #MAX_LABEL_LEN
	sta @search
	bcc :+
	inc @search+1
:	incw @cnt
	ldxy @cnt
	lda @islocal
	beq @notlocal
@local:
	cmpw numlocals
	bne @seek
	beq @notfound
@notlocal:
	cmpw numlabels
	bne @seek

@notfound:
	ldxy @cnt
	lda @islocal
	beq :+
	; calculate the offset from labels (not labels + (numlabels-numlocals))
	add16 numlabels	; cnt += numlabels
	sub16 numlocals	; cnt -= numlocals
:	RETURN_ERR ERR_LABEL_UNDEFINED

@found:	tya
	ldxy @cnt
	RETURN_OK
.endproc

;******************************************************************************
; ADD
; Adds a label to the internal label state.
;  - .XY: the name of the label to add
;  - zp::label_value: the value to assign to the given label name
; out:
;  - .C: set on error or clear if the label was successfully added
.export __label_add
.proc __label_add
@id=zp::tmp0
@label=zp::tmp2
@name=zp::tmp4
@src=zp::tmp6
@dst=zp::tmp8
@cnt=zp::tmpa
@addr=zp::tmpc
@offset=zp::tmp10
@islocal=zp::tmp11
	stxy @name
	jsr isvalid
	bcc @seek
	RETURN_ERR ERR_ILLEGAL_LABEL

@seek:	ldxy @name
	jsr __label_find
	bcs @insert

	; label exists, overwrite its old value
	jsr __label_by_id ; get the address of the label

	lda zp::label_value
.ifdef USE_FINAL
	bank_store_byte #FINAL_BANK_SYMBOLS, @addr
	incw @addr
	lda zp::label_value+1
	bank_store_byte #FINAL_BANK_SYMBOLS, @addr
.else
	ldy #$00
	sta (@addr),y
	iny
	lda zp::label_value+1
	sta (@addr),y
.endif
	RETURN_OK

@insert:
	; @id is the index where the new label will live
	stxy @id

	; flag if label is local or not
	ldxy @name
	jsr __label_is_local
	sta @islocal

;------------------
; open a space for the new label by shifting everything left
@shift:
	; src = labels + (numlabels-1)*16
	lda numlabels+1
	sta @src+1
	lda numlabels
	asl
	rol @src+1
	asl
	rol @src+1
	asl
	rol @src+1
	asl
	rol @src+1
	adc #<labels
	sta @src
	sta @dst
	lda @src+1
	adc #>labels
	sta @src+1
	sta @dst+1

	; addr = label_addresses+(numlabels-1)*2
	lda numlabels+1
	sta @addr+1
	lda numlabels
	asl
	rol @addr+1
	adc #<label_addresses
	sta @addr
	lda @addr+1
	adc #>label_addresses
	sta @addr+1

	iszero numlabels
	bne :+
	jmp @storelabel

	; src -= 16
:	lda @src
	sec
	sbc #MAX_LABEL_LEN
	sta @src
	bcs :+
	dec @src+1

:	; addr -= 2
	decw @addr
	decw @addr

	; cnt = numlabels-id
	lda numlabels
	sec
	sbc @id
	sta @cnt
	lda numlabels+1
	sbc @id+1
	sta @cnt+1

	iszero @cnt
	bne @sh0
	ldxy @dst
	stxy @src
	incw @addr
	incw @addr
	jmp @storelabel

@sh0:
	ldy #MAX_LABEL_LEN-1
.ifdef USE_FINAL
	lda @src
	sta zp::bankaddr0
	lda @src+1
	sta zp::bankaddr0+1
	lda @dst
	sta zp::bankaddr1
	lda @dst+1
	sta zp::bankaddr1+1
	lda #FINAL_BANK_SYMBOLS
	jsr fe3::fcopy
.else
@sh1:
	lda (@src),y
	sta (@dst),y
	dey
	bpl @sh1
.endif

; shift the address too
.ifdef USE_FINAL
	bank_read_byte #FINAL_BANK_SYMBOLS, @addr
	bank_store_byte_rel #FINAL_BANK_SYMBOLS, @addr, #$02
	bank_read_byte_rel #FINAL_BANK_SYMBOLS, @addr, #$01
	bank_store_byte_rel #FINAL_BANK_SYMBOLS, @addr, #$03
.else
	iny
	lda (@addr),y
	tax
	iny
	lda (@addr),y
	pha
	iny
	txa
	sta (@addr),y
	iny
	pla
	sta (@addr),y
.endif
	decw @cnt
	iszero @cnt
	beq @storelabel

	decw @addr
	decw @addr

	ldxy @src
	sub16 #MAX_LABEL_LEN
	stxy @src
	ldxy @dst
	sub16 #MAX_LABEL_LEN
	stxy @dst
	beq @storelabel
	jmp @sh0

;------------------
; insert the label into the new opening
@storelabel:
	ldy #$00
	; write the label
:	lda (@name),y
	beq @storeaddr
	cmp #' '
	beq @storeaddr
	cmp #':'
	beq @storeaddr

	; copy a byte to the label name
.ifdef USE_FINAL
	sty @offset
	bank_store_byte_rel #FINAL_BANK_SYMBOLS, @src, @offset
	ldy @offset
.else
	sta (@src),y
.endif
	iny
	cpy #MAX_LABEL_LEN
	bcc :-

@storeaddr:
	; 0-terminate the label name and write the label value
	lda #$00
.ifdef USE_FINAL
	sty @offset
	bank_store_byte_rel #FINAL_BANK_SYMBOLS, @src, @offset
	lda zp::label_value
	bank_store_byte #FINAL_BANK_SYMBOLS, @addr
	lda zp::label_value+1
	incw @addr
	bank_store_byte #FINAL_BANK_SYMBOLS, @addr
.else
	sta (@src),y
	ldy #$00
	lda zp::label_value
	sta (@addr),y
	lda zp::label_value+1
	iny
	sta (@addr),y
.endif
	incw numlabels
	lda @islocal
	beq :+
	incw numlocals
:	ldxy @id
	RETURN_OK
.endproc


;******************************************************************************
; LABEL_ADDRESS
; Returns the address of the label in (.YX)
; The size of the label is returned in .A (1 if zeropage, 2 if not)
; line is updated to the character after the label.
; IN:
;  - .XY: the address of the label name to get the address of
; OUT:
;  - .XY: the address of the label
;  - .C: is set if no label was found, clear if it was
;  - .A: the size of the label
.export __label_address
.proc __label_address
@table=zp::tmp0
	jsr __label_find	; get the id in YX
	bcc :+
	RETURN_ERR ERR_LABEL_UNDEFINED

:	txa
	asl
	sta @table
	tya
	rol
	sta @table+1
	lda @table
	adc #<label_addresses
	sta @table
	lda @table+1
	adc #>label_addresses
	sta @table+1

.ifdef USE_FINAL
	bank_read_byte #FINAL_BANK_SYMBOLS, @table
	pha
	incw @table
	bank_read_byte #FINAL_BANK_SYMBOLS, @table
	tay
	pla
	tax
	cpy #$00
.else
	ldy #$00
	lda (@table),y
	tax
	iny
	lda (@table),y
	tay
.endif
	bne :+		; get the size of the label's address in .A
	lda #$01
	skw
:	lda #$02
	RETURN_OK
.endproc

;******************************************************************************
; DEL
; Deletes the given label name.
; IN:
;  - .XY: the address of the label name to delete
.export __label_del
.proc __label_del
@id=zp::tmp6
@cnt=zp::tmp8
@cnt2=zp::tmpa
@src=zp::tmpe
@dst=zp::tmp10
@name=zp::tmp12
	stxy @name
	jsr __label_find
	bcc @del
	rts		; not found

@del:	stxy @id
	jsr __label_by_id
	stxy @dst

	; get the destination (dst - 2)
	lda @src
	clc
	adc #$02
	sta @src
	lda @dst+1
	adc #$00
	sta @src+1

	; get the number of addresses to shift
	lda numlabels
	sec
	sbc @id
	sta @cnt
	sta @cnt2
	lda numlabels+1
	sbc @id+1
	sta @cnt+1
	sta @cnt2+1

	; move the addresses down
:
.ifdef USE_FINAL
	bank_read_byte #FINAL_BANK_SYMBOLS, @src
	bank_store_byte #FINAL_BANK_SYMBOLS, @dst
.else
	ldy #$00
	lda (@src),y
	sta (@dst),y
.endif
	incw @src
	incw @dst
.ifdef USE_FINAL
	bank_read_byte #FINAL_BANK_SYMBOLS, @src
	bank_store_byte #FINAL_BANK_SYMBOLS, @dst
.else
	lda (@src),y
	sta (@dst),y
.endif

	incw @src
	incw @dst
	decw @cnt
	ldxy @cnt
	cmpw #0
	bne :-

	; get the source (destination + 16)
	ldxy @id
	jsr __label_name_by_id
	stxy @dst
	lda @dst
	clc
	adc #16
	sta @src
	lda @dst+1
	adc #$00
	sta @src+1

	; move the names down
@nameloop:
.ifdef USE_FINAL
	lda @src
	sta zp::bankaddr0
	lda @src+1
	sta zp::bankaddr0+1
	lda @dst
	sta zp::bankaddr1
	lda @dst+1
	sta zp::bankaddr1+1
	ldy #15
	lda #FINAL_BANK_SYMBOLS
	jsr fe3::fcopy
.else
	ldy #15
:	lda (@src),y
	sta (@dst),y
	dey
	bpl :-
.endif
	lda @src
	clc
	adc #16
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #16
	sta @dst
	bcc @nextname
	inc @dst+1
	bne @nameloop
@nextname:
	decw @cnt2
	ldxy @cnt2
	cmpw #0
	bne @nameloop
	decw numlabels
	ldxy @name
	jsr __label_is_local
	beq :+
	decw numlocals
:	RETURN_OK
.endproc

;******************************************************************************
; ISVALID
; Checks if the label in (zp::tmp4) is valid
; OUT:
;  - .C: set if the label is invalid, clear if valid characters for a label.
.proc isvalid
@name=zp::tmp4
	ldy #$00

; first character must be a letter or '@'
:	lda (@name),y
	iny
	jsr util::is_whitespace
	beq :-
	cmp #$40	; '@'
	beq @l0
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

; following characters are between '0' and ')'
@l0:	lda (@name),y
	beq @done
	jsr util::is_whitespace
	beq @done
	cmp #' '
	bcc @err
	cmp #'Z'+1
	iny
	bcc @l0
@err:   RETURN_ERR ERR_ILLEGAL_LABEL
@done:  RETURN_OK
.endproc

;******************************************************************************
; IS_LOCAL
; Returns with .Z set if the given label is a local label (begins with '@')
; IN:
;  - .XY: the label to test
; OUT:
;  - .A: nonzero if the label is local
.export __label_is_local
.proc __label_is_local
@l=zp::labels
	stxy @l
	ldy #$00
	lda (@l),y
	cmp #'@'
	bne :+
	lda #$01
	rts
:	lda #$00
	rts
.endproc

;******************************************************************************
; LABEL_BY_ID
; Returns the address of the label ID in .YX in .YX
; IN:
;  - .XY: the id of the label to get the address of
; OUT:///
;  - .XY: the address of the given label id
;  - zp::tmpc: the address of the label (same as .XY)
.export __label_by_id
.proc __label_by_id
@addr=zp::tmpc
	txa
	asl
	sta @addr
	tya
	rol
	sta @addr+1
	lda @addr
	adc #<label_addresses
	sta @addr
	tax
	lda @addr+1
	adc #>label_addresses
	sta @addr+1
	tay
	rts
.endproc

;******************************************************************************
; BY_ADDR
; Returns the label for a label given by its address
; IN:
;  - .XY: the label address to get the name of
; OUT:
;  - .XY: the ID of the label
;  - .C: set if no label is found
.export __label_by_addr
.proc __label_by_addr
@addr=zp::tmpe
@cnt=zp::tmp10
	stxy @addr
	ldxy #$ffff
	stxy @cnt
@l0:	incw @cnt
	ldxy @cnt
	cmpw numlabels
	beq @notfound
	jsr __label_by_id
	cmpw @addr
	bne @l0
@found:	clc
	ldxy @cnt
	rts

@notfound:
	sec
	rts
.endproc

;******************************************************************************
; NAME_BY_ID
; Returns the address name of the label ID in .YX in .YX
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XY: the address of the name for the given label id
.export __label_name_by_id
.proc __label_name_by_id
@addr=zp::labels
	sty @addr+1
	txa
	asl
	rol @addr+1
	asl
	rol @addr+1
	asl
	rol @addr+1
	asl
	rol @addr+1
	adc #<labels
	tax
	lda @addr+1
	adc #>labels
	tay
	rts
.endproc

;******************************************************************************
; ISVALID
; checks if the label name in (zp::line) is a valid label name
; OUT:
;  - .C: set if the label is NOT valid
.export __label_isvalid
.proc __label_isvalid
	ldy #$00
; first character must be a letter
:	lda (zp::line),y
	iny
	jsr util::is_whitespace
	beq :-
	cmp #'@'
	beq @l0
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

	;jsr getopcode	; make sure string is not an opcode
	;bcs @cont
	;sec
	;rts

; following characters must be between '0' and 'Z'
@l0:
	lda (zp::line),y
	beq @done
	jsr util::isseparator
	beq @done
	cmp #'0'
	bcc @err
	cmp #'Z'+1
	iny
	bcc @l0
@err:	RETURN_ERR ERR_ILLEGAL_LABEL

	cmp #' '
	beq @done
	cmp #':'
	bne :-

@done:	RETURN_OK
.endproc

;******************************************************************************
; GET_NAME
; Copies the name of the label ID given to the provided buffer
; IN:
;  - .XY: the ID of the label to get the name of
;  - zp::tmp0: the address to copy to
; OUT:
;  - (zp::tmp0): the label name
.export __label_get_name
.proc __label_get_name
@dst=zp::tmp0
@src=zp::labels
@offset=zp::labels+2
	jsr __label_name_by_id
	stxy @src

	ldy #$00
@l0:
.ifdef USE_FINAL
	sty @offset
	bank_read_byte_rel #FINAL_BANK_SYMBOLS, @src, @offset
	ldy @offset
	cmp #$00
.else
	lda (@src),y
.endif
	sta (@dst),y
	beq @done
	iny
	cpy #MAX_LABEL_LEN
	bcc @l0
@done:	rts
.endproc

;******************************************************************************
; GET_ADDR
; Returns the address of the given label ID.
; IN:
;  - .XY: the ID of the label to get the name of
; OUT:
;  - .XY: the address of the label
.export __label_get_addr
.proc __label_get_addr
@src=zp::labels
	jsr __label_by_id
	stxy @src

.ifdef USE_FINAL
	bank_read_byte #FINAL_BANK_SYMBOLS, @src
	pha
	incw @src
	bank_read_byte #FINAL_BANK_SYMBOLS, @src
	tay
	pla
	tax
.else
	ldy #$01
	lda (@src),y
	tax
	dey
	lda (@src),y
	tay
.endif
	rts
.endproc

;******************************************************************************
; CLR_LOCALS
; Clears all local labels from the label table
.export __label_clr_locals
.proc __label_clr_locals
	lda numlabels
	sec
	sbc numlocals
	sta numlabels
	bcs :+
	dec numlabels+1
:	lda #$00
	sta numlocals
	rts
.endproc
