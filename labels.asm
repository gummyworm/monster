.include "errors.inc"
.include "macros.inc"
.include "util.inc"
.include "zeropage.inc"

MAX_LABELS = 256
MAX_LOCALS = 32
MAX_LABEL_LEN = 16

.BSS
;******************************************************************************
.export __label_num
__label_num:
numlabels: .word 0   ; total number of labels

;******************************************************************************
; LABELS
; Table of label names. Each entry corresponds to an entry in label_addresses,
; which contains the value for the label name.
.export labels
labels: .res MAX_LABELS * MAX_LABEL_LEN

;******************************************************************************
; LABEL_ADDRESSES
; Table of addresses for each label
; The address of a given label id is label_addresses + (id * 2)
.export label_addresses
label_addresses: .res 256 * 2

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

	ldxy #labels
	stxy zp::tmp0
	ldx #(MAX_LABELS*MAX_LABEL_LEN)/256
	ldy #$00
@clrlabels:
	sta (zp::tmp0),y
	dey
	bne @clrlabels
	dex
	bne @clrlabels
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
;  - .XY: the id of the label
.export __label_find
.proc __label_find
@cnt=zp::tmp6
@search=zp::tmp8
@label=zp::tmpa
	stxy @label

	lda numlabels
	bne :+
	lda numlabels+1
	bne :+
	RETURN_ERR ERR_LABEL_UNDEFINED ; no labels exist

:	lda #$00
	sta @cnt
	sta @cnt+1
	ldxy #labels
	stxy @search

@seek:	ldy #$00
@l0:	lda (@label),y
	beq @chkend
	jsr util::isseparator
	beq @chkend
	cmp (@search),y
	beq @chmatch
	bcc @notfound	; labels are alphabetical, if our label is not alphabetically greater, we're done
	bne @next  ; if our label IS greater alphabetically, try the next label

@chmatch:
	iny
	cpy #MAX_LABEL_LEN-1
	bcs @found
	bcc @l0
@chkend:
	lda (@search),y
	beq @found

@next:	lda @search
	clc
	adc #MAX_LABEL_LEN
	sta @search
	bcc :+
	inc @search+1
:	incw @cnt
	ldxy @cnt
	cmpw numlabels
	bne @seek
@notfound:
	ldxy @cnt
	RETURN_ERR ERR_LABEL_UNDEFINED

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
;
.export __label_add
.proc __label_add
@id=zp::tmp0
@label=zp::tmp2
@name=zp::tmp4
@src=zp::tmp6
@dst=zp::tmp8
@cnt=zp::tmpa
@addr=zp::tmpc
@skip_shift=zp::tmpe
	stxy @name
	jsr isvalid
	bcc @seek
	RETURN_ERR ERR_ILLEGAL_LABEL

@seek:	ldxy @name
	jsr __label_find
	bcs @insert

	; label exists, overwrite its old value
	jsr __label_by_id ; get the address of the label
	ldy #$00
	lda zp::label_value
	sta (@addr),y
	iny
	lda zp::label_value+1
	sta (@addr),y
	RETURN_OK

@insert:
	stxy @id

;------------------
; open a space for the new label by shifting everything over
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
@sh1:
	lda (@src),y
	sta (@dst),y
	dey
	bpl @sh1

	; shift the address too
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
	bne @sh0

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
	sta (@src),y
	iny
	cpy #MAX_LABEL_LEN
	bcc :-

@storeaddr:
	lda #$00
	sta (@src),y
	; write the address
	ldy #$00
	lda zp::label_value
	sta (@addr),y
	lda zp::label_value+1
	iny
	sta (@addr),y

	incw numlabels
	ldxy @id
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
	ldy #$00
	lda (@table),y
	tax
	iny
	lda (@table),y
	tay
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
@asrc=zp::tmp12
@adst=zp::tmp14
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
:	ldy #$00
	lda (@src),y
	sta (@dst),y
	incw @src
	incw @dst
	lda (@src),y
	sta (@dst),y
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
	ldy #15
:	lda (@src),y
	sta (@dst),y
	dey
	bpl :-
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
	bcc :+
	inc @dst+1
	bne @nameloop
:	decw @cnt2
	ldxy @cnt2
	cmpw #0
	bne @nameloop

	decw numlabels
	RETURN_OK
.endproc

;******************************************************************************
; ISVALID
; Checks if the label in (zp::tmp4) is valid
; OUT:
;  - .C: set if the label is invalid, clear if valid characters for a label.
.proc isvalid
@name=zp::tmp4
	ldy #$00

; first character must be a letter
:	lda (@name),y
	iny
	jsr util::is_whitespace
	beq :-
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

; following characters are between '0' and ')'
@l0:
	lda (@name),y
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
; LABELAT
; Returns the label at the address in (YX)
; IN:
;  - .XY: the address to get the label at
; OUT:
;  - .XY: the label at the given address (if any)
;  - .C: clear if a label was found, set if not
.export __label_labelat
.proc __label_labelat
@msb=zp::tmp4
@num=zp::tmp5
	lda #<(label_addresses+1)
	sta zp::tmp0
	lda #>(label_addresses+1)
	sta zp::tmp0+1
	lda #<label_addresses
	sta zp::tmp2
	lda #>label_addresses
	sta zp::tmp2+1

	sty @msb
	ldy #$00
	sty @num

@l0:	lda @num
	cmp numlabels
	bcc :+
	rts

:	inc @num
	; compare MSB
	lda @msb
	cmp (zp::tmp0),y
	bne @next
	; compare LSB
	txa
	cmp (zp::tmp2),y
	beq @found
@next:	incw zp::tmp0
	incw zp::tmp0
	incw zp::tmp2
	incw zp::tmp2
	bne @l0

@found: ; get the label name
	lda #<labels
	sta zp::tmp0
	lda #>labels
	sta zp::tmp0+1

	ldy #$00
@l1:	dec @num
	beq @done
	lda (zp::tmp0),y
	; move ptr to the next length prefixed label
	sec
	adc zp::tmp0
	sta zp::tmp0
	bcc @l1
	inc zp::tmp0+1
	bne @l1

@done:	ldx zp::tmp0
	ldy zp::tmp0+1
	RETURN_OK
.endproc

;******************************************************************************
; LABEL_BY_ID
; Returns the address of the label ID in .YX in .YX
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XY: the address of the given label id
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
