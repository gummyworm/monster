.include "errors.inc"
.include "macros.inc"
.include "util.inc"
.include "zeropage.inc"

MAX_LABELS = 256
MAX_LOCALS = 32
MAX_LABEL_LEN = 16

;**************************************
.BSS
;**************************************
;--------------------------------------
; total number of labels
numlabels: .word 0

;--------------------------------------
; table of label names. each entry corresponds to an entry in label_addresses,
; which contains the value for the label name.
.export labels
labels: .res MAX_LABELS * MAX_LABEL_LEN
labels_end=*

;--------------------------------------
; table of addresses for each label
; the address of a given label id is label_addresses + (id * 2)
.export label_addresses
label_addresses: .res 256 * 2

;**************************************
.CODE
;**************************************
;--------------------------------------
; CLR
; removes all labels effectively resetting the label state
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

;--------------------------------------
; FIND
; looks for the ID corresponding to the given label and returns it.
; in:
;  - .XY: the name of the label to look for
; out:
;  - .C: set if label is not found
;  - .A: contains the length of the label because why not
;  - .XY: the id of the label
.proc find
@cnt=zp::tmp6
@search=zp::tmp8
@label=zp::tmpa
	stxy @label
	iszero numlabels
	bne :+
	ldxy #$00
	RETURN_ERR ERR_LABEL_UNDEFINED ; no labels exist

:	lda #$00
	sta @cnt
	sta @cnt+1
	ldxy #labels
	stxy @search
@seek:
	ldy #$00
@l0:
	lda (@label),y
	jsr  util::isseparator
	beq @found
	cmp (@search),y
	beq :+
	bcc @notfound	; labels are alphabetical, if our label is not alphabetically greater, we're done
	bne @next
:	iny
	cpy #MAX_LABEL_LEN
	bcc @l0
@next:
	lda @search
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

@found:
	tya
	ldxy @cnt
	RETURN_OK
.endproc

;--------------------------------------
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

@seek:
	ldxy @name
	jsr find
	bcs @insert
	; label exists, overwrite its old value

	jsr labeladdr ; get the address of the label
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


;--------------------------------------
; label_address returns the address of the label in (.YX)
; The size of the label is returned in .A (1 if zeropage, 2 if not)
; line is updated to the character after the label.
; .C is set if no label was found, clear if it was
.export __label_address
.proc __label_address
@table=zp::tmp0
	jsr find	; get the id in YX
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

;--------------------------------------
; del deletes the label name given in .YX
.export __label_del
.proc __label_del
@id=zp::tmp6
@cnt=zp::tmp8
@cnt2=zp::tmpa
@src=zp::tmpe
@dst=zp::tmp10
@asrc=zp::tmp12
@adst=zp::tmp14
	jsr find
	bcc @del
	rts

@del:
	stxy @id
	jsr labeladdr
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
	jsr labelnameaddr
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

;--------------------------------------
; ISVALID
; checks if the label in (zp::line) is valid
;  out:
;   - .C: set if the label is invalid, clear if valid
; characters for a label.
.proc isvalid
	ldy #$00

; first character must be a letter
:	lda (zp::line),y
	iny
	jsr util::is_whitespace
	beq :-
	cmp #'a'
	bcc @err
	cmp #'z'+1
	bcs @err

; following characters are between '0' and ')'
@l0:
	lda (zp::line),y
	beq @done
	jsr util::is_whitespace
	beq @done
	cmp #' '
	bcc @err
	cmp #')'
	iny
	bcc @l0
@err:
	RETURN_ERR ERR_ILLEGAL_LABEL
@done:
	RETURN_OK
.endproc

;--------------------------------------
; labelat returns the label at the address in (YX), if .A is $ff, no label
; was found at the given address.
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
	lda #$ff
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
	lda #$00
	rts
.endproc


;--------------------------------------
; LABELADDR
; returns the address of the label ID in .YX in .YX
; in:
;  - .XY: the id of the label to get the address of
; out:
;  - .XY: the address of the given label id
.proc labeladdr
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

;--------------------------------------
; LABELNAMEADDR
; returns the address name of the label ID in .YX in .YX
; in:
;  - .XY: the id of the label to get the address of
; out:
;  - .XY: the address of the name for the given label id
.proc labelnameaddr
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

;--------------------------------------
; ISVALID
; checks if the label name in (zp::line) is a valid label name
; out:
;  - .C: set if the label is NOT valid
.export __label_isvalid
.proc __label_isvalid
	ldy #$00
	lda (zp::line),y
	jsr util::isoperator
	beq @notlabel

	;jsr getopcode	; make sure string is not an opcode
	;bcs @cont
	;sec
	;rts

@cont:
	ldy #$00
	lda (zp::line),y
	cmp #'.'	; label cannot have '.' prefix
	beq @notlabel
:	lda (zp::line),y
	beq @done
	iny
	cpy #40
	bcs @notlabel
	cmp #' '
	beq @done
	cmp #':'
	bne :-

@done:	RETURN_OK
@notlabel:
	RETURN_ERR ERR_ILLEGAL_LABEL
.endproc
