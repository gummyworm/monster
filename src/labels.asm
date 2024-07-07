.include "errors.inc"
.include "finalex.inc"
.include "macros.inc"
.include "zeropage.inc"

.macro LBLJUMP proc
	pha
	lda #<proc
	sta zp::bankjmpvec
	lda #>proc
	sta zp::bankjmpvec+1
	lda #FINAL_BANK_SYMBOLS
	sta zp::banktmp
	pla
	jmp __final_call
.endmacro

;******************************************************************************
; CONSTANTS
MAX_LABELS    = 256
MAX_ANON      = 1024
MAX_LOCALS    = 32
MAX_LABEL_LEN = 16	; 8 bytes for namespace + 8 for label name
SCOPE_LEN     = 8	; max len of namespace (scope)

;******************************************************************************
; ZEROPAGE
allow_overwrite = zp::labels+4

.export __label_clr
__label_clr:
	LBLJUMP clr

.CODE
.export __label_add
__label_add:
	LBLJUMP add

.export __label_find
__label_find: LBLJUMP find

.export __label_by_addr
__label_by_addr: LBLJUMP by_addr

.export __label_by_id
__label_by_id: LBLJUMP by_id

.export __label_name_by_id
__label_name_by_id: LBLJUMP name_by_id

.export __label_isvalid
__label_isvalid:
	LBLJUMP is_valid

.export __label_get_name
__label_get_name: LBLJUMP get_name

.export __label_get_addr
__label_get_addr: LBLJUMP getaddr

.export __label_is_local
__label_is_local: LBLJUMP is_local

.export __label_set
__label_set: LBLJUMP set

.export __label_set24
__label_set24: LBLJUMP set24

.export __label_del
__label_del: LBLJUMP del

.export __label_address
__label_address: LBLJUMP address

.export __label_setscope
__label_setscope: LBLJUMP set_scope

.export __label_addanon
__label_addanon: LBLJUMP add_anon

.export __label_get_fanon
__label_get_fanon: LBLJUMP get_fanon

.export __label_get_banon
__label_get_banon: LBLJUMP get_banon

;******************************************************************************
; LABELS
; Table of label names. Each entry corresponds to an entry in label_addresses,
; which contains the value for the label name.
.segment "LABELNAMES"
.export labels
labels: .res $6000

.segment "SHAREBSS"
;******************************************************************************
.export __label_num
__label_num:
numlabels: .word 0   	; total number of labels

.export __label_numanon
__label_numanon:
numanon: .word 0	; total number of anonymous labels

.segment "LABEL_BSS"
scope: .res 8		; buffer containing the current scope


;******************************************************************************
; LABEL_ADDRESSES
; Table of addresses for each label
; The address of a given label id is label_addresses + (id * 2)
.export label_addresses
label_addresses = $a000

anon_addrs = $b000

.segment "LABELS"
;******************************************************************************
; SET SCOPE
; Sets the current scope to the given scope.
; This affects local labels, which will be namespaced by prepending the scope.
; IN:
;  - .XY: the address of the scope string to set as the current scope
.proc set_scope
@scope=zp::tmp0
	stxy @scope
	ldy #$00
:	lda (@scope),y
	jsr isseparator
	beq @done
	sta scope,y
	iny
	cpy #SCOPE_LEN
	bne :-
@done:  rts
.endproc

;******************************************************************************
; PREPEND SCOPE
; Prepends the current scope to the label in .XY and returns a buffer containing
; the namespaced label.
; IN:
;  - .XY: the label to add the scope to
; OUT:
;  - .XY: pointer to the buffer containing the scope namespaced label
;  - .C: set if there is no open scope
.proc prepend_scope
@buff=$100
@lbl=zp::labels
	stxy @lbl
	ldx #$00
	lda scope
	bne @l0
	RETURN_ERR ERR_NO_OPEN_SCOPE

@l0:	lda scope,x
	beq :+
	sta @buff,x
	inx
	cpx #$08
	bne @l0

:	ldy #$00
@l1:	lda (@lbl),y
	jsr isseparator
	beq @done
	sta @buff,x
	iny
	inx
	cpx #MAX_LABEL_LEN
	bne @l1
@done:	lda #$00
	sta @buff,x
	ldxy #@buff
	RETURN_OK
.endproc

;******************************************************************************
; CLR
; Removes all labels effectively resetting the label state
.proc clr
	lda #$00
	sta scope
	sta numlabels
	sta numlabels+1
	sta numanon
	sta numanon+1
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
.proc find
@cnt=zp::tmp6
@search=zp::tmp8
@label=zp::tmpa
	stxy @label

	; check (and flag) if the label is local. if it is, we will start
	; searching at the end of the label table, where locals are stored
	jsr is_local
	beq @cont

	; if local, prepend the scope as the namespace
	ldxy @label
	jsr prepend_scope
	bcc :+
	rts		; return err
:	stxy @label

@cont:	ldx numlabels
	bne :+
	ldy numlabels+1
	bne :+
	RETURN_ERR ERR_LABEL_UNDEFINED ; no labels exist

:	lda #$00
	sta @cnt
	sta @cnt+1
	ldxy #labels
	stxy @search

@seek:	ldy #$00
@l0:	lda (@label),y
	jsr isseparator
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
	cmp #$00
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
; SET24
; Adds the label at the given 24 bit (banked) address to the label table.
; If a label already exists, its value is replaced
; IN:
;  - .A:              the bank of the label
;  - .XY:             the address of the label
;  - zp::label_value: the value to assign to the label
; OUT:
;  - .C: set on error or clear if the label was successfully added
.proc set24
@tmplabel = $140	; temporary label storage for banked labels
	stxy zp::bankaddr0
	ldxy #@tmplabel
	stxy zp::bankaddr1
	CALL FINAL_BANK_MAIN, #fe3::copyline
	ldxy #@tmplabel
; fall through
.endproc

;******************************************************************************
; SET
; Set adds the label, but doesn't produce an error if the label already exists
; IN:
;  - .XY: the name of the label to add
;  - zp::label_value: the value to assign to the given label name
; OUT:
;  - .C: set on error or clear if the label was successfully added
.proc set
	lda #$01
	sta allow_overwrite
	bne addlabel
.endproc

;******************************************************************************
; ADD
; Adds a label to the internal label state.
; IN:
;  - .XY: the name of the label to add
;  - zp::label_value: the value to assign to the given label name
; OUT:
;  - .C: set on error or clear if the label was successfully added
.proc add
	lda #$00
	sta allow_overwrite
	; fallthrough
.endproc

;******************************************************************************
; ADDLABEL
; Adds a label to the internal label state.
; IN:
;  - .XY:             the name of the label to add
;  - zp::label_value: the value to assign to the given label name
;  - allow_overwrite: if !0, will not error if label already exists
; OUT:
;  - .C: set on error or clear if the label was successfully added
.proc addlabel
@id=zp::tmp0
@label=zp::tmp2
@name=zp::tmp4
@src=zp::tmp6
@dst=zp::tmp8
@cnt=zp::tmpa
@addr=zp::tmpc
	stxy @name
	jsr is_valid
	bcc @seek
	RETURN_ERR ERR_ILLEGAL_LABEL

@seek:	; check label length
	ldy #$00
:	lda (@name),y
	jsr isseparator
	beq :+
	iny
	bne :-

:	cpy #8+1
	bcc :+
	RETURN_ERR ERR_LABEL_TOO_LONG

:	ldxy @name
	jsr find
	bcs @insert

	lda allow_overwrite
	bne :+
	RETURN_ERR ERR_LABEL_ALREADY_DEFINED

:	; label exists, overwrite its old value
	jsr by_id 		; get the address of the label
	lda zp::label_value

	ldy #$00
	sta (@addr),y

	iny
	lda zp::label_value+1
	sta (@addr),y

	RETURN_OK

@insert:
	; @id is the index where the new label will live
	stxy @id

	; flag if label is local or not
	ldxy @name
	jsr is_local
	beq @shift

	; if local, prepend the scope
	ldxy @name
	jsr prepend_scope
	bcc :+
	rts			; return err
:	stxy @name

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
	; copy the label (16 bytes) to the SYMBOL bank
	ldy #MAX_LABEL_LEN-1
:	lda (@src),y
	sta (@dst),y
	dey
	bpl :-

; shift the address too
	ldy #$00
	lda (@addr),y
	ldy #$02
	sta (@addr),y
	dey
	lda (@addr),y
	ldy #$03
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
	beq @storelabel
	jmp @sh0

;------------------
; insert the label into the new opening
@storelabel:
	ldy #$00
	; write the label
:	lda (@name),y
	beq @storeaddr
	jsr iswhitespace
	beq @storeaddr
	cmp #':'
	beq @storeaddr

	; copy a byte to the label name
	sta (@src),y

	iny
	cpy #MAX_LABEL_LEN
	bcc :-

@storeaddr:
	; 0-terminate the label name and write the label value
	lda #$00

	sta (@src),y

	lda zp::label_value
	ldy #$00
	sta (@addr),y
	lda zp::label_value+1
	iny
	sta (@addr),y

	incw numlabels
	ldxy @id
	RETURN_OK
.endproc

;******************************************************************************
; ADD_ANON
; Adds an anonymous label at the given address
; IN:
;  - .XY: the address to add an anonymous label at
; OUT:
;  - .C: set if there are too many anonymous labels to add another
.proc add_anon
@src=r0
@dst=r2
@loc=r4
@end=r6
@addr=r8
	stxy @addr
	lda numanon+1
	cmp #>MAX_ANON
	bcc :+
	lda numanon
	cmp #<MAX_ANON
	bcc :+
	lda #ERR_TOO_MANY_LABELS
	rts			; return with error (.C) set

:	lda #$00
	sta @end+1

	lda numanon
	asl
	rol @end+1
	adc #<anon_addrs
	sta @end
	lda #>anon_addrs
	adc @end+1
	sta @end+1

	jsr seek_anon
	stxy @loc
	stxy @dst
	cmpw @end
	beq @finish		; skip shift if this is the highest address

	; dst = src + 2
	lda @dst
	sec
	sbc #$02
	sta @src
	lda @dst+1
	sbc #$00
	sta @src+1

	; shift all the existing labels
@shift:	ldy #$00
	lda (@src),y
	sta (@dst),y
	iny
	lda (@src),y
	sta (@dst),y

	lda @src
	ldy @src+1
	tax
	clc
	adc #2
	sta @src
	bcc :+
	inc @src+1
:	cmpw @end	; have we shifted everything yet?
	bne @shift	; loop til we have

@finish:
	; insert the address of the anonymous label we're adding
	lda @addr
	ldy #$00
	sta (@loc),y
	lda @addr+1
	iny
	sta (@loc),y

	incw numanon
	RETURN_OK
.endproc

;******************************************************************************
; SEEK ANON
; Finds the address of the first anonymous label that has a greater address than
; or equal to the given address.
; If there is no anonymous label greater or equal to the address given,
; returns the address of the end of the anonymous labels
; (anon_addrs+(2*numanons))
; This procedure doesn't return the address represented by the anonymous label
; but rather where that label is actually stored.
; IN:
;  - .XY: the address to search for
; OUT:
;  - .XY: the address where the 1st anon label with a bigger address than the
;         one given is stored in the anon_addrs table
.proc seek_anon
@cnt=r0
@seek=r2
@addr=r4
	stxy @addr
	ldxy #anon_addrs

	lda numanon+1
	bne :+
	lda numanon
	bne :+
	; if no anonymous labels defined, return the base address
	rts

:	stxy @seek
	lda #$00
	sta @cnt
	sta @cnt+1

	ldy #$00
@l0:	lda (@seek),y	; get LSB
	tax		; .X = LSB
	incw @seek
	lda (@seek),y	; get MSB
	incw @seek

	cmp @addr+1
	bcc @next	; if MSB is < our address, check next
	bne @found	; if > we're done
	cpx @addr	; MSB is =, check LSB
	bcs @found	; if LSB is >, we're done
@next:
	incw @cnt
	lda @cnt+1
	cmp numanon+1
	bne @l0
	lda @cnt
	cmp numanon
	bne @l0		; loop til we've checked all anonymous labels

	; none found, fall through to get last address
@found:
	lda #$00
	sta @seek+1
	lda @cnt
	asl
	rol @seek+1
	adc #<anon_addrs
	tax
	lda @seek+1
	adc #>anon_addrs
	sta @seek+1
	tay
	rts
.endproc

;******************************************************************************
; GET_FANON
; Returns the address of the nth forward anonymous label relative to the given
; address. That is the nth anonymous label whose address is greater than
; the given address.
; IN:
;  - .XY: the address relative to the anonymous label to get
;  - .A:  how many anonymous labels forward to look
; OUT:
;  - .A:  the size of the address
;  - .XY: the nth anonymous label whose address is > than the given address
;  - .C:  set if there is not an nth forward anonymous label
.proc get_fanon
@cnt=r0
@fcnt=r2
@addr=r4
@seek=r6
	stxy @addr
	sta @fcnt

	ldxy #anon_addrs
	stxy @seek

	ldxy numanon
	cmpw #0
	beq @err		; no anonymous labels defined
	stxy @cnt

@l0:	ldy #$01		; MSB
	lda @addr+1
	cmp (@seek),y
	beq @chklsb		; if =, check the LSB
	bcs @next		; MSB is < what we're looking for, try next

	; MSB is >= base and LSB is >= base address
@f:	dec @fcnt		; is this the nth label yet?
	beq @found		; if our count is 0, yes, end
	bne @next		; if count is not 0, continue

@chklsb:
	dey
	lda @addr
	cmp (@seek),y		; check if our address is less than the seek one
	bcc @f			; if our address is less, this is a fwd anon

@next:	incw @seek
	incw @seek

	; loop until we run out of anonymous labels to search
	lda @cnt
	bne :+
	dec @cnt+1
	bmi @err
	bpl @l0
:	dec @cnt
	jmp @l0

@err:	RETURN_ERR ERR_LABEL_UNDEFINED

@found:	ldy #$01
	lda (@seek),y		; get the MSB of our anonymous label
	pha
	dey
	lda (@seek),y		; get the LSB
	tax
	pla
	tay
	bne :+
	lda #$01		; if MSB is 0, size is 1
	skw
:	lda #$02		; if MSB !0, size is 2
	RETURN_OK
.endproc

;******************************************************************************
; GET_BANON
; Returns the address of the nth backward anonymous address relative to the
; given  address. That is the nth anonymous label whose address is less than
; the given address.
; IN:
;  - .XY: the address relative to the anonymous label to get
;  - .A:  how many anonymous labels backwards to look
; OUT:
;  - .XY: the nth anonymous label whose address is < than the given address
.proc get_banon
@cnt=r0
@bcnt=r8
@addr=r4
@seek=r6
	stxy @addr
	sta @bcnt

	; get address to start looking backwards from
	jsr seek_anon
	stxy @seek

	ldxy numanon
	cmpw #0
	beq @err		; no anonymous labels defined
	stxy @cnt

@l0:	ldy #$01		; MSB
	lda @addr+1
	cmp (@seek),y
	beq @chklsb		; if =, check the LSB
	bcc @next		; MSB is > what we're looking for, try next

	; MSB is >= base and LSB is >= base address
@b:	dec @bcnt		; is this the nth label yet?
	beq @found		; if our count is 0, yes, end
	bne @next		; if count is not 0, continue

@chklsb:
	dey
	lda @addr
	cmp (@seek),y		; check if our address is less than the seek one
	bcs @b			; if our address is <= this is a backward anon

@next:	lda @seek
	sec
	sbc #2
	sta @seek
	tax
	bcs :+
	dec @seek+1

:	; loop until we run out of anonymous labels to search
	ldy @seek+1
	cmpw #anon_addrs-2
	bne @l0

@err:	RETURN_ERR ERR_LABEL_UNDEFINED

@found:	ldy #$00
	lda (@seek),y		; get the MSB of our anonymous label
	tax
	iny
	lda (@seek),y		; get the LSB
	tay
	bne :+
	lda #$01		; if MSB is 0, size is 1
	skw
:	lda #$02		; if MSB !0, size is 2
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
.proc address
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

;******************************************************************************
; DEL
; Deletes the given label name.
; IN:
;  - .XY: the address of the label name to delete
.proc del
@id=zp::tmp6
@cnt=zp::tmp8
@cnt2=zp::tmpa
@src=zp::tmpe
@dst=zp::tmp10
@name=zp::tmp12
	stxy @name
	jsr find
	bcc @del
	rts		; not found

@del:	stxy @id
	jsr by_id
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
	ldy #$00
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
	jsr name_by_id
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
	ldy #MAX_LABEL_LEN-1
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
	RETURN_OK
.endproc

;******************************************************************************
; IS_LOCAL
; Returns with .Z set if the given label is a local label (begins with '@')
; IN:
;  - .XY: the label to test
; OUT:
;  - .A: nonzero if the label is local
;  - .Z: clear if label is local, set if not
.proc is_local
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
; OUT:
;  - .XY: the address of the given label id
;  - zp::tmpc: the address of the label (same as .XY)
.proc by_id
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
.proc by_addr
@other=zp::tmpc
@addr=zp::tmpe
@cnt=zp::tmp10
	stxy @addr
	ldxy #$ffff
	stxy @cnt
@l0:	incw @cnt
	ldxy @cnt
	cmpw numlabels
	beq @notfound
	jsr by_id
	stxy @other

	ldy #$00
	lda (@other),y
	cmp @addr
	bne @l0

	iny
	lda (@other),y
	cmp @addr+1
	bne @l0

@found:	ldxy @cnt
	RETURN_OK

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
.proc name_by_id
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
; checks if the label name given is a valid label name
; IN:
;  - .XY: the address of the label
; OUT:
;  - .C: set if the label is NOT valid
.proc is_valid
@name=zp::tmp4
	stxy @name
	ldy #$00
; first character must be a letter or '@'
:	lda (@name),y
	iny
	jsr iswhitespace
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
@l0:	lda (@name),y
	jsr isseparator
	beq @done
	cmp #'0'
	bcc @err
	cmp #'Z'+1
	iny
	bcc @l0
@err:	RETURN_ERR ERR_ILLEGAL_LABEL
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
.proc get_name
@dst=zp::tmp0
@src=zp::labels
	jsr name_by_id
	stxy @src

	ldy #$00
@l0:	lda (@src),y
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
.proc getaddr
@src=zp::labels
	jsr by_id
	stxy @src

	ldy #$00
	lda (@src),y
	tax
	iny
	lda (@src),y
	tay
	rts
.endproc


;******************************************************************************
; ISWHITESPACE
; Checks if the given character is a whitespace character
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if if the character in .A is whitespace
.proc iswhitespace
	cmp #$0d	; newline
	beq :+
	cmp #$09	; TAB
	beq :+
	cmp #' '
:	rts
.endproc

;******************************************************************************
; is_null_space_comma_closingparen
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is: 0,$0d,' ', ',', or ')'
.proc is_null_return_space_comma_closingparen_newline
	cmp #$00
	beq @done
	jsr iswhitespace
	beq @done
	cmp #','
	beq @done
	cmp #')'
@done:	rts
.endproc

;******************************************************************************
; IS_OPERATOR
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is an operator ('+', '-', etc.)
.proc isoperator
@xsave=zp::util+2
	stx @xsave
	ldx #@numops-1
:	cmp @ops,x
	beq @end
	dex
	bpl :-
@end:	php
	ldx @xsave
	plp
	rts
@ops: 	.byte '(', ')', '+', '-', '*', '/', '[', ']', '^', '&', '.'
@numops = *-@ops
.endproc

;******************************************************************************
; ISSEPARATOR
; IN:
;  - .A: the character to test
; OUT:
;  - .Z: set if the char in .A is any separator
.proc isseparator
	cmp #':'
	beq @yes
	jsr is_null_return_space_comma_closingparen_newline
	bne :+
@yes:	rts
:	jmp isoperator
.endproc
