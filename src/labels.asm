;*******************************************************************************
; LABELS.ASM
; This file defines procedures for creating and retrieving labels.
; Labels map a text string to an address in memory.  They can be looked up
; by address or name.  They are stored in a sorted list to enable efficient
; alphabetic retrieval and are also indexed by address (value) to allow for
; efficient retrieval by address.
;*******************************************************************************

.include "errors.inc"
.include "macros.inc"
.include "ram.inc"
.include "zeropage.inc"

;*******************************************************************************
; CONSTANTS
MAX_ANON      = 750	; max number of anonymous labels
MAX_LABEL_LEN = 32	; 8 bytes for namespace + 16 for label name
SCOPE_LEN     = 8	; max len of namespace (scope)
MAX_LABELS    = 750

;*******************************************************************************
; ZEROPAGE
allow_overwrite = zp::labels+4	; when !0, addlabel will overwrite existing

.export __label_clr
.export __label_add
.export __label_find
.export __label_by_addr
.export __label_by_id
.export __label_name_by_id
.export __label_isvalid
.export __label_get_name
.export __label_get_addr
.export __label_is_local
.export __label_set
.export __label_set24
.export __label_del
.export __label_address
.export __label_setscope
.export __label_addanon
.export __label_get_fanon
.export __label_get_banon
.export __label_index
.export __label_id_by_addr_index

.if FINAL_BANK_SYMBOLS=FINAL_BANK_MAIN

;*******************************************************************************
; Flat memory procedure mappings
__label_clr              = clr
__label_add		 = add
__label_find             = find
__label_by_addr          = by_addr
__label_by_id            = by_id
__label_name_by_id       = name_by_id
__label_isvalid          = is_valid
__label_get_name         = get_name
__label_get_addr         = getaddr
__label_is_local         = is_local
__label_set              = set
__label_set24            = set24
__label_del              = del
__label_address          = address
__label_setscope         = set_scope
__label_addanon          = add_anon
__label_get_fanon        = get_fanon
__label_get_banon        = get_banon
__label_index            = index
__label_id_by_addr_index = id_by_addr_index

.else
;******************************************************************************
; Label JUMP table
.macro LBLJUMP proc_id
	pha
	lda #proc_id
	bpl do_label_proc
.endmacro

.enum proc_ids
CLR = 0
ADD
FIND
BY_ADDR
BY_ID
NAME_BY_ID
IS_VALID
GET_NAME
GETADDR
IS_LOCAL
SET
SET24
DEL
ADDRESS
SET_SCOPE
ADD_ANON
GET_FANON
GET_BANON
INDEX
ID_BY_ADDR_INDEX
.endenum

.RODATA

.linecont +
.define procs clr, add, find, by_addr, by_id, name_by_id, is_valid, get_name, getaddr, is_local, set, set24, del, address, set_scope, add_anon, get_fanon, get_banon, index, id_by_addr_index
.linecont -

procs_lo: .lobytes procs
procs_hi: .hibytes procs

.CODE
__label_clr: LBLJUMP proc_ids::CLR
__label_add: LBLJUMP proc_ids::ADD
__label_find: LBLJUMP proc_ids::FIND
__label_by_addr: LBLJUMP proc_ids::BY_ADDR
__label_by_id: LBLJUMP proc_ids::BY_ID
__label_name_by_id: LBLJUMP proc_ids::NAME_BY_ID
__label_isvalid: LBLJUMP proc_ids::IS_VALID
__label_get_name: LBLJUMP proc_ids::GET_NAME
__label_get_addr: LBLJUMP proc_ids::GETADDR
__label_is_local: LBLJUMP proc_ids::IS_LOCAL
__label_set: LBLJUMP proc_ids::SET
__label_set24: LBLJUMP proc_ids::SET24
__label_del: LBLJUMP proc_ids::DEL
__label_address: LBLJUMP proc_ids::ADDRESS
__label_setscope: LBLJUMP proc_ids::SET_SCOPE
__label_addanon: LBLJUMP proc_ids::ADD_ANON
__label_get_fanon: LBLJUMP proc_ids::GET_FANON
__label_get_banon: LBLJUMP proc_ids::GET_BANON
__label_index: LBLJUMP proc_ids::INDEX
__label_id_by_addr_index: LBLJUMP proc_ids::ID_BY_ADDR_INDEX

;******************************************************************************
; Entrypoint for label routines
.proc do_label_proc
	stx @savex
	tax
	lda procs_lo,x
	sta zp::bankjmpvec
	lda procs_hi,x
	sta zp::bankjmpvec+1
	lda #FINAL_BANK_SYMBOLS
	sta zp::banktmp
@savex=*+1
	ldx #$00
	pla
	jmp __ram_call
.endproc
.export __label_clr
.endif

;******************************************************************************
; LABELS
; Table of label names. Each entry corresponds to an entry in label_addresses,
; which contains the value (address) for the label name.
.segment "LABELNAMES"
.export labels

.ifdef vic20
labels: .res $6000
.else
labels:
.endif

.segment "SHAREBSS"
;******************************************************************************
.export __label_num
__label_num:
numlabels: .word 0   	; total number of labels

.export __label_numanon
__label_numanon:
numanon: .word 0	; total number of anonymous labels

.segment "LABEL_BSS"

;******************************************************************************
; LABEL ADDRESSES
; Table of addresses for each label
; The address of a given label id is label_addresses + (id * 2)
; Labels are also stored sorted by address in label_addresses_sorted.
; A corresponding array maps the sorted addresses to their ID.
;
; e.g. for the following labels:
;    | label |  id   |  address |
;    |-------|-------|----------|
;    |   A   |   1   |  $1003   |
;    |   B   |   2   |  $1009   |
;    |   C   |   3   |  $1000   |
;
; the sorted addresses will look like this:
;    | address_sorted| sorted_id |
;    |---------------|-----------|
;    |    $1000      |    3      |
;    |    $1003      |    1      |
;    |    $1009      |    2      |
.export label_addresses
label_addresses: .res MAX_LABELS*2

.assert * & $01 = $00, error, "label_addresses_sorted must be word aligned"

.export label_addresses_sorted
label_addresses_sorted:     .res MAX_LABELS*2
label_addresses_sorted_ids: .res MAX_LABELS*2

scope: .res 8 ; buffer containing the current scope

; address table for each anonymous label
.export anon_addrs
anon_addrs: .res MAX_ANON*2

.segment "LABELS"
;******************************************************************************
; SET SCOPE
; Sets the current scope to the given scope.
; This affects local labels, which will be namespaced by prepending the scope.
; IN:
;  - .XY: the address of the scope string to set as the current scope
.proc set_scope
@scope=r0
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
	cpx #SCOPE_LEN
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
@cnt=r6
@search=r8
@label=ra
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
	CALL FINAL_BANK_MAIN, ram::copyline
	ldxy #@tmplabel

	; fall through to SET
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
	skw

	; fallthrough to ADD
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

	; fallthrough to ADDLABEL
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
@id=r0
@label=r2
@name=r4
@src=r6
@dst=r8
@cnt=ra
@addr=rc
	sta allow_overwrite	; set overwrite flag (SET) or clear (ADD)

	stxy @name
	jsr is_valid
	bcc @seek
	rts			; return err

@seek:	; get the label length
	ldy #$00
:	lda (@name),y
	jsr isseparator
	beq @lenfound
	iny
	bne :-

@lenfound:
	ldxy @name
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
	; src = labels + (numlabels-1)*MAX_LABEL_LEN
	lda numlabels+1
	sta @src+1
	lda numlabels

	; * MAX_LABEL_LEN
	asl			; *2
	rol @src+1
	asl			; *4
	rol @src+1
	asl			; *8
	rol @src+1
	asl			; *16
	rol @src+1
	asl			; *32
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

	; src -= MAX_LABEL_LEN
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
	; copy the label (MAX_LABEL_LEN bytes) to the SYMBOL bank
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
; ADD ANON
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
;  - .C: set if the given address is greater than all in the table
;        (if .XY represents an address outside the range of the table)
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

@next:	incw @cnt
	lda @cnt+1
	cmp numanon+1
	bne @l0
	lda @cnt
	cmp numanon
	bne @l0		; loop til we've checked all anonymous labels

	; none found, fall through to get last address
	jsr @found
	sec		; given address is > all in table
	rts

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
	clc
	rts
.endproc

;******************************************************************************
; GET FANON
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
; GET BANON
; Returns the address of the nth backward anonymous address relative to the
; given  address. That is the nth anonymous label whose address is less than
; the given address.
; IN:
;  - .XY: the address relative to the anonymous label to get
;  - .A:  how many anonymous labels backwards to look
; OUT:
;  - .XY: the nth anonymous label whose address is < than the given address
;  - .C: set if there is no backwards label matching the given address
.proc get_banon
@bcnt=r8
@addr=r4
@seek=r6
	stxy @addr
	sta @bcnt

	; get address to start looking backwards from
	jsr seek_anon
	bcc :+

	; if we ended after the end of the anonymous label list, move
	; to a valid location in it (to the last item)
	txa
	sbc #$02
	tax
	tya
	sbc #$00
	tay

:	stxy @seek
	ldxy numanon
	cmpw #0
	beq @err		; no anonymous labels defined

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
; LABEL ADDRESS
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
@table=r0
	jsr find	; get the id in YX
	bcc :+
	lda #ERR_LABEL_UNDEFINED
	rts

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
@id=r6
@cnt=r8
@cnt2=ra
@src=re
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

	; get the source (destination + MAX_LABEL_LEN)
	ldxy @id
	jsr name_by_id
	stxy @dst
	lda @dst
	clc
	adc #MAX_LABEL_LEN
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
	adc #MAX_LABEL_LEN
	sta @src
	bcc :+
	inc @src+1
:	lda @dst
	clc
	adc #MAX_LABEL_LEN
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
; IS LOCAL
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
	lda #$01	; flag that label IS local
	rts
:	lda #$00	; flag that label is NOT local
	rts
.endproc

;******************************************************************************
; LABEL BY ID
; Returns the address of the label ID in .YX in .YX
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XY: the address of the given label id
;  - rc:  the address of the label (same as .XY)
.proc by_id
@addr=rc
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
; BY ADDR
; Returns the label for a given address by performing a binary search on the
; cache of sorted label addresses
; NOTE: Labels must be indexed (lbl::index) in order for this function to return
; the correct ID. If you've added a label since the last index, it is necessary
; to re-index.
; IN:
;  - .XY: the label address to get the name of
; OUT:
;  - .XY: the ID of the label (exact match or closest one at address less than
;         the one provided.
;  - .C: set if no EXACT match for the label is found
.proc by_addr
@addr=ra
@lb=rc
@ub=re
@m=zp::tmp10
@top=zp::tmp12
	stxy @addr

	lda numlabels
	asl
	sta @ub
	lda numlabels+1
	rol
	sta @ub+1

	; @lb = label_addresses_sorted
	; @ub = label_addresses_sorted + (numlabels*2)
	lda #<label_addresses_sorted
	sta @lb
	adc @ub
	sta @ub
	sta @top
	lda #>label_addresses_sorted
	sta @lb+1
	adc @ub+1
	sta @ub+1
	sta @top+1

@loop:	lda @ub
	sec
	sbc @lb
	tax
	lda @ub+1
	sbc @lb+1
	bcc @done	; if low > high, not found
	lsr		; calculate (high-low) / 2
	tay
	txa
	ror		; carry cleared because multiple of 2
	and #$02	; align to element size
	adc @lb		; mid = low + ((high - low) / 2)
	sta @m
	tya
	adc @lb+1
	sta @m+1
	lda @addr+1	; load target value MSB
	ldy #1		; load index to MSB
	cmp (@m),Y	; compare MSB
	beq @chklsb
	bcc @modhigh	; A[mid] > value

@modlow:
	; A[mid] < value
	lda @m		; low = mid + element size
	adc #2-1	; carry always set
	sta @lb
	lda @m+1
	adc #0
	sta @lb+1
	jmp @loop

@chklsb:
	lda @addr	; load target value LSB
	dey		; set index to LSB
	cmp (@m),Y	; compare LSB
	beq @done
	bcs @modlow	; A[mid] < value

@modhigh:		; A[mid] > value
	lda @m		; high = mid - element size
	;clc
	sbc #2-1	; carry always clear
	sta @ub
	lda @m+1
	sbc #0
	sta @ub+1
	jmp @loop

@done:	bcc @err

@ok:	; look up the ID for the address
	lda @m
	clc
	adc #<(label_addresses_sorted_ids - label_addresses_sorted)
	sta @m

	lda @m+1
	adc #>(label_addresses_sorted_ids - label_addresses_sorted)
	sta @m+1

	ldy #$00
	lda (@m),y
	tax
	iny
	lda (@m),y
	tay
	RETURN_OK

@err:	ldxy @ub	; get the lower bound of where our search ended
	stxy @m		; and set our result variable to it (ub < lb here)
	jsr @ok		; get the closest label
	cmpw numlabels	; was the result a valid label?
	bcc :+		; if so, continue to return

	; if label wasn't valid, get the highest label by address
	lda @top
	;sec
	sbc #$02
	sta @m
	lda @top+1
	sbc #$00
	sta @m+1
	jsr @ok

:	sec
	rts
.endproc

;******************************************************************************
; ID BY ADDR INDEX
; Returns the ID of the nth label sorted by address.
; IN:
;   - .XY: the index of the label to get from the sorted addresses
; OUT:
;   - .XY: the id of the nth label (in sorted order)
.proc id_by_addr_index
@tmp=rc
	txa
	asl
	sta @tmp
	tya
	rol
	sta @tmp+1
	lda @tmp
	adc #<label_addresses_sorted_ids
	sta @tmp
	lda @tmp+1
	adc #>label_addresses_sorted_ids
	sta @tmp+1
	ldy #$00
	lda (@tmp),y
	tax
	iny
	lda (@tmp),y
	tay
	rts
.endproc

;******************************************************************************
; NAME BY ID
; Returns the address name of the label ID in .YX in .YX
; IN:
;  - .XY: the id of the label to get the address of
; OUT:
;  - .XY: the address of the name for the given label id
.proc name_by_id
@addr=zp::labels
	sty @addr+1
	txa
	asl		; *2
	rol @addr+1
	asl		; *4
	rol @addr+1
	asl		; *8
	rol @addr+1
	asl		; *16
	rol @addr+1
	asl		; *32
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
@name=r4
	stxy @name
	ldy #$00

; first character must be a letter or '@'
@l0:	lda (@name),y
	iny
	jsr iswhitespace
	beq @l0
	cmp #'@'
	beq @cont
	cmp #'a'
	bcc @err
	cmp #'Z'+1
	bcs @err

	;jsr getopcode	; make sure string is not an opcode
	;bcs @cont
	;sec
	;rts

	; following characters must be between '0' and 'Z'
@cont:	ldx #$00
@l1:	inx
	cpx #(MAX_LABEL_LEN/2)+1
	bcs @toolong
	lda (@name),y
	jsr isseparator
	beq @done
	cmp #'0'
	bcc @err
	cmp #'Z'+1
	iny
	bcc @l1
@err:	RETURN_ERR ERR_ILLEGAL_LABEL
@toolong:
	RETURN_ERR ERR_LABEL_TOO_LONG
@done:	RETURN_OK
.endproc

;******************************************************************************
; GET NAME
; Copies the name of the label ID given to the provided buffer
; IN:
;  - .XY: the ID of the label to get the name of
;  - r0:  the address to copy to
; OUT:
;  - (r0): the label name
.proc get_name
@dst=r0
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
; GET ADDR
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
; IS NULL SPACE COMMA CLOSINGPAREN
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

;******************************************************************************
; MACROS
; These macros are used by sort_by_addr

;******************************************************************************
; update @idi and @idj based on the values of @i and @j
; these pointers are offset by a fixed amount from @i and @j
.macro setptrs
	lda @i
	clc
	adc #<(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idi
	lda @i+1
	adc #>(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idi+1

	lda @j
	clc
	adc #<(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idj
	lda @j+1
	adc #>(label_addresses_sorted_ids-label_addresses_sorted)
	sta @idj+1
.endmacro

;******************************************************************************
; copies the unsorted addresses to the sorted addresses array and initializes
; the unsorted ids array
.macro setup
@cnt=r0
@src=r2
@dst=r4
@id=r0
	; @cnt = numlabels*2
	lda numlabels
	sta @cnt
	lda numlabels+1
	sta @cnt+1

	ldxy #label_addresses
	stxy @src
	ldxy #label_addresses_sorted
	stxy @dst

	; copy the addresses
	ldy #$00
@l0:	lda (@src),y
	sta (@dst),y
	iny
	lda (@src),y
	sta (@dst),y
	iny
	bne :+
	inc @src+1	; next page
	inc @dst+1

:	decw @cnt
	bne @l0
	lda @cnt+1
	bne @l0

	; init the unsorted ids array
	ldxy #label_addresses_sorted_ids
	stxy @dst

	lda #$00
	sta @id
	sta @id+1
	tay

@idloop:
	lda @id
	sta (@dst),y	; store LSB
	iny
	lda @id+1
	sta (@dst),y	; store MSB
	iny
	bne :+
	inc @dst+1	; next page

:	incw @id
	lda @id
	cmp numlabels
	bne @idloop
	lda @id+1
	cmp numlabels+1
	bne @idloop
.endmacro

;******************************************************************************
; INDEX
; Updates the by-address sorting of the labels. This allows labels to be looked
; up by their address (see lbl::by_addr).
;
; Code adapted from code by Vladimir Lidovski aka litwr (with help of BigEd)
; via codebase64.org
.proc index
@i   = r0
@j   = r2
@x   = r4
@ub  = r6
@lb  = r8
@tmp = ra
@num = rc
@idi = zp::tmp10
@idj = zp::tmp12
	lda numlabels
	ora numlabels+1
	bne @setup
	rts			; nothing to index

@setup:	setup

	; @num = 2*(numlabels-1)
	lda numlabels
	sec
	sbc #$01
	sta @num
	lda numlabels+1
	sbc #$00
	sta @num+1
	asl @num
	rol @num+1
	jmp @quicksort		; enter the sort routine

@quicksort0:
	tsx
	cpx #16		; stack limit
	bcs @qsok

@qs_csp=*+1
	ldx #$00
	txs

@quicksort:
	lda #<label_addresses_sorted
	clc
	adc @num
	sta @ub
	lda #>label_addresses_sorted
	adc @num+1
	sta @ub+1

	lda #>label_addresses_sorted
	sta @lb+1
	lda #<label_addresses_sorted
	sta @lb

	tsx
	stx @qs_csp

@qsok:	; @i = @lb
	lda @lb
	sta @i
	lda @lb+1
	sta @i+1

	; @j = @ub
	ldy @ub+1
	sty @j+1
	lda @ub
	sta @j

	; @tmp = (@j + @i) / 2
	clc		; this code works only for the evenly aligned arrays
	adc @i
	and #$fc
	sta @tmp
	tya
	adc @i+1
	ror
	sta @tmp+1
	ror @tmp

	; @x = array[(@j+@i) / 2]
	ldy #$00
	lda (@tmp),y
	sta @x
	iny
	lda (@tmp),y
	sta @x+1

@qsloop1:
	; while (array[i] > @x) { inc @i }
	ldy #$00		; compare array[i] and x
	lda (@i),y
	cmp @x
	iny
	lda (@i),y
	sbc @x+1
	bcs @qs_l1
	lda #$02	; move @i to next element
	adc @i
	sta @i
	bcc @qsloop1
	inc @i+1
	bne @qsloop1	; branch always

@qs_l1:	ldy #$00	; compare array[j] and x
	lda @x
	cmp (@j),y
	iny
	lda @x+1
	sbc (@j),y
	bcs @qs_l3

	lda @j
	sec
	sbc #$02	; move @j to prev element
	sta @j
	bcs @qs_l1
	dec @j+1
	bne @qs_l1	; branch always

@qs_l3:
	lda @j		; compare i and j
	cmp @i
	lda @j+1
	sbc @i+1
	bcc @qs_l8

@qs_l6:	setptrs
	lda (@j),y	; swap array[@i] and array[@j]
	tax
	lda (@i),y
	sta (@j),y
	txa
	sta (@i),y

	lda (@idj),y	; swap ids[@i] and ids[@j]
	tax
	lda (@idi),y
	sta (@idj),y
	txa
	sta (@idi),y

	dey
	bpl @qs_l6

	clc
	lda #$02
	adc @i
	sta @i
	bcc :+
	inc @i+1
:	sec
	lda @j
	sbc #$02
	sta @j
	bcs :+
	dec @j+1
	;lda @j
:	cmp @i
	lda @j+1
	sbc @i+1
	;bcc *+5
	jmp @qsloop1

@qs_l8:	lda @lb
	cmp @j
	lda @lb+1
	sbc @j+1
	bcs @qs_l5

	lda @i+1
	pha
	lda @i
	pha
	lda @ub+1
	pha
	lda @ub
	pha
	lda @j+1
	sta @ub+1
	lda @j
	sta @ub
	jsr @quicksort0

	pla
	sta @ub
	pla
	sta @ub+1
	pla
	sta @i
	pla
	sta @i+1

@qs_l5:	lda @i
	cmp @ub
	lda @i+1
	sbc @ub+1
	bcs @qs_l7

	lda @i+1
	sta @lb+1
	lda @i
	sta @lb
	jmp @qsok
@qs_l7: rts
.endproc
