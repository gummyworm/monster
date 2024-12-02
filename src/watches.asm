.include "bitmap.inc"
.include "config.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "finalex.inc"
.include "flags.inc"
.include "gui.inc"
.include "key.inc"
.include "keycodes.inc"
.include "labels.inc"
.include "layout.inc"
.include "macros.inc"
.include "memory.inc"
.include "source.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "vmem.inc"
.include "zeropage.inc"

;******************************************************************************
; CONSTANTS
HEIGHT = WATCHVIEW_STOP-WATCHVIEW_START-1

.BSS
;******************************************************************************
scroll: .byte 0
row:	.byte 0

.CODE
;******************************************************************************
; INIT
; Initializes the watch editor
.export __watches_init
.proc __watches_init
	lda #$00
	sta row
	sta scroll
	rts
.endproc

;******************************************************************************
; EDIT
; Begins the breakpoint editor
.export __watches_edit
.proc __watches_edit
	; display the title
	ldxy #@menu
	lda #WATCHVIEW_STOP
	jmp gui::listmenu
@menu:
.byte HEIGHT				; max height
.word @getkey				; key handler
.word @getdata				; get line handler
.word dbg::numwatches			; # of breakpoints pointer
.word strings::watches_title		; title

;--------------------------------------
@getkey:
	cmp #K_RETURN
	bne @chkdel

	; invoke the memory editor at the selected watch's address
	lda row
	clc
	adc scroll
	tax
	lda dbg::watcheslo,x
	sta view::addr
	lda dbg::watcheshi,x
	sta view::addr+1
	jsr view::edit		; invoke the memory editor
	sec			; after returning from mem editor, quit
	rts

@chkdel:
	cmp #K_DEL		; DEL
	bne @done

	lda row
	clc
	adc scroll
	tax
	jsr __watches_remove

@done:	RETURN_OK

;--------------------------------------
@getdata:
@cnt=zp::tmp13
@range=r3	; if !0, start != stop (watch is a range)
@start=r4	; start address
@stop=r6	; stop address (same as start if NOT range)
@val=r8		; value of watch (if NOT range)
@prev=r9	; previous value of watch (if NOT range)
	sta @cnt

	jsr __watches_getdata
	tax				; save flags

	lda @start
	cmp @stop
	beq :+
	inc @range		; flag that start != stop

:	lda @start+1
	cmp @stop+1
	beq :+
	inc @range		; flag that start != stop

:	; print the watch info
	lda @range		; is it a range of addresses?
	beq @valline		; not a range, continue

; if the start address != stop address, print start and stop
@rangeline:
	; push the stop address
	lda @stop
	pha
	lda @stop+1
	pha

	; push the start address
	lda @start
	pha
	lda @start+1
	pha

	; push SPACE if not dirty or '!' if dirty
	ldy #' '
	txa				; get flags
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value
	ldy #'!'			; dirty
:	tya
	pha

	; if start addr != stop addr, print the address range
	ldx #<strings::watches_range_line
	ldy #>strings::watches_range_line
	bne @getdatadone

; if the start address == stop address, just print the one address and its val
@valline:
	; push current value of the watch
	lda @val
	pha

	; is this watch dirty?
	ldxy #strings::watches_line	; default to "clean" string
	txa				; get flags
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value

	; dirty, push previous value
	lda @prev
	pha				; push previous value if dirty
	ldxy #strings::watches_changed_line

:	; push the address
	lda @start
	pha
	lda @start+1
	pha

@getdatadone:
	jsr text::render
	rts
.endproc

;******************************************************************************
; UPDATE
; Reads the new values of all locations being watched and stores them
; If the watch represents a RANGE of values, does not store new value.
; OUT:
;  - .A: the ID of the first watch triggered (if any)
;  - .C: set if any watch was triggered
.export __watches_update
.proc __watches_update
@cnt=r0
@addr=r1
	ldx dbg::numwatches
	beq @done
	dex
	stx @cnt
@l0:	ldx @cnt
	lda dbg::watch_vals,x
	sta dbg::watch_prevs,x	; store curr value as prev

	ldx @cnt
	lda dbg::watcheslo,x
	ldy dbg::watcheshi,x
	tax

	jsr vmem::load
	ldx @cnt
	cmp dbg::watch_prevs,x	; same as old value?
	beq :+
	pha
	lda #WATCH_DIRTY
	ora dbg::watch_flags,x
	sta dbg::watch_flags,x	; mark value as DIRTY
	pla
:	sta dbg::watch_vals,x	; store new value
	dec @cnt
	bpl @l0
@done:	rts
.endproc

;******************************************************************************
; IN RANGE
; Checks if the given address is in the range of the given watch
; IN:
;  - .XY: the address to check
;  - .A:  the ID of the watch to check
; OUT:
;  - .Z: set if the given address IS within the given watch's range
.proc in_range
@addr=r0
@watchstart=r2
@watchstop=r4
	stxy @addr
	tax
@chklo:
	lda @addr+1
	cmp dbg::watcheshi,x
	bcc @no 		; if MSB < addr's, not in range of this watch
	beq :+
	bcs @chkstop		; if MSB > addr's, addr is above low bound

:	lda @addr
	cmp dbg::watcheslo,x
	bcc @no			; if MSB == addr's and LSB > addr's, try next

@chkstop:
	lda @addr+1
	cmp dbg::watches_stophi,x
	bcc @yes
	beq :+
	bcs @no			; if MSB > addr, not in range of this watch
:	lda @addr
	cmp dbg::watches_stoplo,x ; if LSB <= addr, addr is in range
	beq @yes
	bcc @yes
@no:	sec
	rts
@yes:	RETURN_OK
.endproc

;******************************************************************************
; MARK
; Marks the watch for the given address (if there is one) as DIRTY. Even if
; its value has not changed.
; IN:
;  - .XY: the address to mark dirty (if a watch exists for it)
; OUT:
;  - .C: set if the given address was being watched by at least 1 watch
.export __watches_mark
.proc __watches_mark
@cnt=r6
@addr=r7
@found=r9
	lda #$00
	sta @found

	stxy @addr
	ldx dbg::numwatches
	beq @done
	dex
	stx @cnt

@l0:	lda @cnt
	ldxy @addr
	jsr in_range		; is the address in range for this watch?
	bcs @next
	ldx @cnt
	lda #WATCH_DIRTY
	sta dbg::watch_flags,x	; mark this watchpoint as DIRTY
	inc @found

@next:	dec @cnt
	bpl @l0
@done:	lda @found
	cmp #$01		; set .C if @found >= 1
	rts
.endproc

;******************************************************************************
; ADD
; Adds a watch for the given memory location.
; IN:
;  - .XY: the address to add a watch for
;  - r0:  the STOP address to add the watch for
;  - .A:  the type of watch (WATCH_LOAD, WATCH_STORE, or WATCH_LOAD_STORE)
.export __watches_add
.proc __watches_add
@stop=r0
@addr=r2
@flags=r4
	sta @flags
	stxy @addr
	ldx dbg::numwatches
	beq :+

	; check if watch (in r2) already exists
	jsr getwatch
	bcs @done		; already a watch here, exit

	ldx dbg::numwatches
:	lda @addr
	sta dbg::watcheslo,x
	lda @addr+1
	sta dbg::watcheshi,x

	lda @stop
	sta dbg::watches_stoplo,x
	lda @stop+1
	sta dbg::watches_stophi,x

	ldxy @addr
	jsr vmem::load
	ldx dbg::numwatches
	sta dbg::watch_vals,x

	lda @flags
	sta dbg::watch_flags,x

	inc dbg::numwatches
@done:	rts
.endproc

;******************************************************************************
; REMOVE
; Removes the watch with the given ID
; IN:
;  - .A: the ID of the watch to remove
.export __watches_remove
.proc __watches_remove
@id=r6
	cmp dbg::numwatches
	bcs @done		; can't remove (invalid ID)

	; shift watches down
	tax
@l0:	lda dbg::watcheshi+1,x
	sta dbg::watcheshi,x
	lda dbg::watcheslo+1,x
	sta dbg::watcheslo,x
	inx
	cpx dbg::numwatches
	bcc @l0
@removed:
	dec dbg::numwatches
@done:	clc
@ret:	rts
.endproc

;******************************************************************************
; GETWATCH DATA
; Returns the properties of the given watch
; IN:
;   - .A: the ID of the watch to get the properties of
; OUT:
;   - .A: the flags for the watch
;   - .X: the number of watches total
;   - r4: the start address of the watch
;   - r6: the stop address of the watch
;   - r8: the current value of the watched address
;   - r9: the previous value of the watched address
.export __watches_getdata
.proc __watches_getdata
@start=r4
@stop=r6
@val=r8
@prev=r9
	tax

	lda dbg::watcheslo,x
	sta @start
	lda dbg::watcheshi,x
	sta @start+1

	lda dbg::watches_stoplo,x
	sta @stop
	lda dbg::watches_stophi,x
	sta @stop+1

	lda dbg::watch_vals,x
	sta @val

	lda dbg::watch_prevs,x
	sta @prev

	lda dbg::watch_flags,x
	ldx dbg::numwatches
	rts
.endproc

;******************************************************************************
; GETWATCH
; Returns the index of the watch at the given address
; IN:
;  - r2: the address of the watch
; OUT:
;  - .C: set if the watch exists
;  - .X: the id of the watch
.export getwatch
.proc getwatch
@addr=r2
	ldx dbg::numwatches
@l0:	lda @addr
	cmp dbg::watcheslo-1,x
	bne @next
	lda @addr+1
	cmp dbg::watcheshi-1,x
	beq @done
@next:	dex
	bne @l0
	clc			; no watch exists
@done:	rts
.endproc
