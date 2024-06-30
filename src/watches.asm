.include "bitmap.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
.include "edit.inc"
.include "errors.inc"
.include "expr.inc"
.include "finalex.inc"
.include "flags.inc"
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
; VIEW
; Displays the watches
.export __watches_view
.proc __watches_view
@cnt=zp::tmp13
@range=zp::tmp3			; if !0, start != stop (watch is a range)
@start=zp::tmp4			; start address
@stop=zp::tmp6			; stop address (same as start if NOT range)
@val=zp::tmp8			; value of watch (if NOT range)
	; get the first visible watch's offset
	lda scroll
	sta @cnt

@l0:	ldx @cnt
	cpx dbg::numwatches
	bcc :+
	rts

:	lda #$00
	sta @range			; init RANGE flag to false
	sta strings::watches_line_end	; restore string if it was changed

	; get the START and STOP addresses of the watch
	ldy @cnt
	lda dbg::watcheslo,y
	cmp dbg::watches_stoplo,y
	beq :+
	inc @range			; flag that start != stop
:	sta @start

	lda dbg::watches_stoplo,y
	sta @stop
	lda dbg::watcheshi,y
	cmp dbg::watches_stophi,y
	beq :+
	inc @range
:	sta @start+1
	lda dbg::watches_stophi,y
	sta @stop+1

	; print the watch info
	lda @range			; is it a range of addresses?
	beq @valline			; not a range

; if the start address != stop address, print both
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
	lda dbg::watch_flags,x
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value
	ldy #'!'			; dirty
:	tya
	pha

	; if start addr != stop addr, print the address range
	ldxy #strings::watches_range_line
	jmp @print			; print this line and continue

; if the start address == stop address, just print the one address and its val
@valline:
	; push current value of the watch
	lda dbg::watch_vals,x
	pha

	; push the previous value of the watch
	lda dbg::watch_flags,x
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value
	lda dbg::watch_prevs,x
	pha				; push previous value if dirty
	lda #CH_R_ARROW
	sta strings::watches_line_end	; insert a > between old/new values

:	; push the address
	lda @start
	pha
	lda @start+1
	pha

	; push SPACE if not dirty or '!' if dirty
	ldy #' '
	lda dbg::watch_flags,x
	and #WATCH_DIRTY		; dirty?
	beq :+				; if NOT dirty, don't push previous value
	ldy #'!'			; dirty
:	tya
	pha

	ldxy #strings::watches_line

@print:	lda @cnt
	sec
	sbc scroll
	adc #WATCHVIEW_START	; +1 (carry set)
	jsr text::print

 @nextline:
	lda @cnt
	sec
	sbc scroll
	cmp #HEIGHT-1
	bcs @done
	inc @cnt
	jmp @l0		; next watch
@done:	rts
.endproc

;******************************************************************************
; EDIT
; Enters the watch editor until the user exits it
.export __watches_edit
.proc __watches_edit
	; display the title
	ldxy #strings::watches_title
	lda #MEMVIEW_START
	jsr text::print
	lda #MEMVIEW_START
	jsr bm::rvsline

	jsr __watches_view

	lda #$00
	sta row
	sta scroll
	jmp @redraw	; highlight the first row

@loop:	jsr key::getch
	beq @loop
	cmp #K_QUIT
	bne :+
	rts		; quit

:	ldx #num_commands-1
@getcmd:
	cmp commands,x
	beq @runcmd
	dex
	bpl @getcmd
	bmi @loop	; unknown command

@runcmd:
	lda command_vectorslo,x	 ; vector LSB
	sta zp::jmpvec
	lda command_vectorshi,x  ; vector MSB
	sta zp::jmpvec+1
	jsr zp::jmpaddr		 ; call the command

; redraw after handling the command
@redraw:
	jsr __watches_view
	lda dbg::numwatches
	beq @loop		; no watches, don't highlight
	lda row			; highlight the row selected
	clc
	adc #WATCHVIEW_START+1
	jsr bm::rvsline

	jmp @loop		; continue the watch-viewer loop
.endproc

;******************************************************************************
; COMMANDS
; Table of the command keys valid within the watch viewer
commands:
	.byte K_DEL_WATCH
	.byte K_ADD_WATCH	; prompt for an expression and add watch
	.byte K_DOWN
	.byte K_UP
	.byte K_RETURN
num_commands=*-commands

.define command_vectors command_delete_watch, command_add_watch, down, up, select

command_vectorslo: .lobytes command_vectors
command_vectorshi: .hibytes command_vectors

;******************************************************************************
; UP
; Handles the UP key
.proc up
	dec row
	bpl :+
	inc row
	dec scroll
	bpl :+
	inc scroll
:	rts
.endproc

;******************************************************************************
; DOWN
; Handles the DOWN key
.proc down
	inc row
	lda row
	cmp dbg::numwatches
	bcs :+
	cmp #HEIGHT
	bcc @done
:	dec row
	inc scroll
	lda scroll
	clc
	adc row
	cmp dbg::numwatches
	bcc @done
	dec scroll
@done:	rts
.endproc

;******************************************************************************
; SELECT
; Handles the RETURN key
; Invokes the memory editor/viewer at the address of the selected watch
.proc select
	lda row
	clc
	adc scroll
	tax
	lda dbg::watcheslo,x
	sta view::addr
	lda dbg::watcheshi,x
	sta view::addr+1
	jmp view::edit		; invoke the memory editor
.endproc

;******************************************************************************
; UPDATE
; Reads the new values of all locations being watched and stores them
; If the watch represents a RANGE of values, does not store new value
.export __watches_update
.proc __watches_update
@cnt=zp::tmp0
@addr=zp::tmp1
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
@addr=zp::tmp0
@watchstart=zp::tmp2
@watchstop=zp::tmp4
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
@cnt=zp::tmp6
@addr=zp::tmp7
@found=zp::tmp9
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
; COMMAND ADD WATCH
; Prompts for a start address and (optional) stop address and adds a watch
; at that location
; The syntax for the entry is:
;  <expression> Optional[, <expression>]
.proc command_add_watch
@addr=zp::tmp4
@stop=zp::tmp6
	pushcur
	jsr cur::off

	lda #$01
	sta text::rvs		; enable RVS

	jsr text::clrline
	lda #WATCHVIEW_STOP
	jsr text::drawline	; clear the entry line

	; set bounds for the input
	lda #$00
	sta zp::curx
	lda #18+4
	sta cur::maxx
	lda #WATCHVIEW_STOP
	sta zp::cury

	ldxy #key::getch
	jsr edit::gets

	; evaluate the expression to get start address
	ldxy #mem::linebuffer
	stxy zp::line
	jsr expr::eval
	bcs @done		; if eval failed, return without adding
	stxy @addr

	; evaluate the 2nd expression (if any) to get stop address
	incw zp::line		; move past the separator (,)
	jsr expr::eval
	bcs @done
	stxy zp::tmp0		; stop address
	ldxy @addr		; get start address
@add:	jsr __watches_add	; add the watch

@done:	lda #$00
	sta text::rvs		; disable reverse
	popcur			; restore cursor
	rts
.endproc

;******************************************************************************
; COMMAND DELETE WATCH
; Deletes the watch under the cursor row
.proc command_delete_watch
	; TODO:
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
@stop=zp::tmp0
@addr=zp::tmp2
@flags=zp::tmp4
	sta @flags
	stxy @addr
	ldx dbg::numwatches
	beq :+

	; check if watch already exists
	jsr getwatch
	beq @done		; already a watch here, exit

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
; GETWATCH
; Returns the index of the watch at the given address
; IN:
;  - zp::tmp0: the address of the watch
;
; OUT:
;  - .C: set if the watch exists
;  - .X: the id of the watch * 2
.proc getwatch
@addr=zp::tmp2
	ldx dbg::numwatches
@l0:	lda @addr
	cmp dbg::watcheslo,x
	bne @next
	lda @addr+1
	cmp dbg::watcheshi,x
	beq @done
@next:	dex
	dex
	bpl @l0
@done:	rts
.endproc

