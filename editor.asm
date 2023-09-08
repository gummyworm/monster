.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
.include "config.inc"
.include "ctx.inc"
.include "cursor.inc"
.include "debug.inc"
.include "errors.inc"
.include "file.inc"
.include "format.inc"
.include "key.inc"
.include "layout.inc"
.include "labels.inc"
.include "memory.inc"
.include "source.inc"
.include "state.inc"
.include "string.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "zeropage.inc"

.include "macros.inc"
.import help

;******************************************************************************
; ZEROPAGE
indent = zp::editor	; number of spaces to insert on newline
height = zp::editor+1	; height of the text-editor (shrinks when displaying
			; error, showing debugger, etc.
.CODE
;******************************************************************************
; DRAW_TITLEBAR
; Draws a titlebar at the top of the screen
.IFDEF DRAW_TITLEBAR
	.proc draw_titlebar
		ldxy #titlebar
	lda #$00
	jsr text::puts
	lda #$00
	jmp bm::rvsline
.endproc
.ENDIF

;******************************************************************************
; INIT
; Initializes the editor state
.export __edit_init
.proc __edit_init
	jsr bm::init
	jsr bm::clr
	jsr edit

.IFDEF DRAW_TITLEBAR
	jsr draw_titlebar
.ENDIF
	jsr reset
	jsr text::clrline

	; don't assemble code, just verify it
	lda #$01
	sta state::verify

	lda #CUR_BLINK_SPEED
	sta zp::curtmr

	lda #EDITOR_HEIGHT
	sta height

	ldx #$00
	ldy #EDITOR_ROW_START
	stx indent
	jmp cur::forceset
.endproc

;******************************************************************************
; RUN
; Runs the main loop for the editor
.export __edit_run
.proc __edit_run
main:
	lda #$70
	cmp $9004
	bne *-3

	jsr key::getch
	beq @done
	jsr onkey

@done:	jsr text::update
	jsr text::status
	jmp main
.endproc

;******************************************************************************
; LABEL_ADDR_OR_ORG
; Returns the address of the label given in .XY or, if no label is given (a
; 0-length string is given) the address of the program origin
; IN:
;  - .XY: the address of the label to get the address of
; OUT:
;  - .XY: the address of the given label or the address of the origin if no
;         label was given.
;  - .C: set on error, clear on success
.proc label_addr_or_org
	@lbl=zp::tmp0
	stxy @lbl
	jsr str::len
	cmp #$00
	bne @label
	ldxy asm::origin ; use ORG if no label given
	RETURN_OK

@label:	ldxy @lbl
	jmp lbl::addr
.endproc

;******************************************************************************
; COMMAND_GO
; Begins execution at the address of the label given in .XY
; If no label is given (a 0-length string is given) then begins execution at
; the program's origin.
; IN:
;  - .XY: the address of the label to start debugging at
.proc command_go
	jsr label_addr_or_org
	bcc :+
	rts
stxy zp::jmpvec
	lda #$4c	; JMP
	jsr zp::jmpaddr
	rts
.endproc

;******************************************************************************
; COMMAND_DEBUG
; Starts the debugger at the address specified by the given label.
; If no label is given (a 0-length string is given) then begins debugging at
; the program's origin.
; IN:
;  - .XY: the address of the label to start debugging at
.proc command_debug
	jsr label_addr_or_org
	bcc :+
	rts		; address not found
:	lda #DEBUG_INFO_START_ROW-1
	sta height
	jsr dbg::start	; start debugging at address in .XY
	jsr __edit_init	; re-init the editor
	jmp refresh
.endproc

;******************************************************************************
; RESET
; clears all state relating to the assembly of the active file.
.proc reset
	jsr asm::reset
	jmp lbl::clr
.endproc

;******************************************************************************
.proc assemble_file
@file=zp::editor+2	; pointer to the filename of the file to assemble
	stxy @file
; do the first pass of assembly
@pass1:	lda #$01
	sta state::verify
	sta zp::pass
	jsr asm::include	; assemble the file (pass 1)
	bcs @done		; error, we're done

; reset state after 1st pass
	dec state::verify	; disable verify (assemble the program)
	inc zp::pass
	jsr asm::resetpc	; reset PC
	jsr ctx::init		; reinitialize the context

; store the debug segment info (if debug info is enabled)
	ldx zp::gendebuginfo
	beq @pass2
	jsr dbg::setup

; do the second assembly pass
@pass2:	ldxy @file
	jsr asm::include	; assemble the file (pass 2)
@done:	jmp display_result
.endproc

;******************************************************************************
; COMMAND_ASM
; Assembles the entire source into mem::program
.export command_asm
.proc command_asm
	jsr dbg::init

	; save the current source position and rewind it for assembly
	jsr src::pushp
	jsr src::rewind
	jsr src::next
	jsr reset

	lda zp::gendebuginfo
	beq @pass1
	; set the initial file for debugging
	ldxy #filename
	jsr dbg::setfile

;--------------------------------------
; Pass 1
; do a pass on the source to simply get labels and basic debug info
; (# of lines and # of segments/file)
@pass1: lda #$01
	sta state::verify	; verify; write labels but not code
	sta zp::pass		; set pass number to 1

@pass1loop:
	ldxy src::line
	jsr dbg::setline
	jsr src::readline
	ldxy #mem::linebuffer
	jsr asm::tokenize_pass1
	bcc @p1next
	bcs @err
@p1next:
	jsr src::end
	bne @pass1loop

;--------------------------------------
; Pass 2
; now we have defined labels and enough debug info to generate both the
; program binary and the full debug info (if enabled)
@pass2: inc zp::pass	; pass 2
	ldx zp::gendebuginfo
	beq :+
	jsr dbg::setup  ; we have enough info to init debug now

:	jsr src::rewind
	jsr src::next
	jsr asm::resetpc
	jsr ctx::init
	lda #$00
	sta state::verify	; disable verify - actually assemble the code

@pass2loop:
	ldxy src::line
	jsr dbg::setline
@asm:	jsr src::readline
	ldxy #mem::linebuffer
	jsr asm::tokenize_pass2
	bcc @next

@err:	jsr display_result	; display the error
	jsr src::popp		; clear the src position stack
	jsr src::goto
	jsr dbg::getline	; get the line that failed assembly
	jmp gotoline		; goto that line

@next:	jsr src::end		; check if we're at the end of the source
	bne @pass2loop		; repeat if not
	clc
	jsr display_result	; dispaly success msg
	jsr src::popp
	jsr src::goto
	RETURN_OK
.endproc

;******************************************************************************
; DISPLAY_RESULT
; Displays the result of the assembly. Prints an error if one occurred or
; the size of the assembled program if not
; IN:
;  - .C: set if there was an assembly error
;  - .A: the error code (if error occurred)
;  - zp::asmresult: pointer to the end of the program
.proc display_result
	bcc @printresult
@err:	jsr dbg::getline
	jmp reporterr

@printresult:
	lda #$00
	sta state::verify	; re-enable verify

	; get the size of the assembled program and print it
	lda zp::asmresult
	sec
	sbc asm::origin
	pha
	lda zp::asmresult+1
	sbc asm::origin+1
	pha

	ldxy #@success_msg
	lda #STATUS_ROW-1
	jsr text::print
	lda #STATUS_ROW-2
	sta height

@asmdone:
	lda zp::gendebuginfo
	beq :+
	ldxy zp::virtualpc
	jsr dbg::endseg		; end the last segment
:	RETURN_OK

@success_msg: .byte "done $", $fe, " bytes", 0
.endproc

;******************************************************************************
; COMMAND_ASMDBG
; assembles the source and generates debug information for it
.proc command_asmdbg
	inc $900f
	lda #$01
	sta zp::gendebuginfo	; enable debug info
	jsr command_asm
	bcc :+
	rts			; error
:	dec zp::gendebuginfo	; turn off debug-info
	dec $900f
	RETURN_OK
.endproc

;******************************************************************************
; READINPUT
; Reads command input and returns it (0-terminated) in mem::linebuffer
; a prompt may be given in the address XY. If XY is 0, a ':' will be
; used
; IN:
;  - .XY: a prompt to display or $0000 for no prompt
; OUT:
;  - .C: set if no input was read (the user pressed <-)
.proc readinput
@prompt=zp::tmp0
@result_offset=zp::tmp8
	txa
	pha
	tya
	pha
	jsr text::savebuff
	jsr text::clrline
	pla
	tay
	pla
	tax

	cmpw #0
	beq @terminate_prompt
	stxy @prompt
	ldy #$00
:	lda (@prompt),y
	beq @terminate_prompt
	sta mem::linebuffer,y
	iny
	bne :-
@terminate_prompt:
	lda #':'
	sta mem::linebuffer,y

	lda zp::curx
	pha
	lda zp::cury
	pha

	iny
	sty @result_offset

	; set cursor limit for the prompt
	ldx @result_offset
	ldy #STATUS_ROW-1
	jsr cur::setmin
	ldx #40
	ldy #STATUS_ROW+1
	jsr cur::setmax

	; set the cursor
	ldx @result_offset
	ldy #STATUS_ROW
	jsr cur::set

	ldxy #mem::linebuffer
	lda #STATUS_ROW
	jsr text::drawline
@getkey:
        lda #$70
        cmp $9004
        bne *-3
	jsr text::update

	jsr key::getch
	beq @getkey
	cmp #$0d
	beq @done
	cmp #$5f	; <- (done)
	beq @exit
	jsr text::putch
	jmp @getkey

@done:	clc ; clear carry for success
@exit:	    ; carry is implicitly set by CMP for ==
	php
	jsr edit
	plp
	pla
	tay
	pla
	tax
	php	; save success state
	; restore curosr/editor
	jsr cur::set
	; move the read text into $100
	ldx @result_offset
@saveres:
	lda mem::linebuffer,x
	sta $100,x
	beq :+
	inx
	bne @saveres

:	jsr text::restorebuff
	ldx @result_offset
	ldy #$01
	plp	; get success state
	rts
.endproc

;******************************************************************************
; DOCOMMAND
; Executes the given command ID
; IN:
;  - .A: the command ID for the command to execute
.proc docommand
@prompt=$100
@cmd=@prompt
	sta @prompt

	; construct the prompt
	lda #$00
	sta $101
	ldxy #@prompt
	jsr readinput
	bcc :+
	rts

:	ldx #@num_commands-1
	lda @cmd
:	cmp @command_codes,x
	beq @found
	dex
	bpl :-
	rts

@found:
	txa
	asl
	tax
	lda @command_table,x
	sta @cmd_vec
	lda @command_table+1,x
	sta @cmd_vec+1

	ldx #<$102
	ldy #>$102
@cmd_vec=*+1
	jmp $0000

; commands
@command_codes:
.byte 'g'
.byte 'd'
.byte 'o'
.byte 's'
.byte 'x'
.byte 'a'
@num_commands=*-@command_codes
@command_table:
.word command_go
.word command_debug
.word load
.word save
.word scratch
.word assemble_file
.endproc

;******************************************************************************
; ONKEY
; Handles a keypress from the user
.proc onkey
	ldx #@num_special_keys-1

@l0:	cmp @specialkeys,x
	beq @special
	dex
	bpl @l0

	; handle the "docommand" functions
	cmp #$b0	; C=<A> (Assemble file)
	bne :+
	lda #'a'
	bne @do
:	cmp #$a5	; C=<G> (Go)
	bne :+
	lda #'g'
	bne @do
:	cmp #$ac	; C=<D> (Debug)
	bne :+
	lda #'d'
	bne @do
:	cmp #$b9	; C=<O> (Open)
	bne :+
	lda #'o'
	bne @do
:	cmp #$ae	; C=<S> (Save)
	bne :+
	lda #'s'
	bne @do
:	cmp #$bd	; C=<X> (Scratch)
	bne @insert
	lda #'x'
@do:	jmp docommand

@insert:
	jsr insert
	jsr cur::off
	jmp cur::on

@special:
	txa
	asl
	tax
	lda @specialkeys_vectors,x
	sta @vec
	lda @specialkeys_vectors+1,x
	sta @vec+1
@vec=*+1
	jmp $f00d

@specialkeys:
	.byte $13	; HOME
	.byte $85	; F1 (save)
	.byte $89	; F2 (save as)
	.byte $86	; F3 (assemble)
	.byte $8a	; F4 (debug)
	.byte $87	; F5 (nop)
	.byte $bc	; C=<C> (refresh)
	.byte $b4	; C=<H> (HELP)
	.byte $b2	; C=<R> (rename)
	.byte $be	; C=<V> (view)
	.byte $b6	; C=<L> (dir)
	.byte $b7	; C=<Y> (list symbols)
	.byte $a7	; C=<M> (gotoline)
@num_special_keys=*-@specialkeys
@specialkeys_vectors:
	.word home
	.word save
	.word saveas
	.word command_asm
	.word command_asmdbg
	.word command_nop
	.word refresh
	.word help
	.word rename
	.word memview
	.word dir
	.word list_symbols
	.word command_gotoline
.endproc

;******************************************************************************
; HOME
; Moves the cursor to start of the current line
.proc home
	ldx zp::curx
	beq :+
	jsr src::up
	ldx #$00
	ldy zp::cury
	jsr cur::set
:	rts
.endproc

;******************************************************************************
; COMMAND_NOP
; Does nothing
.proc command_nop
	rts
.endproc

;******************************************************************************
; Refresh
; Redraws the screen
.proc refresh
	jsr cur::off
	jsr __edit_init
	jsr src::rewind
	jsr src::next	; first character index is 1

@l0:	jsr src::readline
	bcs @done
@newl:	jsr drawline
	jmp @l0
@done:	jsr src::atcursor
	cmp #$0d
	bne :+
	; if the last char is a newline, advance/scroll/etc.
	jmp drawline

:	lda zp::cury
	ldxy #mem::linebuffer
	jsr text::drawline
	ldxy #mem::linebuffer
	jsr str::len
	tax
	ldy zp::cury
	jsr cur::set
	rts
	;jmp text::clrline
.endproc

;******************************************************************************
; LIST_SYMBOLS
; Lists the symbols in the program
.proc list_symbols
@cnt=zp::tmp7
@addr=zp::tmp9
@row=zp::tmpb
	jsr bm::save
	jsr bm::clr

	ldxy lbl::num
	cmpw #0
	beq @done

	ldxy #$00
	stxy @cnt
	stx @row

@l0:	jsr lbl::by_id		; get the symobl address
	stxy @addr
	ldy #$00
	lda (@addr),y
	pha
	iny
	lda (@addr),y
	pha

	ldxy @cnt
	jsr lbl::name_by_id	; get the symbol name
	tya
	pha
	txa
	pha

	lda @row
	ldxy #@sym_line
	jsr text::print

	inc @row
	incw @cnt
	ldxy @cnt
	cmpw lbl::num
	bne @l0

@done:	; wait for a key and restore screen
	jsr key::getch
	beq @done
	jmp bm::restore

@sym_line:
	.byte ESCAPE_STRING,":$",ESCAPE_VALUE,0
.endproc

;******************************************************************************
; DIR
; Lists the directory
.proc dir
	jsr bm::save
	jsr text::clrline
	jsr text::dir

	; get a selection
:	jsr key::getch
	cmp #$0d
	bne :-
	jmp bm::restore
.endproc

;******************************************************************************
; Rename
; Gets user input to rename the buffer and applies the new name.
.proc rename
	jsr text::savebuff
	jsr text::clrline
	; TODO: use readinput or something
	; getinput mem::statusline+23,0,23,(40-16)
	ldxy #mem::linebuffer
	; TODO: jsr file::rename
	jsr text::restorebuff
	lda zp::cury
	jmp text::drawline
.endproc

;******************************************************************************
; SAVEAS
; Allows the user to name the current buffer- then writes it to a file
; of the same name.
.proc saveas
	jmp save
.endproc

;******************************************************************************
; EDIT
; Configures the cursor/screen/etc. for editing
.proc edit
	lda #$01
	sta text::insertmode
	ldx #$00
	ldy #EDITOR_ROW_START
	jsr cur::setmin
	ldx #40
	ldy #STATUS_ROW
	jmp cur::setmax
.endproc

;******************************************************************************
; SAVE
; Writes the source buffer to a file.
; IN:
;  - .XY: the filename to save the source to
.proc save
@file=zp::tmp9
	stx @file
	sty @file+1

	; get the file length
	jsr str::len
	pha

	ldxy #@savingmsg
	lda #STATUS_ROW
	jsr text::print

	ldx @file
	ldy @file+1
	pla
	jsr file::save
	cmp #$00
	bne @err
	rts	; no error
@err:
	rts
	pha
	lda #$00
	pha
	ldxy #@errmsg
	lda #STATUS_ROW
	jsr text::print
	rts
@savingmsg:
	.byte "saving...",0
@errmsg:
.byte "failed to save file; error ", $fe, 0
.endproc

;******************************************************************************
; SCRATCH
; Deletes the given file
; IN:
;  - .XY: the filename of the file to delete
.proc scratch
@file=zp::tmp9
	stx @file
	sty @file+1

	; get the file length
	jsr str::len
	pha

	ldxy #@savingmsg
	lda #STATUS_ROW
	jsr text::print

	ldx @file
	ldy @file+1
	pla
	jsr file::scratch
	cmp #$00
	bne @err
	rts	; no error
@err:
	pha
	lda #$00
	pha
	ldxy #@errmsg
	lda #STATUS_ROW
	jmp text::print
@savingmsg:
	.byte "deleting...",0
@errmsg:
	.byte "failed to delete file; error ", $fe, 0
.endproc

;******************************************************************************
; LOAD
; Loads the file from disk into the source buffer
.proc load
@file=zp::tmp9
@dst=zp::tmpb
	stx @file
	sty @file+1

	; reinitialize the editor (clear screen, etc.)
	jsr __edit_init

	; get the file length
	ldxy @file
	jsr str::len

@found: tya
	pha

	ldxy #@loadingmsg
	lda #STATUS_ROW
	jsr text::print

	; set the address to load file into
	ldxy #src::buffer
	stxy @dst

	ldxy @file
	pla
	jsr file::load
	cmp #$00
	bne @err
	jsr reset

	jmp refresh
	jsr text::clrline

@err:	pha
	lda #$00
	pha
	ldxy #@errmsg
	lda #STATUS_ROW
	jsr text::print
	rts
@loadingmsg:
	.byte "loading...",0
@errmsg:
.byte "failed to load file; error $", $fe, 0
.endproc

;******************************************************************************
; LINEDONE
; Attempts to compile the line entered in (mem::linebuffer)
.proc linedone
@i=zp::tmpa
	; insert \n into source buffer and terminate text buffer
	lda #$0d
	jsr src::insert
	lda #$00
	jsr text::putch

	lda zp::curx
	beq @format	; @ column 0, skip to insert (format will be ignored)

	; check if the current line is valid
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr asm::tokenize
	bcs @err

	; reset indent
	ldx #$00
	stx indent
@format:
	; format the line
	cmp #ASM_LABEL
	beq @fmt
	cmp #ASM_MACRO
	beq @fmt
	cmp #ASM_OPCODE
	bne @nextline	; no formatting
@fmt:	ldx #$00
	stx indent	; 2 space indent
	jsr fmt::line

@nextline:
	jsr drawline

	; redraw the cleared status line
	jsr text::update

	; redraw everything from <cursor> to EOL on next line
	jsr src::get
	ldxy #mem::linebuffer
	lda zp::cury
	jsr text::print

; insert spaces for the indent in the source
	lda indent
	beq @done
	sta @i
@indent:
	lda #' '
	jsr text::putch
	lda #' '
	jsr src::insert
	dec @i
	bne @indent
@done:	rts

@err:	lda #$ff
	; highlight the error line
	ldx #ERROR_COLOR
	lda zp::cury
	jsr text::hiline
	jmp @nextline
.endproc

;******************************************************************************
; DRAWLINE
; Draws the line in mem::linebuffer at the current cursor position.
; The cursor is then updated and the screen scrolled.
; IN:
;  zp::cury: row to draw the line
;  indent: indent level (to place the cursor at after drawing)
.proc drawline
	lda zp::cury
	jsr text::drawline
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr text::hioff
@nextline:
	; scroll lines below cursor position
	ldy zp::cury
	iny
	cpy height
	bcc :+
	; if we're at the bottom, scroll whole screen up
	ldx #EDITOR_ROW_START
	lda height
	jsr text::scrollup
	ldy height
	dey
	jmp @setcur

:	tya
	ldx height
	dex
	jsr text::scrolldown

	ldy zp::cury
	iny
@setcur:
	ldx #$00
	jmp cur::set
.endproc

;******************************************************************************
; MEMVIEW
; executes the memory view and returns when the user is done
.proc memview
	ldx #<src::buffer
	ldy #>src::buffer
	jsr view::edit
	jmp edit
.endproc

;******************************************************************************
; CLRERROR
; Clears any error message
.proc clrerror
	jsr text::clrline
	ldxy #mem::linebuffer
	lda #ERROR_ROW
	jsr text::putz
	jmp text::hioff
.endproc

;******************************************************************************
; INSERT
; Adds a character at the cursor position.
.proc insert
	cmp #$80
	bcs @controlcodes
	cmp #' '
	bcs @printable

@controlcodes:
	ldx #numccodes-1
:	cmp controlcodes,x
	beq @cc
	dex
	bpl :-
	jmp @put

@cc:	txa
	asl
	tax
	lda ccvectors,x
	sta @j
	lda ccvectors+1,x
	sta @j+1
@j=*+1
	jmp $0000

@printable:
	ldx text::insertmode
	bne @put
@replace:
	jsr src::replace
	jmp text::putch
@put:	jsr src::insert
	jmp text::putch
.endproc

;******************************************************************************
; CCUP
; Handles the up cursor key
.proc ccup
@cnt=zp::tmp8
	ldxy src::line
	cmpw #1
	bne @cont	; at line 1, don't scroll
	jsr src::up	; move source to start of line
	ldx #$00
	ldy zp::cury
	jmp cur::set	; move cursor to start of line

@cont:	jsr src::up	; move up a line (or to start of line)
	lda zp::curx	; leftmost column
	beq :+
	jsr src::up	; if not left-aligned move up again
:	jsr src::get

	; move source to lesser of curx or newline ($0d)
	lda #$ff
	sta @cnt
:	inc @cnt
	ldx @cnt
	cpx zp::curx
	bcs :+
	jsr src::next
	cmp #$0d
	bne :-
	jsr src::prev
	ldx @cnt

:	ldy zp::cury
	beq @scroll
	dey
	jsr cur::set
	jmp @redraw

@scroll:
	lda #EDITOR_ROW_START
	ldx height
	jsr text::scrolldown	; cursor wasn't moved, scroll
	ldy #$00
	ldx @cnt
	jsr cur::set
@redraw:
@redraw2:
	lda zp::cury
	ldxy #mem::linebuffer
	jmp text::drawline
.endproc

;******************************************************************************
; CCLEFT
; Handles the left cursor key
.proc ccleft
	lda zp::curx
	beq :+
	pha
	jsr cur::left
	pla
	cmp zp::curx
	beq :+
	jmp src::prev
:	rts
.endproc

;******************************************************************************
; CCRIGHT
; Handles the right cursor key
.proc ccright
	jsr src::right
	bcc :+
	jmp cur::right
:	rts
.endproc

;******************************************************************************
; CCDOWN
; Handles the down cursor key
.proc ccdown
@cnt=zp::tmp6
@newy=zp::tmp7
@xend=zp::tmp8
	jsr src::end
	bne :+
	rts		; cursor is at end of source file, return

:	lda #$00
	sta @cnt
	lda zp::cury
	sta @newy

	jsr src::down
	bcc :+
	jsr src::up
	jsr src::get
	lda #$ff
	sta @xend
	jmp @movex

:	inc @newy
	lda zp::curx
	sta @xend
	jsr src::get

	; if the cursor is on a newline, we're done
	jsr src::end
	beq @movecur
	jsr src::next
	jsr src::atcursor
	cmp #$0d
	php
	jsr src::prev
	plp
	beq @movecur

@movex:
	jsr src::next
	cmp #$0d
	bne :+
	jsr src::prev	; don't pass the newline
	jmp @movecur
:	inc @cnt
	lda @cnt
	cmp @xend
	bcs @movecur
	jsr src::end
	bne @movex

@movecur:
	lda @cnt
	pha
	ldy @newy
	cpy height
	beq @redraw
	bcc @redraw	; no need to scroll

	ldx #EDITOR_ROW_START
	lda height
	jsr text::scrollup	; cursor wasn't moved, scroll
	ldy height
@redraw:
	pla
	tax
	jsr cur::set
	ldxy #mem::linebuffer
	lda zp::cury
	jmp text::drawline
.endproc

;******************************************************************************
; CCDEL
; Handles the DEL key
.proc ccdel
@cnt=zp::tmp6
	jsr src::start
	beq @done

	jsr src::backspace
	lda #$14
	jsr text::putch
	bcs @prevline
@done:	rts

@prevline:
	; move the cursor
	ldy #$ff
	ldx #0
	jsr cur::move

	; scroll everything up from below the line we deleted
	ldx zp::cury
	lda height
	sec
	sbc #$01
	jsr text::scrollup
.IFDEF DRAW_TITLEBAR
	jsr draw_titlebar
.ENDIF

	jsr text::clrline
	; get the length of the line we're moving up
	jsr src::get

	; if the current char is a newline, we're done
	jsr src::atcursor
	cmp #$0d
	beq @redraw

	ldxy #mem::linebuffer
	jsr str::len
	sta @line2len

	; get the new cursor position ( new_line_len - (old_line2_len))
	jsr src::up
	;jsr src::start
	;beq @redraw
	jsr src::get
	ldxy #mem::linebuffer
	jsr str::len
	sec
@line2len=*+1
	sbc #$00
	sta @cnt
	dec @cnt
	bmi @redraw
@endofline:
	inc zp::curx
	jsr src::next
	dec @cnt
	bpl @endofline

@redraw:
	lda zp::cury
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jmp text::drawline
.endproc

;******************************************************************************
; CANCEL
; If an error is being displayed, hides it.
.proc cancel
	ldy #EDITOR_HEIGHT
	sty height
	ldx #40
	jsr cur::setmax
	rts
.endproc

;******************************************************************************
; COMMAND_GOTOLINE
; Gets a line number from the user and moves the cursor and source to that line
.proc command_gotoline
	ldxy #$0000
	jsr readinput
	jsr atoi	; convert (YX) to line #
	bcs @done
	cmpw #$0000
	bne :+
	ldxy #$0001
:	jmp gotoline
@done:	rts
.endproc

;******************************************************************************
; GOTOLINE
; Sets the editor to the line in .YX and refreshes the screen.
.export __edit_gotoline
__edit_gotoline:
.proc gotoline
@target=zp::tmp6
@row=zp::tmp8
@seekforward=zp::tmp9	; 0=backwards 1=forwards
@diff=zp::tmpa		; lines to move up or down
@rowsave=zp::tmpc
	cmpw src::lines	; is target < total # of lines?
	bcc :+		; yes, move to target
	ldxy src::lines ; no, move to the last line
:	stxy @target

	lda zp::curx
	beq :+
	jsr src::up	; if we're not already, move to the start of the line

:	ldy zp::cury
	ldx #$00
	stx @seekforward
	jsr cur::set	; move cursor to column 0

	ldxy @target
	cmpw src::line	; is the target forward or backward?
	bne :+
	rts		; already on the target line
:	bcc @beginbackward		; backwards
	inc @seekforward

; get the number of lines to move forwards
@beginforward:
	lda @target
	sec
	sbc src::line
	sta @diff
	lda @target+1
	sbc src::line+1
	sta @diff+1

	bne @long
	lda zp::cury
	clc
	adc @diff
	cmp height
	bcs @long
	jmp @short

; get the number of lines to move backwards
@beginbackward:
	lda src::line
	sec
	sbc @target
	sta @diff
	lda src::line+1
	sbc @target+1
	sta @diff+1

	bne @long
	lda zp::cury	; is (cury - diff) > 0? (is the line on screen?)
	sec
	sbc @diff
	bmi @long

@short: ldy #$00
	lda @seekforward
	bne :+

	; move up and move cursor
	ldx @diff
	jsr src::upn
	lda #$00
	sec
	sbc @diff
	tay
	ldx #$00
	jmp cur::move

:	; move down and move cursor
	ldx @diff
	jsr src::downn
	ldy @diff
	ldx #$00
	jmp cur::move

@long:  ; get first line of source buffer to render
	; (target +/- (EDITOR_HEIGHT - cury))
	lda @diff
	sec
	sbc height
	tax
	lda @diff+1
	sbc #$00
	tay

	bpl @movesrc
	; (diff - EDITOR_HEIGHT) < 0, need to move in the opposite direction
	stxy @diff
	lda #$00
	sec
	sbc @diff
	tax
	ldy #$00		; hi byte is 0, can't be < -EDITOR_HEIGHT
	lda @seekforward
	beq :+
	jsr src::upn		; move up before we render downward
	jmp @longf_cont
:	jsr src::downn		; move down before we we render upward
	jmp @longb_cont

; move up or down through the source to get to the start line that we'll
; redraw from
@movesrc:
	lda @seekforward
	beq @longb

@longf:	jsr src::downn	; go to the first line to render
@longf_cont:
	lda #EDITOR_ROW_START
	jmp @longmove_cont

@longb:	jsr src::upn	; go to the first line to render
	;jsr src::up	; one more TODO: why?
@longb_cont:
	lda height

@longmove_cont:
	sta @row
@l0: 	jsr src::get
	ldxy #mem::linebuffer
	lda @row
	jsr text::drawline

	lda @seekforward
	bne @rowdown

@rowup: lda @row
	beq @renderdone ; cmp #EDITOR_ROW_START-1; bcc @..  for non-zero starts
	dec @row
	jsr src::up
	bcc @l0

	; draw the last row (src::up will return .C=1 for it)
	jsr src::get
	ldxy #mem::linebuffer
	lda #$00
	jsr text::drawline
	jmp @renderdone

@rowdown:
	ldx @row
	cpx height
	bcs @renderdone
	inc @row
	jsr src::down
	bcc @l0

@clearextra:
	jsr text::clrline
	lda @row
	sta @rowsave
@clrloop:
	ldxy #mem::linebuffer
	jsr text::drawline
	inc @row
@clrnext:
	ldx @row
	dex
	cpx height
	bcc @clrloop

	lda @rowsave
	sta @row
@renderdone:
	; move the cursor to the top if we searched backwards or bottom
	; if forward
	ldy @row
	ldx #$00
	jmp cur::set
.endproc

;******************************************************************************
; REPORTERR
; reports the given error
; in:
;  -.A: the error code
;  -.XY: the line number of the error
;  - mem::linebuffer: the line containing the error
.proc reporterr
@err=zp::tmp0
	sta @err

	; push the line number
	txa
	pha
	tya
	pha

	; push pass #
	lda zp::pass
	pha
	lda #$00
	pha

	; display the line containing the error
	ldxy #mem::linebuffer
	lda #ERROR_ROW+1
	jsr text::putz

	lda @err
	jsr err::get	; get the address of the error

	lda #<@line_err
	sta zp::tmp0
	lda #>@line_err
	sta zp::tmp0+1
	jsr str::cat

	lda #ERROR_ROW
	jsr text::print

	lda #ERROR_ROW-1
	sta height
	ldx #40
	jmp cur::setmax
@line_err: .byte ";pass ", ESCAPE_VALUE_DEC,";line ", ESCAPE_VALUE_DEC,0
.endproc

.DATA
;******************************************************************************
controlcodes:
.byte $9d	; left
.byte $1d	; right
.byte $91	; up arrow
.byte $11	; down
.byte $14	; delete
.byte $0d	; RETURN
.byte $5f	; <- (left arrow)
numccodes=*-controlcodes

;******************************************************************************
ccvectors:
.word ccleft    ; left
.word ccright	; right
.word ccup      ; up
.word ccdown	; down
.word ccdel 	; delete
.word linedone	; RETURN
.word cancel	; <-

;******************************************************************************
.IFDEF DRAW_TITLEBAR
titlebar:
.byte "monster                      c=<h>: help"
.ENDIF

filename: .byte "test",0
