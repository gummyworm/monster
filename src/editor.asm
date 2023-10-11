.include "asm.inc"
.include "bitmap.inc"
.include "codes.inc"
.include "config.inc"
.include "ctx.inc"
.include "cursor.inc"
.include "debug.inc"
.include "draw.inc"
.include "errors.inc"
.include "file.inc"
.include "format.inc"
.include "key.inc"
.include "keycodes.inc"
.include "layout.inc"
.include "labels.inc"
.include "linebuffer.inc"
.include "memory.inc"
.include "source.inc"
.include "state.inc"
.include "string.inc"
.include "strings.inc"
.include "text.inc"
.include "util.inc"
.include "view.inc"
.include "zeropage.inc"

.include "macros.inc"

;******************************************************************************
; CONSTANTS
MODE_COMMAND = 1
MODE_INSERT  = 2
MODE_VISUAL  = 3

START_MODE = MODE_COMMAND

SCREEN_H = 23

MAX_HIGHLIGHTS = 8

MAX_JUMPS = 8

;******************************************************************************
; ZEROPAGE
indent = zp::editor	; number of spaces to insert on newline
height = zp::editor+1	; height of the text-editor (shrinks when displaying
			; error, showing debugger, etc.
mode   = zp::editor_mode	; editor mode (COMMAND, INSERT)


.export __edit_height
__edit_height = height

.BSS
;******************************************************************************
readonly: .byte 0	; if !0 no edits are allowed to be made via the editor

numhighlights: .byte 0
highlighted_lines: .res MAX_HIGHLIGHTS*2 ; line numbers that are highlighted

jumplist: .res 8*2	; line #'s between jumps
jumpptr:  .byte 0	; offset to jumplist

buffptr: .word 0 	; copy buffer pointer (also bytes in copy buffer)

visual_start_line: .word 0	; the line # a selection began at
visual_start_x:    .byte 0	; the x-position a selection began at

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
	jsr cancel

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

	lda #START_MODE
	sta mode

	ldxy #mem::copybuff
	stxy buffptr

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

	pha
	lda mode
	cmp #MODE_VISUAL
	beq :+			; leave cursor on if in VISUAL mode
	jsr cur::off
:	pla

	jsr __edit_handle_key

	lda mode
	cmp #MODE_VISUAL
	beq @done
	jsr cur::on

@done:	jsr text::update
	jsr text::status
	lda #EDITOR_HEIGHT
	sta height
	jmp main
.endproc

;******************************************************************************
; HANDLE_KEY
; Handles a keypress within the editor
; IN:
;  - .A: the key that was pressed (as from key::getch)
; OUT:
;  - .C: set if the key resulted in no action in the editor
.export __edit_handle_key
.proc __edit_handle_key
	ldx mode
	cpx #MODE_VISUAL	; handle keys in VISUAL mode like COMMAND
	beq @cmd
	cpx #MODE_COMMAND
	bne @ins
@cmd:	jsr onkey_cmd
	jmp @validate
@ins:	jsr onkey
@validate:
	; make sure cursor is on a valid character
	lda text::insertmode
	jsr src::after_cursor
	cmp #$80
	bcc @keydone
	jsr ccright	; try to move past the non-source char
	bcc @validate
@keydone:
	rts
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
	ldxy #$0000
	jsr readinput
	bcs @ret
	rts

	jsr label_addr_or_org
	bcc :+
@ret:	rts
	stxy zp::jmpvec
	jmp zp::jmpaddr
.endproc

;******************************************************************************
; COMMAND_DEBUG
; Starts the debugger at the address specified by the given label.
; If no label is given (a 0-length string is given) then begins debugging at
; the program's origin.
; IN:
;  - .XY: the address of the label to start debugging at
.proc command_debug
@addr=zp::editortmp
	ldxy #$0000
	jsr readinput
	bcs @ret

	jsr label_addr_or_org
	stxy @addr
	bcc :+
@ret:	rts		; address not found

:	jsr enter_command
	lda #DEBUG_MESSAGE_LINE-1
	sta height
	inc readonly	; enable read-only mode
	jsr home_line	; avoid problems with cursor-y being below new height
	ldxy @addr
	jsr dbg::start	; start debugging at address in .XY
	dec readonly
	lda #EDITOR_HEIGHT
	sta height
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
@filename=mem::backbuff
	ldxy #$0000
	jsr readinput
	bcc :+
	rts

:	lda #<@filename
	sta zp::tmp0
	lda #>@filename
	sta zp::tmp0+1
	jsr str::copy
	jsr dbg::init
	jsr reset

	lda #$01
	sta state::verify	; verify for 1st pass
	sta zp::gendebuginfo
	sta zp::pass

; do the first pass of assembly
@pass1:
	ldxy #@filename
	jsr asm::include	; assemble the file (pass 1)
	bcs @done		; error, we're done

	; end the last segment (if debug info generation enabled)
	lda zp::gendebuginfo
	beq @done
	ldxy zp::asmresult
	jsr dbg::endseg

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
@pass2:	ldxy #@filename
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
	jsr reset

	lda zp::gendebuginfo
	beq @pass1

	; set the initial file for debugging
	ldxy #$01
	stxy dbg::srcline
	lda src::activebuff
	jsr src::filename
	bcs @err
	jsr dbg::setfile

;--------------------------------------
; Pass 1
; do a pass on the source to simply get labels and basic debug info
; (# of lines and # of segments/file)
@pass1:	lda #$01
	sta state::verify	; verify 1st pass
	sta zp::pass		; set pass number to 1

@pass1loop:
	ldxy src::line
	stxy dbg::srcline
	jsr src::readline
	ldxy #mem::linebuffer
	jsr asm::tokenize_pass1
	bcs @err
	jsr src::end
	bne @pass1loop

	; end the last segment (if debug info generation enabled)
	lda zp::gendebuginfo
	beq :+
	ldxy zp::asmresult
	jsr dbg::endseg

;--------------------------------------
; Pass 2
; now we have defined labels and enough debug info to generate both the
; program binary and the full debug info (if enabled)
@pass2: inc zp::pass		; pass 2
	ldx zp::gendebuginfo
	beq :+
	jsr dbg::setup  ; we have enough info to init debug now

:	jsr src::rewind
	jsr asm::resetpc
	jsr ctx::init
	dec state::verify

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
	clc			; success
	jsr display_result	; dispaly success msg
	jsr src::popp
	jsr src::goto
	lda zp::curx
	beq :+
	jsr src::up
:	jsr src::get
	ldx #$00
	stx zp::curx

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
	lda #$01
	sta state::verify	; re-enable verify

	; get the size of the assembled program and print it
	lda asm::pcset		; did this program actually assemble > 0 bytes?
	bne :+
	lda #$00		; if not, just push a zero value for bytes
	pha
	pha
	beq @success

:	lda zp::asmresult
	sec
	sbc asm::origin
	pha
	lda zp::asmresult+1
	sbc asm::origin+1
	pha

@success:
	ldxy #@success_msg
	lda #STATUS_ROW-1
	jsr text::print
	lda #STATUS_ROW-2
	sta height

@asmdone:
	RETURN_OK

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
; GETS
; Accepts user input at the current cursor position and returns the input
; after the user presses RETURN
; IN:
;  - .XY: callback to a function to accept/validate input (e.g. key::getch)
; OUT:
;  - .XY: 	      address of the string that was read
;  - .C:              set if no input was given (e.g. <-)
;  - mem::linebuffer: the string contents
.export __edit_gets
.proc __edit_gets
@result_offset=zp::tmp8
	stxy zp::jmpvec

	ldx zp::curx
	stx @result_offset	; offset to the user-input in line buffer

@getloop:
        lda #$70
        cmp $9004
        bne *-3
	jsr text::update

	jsr zp::jmpaddr		; call key-get func
	cmp #$00
	beq @getloop
	cmp #K_RETURN
	beq @done
	cmp #K_QUIT	; <- (done)
	beq @exit
	jsr text::putch
	lda zp::cury
	jsr text::drawline
	jmp @getloop

@done:	clc 	; clear carry for success
@exit:	    	; carry is implicitly set by CMP for ==
	php	; save success state

	; move the read text into $100
	ldx @result_offset
@saveres:
	lda mem::linebuffer,x
	sta $100,x
	beq :+
	inx
	bne @saveres

:	; get address of the result
	ldx @result_offset
	ldy #$01

	plp			; get success state
	rts
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
	stxy @prompt
	jsr text::savebuff
	jsr text::clrline

	pushcur			; save the cursor state

	; save insert mode etc.
	lda zp::editor_mode
	pha
	lda text::insertmode	; save current insertion mode
	pha
	lda #MODE_INSERT
	sta zp::editor_mode	; use INSERT mode for input
	lda #TEXT_INSERT
	sta text::insertmode	; also TEXT INSERT mode (rendering)

	ldxy @prompt
	cmpw #0
	beq @terminate_prompt
	ldy #$00

	; read the user prompt into the linebuffer
:	lda (@prompt),y
	beq @terminate_prompt
	sta mem::linebuffer,y
	iny
	bne :-

@terminate_prompt:
	lda #':'
	sta mem::linebuffer,y
	iny
	sty cur::minx
	sty zp::curx
	lda #STATUS_ROW		; Y = status line
	sta zp::cury
	jsr text::drawline	; clear line & display prompt
	ldxy #key::getch	; key-input callback
	jsr __edit_gets		; read the user input
	php			; save success state

	lda #40
	sta cur::maxx		; restore cursor x limit

	jsr text::restorebuff
	ldx @result_offset
	ldy #$01

	plp			; get success state
	pla
	sta text::insertmode	; restore insert mode
	pla
	sta zp::editor_mode	; restore editor mode
	popcur			; restore cursor

	rts
.endproc

;******************************************************************************
; ONKEY
; Handles a keypress from the user in INSERT mode
.proc onkey
@insert:
	jsr handle_universal_keys
	bcc :+
	rts
:	jmp insert
.endproc

;******************************************************************************_
; ONKEY_CMD
; Handles a keypress from the user in COMMAND mode
.proc onkey_cmd
@reps=zp::editortmp
	jsr handle_universal_keys
	bcc :+
	rts

:	ldx #$01
	stx @reps		; init reps to 1
@getreps:
	cmp #$5f		; <-
	bne :+
	rts			; exit this command
:	; check if a number is given (to repeat the following commands)
	jsr key::isdec
	bcc @check_cmds
	cmp #'0'
	beq @check_cmds
	sbc #'0'		; .C already set
	sta @reps

:	jsr key::getch	; get another key for the command to do @reps times
	beq :-

@check_cmds:
	ldx #numcommands-1
:	cmp commands,x
	beq @found
	dex
	bpl :-
	rts		; no key found

@found:	lda command_vecs_lo,x
	sta zp::jmpvec
	lda command_vecs_hi,x
	sta zp::jmpvec+1

; repeat the command for the number of reps the user requested
@doreps:
	jsr zp::jmpaddr
	dec @reps
	bne @doreps
	rts

.endproc

;******************************************************************************_
; ENTER_INSERT
; Enters INSERT mode
.proc enter_insert
	jsr is_readonly
	beq @done		; can't INSERT in r/o mode
	lda #MODE_INSERT
	sta mode
	lda #TEXT_INSERT
	sta text::insertmode
@done:	rts
.endproc

;******************************************************************************_
; ENTER_COMMAND
; Enters COMMAND mode
.proc enter_command
	jsr deselect	; unhighlight selection (if we were in VISUAL mode)
	lda #CUR_NORMAL
	sta cur::mode
	jsr ccleft	; insert places cursor after char
	lda #MODE_COMMAND
	sta mode
@done:  rts
.endproc

;******************************************************************************_
; ENTER VISUAL
; Enters VISUAL mode
.proc enter_visual
	jsr cur::on

	lda #MODE_VISUAL
	sta mode
	lda #CUR_SELECT
	sta cur::mode
	lda #TEXT_REPLACE
	sta text::insertmode

	; save current editor position
	ldxy src::line
	stxy visual_start_line
	lda zp::curx
	sta visual_start_x

	; save current source position
	jmp src::pushp
.endproc

;******************************************************************************_
; REPLACE
.proc replace
	lda #MODE_INSERT
	sta mode
	lda #TEXT_REPLACE
	sta text::insertmode
	lda #CUR_NORMAL
	sta cur::mode
	rts
.endproc

;******************************************************************************_
; REPLACE_CHAR
.proc replace_char
@ch=zp::tmp0
	jsr is_readonly
	beq @done
:	jsr key::getch	; get the character to replace with
	beq :-
	sta @ch
	lda text::insertmode
	pha
	lda #TEXT_REPLACE
	sta text::insertmode
	lda @ch
	jsr insert
	dec zp::curx	; don't advance cursor
	pla
	sta text::insertmode
@done:	rts
.endproc

;******************************************************************************_
; INSERT_START
; moves cursor to start of line and enters INSERT mode
.proc insert_start
	jsr enter_insert
	jmp home
.endproc

;******************************************************************************_
; APPEND_TO_LINE
.proc append_to_line
	jsr enter_insert
@l0:	jsr ccright
	bcc @l0
	jsr src::end
	beq @done
@done:  rts
.endproc

;******************************************************************************_
; APPEND_CHAR
.proc append_char
	jsr enter_insert
	jsr src::end
	beq @done
	jsr src::next
	inc zp::curx
@done:	rts
.endproc

;******************************************************************************_
; END_OF_WORD
.proc endofword
	jsr ccright
	bcs @done
	jsr ccright
	bcs @done

@l0:	jsr ccright
	bcs @done
	jsr src::after_cursor
	jsr util::isalphanum
	beq @l0
	jsr ccleft	; move back to the last char
@done:	rts
.endproc

;******************************************************************************_
; END_OF_LINE
.proc end_of_line
@l0:	jsr ccright
	bcc @l0
@done:	rts
.endproc

;******************************************************************************_
.proc prev_empty_line
	jsr home	; move back to column zero
:	jsr src::start
	beq @done
	jsr ccup
	jsr src::after_cursor
	cmp #$0d
	bne :-
@done:	rts
.endproc

;******************************************************************************_
.proc next_empty_line
	jsr home	; move back to column zero
:	jsr src::end
	beq @done
	jsr ccdown
	jsr src::atcursor
	cmp #$0d
	bne :-
@done:	rts
.endproc

;******************************************************************************_
; BEGINWORD
.proc beginword
@l0:	jsr ccleft
	bcs @done

	jsr src::atcursor
	jsr util::isalphanum
	beq @l0
@done:	rts
.endproc

;******************************************************************************_
; BEGIN_NEXT_LINE
.proc begin_next_line
	jsr ccdown
	jmp home
.endproc

;******************************************************************************_
; DELETE
.proc delete
:	jsr key::getch	; get a key to decide what to delete
	beq :-
	ldx #@numcmds-1
:	cmp @subcmds,x
	beq @found
	dex
	bpl :-
@notfound:
	rts
@found:	lda @subcmdslo,x
	sta zp::jmpvec
	lda @subcmdshi,x
	sta zp::jmpvec+1
	jmp zp::jmpaddr
@subcmds:
.byte $77	; w delete word
.byte $64	; d delete line
.byte $24	; $ (end of line)
@numcmds=*-@subcmds

.define subcmds delete_word, delete_line, delete_to_end
@subcmdshi: .hibytes subcmds
@subcmdslo: .lobytes subcmds
.endproc

;******************************************************************************_
.proc delete_to_begin
	rts
.endproc

;******************************************************************************_
.proc delete_line
	jsr is_readonly
	bne :+
	rts

:	lda #TEXT_INSERT
	sta text::insertmode

	; if line length is 0, just do one backspace and we're done
	jsr text::linelen
	cpx #$00
	bne :+
	jsr backspace
	jmp @done

:	jsr ccdown
	bcs @l0
	jsr home
@l0:	jsr backspace
	jsr buff_putch
	lda zp::curx
	bne @l0
	jsr src::end
	bne @done
	jsr @done
	jmp ccup

@done:	lda #TEXT_REPLACE
	sta text::insertmode
	lda zp::cury
	jmp text::drawline
.endproc

;******************************************************************************_
.proc delete_char
	jsr delch
	jmp redraw_to_end_of_line
.endproc

;******************************************************************************_
.proc delete_to_end
@l0:	jsr delch
	jsr src::after_cursor
	cmp #$0d
	bne @l0
	jmp redraw_to_end_of_line
.endproc

;******************************************************************************_
.proc delete_word
@endonalpha=zp::tmp1
	; if we're on an alphanum char, end on the first non-alphanum char
	; if we're NOT on an alphanum char, end on the first alphanum char
	jsr src::after_cursor
	ldx #$00
	jsr util::isalphanum
	beq :+
	inx
:	stx @endonalpha	; flag if we are to end on an alphanum char or not

@l0:	jsr delch
	bcs @done

	; check if this is a character we're looking to end on
	jsr src::after_cursor
	jsr util::isalphanum
	php
	ldx @endonalpha
	bne :+
	plp
	bne @l0
:	plp
	beq @l0

@done:  jmp redraw_to_end_of_line
.endproc

;******************************************************************************
; PASTE BELOW
; Pastes the contents of the copy buffer to the line below the cursor
.proc paste_below
	jsr enter_insert
	jsr ccright
	jmp paste_buff
.endproc

;******************************************************************************
; PASTE ABOVE
; Pastes the contents of the copy buffer to the line above the cursor
.proc paste_above
	jsr enter_insert
	jsr home
	jsr paste_buff
	jmp ccup
.endproc

;******************************************************************************
; PASTE BUFFER
; Inserts the contents of the buffer at the current cursor position and returns
; to command mode
.proc paste_buff
:	jsr buff_getch
	bcs @done
	jsr insert
	jmp :-
@done:	jmp enter_command
.endproc

;******************************************************************************
; YANK
; In SELECT mode, copies the selected text to the copy buffer. If not in SELECT
; mode, does nothing
.proc yank
@cur=zp::editortmp+1
@end=zp::editortmp+3
@size=zp::editortmp+5
@start=zp::editortmp+7
	lda mode
	cmp #MODE_VISUAL
	beq :+
	rts

:	jsr src::pos	; get the current source position
	stxy @start
	stxy @cur

	jsr src::popp	; get the source position we started at
	stxy @end
	cmpw @cur
	beq @done	; nothing copied
	bcs @cont	; end > cur, don't swap

	; end > cur; swap them
	lda @cur
	sta @end
	lda @cur+1
	sta @end+1
	stxy @cur

@cont:	; Update pointer:
	;  the source pos ends on the character BEFORE the one we want to copy
	incw @end
	ldxy @end	; starting from the END, copy to copy buffer
	jsr src::goto	; go to the start position

	ldxy @end
	sub16 @cur
	stxy @size

@copy:	jsr src::atcursor
	jsr buff_putch	; add the character to the copy buffer
	bcs :+		; buffer is full
	jsr src::prev
	jsr src::pos
	cmpw @cur	; are we back at the START of the selection yet?
	bne @copy	; continue until we are

:	; restore source position
	ldxy @start
	jsr src::goto

	; display message
	jsr enter_command
	jsr refresh	; unhighlight selection
	lda @size
	pha
	lda @size+1
	pha
	ldxy #@yoinkmsg
	lda #STATUS_ROW-1
	jsr text::print

@done:	jmp enter_command
@yoinkmsg: .byte "yoink ",ESCAPE_VALUE_DEC,0
.endproc

;******************************************************************************
.proc comment_out
:	jsr key::getch	; get a key to decide what to comment out
	beq :-

	cmp #$3b	; if another comment, generate a banner
	bne @check_ban_up
	jsr text::linelen

	cpx #$00
	bne @ban_down	; if line is not empty open line and add banner to it
@ban_cur:
	jsr enter_insert
	jsr comment_banner
	jmp newl

@ban_down:
	jsr open_line_below
	jsr comment_banner
	jmp ccup

@check_ban_up:
	cmp #$5d	; SHIFT-; (generate banner above)
	bne @done
	jsr open_line_above
	jsr comment_banner
	jmp ccdown

@done:	rts
.endproc

;******************************************************************************
.proc comment_banner
@cnt=zp::editortmp+2
	lda #38
	sta @cnt
:	lda #';'
	jsr insert
	dec @cnt
	bne :-
	lda zp::cury
	jsr text::drawline
	rts
.endproc

;******************************************************************************
.proc word_advance
	jsr src::end
	beq @done
@l0:	jsr ccright
	bcs @done
	jsr src::after_cursor
	jsr util::isseparator
	bne @l0
	jsr ccright	; move after separator
@done:	rts
.endproc

;******************************************************************************
.proc last_line
@l0:	jsr ccdown
	bcs @done
	lda zp::cury
	cmp height
	bcc @l0
@done:	jmp home
.endproc

;******************************************************************************
.proc home_line
	jsr home	; move to start of line
	jsr src::start	; at start of file?
	beq @done	; if so, we're done
@l0:	lda zp::cury
	beq @done
	jsr ccup	; move UP until cursor is at top row
	bcc @l0
@done:	rts
.endproc

;******************************************************************************
.proc goto_end
	ldxy #$ffff
	jsr gotoline
	jmp add_jump_point
.endproc

;******************************************************************************
.proc goto_start
:	jsr key::getch
	beq :-
	cmp #$67		; get second 'g' to confirm movement
	beq :+
	rts
:	ldxy #1
	lda mode
	cmp #MODE_VISUAL
	bne @gotoline
	; if we're in visual mode, go up line by line to highlight
:	jsr ccup
	bcc :-
	bcs @done
@gotoline:
	jsr gotoline
@done:	jmp add_jump_point
.endproc

;******************************************************************************
.proc open_line_above
	jsr is_readonly
	bne :+
	rts
:	jsr enter_insert
	jsr home	; move to start of line
	jsr newl
	jmp ccup
.endproc

;******************************************************************************
.proc open_line_below
	jsr is_readonly
	bne :+
	rts
:	jsr enter_insert
	jsr end_of_line
	jmp newl
.endproc

;******************************************************************************
; HANDLE_UNIVERSAL_KEYS
; Handles keys that behave the same regardless of which mode the editor is in
; IN:
;  - .A: the key to handle
; OUT:
;  - .C: set if the key was handled or clear if the caller must handle it
.proc handle_universal_keys
	ldx #@num_special_keys-1

@l0:	cmp @specialkeys,x
	beq @special
	dex
	bpl @l0

@check_ccodes:
	cmp #$80
	bcs @controlcodes
	cmp #' '
	bcs @not_ccode

@controlcodes:
	ldx #numccodes-1
:	cmp controlcodes,x
	beq @cc
	dex
	bpl :-
	clc
	rts

@cc:	lda ccvectorslo,x
	sta zp::jmpvec
	lda ccvectorshi,x
	sta zp::jmpvec+1
	jsr zp::jmpaddr
	sec
	rts

@special:
	lda @specialvecslo,x
	sta zp::jmpvec
	lda @specialvecshi,x
	sta zp::jmpvec+1
	jsr zp::jmpaddr
	sec
	rts

@not_ccode:
	clc		; not a universal key code; return to be handled
	rts

@specialkeys:
	.byte K_HOME		; HOME
	.byte K_ASM 		; assemble
	.byte K_ASM_DEBUG	; debug
	.byte K_SHOW_BUFFERS	; show buffers
	.byte K_REFRESH		; refresh
	.byte K_RENAME		; rename
	.byte K_DIR		; dir
	.byte K_LIST_SYMBOLS	; list symbols
	.byte K_GOTO_LINE	; gotoline
	.byte K_FIND		; find
	.byte K_CLOSE_BUFF	; close buffer
	.byte K_NEW_BUFF	; new buffer
	.byte K_SET_BREAKPOINT	; set breakpoint
	.byte K_JUMPBACK	; jump back

	.byte K_GOTO_BUFF1	; go-to buffer 1
	.byte K_GOTO_BUFF2	; go-to buffer 2
	.byte K_GOTO_BUFF3	; go-to buffer 3
	.byte K_GOTO_BUFF4	; go-to buffer 4
	.byte K_GOTO_BUFF5	; go-to buffer 5
	.byte K_GOTO_BUFF6	; go-to buffer 6
	.byte K_GOTO_BUFF7	; go-to buffer 7
	.byte K_GOTO_BUFF8 	; go-to buffer 8

	.byte K_NEXT_BUFF	; C= + > next buffer
	.byte K_PREV_BUFF	; C= + < previous buffer

	.byte K_GO		; go
	.byte K_DEBUG		; debug
	.byte K_OPEN		; open file
	.byte K_SAVE		; save file
	.byte K_SCRATCH		; scratch file
	.byte K_ASM_FILE	; assemble file

	.byte K_QUIT		; <- (return to COMMAND mode)
@num_special_keys=*-@specialkeys

.linecont +
.define specialvecs home, command_asm, command_asmdbg, show_buffers, refresh, \
	rename, dir, list_symbols, command_gotoline, command_find, \
	close_buffer, new_buffer, set_breakpoint, jumpback, \
	buffer1, buffer2, buffer3, buffer4, buffer5, buffer6, buffer7, buffer8,\
	next_buffer, prev_buffer, \
	command_go, command_debug, command_load, save, scratch, assemble_file, \
	cancel
.linecont -
@specialvecslo: .lobytes specialvecs
@specialvecshi: .hibytes specialvecs
.endproc

;******************************************************************************
; HOME
; Moves the cursor to start of the current line
.proc home
@l0:	jsr ccleft
	bcc @l0
	rts
.endproc

;******************************************************************************
; Refresh
; Redraws the screen
.export __edit_refresh
__edit_refresh:
.proc refresh
@saveline=zp::editortmp
	jsr bm::clr
	jsr cur::off

	ldxy src::line
	stxy @saveline

	; move source/cursor to top-left of screen
	jsr home_line

	; redraw the visible lines
@l0:	jsr text::clrline
	jsr src::readline
	sta zp::curx
	php
	lda zp::cury
	jsr text::drawline
	plp
	bcs @done
	inc zp::cury
	lda zp::cury
	cmp height
	beq @l0
	bcc @l0

@done:	; restore cursor and source
	ldxy @saveline
	jmp gotoline
.endproc

;******************************************************************************
; RESIZE
; Resizes the editor to the given number of rows. The cursor is moved to be
; within the new size if it would be out of bounds.
; IN:
;  - .A: the new size of the editor in rows.
.export __edit_resize
.proc __edit_resize
	cmp height	; is the new height bigger or smaller?
	sta height
	beq @done	; same size
	bcs @bigger	; new size is bigger, redraw screen
@smaller:
@l0:	lda zp::cury
	cmp height
	bcc @done	; cursor is in new height's range
	jsr ccup
	bcc @l0		; loop until cursor is on screen
@done:	rts
@bigger:
	jmp refresh
.endproc

;******************************************************************************
; LIST_SYMBOLS
; Lists the symbols in the program
.proc list_symbols
@cnt=zp::tmp7
@addr=zp::tmp9
@row=zp::tmpb
	jsr bm::save

	ldxy lbl::num
	cmpw #0
	beq @done

	ldxy #$00
	stxy @cnt

@l0:	stx @row
	jsr bm::clr
@l1:	ldxy #$100
	stxy zp::tmp0		; destination buffer for getname
	ldxy @cnt
	jsr lbl::getname	; get the symbol name
	lda #$01
	pha
	lda #$00
	pha

	ldxy @cnt
	jsr lbl::getaddr	; get the symbol address
	txa
	pha
	tya
	pha

	lda @row
	ldxy #@sym_line
	jsr text::print

	inc @row
	lda @row
	cmp #SCREEN_H
	beq @done		; end of screen
	incw @cnt
	ldxy @cnt
	cmpw lbl::num
	bne @l1

@done:	; wait for a key
	jsr key::getch
	beq @done
	cmp #$11		; down
	beq @pgdown
	cmp #$91		; up
	beq @pgup
	cmp #$5f		; <-
	bne @done
	jmp bm::restore

@pgdown:
	ldxy @cnt		; @cnt is already +SCREEN_H
	cmpw lbl::num		; are we at the end of the symbols?
	bcs @done		; yes, don't switch pages
	bcc @cont

@pgup:	ldxy @cnt
	sub16 #(SCREEN_H*2)-1
	bpl @cont
	ldxy #$00

@cont:	stxy @cnt
	ldx #$00
	jmp @l0

@sym_line:
	.byte "$",ESCAPE_VALUE,": ", ESCAPE_STRING, 0
.endproc

;******************************************************************************
; SET_BREAKPOINT
; insert a BREAKPOINT character at the beginning of the line
.export __edit_set_breakpoint
__edit_set_breakpoint:
.proc set_breakpoint
@savex=zp::editortmp
	lda zp::curx
	sta @savex
	jsr home	; go to col 0 (or 1 if there's already a breakpoint)
@insert:
	lda text::insertmode
	pha			; save insert mode
	lda #TEXT_INSERT
	sta text::insertmode

	jsr src::after_cursor
	cmp #BREAKPOINT_CHAR
	bne @add
@remove:
	jsr src::delete
	lda #$14
	inc zp::curx
	jsr text::putch
	lda zp::cury
	jsr text::drawline
	jmp @cont

@add:	lda #BREAKPOINT_CHAR
	jsr src::insert
	jsr text::putch

@cont:	pla
	sta text::insertmode	; restore insert mode

@restore:
	jsr ccright
	bcs @done
	lda zp::curx
	cmp @savex
	bcc @restore

@done:	rts
.endproc

;******************************************************************************
; NEW_BUFFER
; Creates a new buffer and sets it as the new active buffer
; or returns an error if one could not be made
; OUT:
;  - .C: set if a buffer could not be made
.proc new_buffer
	ldx zp::curx
	ldy zp::cury
	jsr src::new
	ldxy #strings::null
	jsr src::name	; rename buffer to empty name
	jmp __edit_init
.endproc

;******************************************************************************
; CLOSE_BUFFER
; Frees the current source buffer and moves to the previous buffer (if there
; is one) or a new source.
.proc close_buffer
	jsr src::close
	jmp refresh	; refresh the new buffer
.endproc

;******************************************************************************
; NEXT_BUFFER
; Moves to the source buffer before the active one. If we are already at the
; first buffer, does nothing
.proc next_buffer
	jsr src::save		; save the active buffer's state
	ldx src::activebuff
	inx
	txa
	jsr src::setbuff
	bcs @done
	jmp refresh
@done:	rts
.endproc

;******************************************************************************
; PREV_BUFFER
; Moves to the source buffer after the active one. If we are already at the
; last buffer, does nothing
.proc prev_buffer
	jsr src::save		; save the active buffer's state
	ldx src::activebuff
	dex
	bmi @done
	txa
	jsr src::setbuff
	bcs @done
	jmp refresh
@done:	rts
.endproc

;******************************************************************************
; BUFFER NAVIGATION HANDLERS
buffer1: lda #$00
	 skw
buffer2: lda #$01
	 skw
buffer3: lda #$02
	 skw
buffer4: lda #$03
	 skw
buffer5: lda #$04
	 skw
buffer6: lda #$05
	 skw
buffer7: lda #$06
	 skw
buffer8: lda #$07
goto_buffer:
	pha
	jsr src::save
	pla
	jsr src::setbuff
	bcs @done
	jmp refresh
@done:	rts

;******************************************************************************
; SHOW_BUFFERS
; Displays the filenames and their respective ID's for every open buffer
.proc show_buffers
@cnt=zp::tmp9
	jsr bm::save

	lda #$00
	sta @cnt

@l0:	ldy #$00
	lda @cnt
	jsr src::filename
	tya
	pha
	txa
	pha

@id:	lda @cnt
	clc
	adc #$01	; display buffer ID as 1-based
	pha
	lda #$00
	pha

	; display a '*' before filename if the buffer is dirty
	lda @cnt
	jsr src::getflags
	ldx #' '
	and #FLAG_DIRTY
	beq :+
	ldx #'*'
:	stx @dirty_marker

	lda @cnt
	ldxy #@buffer_line
	jsr text::print

@next:	inc @cnt
	lda @cnt
	cmp src::numbuffers
	bcc @l0
	jsr draw::hline

@getch: jsr key::getch		; get a key to confirm
	beq @getch
	cmp #K_QUIT
	beq @done
	cmp #'1'
	bcc @getch
	cmp #'8'+1
	bcs @getch
	sec
	sbc #'1'
	jmp goto_buffer

@done:	jmp bm::restore

@noname:       .byte "[no name]",0
@buffer_line:  .byte ESCAPE_VALUE_DEC," :"
@dirty_marker: .byte " "
	       .byte ESCAPE_STRING,0
.endproc

;******************************************************************************
; DIR
; Lists the directory
; NOTE: this routine is limited to 128 files
; The max supported by the 1541 is 144 and this routine could easily be
; modified to support as many.
; It could also easily be modified to support more (e.g. for the 1581)
.proc dir
@line=zp::tmp8
@row=zp::tmpa
@select=zp::tmpb
@cnt=zp::tmpc
@scroll=zp::tmpd
@dirbuff=mem::spare+40		; 0-40 will be corrupted by text routines
@namebuff=mem::spareend-40	; buffer for the file name
@fptrs=mem::spareend-(128*2)	; room for 128 files

	jsr bm::save

	jsr file::loaddir
	ldxy #@dirbuff+5
	stxy @line

	lda #$00
	sta @scroll
	sta @cnt
	sta @row
	lda #$01
	sta @select

@getdiskname:
	ldx #@dirmsglen
:	lda @dirmsg-1,x
	sta @namebuff-1,x
	dex
	bne :-
	; parse the name of the file
	lda #>(@namebuff+@dirmsglen-1)
	sta zp::tmp0+1
	lda #<(@namebuff+@dirmsglen-1)
	sta zp::tmp0
	ldxy #@dirbuff+5
	jsr util::parse_enquoted_string
	jmp @l2

@l0:    jsr text::clrline
	incw @line	; skip line #
	incw @line

	lda @cnt
	asl
	tax
	lda @line+1
	sta @fptrs+1,x	; save pointer to this filename
	tay
	lda @line
	sta @fptrs,x
	tax

	; parse the name of the file
	lda #<@namebuff
	sta zp::tmp0
	lda #>@namebuff
	sta zp::tmp0+1
	jsr util::parse_enquoted_string

@l2:	ldy #$00
	lda (@line),y
	incw @line
	tay		; set .Z if 0
	bne @l2

@next:  ; read line link
	ldy #$00
	lda (@line),y
	bne :+
	iny
	lda (@line),y
	beq @cont
:	incw @line
	incw @line

	; print the line
	ldxy #@namebuff
	lda @row
	jsr text::print

	; next line
	inc @cnt
	inc @row
	lda @row
	cmp #SCREEN_H
	bne @l0

@cont:	; draw a line to separate file display
	lda @row
	jsr draw::hline
	jsr text::clrline

	; highlight the first item
	lda @select
	jsr bm::rvsline

; at the end of the screen, get user input for page up/down
@key:	jsr key::getch
	beq @key

; check the arrow keys (used to select a file)
@checkdown:
	cmp #$11
	bne @checkup
	lda @select
	jsr bm::rvsline
@rowdown:
	inc @select
	lda @select
	cmp @row
	bcc @hiline
	dec @select
	lda @select
	bcs :+
@checkup:
	cmp #$91
	bne @checkret
	lda @select
	jsr bm::rvsline
@rowup:
	dec @select
	bne :+
	inc @select		; lowest selectable row is 1
:	lda @select
@hiline:
	jsr bm::rvsline
	jmp @key

; check the RETURN key (to open a file)
@checkret:
	cmp #$0d		; select file and load
	beq @loadselection
	cmp #$5f		; <-
	beq @exit
	bne @key

; user selected a file (RETURN), load it and exit the directory view
@loadselection:
	lda @select
	clc
	adc @scroll
	asl
	tax
	lda @fptrs,x
	ldy @fptrs+1,x
	tax
	lda #<@namebuff
	sta zp::tmp0
	lda #>@namebuff
	sta zp::tmp0+1
	jsr util::parse_enquoted_string
	jsr bm::restore
	ldxy #@namebuff
	jmp __edit_load 	; load the file

@exit:  jmp bm::restore
@dirmsg: .byte "dir:",0
@dirmsglen=*-@dirmsg
.endproc

;******************************************************************************
; Rename
; Gets user input to rename the buffer and applies the new name.
.proc rename
	ldxy #$0000
	jsr readinput
	bcs @done
	jmp src::name
@done:	rts
.endproc


;******************************************************************************
; EDIT
; Configures the cursor/screen/etc. for editing
.proc edit
	lda #TEXT_INSERT
	sta text::insertmode
	ldx #$00
	stx readonly
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
	ldxy #$0000
	jsr readinput
	bcs @ret

	stxy @file

	ldxy #strings::saving
	lda #STATUS_ROW
	jsr text::print

	ldxy @file
	jsr str::len	; get the file length
	pha

	ldxy @file
	jsr src::name
	pla
	ldxy @file
	jsr file::save

	cmp #$00
	bne @err

	; clear flags on the source buffer and return
	jmp src::setflags

@err:	pha		; push error code
	lda #$00
	pha
	ldxy #strings::edit_file_save_failed
	lda #STATUS_ROW
	jsr text::print
@ret:	rts
.endproc

;******************************************************************************
; SCRATCH
; Deletes the given file
; IN:
;  - .XY: the filename of the file to delete
.proc scratch
@file=zp::tmp9
	ldxy #$0000
	jsr readinput
	bcs @ret

	stxy @file

	; get the file length
	jsr str::len
	pha

	ldxy #strings::deleting
	lda #STATUS_ROW
	jsr text::print

	ldx @file
	ldy @file+1
	pla
	jsr file::scratch
	cmp #$00
	bne @err
@ret:	rts	; no error
@err:
	pha
	lda #$00
	pha
	ldxy #strings::edit_file_delete_failed
	lda #STATUS_ROW
	jmp text::print
.endproc

;******************************************************************************
; LOAD
; Loads the file from disk into the source buffer
; OUT:
;  - .C: set if file could not be loaded into a buffer
.export __edit_load
.proc __edit_load
@file=zp::tmp9
@dst=zp::tmpb
@search=zp::tmpb
	stxy @file

	; check if the file is already open in one of our buffers
	lda src::numbuffers
	cmp #MAX_SOURCES
	bcs @replace		; too many sources, close current and replace

	jsr src::buffer_by_name
	bcs @notfound

@switch_buff:
; buffer already loaded, switch to it
	cmp src::activebuff
	bne :+
	RETURN_OK		; buffer already active; quit
:	pha
	jsr src::save		; save the current buffer's state
	pla
	jsr src::setbuff	; switch to the new buffer
	jmp refresh

@replace:
; there too many open buffers, open a new one
	jsr src::close

@notfound:
; buffer doesn't exist in any RAM bank, load from disk
	; get the file length
	ldxy @file
	jsr str::len
	pha

	; display loading...
	ldxy #strings::loading
	lda #STATUS_ROW-1
	jsr text::print

	; load the file
	ldxy @file
	pla
	jsr file::load
	bcs @err

	jsr src::setflags	; clear flags on the source buffer
	jsr src::rewind		; rewind the source
	lda #$00
	sta zp::curx
	sta zp::cury		; reset cursor
	jsr refresh
	RETURN_OK

@err:	pha
	lda #$00
	pha
	ldxy #strings::edit_file_load_failed
	lda #STATUS_ROW-1
	jsr text::print
	sec			; error
	rts
.endproc

;******************************************************************************
; COMMAND_LOAD
; Handles the load command
.proc command_load
	ldxy #$0000
	jsr readinput
	bcs @err

	jsr __edit_load
	bcs @err

	; reinitialize the editor (clear screen, etc.)
	jmp reset
@err:	rts
.endproc

;******************************************************************************
.proc newl
	jsr is_readonly
	bne :+
	jmp begin_next_line

:	; insert \n into source buffer and terminate text buffer
	lda #$0d
	jsr src::insert
	lda #$00
	jsr text::putch

@nextline:
	jsr drawline
	; redraw everything from <cursor> to EOL on next line
	jsr src::get
	ldxy #mem::linebuffer
	lda zp::cury
	jsr text::print

@done:	lda zp::curx
	beq @ret
:	jsr src::prev
	dec zp::curx
	bne :-
@ret:	rts
.endproc

;******************************************************************************
; LINEDONE
; Attempts to compile the line entered in (mem::linebuffer)
.proc linedone
@i=zp::tmpa
	jsr is_readonly
	bne :+
	jmp begin_next_line

:	; insert \n into source buffer and terminate text buffer
	lda #$0d
	jsr src::insert
	lda #$00
	jsr text::putch

	lda zp::curx
	beq @format	; @ column 0, skip to insert (format will be ignored)

	lda #$00
	sta zp::gendebuginfo
	; check if the current line is valid
	ldx #<mem::linebuffer
	ldy #>mem::linebuffer
	jsr asm::tokenize
	bcs @err
	pha		; save token type

	; clear the error row
	lda #STATUS_ROW-1
	jsr bm::clrline

	; reset indent
	ldx #$00
	stx indent
@format:
	; format the line
	pla			; get token type
@fmt:	cmp #ASM_COMMENT	; if this is a comment, don't indent
	bne :+
	ldx #INDENT_LEVEL	; anything but a comment, indent
:	stx indent
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

@ident:	lda #' '
	jsr src::insert
	jsr text::putch
	dec @i
	bne @ident

@done:	lda zp::curx
	beq @ret
:	jsr src::prev
	dec zp::curx
	bne :-
@ret:	rts

@err:	jsr report_typein_error
	jmp @nextline
.endproc

;******************************************************************************
; REDRAW_TO_END_OF_LINE
; Redraws the line starting at the cursor's x position to the next $0d in the
; source
.proc redraw_to_end_of_line
@x=zp::tmp7
@cnt=zp::tmp8
	lda zp::curx
	sta @x
	lda #$00
	sta @cnt

:	jsr src::end
	beq @draw
	jsr src::next
	inc @cnt
	cmp #$0d
	beq @draw
	ldx @x
	sta mem::linebuffer,x
	inc @x
	bne :-

@draw:	lda #$00
	ldx @x
	sta mem::linebuffer,x
	lda zp::cury
	ldxy #mem::linebuffer
	jsr text::print

	; move back to where we were in the source
	lda @cnt
	beq @done
@restore:
	jsr src::prev
	dec @cnt
	bne @restore

@done:	rts
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
@nextline:
	; scroll lines below cursor position
	ldy zp::cury
	cpy height
	bcc :+
	; if we're at the bottom, scroll whole screen up
	ldx #EDITOR_ROW_START
	lda height
	jsr text::scrollup
	; and clear the new line
	jsr text::clrline
	lda height
	jsr text::drawline

	ldy height
	jmp @setcur

:	iny
	tya
	ldx height
	jsr text::scrolldown

	ldy zp::cury
	iny
@setcur:
	ldx #$00
	jmp cur::set
.endproc

;******************************************************************************
; CLRERROR
; Clears any error message
.proc clrerror
	jsr text::clrline
	ldxy #mem::linebuffer
	lda #ERROR_ROW
	jsr text::putz
.endproc

;******************************************************************************
; INSERT
; Adds a character at the cursor position.
.proc insert
	jsr is_readonly
	bne :+
@done:	rts

:	cmp #$80
	bcs @done		; non-printable

	cmp #$14		; handle DEL
	bne :+
	jmp ccdel
:	cmp #$0d
	bne :+
	jmp linedone		; handle RETURN
:	ldx text::insertmode
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
@xend=zp::tmp9
@ch=zp::tmpa
	jsr src::atcursor
	sta @ch

	lda mode
	cmp #MODE_VISUAL
	bne @novis

@novis:	ldx zp::curx
	stx @xend

	; if we are in VISUAL mode, highlight to the beginning of the line
	lda mode
	cmp #MODE_VISUAL
	bne @up

@sel:	jsr ccleft
	bcc @sel

	; handle cursor state for VISUAL mode
	ldxy src::line
	cmpw visual_start_line
	bcc @up
	bne @toggle
	lda zp::curx
	cmp visual_start_x
	bcc @up
@toggle:
	jsr cur::toggle	; if we're deselecting, toggle cursor off

@up:	jsr src::up	; move up a line or to start of line
	bcc @cont
	lda @ch
	cmp #$0d
	beq @cont	; if we crossed a newline, continue
	jsr src::get	; get the contents of the line we're on now
	lda #$00	; start of source, just move to the leftmost column
	sta zp::curx
	sta zp::cury
	sec
	rts

@cont:	ldx zp::curx
	beq :+
	jsr src::up	; move to start of line we're moving to
:	jsr text::clrline
	jsr src::get	; read the line we're moving to into linebuffer

	ldxy #mem::linebuffer
	jsr str::len
	sta zp::curx

:	jsr src::right
	bcc :-

@checkscroll:
	ldy zp::cury		; is cursor at row 0?
	beq @scroll		; if it is, scroll the screen down
	dec zp::cury		; if not, decrement it
	bpl @redraw

@scroll:
	lda #EDITOR_ROW_START
	ldx height
	jsr text::scrolldown	; cursor wasn't moved, scroll
@redraw:
	lda zp::cury
	jsr text::drawline

@movex: ldx zp::curx
	beq @rvs
	cpx @xend
	beq @rvs
	dec zp::curx
	jsr src::prev
	bcc @movex

@rvs:	jsr ccup_highlight	; handle highlight for the new row
@done:	RETURN_OK
.endproc

;******************************************************************************
; CCUP HIGHLIGHT
; Highlights the line that the cursor is on if the editor is in VISUAL mode
; This is called after the ccup logic
; the logic defining what to highlight is as follows:
;  if we are SELECTING (current line < visual-start-line):
;    hightlight from [cur-x, end-of-line]
;  if DESELECTING (visual-start-line < current line):
;    highlight from [0, cur-x]
;  if cur line == start line:
;    highlight from [cur-x, visual-start-x]
;
; If the editor is not in visual mode, this routine does nothing
.proc ccup_highlight
@togglecur=zp::tmp6
	lda mode
	cmp #MODE_VISUAL
	bne @done

	lda #$00
	sta @togglecur

	ldxy src::line
	cmpw visual_start_line
	bcc @sel
	beq @eq

@desel:	; highlight from [0, cur-x]
	ldy #$00
	ldx zp::curx
	beq @toggle
	inc @togglecur
	jmp @rvs

@sel:	; highlight from [cur-x, end-of-line]
	jsr text::linelen
	ldy zp::curx
	jmp @rvs

@eq:	; highlight between cur-x and visual-start-x
	ldy zp::curx
	cpy visual_start_x
	beq @toggle
	ldx visual_start_x
	inc @togglecur

@rvs:	lda zp::cury
	jsr bm::rvsline_part
	lda @togglecur
	beq @done
@toggle:
	jsr cur::toggle
@done:	rts
.endproc

;******************************************************************************
; DELCH
; Delete the character under the cursor
; If the character can not be deleted does nothing and returns .C set.
; Will not cross line boundaries
; OUT:
;  - .C: set if no character could be deleted.
.proc delch
	jsr is_readonly
	beq @no_del

	jsr src::end
	beq @no_del
	jsr src::after_cursor
	cmp #$0d
	beq @done		; last character
	jsr src::delete
	clc
	rts

@done:	ldx zp::curx
	beq @no_del
	lda #$00
	dex
	sta mem::linebuffer,x
	dec zp::curx
	jsr src::backspace
	clc
	rts
@no_del:
	sec
	rts
.endproc

;******************************************************************************
; CCLEFT
; Handles the left cursor key
; OUT:
;  - .C: set if cursor could not be moved
.proc ccleft
@deselect=zp::tmp2
	lda zp::curx
	beq @nomove

	lda mode
	cmp #MODE_VISUAL
	bne @movecur
	lda #$00
	sta @deselect

	; if (cur-line > visual_start_line) we are DESELECTING: unhighlight
	ldxy src::line
	cmpw visual_start_line
	beq @eq
	bcc @movecur
@desel: lda #$01
	bne :+

@eq:	lda zp::curx
	cmp visual_start_x
	beq @movecur
	lda #$00
	adc #$00
:	sta @deselect
	beq @movecur
	jsr cur::toggle		; turn off (deselect) old cursor position

@movecur:
	jsr cur::left

	lda mode
	cmp #MODE_VISUAL
	bne :+
	lda @deselect
	bne :+
	jsr cur::toggle

:	jsr src::prev
	clc
	rts
@nomove:
	sec
	rts
.endproc

;******************************************************************************
; CCRIGHT
; Handles the right cursor key
; OUT:
;  - .C: set if cursor could not be moved
.proc ccright
@deselect=zp::tmp0
	lda text::insertmode
	cmp #TEXT_INSERT
	beq @ins

@rep:	jsr src::right_rep
	bcc @ok
	sec
	rts

@ins:	jsr src::right
	bcc @ok
	sec
	rts

@ok:	; turn off the old cursor if we're unhighlighting
	lda #$00
	sta @deselect

	lda mode
	cmp #MODE_VISUAL
	bne @movecur

	ldxy src::line
	cmpw visual_start_line
	beq @eq
	bcs @movecur
@desel:	lda #$01
	bne :+

@eq:	lda visual_start_x
	cmp zp::curx
	beq @movecur
	lda #$00
	adc #$00
:	sta @deselect
	beq @movecur
	jsr cur::toggle

@movecur:
	jsr cur::right
	lda mode
	cmp #MODE_VISUAL
	bne :+
	lda @deselect
	bne :+
	jsr cur::toggle
:	clc
	rts
.endproc

;******************************************************************************
; CCDOWN
; Handles the down cursor key
; OUT:
;  - .C: clear if the cursor was moved DOWN or screen scrolled
.proc ccdown
@xend=zp::tmpa
@selecting=zp::tmpb
	jsr src::end
	bne :+
	sec		; cursor could not be moved
	rts		; cursor is at end of source file, return

:	lda mode
	cmp #MODE_VISUAL
	bne :+
	ldxy src::line
	cmpw visual_start_line
	lda #$00
	rol
	sta @selecting

:	lda zp::curx
	sta @xend

	; if we are in VISUAL mode, highlight to the end of the line
	lda mode
	cmp #MODE_VISUAL
	bne @cont

@sel:	jsr ccright
	bcc @sel

	; if this is a selection, toggle the cursor
	lda @selecting
	bne @cont
	jsr cur::toggle

@cont:	jsr src::down
	bcc @down

	; can't move down, move cursor to end of line
	ldxy #mem::linebuffer
	jsr str::len
	sta zp::curx
	sec		; cursor could not be moved down
	rts

@down:	jsr src::get	; get the data for this in linebuffer
	inc zp::cury	; move row down
	lda #$00
	sta zp::curx

	lda zp::cury
	cmp height
	beq @redraw
	bcc @redraw	; no need to scroll

@scroll:
	ldx #EDITOR_ROW_START
	lda height
	jsr text::scrollup	; cursor wasn't moved, scroll
	lda height
	sta zp::cury

@redraw:
	jsr text::drawline

@movex:	lda @xend
	beq @rvs
@xloop:	jsr src::right
	bcs @rvs
	inc zp::curx
	lda zp::curx
	cmp @xend
	bcc @xloop

@rvs:	jsr ccdown_highlight
@done:	RETURN_OK
.endproc

;******************************************************************************
; CCDOWN HIGHLIGHT
; Highlights the line that the cursor is on if the editor is in VISUAL mode
; This is called after the ccdown logic
; the logic defining what to highlight is as follows:
;  if we are SELECTING (current line > visual-start-line):
;    hightlight from [0, cur-x}
;  if DESELECTING (visual-start-line > current line):
;    highlight from [cur-x, end-of-line]
;  if cur line == start line:
;    highlight from [cur-x, visual-start-x]
;
; If the editor is not in visual mode, this routine does nothing
.proc ccdown_highlight
@togglecur=zp::tmp6
	lda mode
	cmp #MODE_VISUAL
	bne @done

	lda #$00
	sta @togglecur

	ldxy src::line
	cmpw visual_start_line
	beq @eq
	bcs @sel

@desel:	; highlight from [cur-x, end-of-line]
	jsr text::linelen
	ldy zp::curx
	jmp @rvs

@sel:	; highlight from [0, cur-x]
	inc @togglecur
	ldx zp::curx
	beq @toggle	; col 0: just toggle the cursor
	ldy #$00
	beq @rvs

@eq:	; highlight between cur-x and visual-start-x
	ldy zp::curx
	cpy visual_start_x
	beq @toggle
	ldx visual_start_x
	inc @togglecur

@rvs:	lda zp::cury
	jsr bm::rvsline_part
	lda @togglecur
	beq @done
@toggle:
	jsr cur::toggle
@done:	rts
.endproc

;******************************************************************************
; BACKSPACE
; Deletes the previous character in the source and moves the cursor as needed.
; Call text::drawline (with the newly updated cursor Y position) to render the
; result of this routine.
; OUT:
;  - mem::linebuffer: the new rendering of the line
;  - zp::curx: updated
;  - zp::cury: update
;  - .A: the character that was deleted (0 if none)
.proc backspace
@cnt=zp::tmp6
@line2len=zp::tmp7
@char=zp::tmp8
	lda #$00
	sta @char
	jsr src::backspace
	bcs @done
	sta @char
	lda #$14
	jsr text::putch
	bcc @done

@prevline:
	; move the cursor
	ldy #$ff
	ldx #0
	jsr cur::move

	; scroll everything up from below the line we deleted
	ldx zp::cury
	inx
	cpx height
	beq @noscroll	; if cursor is at end of screen, nothing to scroll
	lda height
	dex
	jsr text::scrollup
.IFDEF DRAW_TITLEBAR
	jsr draw_titlebar
.ENDIF

@noscroll:
	; clear the last line on the screen
	jsr text::clrline
	lda height
	jsr text::drawline

	; get the line we're moving up to in linebuffer
	jsr src::get

	; if the current char is a newline, we're done
	jsr src::atcursor
	cmp #$0d
	beq @done

	ldxy #mem::linebuffer
	jsr str::len
	sta @line2len

	; get the new cursor position ( new_line_len - (old_line2_len))
	jsr src::up
	jsr src::get
	ldxy #mem::linebuffer
	jsr str::len
	sec
	sbc @line2len
	sta @cnt
	beq @done
	dec @cnt
	bmi @done
@endofline:
	inc zp::curx
	jsr src::next
	dec @cnt
	bpl @endofline
@done:	lda @char
	rts
.endproc

;******************************************************************************
; CCDEL
; Handles the DEL key
.proc ccdel
	jsr src::start
	bne :+
@done:	rts

:	lda mode
	cmp #MODE_COMMAND	; handle COMMAND mode like REPLACE
	beq @del_rep

	jsr is_readonly
	beq @del_rep		; handle DEL in r/o mode the same as REPLACE
	lda text::insertmode
	bne @del_ins

@del_rep:
	; if we're replacing (or in r/o mode), just decrement cursor if we can
	lda zp::curx
	beq @done
	dec zp::curx
	jmp src::prev

@del_ins:
	jsr backspace
	lda zp::cury
	jmp text::drawline
.endproc

;*****************************************************************************
; SCROLLUP
; scrolls everything from the cursor's position up
.proc scrollup
	; scroll everything up from below the line we deleted
	ldx zp::cury
	inx
	cpx height
	beq @noscroll	; if cursor is at end of screen, nothing to scroll
	lda height
	dex
	jsr text::scrollup
@noscroll:
	rts
.endproc

;******************************************************************************
; SCROLLDOWN
; scrolls everything from the cursor's position down
.proc scrolldown
	lda zp::cury
	ldx height
	jmp text::scrolldown
.endproc

;******************************************************************************
; CANCEL
; Returns to COMMAND mode.
; If an error is being displayed, hides it.
.proc cancel
	ldy #EDITOR_HEIGHT
	sty height
	ldx #40
	iny
	jsr cur::setmax

	lda #TEXT_REPLACE
	sta text::insertmode
	jmp enter_command
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
:	jsr gotoline		; go to the target line
	jmp add_jump_point	; save the current position as a jump point
@done:	rts
.endproc

;******************************************************************************
; COMMAND_FIND
; Gets a string from the user and searches (forward) for it in the source file
.proc command_find
	ldxy #$0000
	jsr readinput
	jmp __edit_find
@done:	rts
.endproc

;******************************************************************************
; FIND
; Searches for the text given in .YX and moves the cursor to it if it's
; found
; IN:
;  - .YX: the text to find (0-terminated)
.proc __edit_find
@string=zp::str0
@seekptr=zp::str2
@tofind=zp::tmp2
@target=zp::tmp8
@len=zp::tmpa
@cnt=zp::tmpd
@searchbuff=$120	; buffer of bytes to search
	stxy @string
	jsr str::len
	sta @len
	bne :+
	rts		; if 0-length string, don't search

:	jsr src::pushp	; save source position
	lda #$00
	sta @cnt

; fill the search buffer (16 bytes)
@l0:	jsr src::next
	jsr src::end
	bne :+
	lda #$00
:	ldy @cnt
	sta @searchbuff,y
	inc @cnt
	cpy #15
	bne @l0

	ldxy #@searchbuff
	stxy @seekptr

; see if the text we're looking for is in the buffer
@seekloop:
	jsr str::comparez
	beq @found

; if no match, shift the buffer, load a new byte, and try again
@next:	lda @searchbuff+1
	beq @notfound	  ; if buffer starts with 0 (EOF), we're done
	ldx #$00
@l1:	lda @searchbuff+1,x
	sta @searchbuff,x
	inx
	cpx #15
	bcc @l1

	; get a new byte (use 0 if EOF)
	lda #$00
	jsr src::end
	beq :+
	jsr src::next
:	ldy #15
	sta (@seekptr),y
	jmp @seekloop		; if not EOF, keep seeking

@notfound:
	jsr src::popp
	jmp src::goto

@found:	ldxy src::line	; get the line we're moving to
	stxy @target

; for every newline in the buffer AFTER the text we're looking for
; decrement 1 from our target line
	ldy @len
	dey
@l2:	lda @searchbuff,y
	cmp #$0d
	bne :+
	decw @target
:	iny
	cpy #16
	bne @l2

	; move source back to 1st matching character by retreating 16 bytes
	lda #15
	sta @cnt
@l3:	ldx @cnt
	lda @searchbuff,x
	beq :+
	jsr src::prev	; go back 16 bytes (to get to start of buffer)
:	dec @cnt
	bpl @l3

	inc @cnt

	; find the x-offset in the line by retreating til newline
	jsr src::atcursor
	cmp #$0d
	beq @move

:	jsr src::prev
	bcs @move
	inc @cnt
	cmp #$0d
	bne :-

	; move to the line containing the search word
@move:	jsr src::popp	; get old source position
	jsr src::goto	; and restore it
	ldxy @target
	jsr gotoline	; go to the new line

	; update the x cursor position to the first character of the search
	lda @cnt
	beq @done
:	jsr ccright
	dec @cnt
	bne :-
@done:	rts
.endproc

;******************************************************************************
; GOTOLINE
; Sets the editor to the line in .YX and refreshes the screen.
; IN:
;  - .XY: the line number to go to
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

	jsr home	; if we're not already, move to the start of the line

	ldx #$00
	stx @seekforward

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
	bcc @long

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
	jmp @shortdone

:	; move down and move cursor
	ldx @diff
	jsr src::downn
	ldy @diff
	ldx #$00
@shortdone:
	jsr cur::move
	jmp src::get

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
; HIGHLIGHT_LINE
; IN:
;  - .XY: the line number to highlight
.export __highlight_line
.proc __highlight_line
@x=zp::tmp0
	stx @x
	lda numhighlights
	asl
	tax
	lda @x
	sta highlighted_lines,x
	tya
	sta highlighted_lines+1,x
	inc numhighlights
	rts
.endproc

;******************************************************************************
; CLEAR_HIGHLIGHTS
; Removes all highlights
.export clear_highlights
.proc clear_highlights
	lda #$00
	sta numhighlights
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
	jsr str::uncompress

	lda #<strings::edit_line_err
	sta zp::tmp0
	lda #>strings::edit_line_err
	sta zp::tmp0+1
	jsr str::cat

	lda #ERROR_ROW
	jsr text::print

	ldy #ERROR_ROW-1
	sty height
	ldx #40
	iny
	jmp cur::setmax
.endproc

;******************************************************************************
; REPORT TYPEIN ERROR
; Reports just the error message for the given error.
; This is used for giving the user realtime errors as they are typing in their
; program
; IN:
;  - .A: the error code
.proc report_typein_error
	cmp #$00
	beq @done	; no error
	jsr err::get
	jsr str::uncompress
	lda #STATUS_ROW-1
	jsr text::print
@done:	rts
.endproc

;******************************************************************************
; SRC2SCREEN
; Takes the given source line number and returns its row position on the screen.
; IN:
;  - .XY: the line number to get the screen row of
; OUT:
;  - .A: the row that the line number resides on
;  - .C: set if the line number is not on screen
.export __edit_src2screen
.proc __edit_src2screen
@line=zp::tmp0
@startline=zp::tmp2
@endline=zp::tmp4
	stxy @line
	lda src::line
	sec
	sbc zp::cury
	sta @startline
	lda src::line+1
	sbc #$00
	sta @startline+1

	lda @startline
	sec			; +1
	adc height
	sta @endline
	lda @startline+1
	adc #$00
	sta @endline+1

	ldxy @line
	cmpw @startline
	bcc @done
	cmpw @endline
	bcs @done

	lda @line
	sec
	sbc @startline		; will be [0, BRKVIEW_START)
	RETURN_OK

@done:	sec			; line off screen
	rts
.endproc

;******************************************************************************
; ADD JUMP POINT
; Adds a jump point at the current source position
.proc add_jump_point
@end=zp::tmp0
	lda jumpptr
	cmp #MAX_JUMPS
	bcc @cont
	asl
	sta @end

	ldx #$00
; shift all existing jumps down
:	lda jumplist+2,x
	sta jumplist,x
	lda jumplist+3,x
	sta jumplist+1,x
	inx
	inx
	cpx @end
	bcc :-

	dec jumpptr

; add the new jump to the end of the jumplist
@cont:	lda jumpptr
	asl
	tax
	lda src::line
	sta jumplist,x
	lda src::line+1
	sta jumplist+1,x

	inc jumpptr
	rts
.endproc

;******************************************************************************
; JUMPBACK
; Jumps back to the last source position the user has jumped from
.export jumpback
.proc jumpback
	lda jumpptr
	bne :+
	rts		; jumplist is empty

:	dec jumpptr
	lda jumpptr
	asl
	tax
	ldy jumplist+1,x
	lda jumplist,x
	tax
	jmp gotoline
.endproc

;******************************************************************************
; BUFF PUTCH
; Pushes the given character onto the copy/paste buffer
; IN:
;  - .A: the character to put into the buffer
; OUT:
;  - .C: set if the buffer is full (couldn't add char)
.proc buff_putch
@buff=zp::tmp0
	ldxy buffptr
	stxy @buff
	cmpw #mem::copybuff+MAX_COPY_SIZE	; buffer is full
	bcs @done
	ldy #$00
	sta (@buff),y
	incw buffptr
	clc
@done:	rts
.endproc

;******************************************************************************
; BUFF GETCH
; Gets the last character that was PUT to the buffer
; OUT:
;  - .A: the last character PUT into the buffer (0 if none)
;  - .C: set if the buffer is empty
.proc buff_getch
@buff=zp::tmp0
	ldxy buffptr
	stxy @buff
	cmpw #mem::copybuff
	beq @done		; buffer empty

	ldy #$00
	decw buffptr
	decw @buff
	lda (@buff),y
	clc
@done:	rts
.endproc

;******************************************************************************
; DESELECT
; Unselects any text that is currently selected
.proc deselect
	lda mode
	cmp #MODE_VISUAL
	bne @done
	lda #CUR_OFF
	sta cur::status
	;jmp refresh
@done:	rts
.endproc

;******************************************************************************
; IS READONLY
; Returns .Z set if the buffer should not allow edits (true if readonly has
; been explictly enabled or if we are in a VISUAL editing mode)
.proc is_readonly
	ldx readonly
	bne @ro
	ldx mode
	cpx #MODE_VISUAL
	rts
@ro:	ldx #$00
	rts
.endproc

.RODATA
;******************************************************************************
controlcodes:
.byte K_LEFT	; left
.byte K_RIGHT	; right
.byte K_UP	; up arrow
.byte K_DOWN	; down
numccodes=*-controlcodes

;******************************************************************************
ccvectors:
.define ccvectors ccleft, ccright, ccup, ccdown
ccvectorslo: .lobytes ccvectors
ccvectorshi: .hibytes ccvectors

;******************************************************************************
commands:
	.byte $68	; j (left)
	.byte $6c	; l (right)
	.byte $6b	; k (up)
	.byte $6a	; j (down)
	.byte $65	; e (end of word)
	.byte $62	; b (beginning of word)
	.byte $49	; I (insert start of line)
	.byte $69	; i (insert)
	.byte $72	; r (replace char)
	.byte $52	; r (replace mode)
	.byte $41	; A (append to line/insert)
	.byte $61	; a (append to character)
	.byte $64	; d (delete)
	.byte $70	; p (paste below)
	.byte $50	; P (paste above)
	.byte $78	; x (erase char)
	.byte $77	; w (word advance)
	.byte $30	; 0 (column 0)
	.byte $4c	; L (last line)
	.byte $48	; H (home [first] line)
	.byte $14	; DEL (back)
	.byte $20	; SPACE (right)
	.byte $47	; G (goto end)
	.byte $67	; g (goto start)
	.byte $4f	; O (Open line above cursor)
	.byte $6f	; o (Open line below cursor)
	.byte $24	; $ (end of line)
	.byte $5b	; [ (previous empty line)
	.byte $5d	; ] (next empty line)
	.byte $0d	; RETURN (go to start of next line)
	.byte $3b	; ; (comment out)
	.byte $76	; v (enter visual mode)
	.byte $56	; V (enter visual line mode)
	.byte $79	; y (yank)
numcommands=*-commands

; command tables for COMMAND mode key commands
.linecont +
.define cmd_vecs ccleft, ccright, ccup, ccdown, endofword, beginword, \
	insert_start, enter_insert, replace_char, replace, append_to_line, \
	append_char, delete, paste_below, paste_above, delete_char, \
	word_advance, home, last_line, home_line, ccdel, ccright, goto_end, \
	goto_start, open_line_above, open_line_below, end_of_line, \
	prev_empty_line, next_empty_line, begin_next_line, comment_out, \
	enter_visual, enter_visual, yank
.linecont -
command_vecs_lo: .lobytes cmd_vecs
command_vecs_hi: .hibytes cmd_vecs
