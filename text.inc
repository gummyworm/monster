.import __text_len
.import __text_clrline
.import __text_colstart
.import __text_puts
.import __text_charmap
.import __text_putch
.import __text_status
.import __text_update
.import __text_print
.import __text_puterr
.import __text_hiline
.import __text_hioff
.import __text_drawline
.import __text_insertmode
.import __text_scroll
.import __text_get
.import __text_linelen
.import __text_savebuff
.import __text_restorebuff

; getinput sets the cursor to (X,Y) and reads input into arg (up to sz bytes)
.macro getinput arg, xpos, ypos, sz
	ldxy arg
	stxy zp::tmp0
	ldx #xpos
	ldy #ypos
	jsr cur::setmin
	jsr cur::set
	ldx #xpos+sz
	ldy #ypos
	jsr cur::setmax
	lda #sz
	jsr __text_get
.endmacro

.scope text
        len      = __text_len
	clrline  = __text_clrline
        colstart = __text_colstart
        puts     = __text_puts
	putch    = __text_putch
	print    = __text_print
        charmap  = __text_charmap
	status   = __text_status
	update   = __text_update
	hiline   = __text_hiline
	hioff    = __text_hioff
	drawline = __text_drawline
	scroll   = __text_scroll
	insertmode = __text_insertmode
	linelen  = __text_linelen
	savebuff = __text_savebuff
	restorebuff = __text_restorebuff
.endscope
