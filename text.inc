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
.import __text_scrolldown
.import __text_get
.import __text_linelen
.import __text_savebuff
.import __text_restorebuff
.import __text_dir

; getinput sets the cursor to (X,Y) and reads input into arg (up to sz bytes)
.macro getinput arg, xpos, ypos, sz
	lda zp::curx
	pha
	lda zp::cury
	pha
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
	pla
	tay
	pla
	tax
	jsr cur::set
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
	scrolldown = __text_scrolldown
	insertmode = __text_insertmode
	linelen  = __text_linelen
	savebuff = __text_savebuff
	restorebuff = __text_restorebuff
	dir = __text_dir
.endscope
