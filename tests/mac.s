.org $7500
.mac ldxy val
	ldx #<val
	ldy #>val
.endmac

.mac stxy val
	stx val
	sty val+1
.endmac

	ldxy $1234
	stxy $1000
