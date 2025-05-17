.segment "UDGEDIT"
;*******************************************************************************
; EDIT
; Activates the UDG editor
; OUT:
;  - r8-rf: the character that the user created
;  - .A:    0: no graphic created or updated
;           1: new graphic created
;           2: graphic updated
.export __udg_edit
.proc __udg_edit
	; TODO:
	rts
.endproc
