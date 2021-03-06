.ifndef ZEROPAGE_INC
ZEROPAGE_INC = 1

.scope zp
;--------------------------------------
;reserved for routines' arguments
arg0 = $20
arg1 = $21
arg2 = $22
arg3 = $23
err = $24 ; 1 byte error that procedures may set in the case of an error

;--------------------------------------
;temporary storage
tmp0 = $f0
tmp1 = $f1
tmp2 = $f2
tmp3 = $f3
tmp4 = $f4
tmp5 = $f5
tmp6 = $f6
tmp7 = $f7
tmp8 = $f8
tmp9 = $f9
tmpa = $fa
tmpb = $fb
tmpc = $fc
tmpd = $fd
tmpe = $fe
tmpf = $ff
tmp10= $e0
tmp11= $e1
tmp12= $e2
tmp13= $e3
tmp14= $e4
tmp15= $e5
;--------------------------------------
;storage for IRQ handlers
irq0 = $e6
irq1 = $e7
irq2 = $e8
irq3 = $e9
irq4 = $ea
;--------------------------------------
; cursor
curx = $eb
cury = $ec

gap=$ed 	; the pointer for the cursor in the source buffer
gapsize=$ef 	; offset to the end of gap buffer

.endscope
.endif
