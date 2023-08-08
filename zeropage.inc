.ifndef ZEROPAGE_INC
ZEROPAGE_INC = 1

.scope zp

;--------------------------------------
app_vars = $e9
app_vars_size=$ec-app_vars

;--------------------------------------
; $40-$50 are used during assembly
asm=$40		; base address of assembly tmp vars
line=$4c	; ptr to text being processed
label_value=$50	; arguments to label functions

;--------------------------------------
asmresult = $e9	; pointer where code is assembled
bank      = $eb ; the current bank in the final expansion being worked on

;--------------------------------------
; cursor
curx = $eb
cury = $ec

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
tmp16= $e6
tmp17= $e7

.endscope
.endif
