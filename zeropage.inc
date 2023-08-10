.ifndef ZEROPAGE_INC
ZEROPAGE_INC = 1

.scope zp

;--------------------------------------
; $30-$40 reserved for string manipulation routines
; which require a fair amount of zeropage space.
; You may use these if you don't need to use any 
; string functions
str0 = $30
str1 = $31
str2 = $32
str3 = $33
str4 = $34
str5 = $35
str6 = $36
str7 = $37
str8 = $38
str9 = $39
stra = $3a

;--------------------------------------
; $40-$50 are used during assembly
asm=$40		; base address of assembly tmp vars
line=$4c	; ptr to text being processed
label_value=$50	; arguments to label functions

;--------------------------------------
; $b0-$c0 reserved for file stuff
device = $ba	; the last used drive #
file = $bb	; the most recently accessed file

;--------------------------------------
; $d0-$d2 assembly state
asmresult = $d0	; pointer where code is assembled
bank      = $d2 ; the current bank in the final expansion being worked on

;--------------------------------------
; $d3-$d4 cursor state
curx = $d3
cury = $d4

;--------------------------------------
; $e0-$ff temporary storage
; this is the primary scratchpad for routines. 
; Be cautious when relying on this data to be in tact
; after calling a procedure as it is likely that that 
; procedure uses some of these locations
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
tmp18=$e8
tmp19=$e9
tmp1a=$ea
tmp1b=$eb
tmp1c=$ec
tmp1d=$ed
tmp1e=$ee
tmp20=$ef
.endscope
.endif
