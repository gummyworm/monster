.ifndef ZEROPAGE_INC
ZEROPAGE_INC = 1

.scope zp

;******************************************************************************
; $00-$03 is the JMP vector
jmpaddr = $00
jmpvec  = $01 ; $00 stores a JMP instruction, this is its address

;******************************************************************************
; $04-$07 is used for final expansion temp
final   = $04
banksp  = $05	; bank stack pointer
bankval = $07	; the value written by store_byte
bankjmpaddr = $08
bankjmpvec  = $09

;******************************************************************************
; $0b-$0f are reserved for math
math =  $0b

;******************************************************************************
; $10-$30 is reserved for debug info generation
debug    = $10

;******************************************************************************
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

;******************************************************************************
; $40-$50 are used during assembly, disassembly, linking, and GUI routines
asm          = $40	; base address of assembly tmp vars
disasm       = $40	; base address of disassembly tmp vars
pass         = $4c	; the pass of assembly being done (1 or 2)
line         = $4d	; ptr to text being processed
gendebuginfo = $4f 	; flag to generate debug info
link         = $40	; base address of linker temp storage
gui          = $40

;******************************************************************************
; $50-$60 are used for labels
label_value=$50	; arguments to label functions
labels=$52

;******************************************************************************
; $60-$70 are used by the "context" during assembly
ctx=$60

;******************************************************************************
; $7f-$90 are used for macro related things during assembly
macros = $7f

;******************************************************************************
; $90-$97 used for expression evaluation
expr = $90

;******************************************************************************
; $98-9C are used by KERNAL I/O and should be avoided
numfiles = $98

;******************************************************************************
; $9d-$a0 used for bank switching storage
bank         = $9d 	; the current bank in the final expansion being worked on
banktmp      = $9e
bankstack    = $9e+3	; small call stack for fe3::call (bank bytes only)

bankoffset   = $a6	; offset for relative reads/writes
bankaddr0    = $a7
bankaddr1    = $a9

;******************************************************************************
; $ac-$b8 is reserved for text displaying/bitmap stuff
text = $ac

;******************************************************************************
; $b8-$c0 reserved for file stuff
lfs    = $b8	; active logical file (SETLFS)
sa     = $b9	; active secondary address (SETLFS)
		; NOTE: secondary addresses must be unique for each open file
device = $ba	; the last used drive #
file   = $bb	; the most recently accessed file

keycnt = $cb	; # of keys in key buffer
key    = $cb	; current keypress 

keytab = $f5	; key decode table

;******************************************************************************
; $d0-$d5 assembly state
asmresult = $d0	; pointer where code is assembled
virtualpc = $d2 ; pointer to address address code will live at during runtime (.RORG)
verify 	  = $d5

;******************************************************************************
; $d6-$d7 cursor state
curx   = $d6
cury   = $d7
curtmr = $d8

;******************************************************************************
; $d9-$e0 editor
editor 	  	= $d9
editor_mode	= $db	; editor mode (MODE_COMMAND or MODE_INSERT)
editortmp 	= $dc

;******************************************************************************
; $e0-$ff temporary storage
; this is the primary scratchpad for routines. 
; Be cautious when relying on this data to be in tact
; after calling a procedure as it is likely that that 
; procedure uses some of these locations
tmp0  = $f0
tmp1  = $f1
tmp2  = $f2
tmp3  = $f3
tmp4  = $f4
tmp5  = $f5
tmp6  = $f6
tmp7  = $f7
tmp8  = $f8
tmp9  = $f9
tmpa  = $fa
tmpb  = $fb
tmpc  = $fc
tmpd  = $fd
tmpe  = $fe
tmpf  = $ff
tmp10 = $e0
tmp11 = $e1
tmp12 = $e2
tmp13 = $e3
tmp14 = $e4
tmp15 = $e5
tmp16 = $e6
tmp17 = $e7

;******************************************************************************
; $e8-$ef used for utility 
util = $e8
.endscope

r0 = $f0
r1 = $f1
r2 = $f2
r3 = $f3
r4 = $f4
r5 = $f5
r6 = $f6
r7 = $f7
r8 = $f8
r9 = $f9
ra = $fa
rb = $fb
rc = $fc
rd = $fd
re = $fe
rf = $ff

.endif
