.ifndef RAM_INC
RAM_INC = 1

.include "zeropage.inc"

.ifdef vic20
.include "vic20/ram.inc"
.endif

.ifdef c64
.include "c64/ram.inc"
.endif

.import __ram_call
.import __ram_jmp
.import __ram_copy
.import __ram_memcpy
.import __ram_copy_banked
.import __ram_copy_line
.import __ram_store_byte
.import __ram_bank_store_rel
.import __ram_get_byte
.import __ram_load_byte
.import __ram_load_byte_off

;******************************************************************************
; SET SEGMENT
; Equivalent to the `.segment` directive, but also sets a variable so that
; other RAM macros know which bank the calling code resides in.
; It is important NOT to use other segment-setting directives (.segment,
; .CODE, .BSS, .DATA, or .RODATA) in order for this variable to correctly be
; set.
.macro SET_SEGMENT name
	__current_bank .set bank
.endmacro

;;******************************************************************************
;; CALL
;; Calls the given routine.  If the routine is within the same bank as the
;; active segment, will evaluate to a simple JSR.  If not, will execute the
;; platform-specific code to call the routine
;.macro CALL bank, proc
;	.if bank = __current_bank
;		; target procedure is in current bank
;		jsr proc
;	.else
;		; target procedure is in another bank
;	.endif
;.endmacro

;;******************************************************************************
;; JUMP
;; Jumps to the given routine.  If the routine is within the same bank as the
;; active segment, will evaluate to a simple JMP.  If not, will execute the
;; platform-specific code to jump to the routine
;.macro JUMP bank, proc
;	.if bank = __current_bank
;		; target procedure is in current bank
;		jmp proc
;	.else
;		; target procedure is in another bank
;	.endif
;.endmacro

.scope ram
	call      = __ram_call
	copy      = __ram_memcpy
	copybanked= __ram_copy_banked
	copyline  = __ram_copy_line
	jump      = __ram_jmp
	store     = __ram_store_byte
	store_off = __ram_bank_store_rel
	get_byte  = __ram_get_byte
	load      = __ram_load_byte
	load_off  = __ram_load_byte_off

	load_byte_off  = __ram_load_byte_off
.endscope

.endif
