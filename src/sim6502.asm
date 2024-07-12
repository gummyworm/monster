;******************************************************************************
; SIM6502.ASM
; This file contains the code to simulate the 6502. This is used to
; determine what memory/registers are affected by an instruction, count the
; number of cycles that instructions execute, and to determine what the
; next instruction that will be executed as.
;******************************************************************************

.include "asmflags.inc"
.include "errors.inc"
.include "macros.inc"
.include "memory.inc"
.include "vmem.inc"
.include "zeropage.inc"

;******************************************************************************
.BSS

.export __sim_register_state
.export __sim_pc
.export __sim_reg_a
.export __sim_reg_x
.export __sim_reg_y
.export __sim_reg_sp
.export __sim_reg_p

__sim_register_state:
__sim_pc:     .word 0
__sim_reg_a:  .byte 0
__sim_reg_x:  .byte 0
__sim_reg_y:  .byte 0
__sim_reg_sp: .byte 0
__sim_reg_p:  .byte 0

; VIA state
__sim_via1_t1:  .byte 0
__sim_via1_t2:  .byte 0
__sim_via1_ier: .byte 0
__sim_via1_ifr: .byte 0

__sim_via2_t1:  .byte 0
__sim_via2_t2:  .byte 0
__sim_via2_ier: .byte 0
__sim_via2_ifr: .byte 0

; if !0, a relative branch will be taken next STEP
.export __sim_branch_taken
__sim_branch_taken:  .byte 0

; the next PC after the current instruction is executed
.export __sim_next_pc
__sim_next_pc: .word 0

; next opcode that will be executed
.export __sim_op
__sim_op: .byte 0

; address modes used by current instruction
.export __sim_op_mode
__sim_op_mode: .byte 0

; flag of what a given instruction affects, OP_LOAD, OP_STORE, OP_REG_A, etc.
.export __sim_affected
__sim_affected: .byte 0

; address that is written/loaded by a given STEP
.export __sim_effective_addr
__sim_effective_addr: .word 0

; stopwatch of cycles counted by simulator since last reset
.export __sim_stopwatch
__sim_stopwatch: .res 3

.segment "DEBUGGER"
;******************************************************************************
; NEXT_INSTRUCTION
; Given the address of the current instruction, returns the address of the next
; instruction that will be executed.
; Instructions that are considered for branches are:
;  - JSR
;  - JMP
;  - JMP (indirect)
;  - Bxx (all the conditional branches)
;  - RTI
;  - RTS
; IN:
;  - .XY: the address of the current instruction
;  - .A:  the size of the current instruction
; OUT:
;  - .XY:           the address of the next instruction that will be executed
;  - __sim_op:      the next opcode that will be executed
;  - __sim_next_pc: the address of the next instruction that will be executed
; TODO: handle interrupts (VIA timers)
; TODO: handle BRK
.export __sim_next_instruction
.proc __sim_next_instruction
@op=r0
@sz=r2
@y=r3
@msb=r4
@opcode=r5
@operand=r6
	jsr @getnextpc
	stxy __sim_next_pc
	rts

@getnextpc:
	sta @sz
	stxy @op

	lda #$00
	sta __sim_branch_taken 	; clear branch taken flag

	; get the opcode
	ldxy @op
	jsr vmem::load
	sta @opcode
	sta __sim_op

	ldxy @op
	lda #$01
	jsr vmem::load_off
	sta @operand

	ldxy @op
	lda #$02
	jsr vmem::load_off
	sta @operand+1

	lda @opcode
	cmp #$20	; JSR?
	beq @jmpjsr

@notjsr:
	beq @jmpjsr
	cmp #$6c	; JMP (ind)?
	bne :+

	ldxy @operand
	jsr vmem::load
	pha
	incw @operand
	ldxy @operand
	jsr vmem::load
	tay
	pla
	tax
	rts

:	cmp #$4c	; JMP?
	bne @notjmp

; for JMP and JSR just set PC to the operand
@jmpjsr:
	ldxy @operand
	rts

@notjmp:
	cmp #$60
	bne @notrts

; next instruction is stack address + 1
@rts:   ldy __sim_reg_sp
	lda $100+1,y
	clc
	adc #$01
	tax
	lda $100+2,y
	adc #$00
	tay
	rts

@notrts:
	cmp #$40
	bne @notrti

@rti:	ldy __sim_reg_sp
	ldx $100,y
	lda $100+1,y
	tay

@notrti:
	and #$1f
	cmp #$10
	beq @branch

; not a control-flow instruction, just add the size of the instruction to the PC
@nocontrol:
	lda __sim_pc
	clc
	adc @sz
	tax
	lda __sim_pc+1
	adc #$00
	tay
	rts

; handle branches. branch is taken if corresponding flag for top two bits
; of branch opcode equal bit 5 of the opcode
; e.g. 11010000 will branch if the Z (zero) flag, which is the flag represented
; by bits 6 & 7 (11) is clear- the 0 in bit 5 in this example.
@branch:
	; get top 2 bits (xx) of branch to get type of branch
	lda @opcode
	asl
	rol
	rol
	and #$03
	tax

	; get the y (bit 5) in the same bit position as the flag we're testing
	lda @opcode
	and #$20
	beq :+			; if bit y is 0, use $00
	lda branch_masks,x	; if bit y is 1, use the mask
:	sta @y

	lda branch_masks,x
	and __sim_reg_p	; isolate the bit we're interested in
	eor @y		; if y != .P[xx], no branch
	beq @takebranch

; branch isn't taken, just add 2 to the PC
@nobranch:
	lda __sim_pc
	clc
	adc #$02
	tax
	lda __sim_pc+1
	adc #$00
	tay
	rts

; branch is taken, add the operand (offset) + 2
@takebranch:
	inc __sim_branch_taken
	lda @operand
	bpl :+
	lda #$ff
	skw
:	lda #$00
	sta @msb

	lda __sim_pc
	clc
	adc @operand
	tax

	iny
	lda __sim_pc+1
	adc @msb
	tay

	txa
	clc
	adc #$02
	tax
	tya
	adc #$00
	tay
	rts

.endproc

;******************************************************************************
; GET SIDE EFFECTS
; Checks if the given instruction requires any RAM state and handles the
; creation of any state needed to handle them.
; This essentially involves checking if the instruction accesses any RAM and,
; if it does, settting __sim_effective_addr to the address that will be affected
; IN:
;  - .XY: address of the binary instruction
;  - .A: the size of the instruction
;  - r0: the address modes for the instruction (see asm::disassemble)
; OUT:
;  - .C:                   clear if the instruction is desctructive
;  - __sim_effective_addr: holds the address of the byte that will be
;                          loaded/stored
;  - __sim_affected:       stores the flags with the CPU/mem state the operation
;                          affects
.export __sim_get_side_effects
.proc __sim_get_side_effects
@instruction=r2
@opsz=r4
@target=r5
@opcode=r7
@offset=zp::bankval
	stxy @instruction
	sta @opsz

	; if 0 byte opcode, it's either PHA, PHP or doesn't touch RAM
	cmp #$00
	bne @cont
	sec			; no memory side-effects; TODO: check PHA
	rts

	; save the debugger's contents at the @instruction address
@cont:	; get the instruction opcode/operand at the @instruction address
	ldxy @instruction
	jsr vmem::load			; opcode
	sta @opcode

	incw @instruction
	ldxy @instruction
	jsr vmem::load			; operand (1st byte)
	sta @target

	lda #$00
	ldx @opsz
	cpx #$03
	bcc :+
	incw @instruction
	ldxy @instruction
	jsr vmem::load			; operand (2nd byte)
:	sta @target+1

	; get the side-effects for this operation
	ldx @opcode
	lda side_effects_tab,x
	sta __sim_affected			; save the side-effects for aux uses

	and #OP_STORE | OP_LOAD		; is operation a load or store?
	bne :+
	ldxy #$6666
	stxy __sim_effective_addr	; don't save anything

:	; get effective target address of this instruction and save it
	; we will save/restore state before/after a BRK using this
	jsr get_effective_addr
	stxy __sim_effective_addr
	rts
.endproc

;******************************************************************************
; GET EFFECTIVE ADDR
; Returns the effective address for the given opcode/operand. This may be the
; same as the operand (if the opcode represents a zeropage or absolute address
; mode) or not (indirect or indexed address modes)
; IN:
;  - .XY: the operand
;  - .A:  the opcode
;  - r0:  the address mode of the instruction
; OUT:
;  - .XY: the effective address of the instruction
.proc get_effective_addr
@target=r5
	lda __sim_op_mode
	cmp #MODE_X_INDEXED|MODE_ZP|MODE_INDIRECT	; x,ind?
	bne @check_ind_y
@ind_x:
	; add .X register to ZP target to get target address (wrapping is fine)
	; THEN load the address at this target
	ldx __sim_reg_x
	ldy @target
	lda mem::prog00,y
	sta @target
	iny
	lda mem::prog00,x
	sta @target+1
	jmp @get_ind

;--------------------------------------
@check_ind_y:
	lda __sim_op_mode
	cmp #MODE_Y_INDEXED|MODE_ZP|MODE_INDIRECT	; y,ind?
	bne @check_rel_y
	; get the value of the ZP location in the *user* ZP
	ldy @target
	lda mem::prog00,y
	iny
	lda mem::prog00,y
	sta @target+1
	; add the .Y register value to the address from the ZP
	lda __sim_reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	jmp @done

;--------------------------------------
@check_rel_y:
	lda __sim_op_mode
	and #MODE_Y_INDEXED		; y indexed?
	beq @check_rel_x
	; add the value of .Y to the target get the target address
	lda __sim_reg_y
	clc
	adc @target
	sta @target
	bcc :+
	inc @target+1
:	lda __sim_op_mode
	and #MODE_ZP
	beq @done
	lda #$00		; if ZP,y clear the MSB of target
	sta @target+1
	beq @done

;--------------------------------------
@check_rel_x:
	lda __sim_op_mode
	and #MODE_X_INDEXED		; x indexed?
	beq @check_ind
	; add the value of .X to get the target address
	lda __sim_reg_x
	clc
	adc @target
	sta @target
	bcc @done
	inc @target+1
	lda __sim_op_mode
	and #MODE_ZP
	beq @done
	lda #$00		; if ZP,x clear the MSB of target
	sta @target+1
	beq @done

@check_ind:
	lda __sim_op_mode
	cmp #MODE_INDIRECT|MODE_ABS	; JMP (ind) ?
	bne @abs_or_zp
	; read the target of the indirect JMP and set @target to it
@get_ind:
	ldxy @target
	jsr vmem::load
	pha
	incw @target
	ldxy @target
	jsr vmem::load
	sta @target+1
	pla
	sta @target

@abs_or_zp:
@done:	ldxy @target
	RETURN_OK
.endproc

;******************************************************************************
; COUNT CYCLES
; Counts the number of cycles that the given instruction will execute
; IN:
;  - .A: the opcode of the instruction
; OUT:
;  - .A: the number of cycles the instruction will use
.export __sim_count_cycles
.proc __sim_count_cycles
@cycles=r0
	lsr
	tax
	bcc @l
@r:	lda #$0f
	and timings,x
	bne @fix

@err:	lda #$ff
	sec
	rts

@l:	lda timings,x
	lsr
	lsr
	lsr
	lsr
	beq @err

@fix:	sta @cycles

@chkbra:
	lda __sim_branch_taken
	beq @chkpagecross
	inc @cycles		; +1 cycle if branch taken
	lda __sim_next_pc+1	; get target MSB
	cmp __sim_pc+1		; is the target on the same page?
	bne @penalty		; different page -> 1 MORE cycle penalty
	beq @done

;abs,x abs,y and (zp),y need an extra cycle if indexing crosses page boundary
@chkpagecross:
	; (zp),y
	lda #MODE_ZP|MODE_Y_INDEXED|MODE_INDIRECT
	and __sim_op_mode
	cmp #MODE_ZP|MODE_Y_INDEXED|MODE_INDIRECT
	beq @chkpage

	; abs,x and abs,y
	lda #MODE_ABS|MODE_X_INDEXED|MODE_Y_INDEXED
	and __sim_op_mode
	beq @done
	and #MODE_ABS
	beq @done
@chkpage:
	lda __sim_effective_addr
	cmp #$ff		; is effective address LSB $ff?
	bne @done		; no penalty if not (no page boundary crossed)
@penalty:
	inc @cycles

@done:	lda @cycles
	rts
.endproc

.RODATA
;******************************************************************************
; OERATION SIDE EFFECTS TABLE
; This table contains all opcodes and what state they affect.
; This is used to display changes made by a given instruction as well as
; determine what (internal) state the debugger needs to preserve and restore
; between steps.
; If a value in internal memory is accessed (OP_LOAD/OP_STORE) the user
; value for that memory must be swapped in before executing the instruction
; $0-
side_effects_tab:
.byte $00			; $00 BRK
.byte OP_REG_A|OP_LOAD		; $01 ORA x,ind
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $05 ORA zpg
.byte OP_LOAD|OP_STORE 		; $06 ASL zpg
.byte $00			; ---
.byte OP_STACK|OP_STORE		; $08 PHP
.byte OP_REG_A			; $09 ORA #
.byte OP_REG_A			; ASL A
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $0d ORA abs
.byte OP_LOAD|OP_STORE		; $0e ASL abs
.byte $00			; ---

; $1-
.byte OP_PC			; $10 BPL rel
.byte OP_REG_A|OP_LOAD		; $11 ORA ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $15 ORA zpg,x
.byte OP_LOAD|OP_STORE 		; $16 ASL zpg,x
.byte $00			; ---
.byte OP_FLAG			; $18 CLC
.byte OP_REG_A|OP_LOAD		; $19 ORA abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $1d ORA abs,x
.byte OP_LOAD|OP_STORE		; $1e ASL abs,x
.byte $00			; ---

; $2-
.byte OP_PC|OP_STACK|OP_STORE	; $20 JSR abs
.byte OP_REG_A|OP_LOAD		; $21 AND x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD		; $24 BIT zpg
.byte OP_REG_A|OP_LOAD		; $25 AND zpg
.byte OP_LOAD|OP_STORE 		; $26 ROL zpg
.byte $00			; ---
.byte OP_FLAG|OP_STACK|OP_LOAD	; $28 PLP
.byte OP_REG_A     		; $29 AND #
.byte OP_REG_A			; $2a ROL A
.byte $00			; ---
.byte OP_FLAG|OP_LOAD		; $2c BIT abs
.byte OP_LOAD|OP_REG_A		; $2d AND abs
.byte OP_LOAD|OP_STORE	        ; $2e ROL abs
.byte $00			; ---

; $3-
.byte OP_PC			; $30 BMI rel
.byte OP_REG_A|OP_LOAD		; $31 AND ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $35 AND zpg,x
.byte OP_LOAD|OP_STORE 		; $36 ROL zpg,x
.byte $00			; ---
.byte OP_FLAG			; $38 SEC
.byte OP_REG_A|OP_LOAD    	; $39 AND abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $3d AND abs,x
.byte OP_LOAD|OP_STORE	        ; $3e ROL abs,x
.byte $00			; ---

; $4-
.byte OP_STACK|OP_PC|OP_LOAD|OP_FLAG	; $40 RTI
.byte OP_REG_A|OP_LOAD			; $42 EOR x, ind
.byte $00				; ---
.byte $00				; ---
.byte $00				; ---
.byte OP_REG_A|OP_LOAD			; $45 EOR zpg
.byte OP_LOAD|OP_STORE 			; $46 LSR zpg
.byte $00				; ---
.byte OP_STACK|OP_STORE			; $48 PHA
.byte OP_REG_A|OP_LOAD    		; $49 EOR #
.byte OP_REG_A                  	; $4a LSR A
.byte $00				; ---
.byte OP_PC				; $4c JMP abs
.byte OP_LOAD|OP_REG_A			; $4d EOR abs
.byte OP_LOAD|OP_STORE			; $4e LSR abs
.byte $00				; ---

; $5-
.byte OP_PC			; $50 BVC rel
.byte OP_REG_A|OP_LOAD		; $51 EOR ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $55 EOR zpg,x
.byte OP_LOAD|OP_STORE 		; $56 LSR zpg,x
.byte $00			; ---
.byte OP_FLAG			; $58 CLI
.byte OP_REG_A|OP_LOAD    	; $59 EOR abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $5d EOR abs,x
.byte OP_LOAD|OP_STORE	        ; $5e LSR abs,x
.byte $00			; ---

; $6-
.byte OP_STACK|OP_PC|OP_LOAD	; $60 RTS
.byte OP_REG_A|OP_LOAD		; $61 ADC x,ind
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $65 ADC zpg
.byte OP_LOAD|OP_STORE 		; $66 ROR zpg
.byte $00			; ---
.byte OP_STACK|OP_LOAD|OP_REG_A	; $68 PLA
.byte OP_REG_A|OP_LOAD    	; $69 ADC #
.byte OP_REG_A                  ; $6a ROR A
.byte $00			; ---
.byte OP_PC			; $4c JMP ind
.byte OP_LOAD|OP_REG_A		; $4d ADC abs
.byte OP_LOAD|OP_STORE	        ; $4e ROR abs
.byte $00			; ---

; $7-
.byte OP_PC			; $70 BVS rel
.byte OP_REG_A|OP_LOAD		; $71 ADC ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_REG_A|OP_LOAD		; $55 ADC zpg,x
.byte OP_LOAD|OP_STORE 		; $56 ROR zpg,x
.byte $00			; ---
.byte OP_FLAG			; $58 SEI
.byte OP_REG_A|OP_LOAD    	; $59 ADC abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_A		; $5d ADC abs,x
.byte OP_LOAD|OP_STORE  	; $5e ROR abs,x
.byte $00			; ---

; $8-
.byte $00			; ---
.byte OP_STORE			; $81 STA x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $84 STY zpg
.byte OP_STORE   		; $85 STA zpg
.byte OP_STORE 		        ; $86 STX zpg
.byte $00			; ---
.byte OP_REG_Y              	; $98 DEY
.byte $00			; ---
.byte OP_REG_A                  ; $8a TXA
.byte $00			; ---
.byte OP_STORE			; $8c STY abs
.byte OP_STORE			; $8d STA abs
.byte OP_STORE			; $8e STX abs
.byte $00			; ---

; $9-
.byte OP_PC			; $90 BCC rel
.byte OP_STORE			; $91 STA ind,y
.byte $00			; ---
.byte $00			; ---
.byte OP_STORE			; $94 STY zpg,x
.byte OP_STORE   		; $95 STA zpg,x
.byte OP_STORE 		        ; $96 STX zpg,y
.byte $00			; ---
.byte OP_REG_A              	; $98 TYA
.byte $00			; ---
.byte OP_STORE                	; $99 STA abs,Y
.byte OP_STACK			; $9a TXS
.byte $00			; ---
.byte OP_STORE			; $9d STA abs,x
.byte OP_STORE			; ---
.byte $00			; ---

; $a-
.byte OP_REG_Y			; $a0 LDY #
.byte OP_LOAD|OP_REG_A		; $a1 LDA x,ind
.byte OP_REG_X			; $a2 LDX #
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $a4 LDY zpg
.byte OP_LOAD|OP_REG_A 		; $a5 LDA zpg
.byte OP_LOAD|OP_REG_X	        ; $a6 LDX zpg
.byte $00			; ---
.byte OP_REG_Y              	; $a8 TAY
.byte OP_REG_A			; $a9 LDA #
.byte OP_REG_X                  ; $aa TAX
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $ac LDY abs
.byte OP_LOAD|OP_REG_A		; $ad LDA abs
.byte OP_LOAD|OP_REG_X		; $ae LDX abs
.byte $00			; ---

; $b-
.byte OP_PC                     ; $b0 BCS rel
.byte OP_LOAD|OP_REG_A		; $b1 LDA ind,y
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $b4 LDY zpg,x
.byte OP_LOAD|OP_REG_A 		; $b5 LDA zpg,x
.byte OP_LOAD|OP_REG_X	        ; $b6 LDX zpg,y
.byte $00			; ---
.byte OP_FLAG               	; $b8 CLV
.byte OP_REG_A|OP_LOAD		; $b9 LDA abs,y
.byte OP_REG_X                  ; $ba TSX
.byte $00			; ---
.byte OP_LOAD|OP_REG_Y		; $bc LDY abs,y
.byte OP_LOAD|OP_REG_A		; $ad LDA abs,x
.byte OP_LOAD|OP_REG_X		; $ae LDX abs,y
.byte $00			; ---

; $c-
.byte OP_FLAG                   ; $c0 CPY #
.byte OP_LOAD|OP_FLAG           ; $c1 CMP x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD           ; $c4 CPY zpg
.byte OP_LOAD|OP_FLAG 		; $c5 CMP zpg
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $c6 DEC zpg
.byte $00			; ---
.byte OP_REG_Y|OP_FLAG          ; $c8 INY
.byte OP_FLAG                   ; $c9 CMP #
.byte OP_REG_X|OP_FLAG          ; $ca DEX
.byte $00			; ---
.byte OP_FLAG|OP_LOAD   	; $cc CPY abs
.byte OP_FLAG|OP_LOAD     	; $cd CMP abs
.byte OP_LOAD|OP_STORE		; $ce DEC abs
.byte $00			; ---

; $d-
.byte OP_PC                     ; $b0 BNE rel
.byte OP_LOAD|OP_FLAG 		; $b1 CMP ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG  		; $d5 CMP zpg,x
.byte OP_LOAD|OP_STORE	        ; $d6 DEC zpg,y
.byte $00			; ---
.byte OP_FLAG               	; $d8 CLD
.byte OP_REG_A|OP_STORE|OP_FLAG	; $d9 CMP abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG		; $dd CMP abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $de DEC abs,y
.byte $00			; ---

; $e-
.byte OP_FLAG                   ; $e0 CPX #
.byte OP_LOAD|OP_FLAG|OP_REG_A  ; $e1 SBC x,ind
.byte $00			; ---
.byte $00			; ---
.byte OP_FLAG|OP_LOAD           ; $e4 CPX zpg
.byte OP_LOAD|OP_FLAG|OP_REG_A	; $e5 SBC zpg
.byte OP_LOAD|OP_STORE|OP_FLAG  ; $e6 INC zpg
.byte $00			; ---
.byte OP_REG_X|OP_FLAG          ; $e8 INX
.byte OP_REG_A	                ; $e9 SBC #
.byte $00			; $ea NOP
.byte $00			; ---
.byte OP_FLAG|OP_LOAD   	; $ec CPX abs
.byte OP_FLAG|OP_LOAD|OP_REG_A 	; $ed SBC abs
.byte OP_LOAD|OP_STORE|OP_FLAG	; $ee INC abs
.byte $00			; ---

; $e-
.byte OP_PC                     ; $f0 BEQ rel
.byte OP_LOAD|OP_FLAG 		; $f1 SBC ind,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG|OP_REG_A 	; $f5 SBC zpg,x
.byte OP_LOAD|OP_STORE	        ; $e6 INC zpg,x
.byte $00			; ---
.byte OP_FLAG               	; $f8 SED
.byte OP_REG_A|OP_FLAG|OP_LOAD  ; $f9 SBC abs,y
.byte $00			; ---
.byte $00			; ---
.byte $00			; ---
.byte OP_LOAD|OP_FLAG|OP_REG_A	; $fd SBC abs,x
.byte OP_LOAD|OP_STORE|OP_FLAG	; $fe INC abs,x
.byte $00			; ---

;******************************************************************************
; corresponding masks for top 2 bits of opcode to flags in the status register
branch_masks:
.byte $80	; negative
.byte $40	; overflow
.byte $01	; carry
.byte $02	; zero

;******************************************************************************
; Instruction timing table
; Each byte contains the timing for two instructions beginning at opcodes $00
; and $01 and ending at opcodes $fe and $ff.
; The MSB represents the even opcode (0, 2, etc.) and the LSB the odd one.
; For example, BRK and ORA x,ind (opcodes 0 and 1) are stored as:
;   (BRK cycles << 4) | (ORA x, ind cycles)
timings:
.byte $70|$06	; BRK       | ORA x,ind
.byte $00
.byte $00|$03	; --        | ORA zpg
.byte $50|$00	; ASL zpg   | --
.byte $30|$02	; PHP       | ORA #
.byte $20|$00	; ASL       | --
.byte $00|$04	; --        | ORA abs
.byte $60|$00	; ASL abs   | --

; $10
.byte $20|$05   ; BPL       | ORA ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | ORA zpg,x
.byte $60|$00   ; ASL zpg,x | --
.byte $20|$04   ; CLC       | ORA abs,y
.byte $00	; --        | --
.byte $20|$04   ; CLC       | ORA abs,x
.byte $70|$00   ; ASL abs,x | --

; $20
.byte $60|$06	; JSR       | AND x,ind
.byte $00	; --        | --
.byte $30|$03	; BIT zpg   | AND zpg
.byte $50|$00	; ROL zpg   | --
.byte $40|$02	; PLP       | AND #
.byte $20|$00	; ROL       | --
.byte $40|$04	; BIT abs   | AND abs
.byte $60|$00	; ROL abs   | --

; $30
.byte $20|$05	; BMI       | AND ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | AND zpg,x
.byte $60|$00   ; ROL zpg,x | --
.byte $20|$04	; SEC       | AND abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | AND abs,x
.byte $70|$00   ; ROL abs,x | --

; $40
.byte $60|$06	; RTI       | EOR x,ind
.byte $00	; --        | --
.byte $00|$03	; --        | EOR zpg
.byte $50|$00	; LSR zpg   | --
.byte $30|$02	; PHA       | EOR #
.byte $20|$00	; LSR       | --
.byte $30|$04	; JMP abs   | EOR abs
.byte $60|$00	; LSR abs   |

; $50
.byte $20|$05	; BVC       | EOR ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | EOR zpg,x
.byte $60|$00	; LSR zpg,x | --
.byte $20|$04	; CLI       | EOR abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | EOR abs,x
.byte $70|$00	; LSR abs,x | --

; $60
.byte $60|$06   ; RTS       | ADC x,ind
.byte $00	; --        | --
.byte $00|$03	; --        | ADC zpg
.byte $50|$00	; ROR zpg   | --
.byte $40|$02	; PLA       | ADC #
.byte $20|$00	; ROR       | --
.byte $50|$04	; JMP ind   | ADC abs
.byte $60|$00	; ROR abs   | --

; $70
.byte $20|$05	; BVS       | ADC ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | ADC zpg,x
.byte $60|$00	; ROR zpg,x | --
.byte $20|$04	; SEI       | ADC abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | ADC abs,x
.byte $70|$00	; ROR abs,x | --

; $80
.byte $00|$06	; --        | STA x,ind
.byte $00	; --        | --
.byte $30|$03	; STY zpg   | STA zpg
.byte $30|$00	; STX zpg   | --
.byte $20|$00	; DEY       | --
.byte $20|$00	; TXA       | --
.byte $40|$04	; STY abs   | STA abs
.byte $40|$00	; STX abs   | --

; $90
.byte $20|$06	; BCC       | STA ind,y
.byte $00	; --        | --
.byte $40|$04	; STY zpg,x | STA zpg,x
.byte $40|$00	; STX zpg,y | --
.byte $20|$05	; TYA       | STA abs,y
.byte $20|$00	; TXS       | --
.byte $00|$05	; --        | STA abs,x
.byte $00	; --        | --

; $A0
.byte $20|$06	; LDY #     | LDA X,ind
.byte $20|$00	; LDX #     | --
.byte $30|$03	; LDY zpg   | LDA zpg
.byte $30|$00	; LDX zpg   | --
.byte $20|$02	; TAY       | LDA #
.byte $20|$00	; TAX       | --
.byte $40|$04	; LDY abs   | LDA abs
.byte $40|$00	; LDX abs

; $B0
.byte $20|$05	; BCS       | LDA ind,y
.byte $00	; --        | --
.byte $40|$04   ; LDY zpg,x | LDA zpg,x
.byte $40|$00	; LDX zpg,y | --
.byte $20|$04	; CLV       | LDA abs,y
.byte $20|$00	; TSX       | --
.byte $40|$04	; LDY abs,x | LDA abs,x
.byte $40|$00	; LDX abs,y | --

; $C0
.byte $20|$06	; CPY #     | CMP x,ind
.byte $00	; --        | --
.byte $30|$03	; CPY zpg   | CMP zpg
.byte $50|$00	; DEC zpg   | --
.byte $20|$02	; INY       | CMP #
.byte $20|$00	; DEX       | --
.byte $40|$04	; CPY abs   | CMP abs
.byte $60|$00	; DEC abs   | --

; $D0
.byte $20|$05	; BNE       | CMP ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | CMP zpg,x
.byte $60|$00	; DEC zpg,x | --
.byte $20|$04	; CLD       | CMP abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | CMP abs,x
.byte $70|$00	; DEC abs,x | --

; $E0
.byte $20|$06	; CPX #     | SBC x,ind
.byte $00	; --        | --
.byte $30|$03	; CPX zpg   | SBC zpg
.byte $50|$00	; INC zpg   | --
.byte $20|$02	; INX       | SBC #
.byte $20|$00	; NOP       | --
.byte $40|$04	; CPX abs   | SBC abs
.byte $60|$00	; INC abs

; $F0
.byte $20|$05	; BEQ       | SBC ind,y
.byte $00	; --        | --
.byte $00|$04	; --        | SBC zpg,x
.byte $60|$00	; INC zpg,x | --
.byte $20|$04	; SED       | SBC abs,y
.byte $00	; --        | --
.byte $00|$04	; --        | SBC abs,x
.byte $70|$00	; INC abs,x | --
