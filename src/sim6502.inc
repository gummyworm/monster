.import __sim_affected
.import __sim_count_cycles
.import __sim_get_side_effects
.import __sim_next_instruction
.import __sim_next_pc
.import __sim_jammed

.import __sim_op
.import __sim_operand
.import __sim_op_mode
.import __sim_register_state
.import __sim_branch_taken
.import __sim_pc
.import __sim_reg_a
.import __sim_reg_x
.import __sim_reg_y
.import __sim_reg_sp
.import __sim_reg_p
.import __sim_effective_addr
.import __sim_stopwatch

.scope sim
	affected         = __sim_affected
	branch_taken     = __sim_branch_taken
	count_cycles     = __sim_count_cycles
	get_side_effects = __sim_get_side_effects
	jammed           = __sim_jammed
	next_instruction = __sim_next_instruction
	next_pc          = __sim_next_pc
	op               = __sim_op
	operand          = __sim_operand
	op_mode          = __sim_op_mode

	stopwatch      = __sim_stopwatch
	effective_addr = __sim_effective_addr
	register_state = __sim_register_state
	pc             = __sim_pc
	reg_a          = __sim_reg_a
	reg_x          = __sim_reg_x
	reg_y          = __sim_reg_y
	reg_sp         = __sim_reg_sp
	reg_p          = __sim_reg_p
.endscope
