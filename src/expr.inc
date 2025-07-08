.import __expr_end_on_whitespace
.import __expr_eval
.import __expr_getval
.import __expr_global_id
.import __expr_global_op
.import __expr_contains_global
.import __expr_requires_reloc

.scope expr
	contains_global = __expr_contains_global
	end_on_ws       = __expr_end_on_whitespace
	eval            = __expr_eval
	getval          = __expr_getval
	global_id       = __expr_global_id
	global_op       = __expr_global_op
	requires_reloc  = __expr_requires_reloc
.endscope
