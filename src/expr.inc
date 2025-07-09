.import __expr_end_on_whitespace
.import __expr_eval
.import __expr_getval
.import __expr_requires_reloc

.importzp __expr_global_id
.importzp __expr_global_op
.importzp __expr_global_postproc
.importzp __expr_contains_global

.scope expr
	contains_global = __expr_contains_global
	end_on_ws       = __expr_end_on_whitespace
	eval            = __expr_eval
	getval          = __expr_getval
	global_id       = __expr_global_id
	global_op       = __expr_global_op
	global_postproc = __expr_global_postproc
	requires_reloc  = __expr_requires_reloc
.endscope
