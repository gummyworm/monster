.import __expr_crosses_section
.import __expr_end_on_whitespace
.import __expr_eval
.import __expr_eval_list
.import __expr_requires_reloc
.import __expr_rpnlist
.import __expr_rpnlistlen

.scope expr
	crosses_section = __expr_crosses_section
	end_on_ws       = __expr_end_on_whitespace
	eval            = __expr_eval
	eval_list       = __expr_eval_list
	requires_reloc  = __expr_requires_reloc
	rpnlist         = __expr_rpnlist
	rpnlistlen      = __expr_rpnlistlen
.endscope
