.import __expr_end_on_whitespace
.import __expr_eval
.import __expr_eval_list
.import __expr_kind
.import __expr_postproc
.import __expr_rpnlist
.import __expr_rpnlistlen
.import __expr_section
.import __expr_symbol

VAL_ABS = 0
VAL_REL = 1

.scope expr
	end_on_ws  = __expr_end_on_whitespace
	eval       = __expr_eval
	eval_list  = __expr_eval_list
	kind       = __expr_kind
	rpnlist    = __expr_rpnlist
	rpnlistlen = __expr_rpnlistlen
	section    = __expr_section
	symbol     = __expr_symbol
	postproc   = __expr_postproc
.endscope
