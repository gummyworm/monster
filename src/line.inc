.import __line_inc
.import __line_nextch
.import __line_process_ws
.import __line_process_word

.scope line
	incptr       = __line_inc
	nextch       = __line_nextch
	process_ws   = __line_process_ws
	process_word = __line_process_word
.endscope
