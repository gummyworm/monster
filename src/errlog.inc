.import __errlog_numerrs
.import __errlog_activate
.import __errlog_clear
.import __errlog_log

.scope errlog
	activate = __errlog_activate
	clear    = __errlog_clear
	log      = __errlog_log
	numerrs  = __errlog_numerrs
.endscope
