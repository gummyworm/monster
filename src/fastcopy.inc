.import __fastcopy_init
.import __fastcopy_save
.import __fastcopy_restore

.scope fcpy
	init    = __fastcopy_init
	save    = __fastcopy_save
	restore = __fastcopy_restore
.endscope
