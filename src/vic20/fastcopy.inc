.import __fastcopy_save_prog_state
.import __fastcopy_save_debug_state
.import __fastcopy_restore_debug_state
.import __fastcopy_restore_prog_state
.import __fastcopy_save_user_zp
.import __fastcopy_save_debug_zp
.import __fastcopy_restore_debug_low
.import __fastcopy_restore_debug_zp
.import __fastcopy_restore_user_zp

.scope fcpy
	restore_debug_state = __fastcopy_restore_debug_state
	restore_debug_low   = __fastcopy_restore_debug_low
	restore_debug_zp    = __fastcopy_restore_debug_zp
	restore_progstate   = __fastcopy_restore_prog_state
	restore_user_zp     = __fastcopy_restore_user_zp
	save_debug_state    = __fastcopy_save_debug_state
	save_prog_state     = __fastcopy_save_prog_state
	save_debug_zp       = __fastcopy_save_debug_zp
	save_user_zp        = __fastcopy_save_user_zp
.endscope
