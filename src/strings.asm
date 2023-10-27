.include "text.inc"

;******************************************************************************
.RODATA

.export __str_noname
__str_noname: .byte "[no name]",0

.export __str_null
__str_null = *-1

.export __str_endrep
__str_endrep: .byte ".endrep"

.export __str_endmac
__str_endmac: .byte ".endmac"

.export __str_breakpoints_title
__str_breakpoints_title: .byte ESCAPE_SPACING,14, "breakpoints",0

.export __str_question_marks
__str_question_marks: .byte "???",0

.export __str_debug_brk_line
__str_debug_brk_line: .byte "brk in line ",ESCAPE_VALUE_DEC,0

.export __str_debug_brk_addr
__str_debug_brk_addr: .byte "brk @ ", ESCAPE_VALUE,0

.export __str_debug_registers
__str_debug_registers: .byte " pc  a  x  y  sp nv-bdizc  addr      clk",0

.export __str_debug_stop_debugging
__str_debug_stop_debugging: .byte "stop debugging? (press 'y' to quit)",0

.export __str_edit_line_err
__str_edit_line_err: .byte ";pass ", ESCAPE_VALUE_DEC,";line ", ESCAPE_VALUE_DEC,0

.export __str_edit_file_load_failed
__str_edit_file_load_failed: .byte "failed to load file; error $",ESCAPE_BYTE,0

.export __str_edit_file_delete_failed
__str_edit_file_delete_failed: .byte "failed to delete file; error ", ESCAPE_BYTE, 0

.export __str_deleting
__str_deleting: .byte "deleting...",0

.export __str_loading
__str_loading: .byte "loading...",0

.export __str_saving
__str_saving: .byte "saving...",0

.export __str_edit_file_save_failed
__str_edit_file_save_failed: .byte "failed to save file; error ", ESCAPE_BYTE, 0

.export __str_watches_title
__str_watches_title: .byte ESCAPE_SPACING,16, "watches",0

.export __str_dir
__str_dir: .byte "$"

.export __str_memory
__str_memory: .byte "memory",0

.export __str_segments
__str_segments: .byte "segments",0

.export __str_load
__str_load: .byte "load",0

.export __str_run
__str_run: .byte "run",0

.export __str_watches_range_line
__str_watches_range_line: .byte "$", ESCAPE_VALUE, "-$", ESCAPE_VALUE

;******************************************************************************
; These strings are modified thus are not in RODATA
.DATA
; <filename> l: <line no.> <symbol> : <addr>
.export __str_breakpoints_line
__str_breakpoints_line: .byte " ", ESCAPE_STRING, " l:", ESCAPE_VALUE_DEC, " [", ESCAPE_STRING, "] $", ESCAPE_VALUE,0

; <"address>: <val>
.export __str_watches_line
__str_watches_line:
.byte "$", ESCAPE_VALUE, ": ", ESCAPE_BYTE, 0
.export __str_watches_line_end
__str_watches_line_end=*-1


.export __str_memview_title
__str_memview_title: .byte "          memory[$1000]",0
