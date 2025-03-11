;*******************************************************************************
; STRINGS.ASM
; This file contains string constants used throughout the program.
;*******************************************************************************

.include "text.inc"

.define yes_no "(", $79, "/", $6e, ")"		; lowercase (y/n)

;*******************************************************************************
.RODATA

.export __str_buffers
__str_buffers: .byte "buffers",0

.export __str_noname
__str_noname: .byte "[no name]",0

.export __str_null
__str_null = *-1

.export __str_endrep
__str_endrep: .byte ".endrep"

.export __str_breakpoints_title
__str_breakpoints_title: .byte ESCAPE_SPACING,14, "breakpoints",0

.export __str_question_marks
__str_question_marks: .byte "???",0

.export __str_debug_brk_line
__str_debug_brk_line: .byte "brk in ", ESCAPE_STRING, " line ",ESCAPE_VALUE_DEC,0

.export __str_debug_brk_addr
__str_debug_brk_addr: .byte "brk @ ", ESCAPE_VALUE, "  ", ESCAPE_STRING,0

.export __str_debug_registers
__str_debug_registers: .byte " pc  a  x  y  sp nv-bdizc  addr      clk",0

.export __str_debug_stop_debugging
__str_debug_stop_debugging: .byte "stop debugging? (press 'y' to quit)",0

.export __str_edit_line_err
__str_edit_line_err: .byte ESCAPE_STRING, " l", ESCAPE_VALUE_DEC,":", ESCAPE_STRING,0

.export __str_edit_file_load_failed
__str_edit_file_load_failed: .byte "failed to load file; error $",ESCAPE_BYTE,0

.export __str_edit_file_delete_failed
__str_edit_file_delete_failed: .byte "failed to delete file; error ", ESCAPE_BYTE, 0

.export __str_file_open_failed
__str_file_open_failed: .byte "failed to open file; error ", ESCAPE_BYTE, 0

.export __str_no_file
__str_no_file: .byte "no input file specified", 0

.export __str_deleting
__str_deleting: .byte "deleting...",0

.export __str_loading
__str_loading: .byte "loading...",0

.export __str_saving
__str_saving: .byte "saving...",0

.export __str_assembling
__str_assembling: .byte "assembling...",0

.export __str_edit_file_save_failed
__str_edit_file_save_failed: .byte "failed to save file; error ", ESCAPE_BYTE, 0

.export __str_watches_title
__str_watches_title: .byte ESCAPE_SPACING,16, "watches",0

.export __str_dir
__str_dir: .byte "$",0

.export __str_memory
__str_memory: .byte "memory",0

.export __str_segments
__str_segments: .byte "segments",0

.export __str_load
__str_load: .byte "load",0

.export __str_link
__str_link: .byte "link",0

.export __str_run
__str_run: .byte "run",0

.export __str_watches_range_line
__str_watches_range_line: .byte ESCAPE_BYTE, ESCAPE_CHAR, " $", ESCAPE_VALUE, "-$", ESCAPE_VALUE,0

.export __str_errors
__str_errors: .byte "errors",0

.export __str_invalid_command
__str_invalid_command: .byte "invalid command", 0

.export __str_jam_detected
__str_jam_detected: .byte "jam detected",0

.export __str_illegal_detected
__str_illegal_detected: .byte "illegal detected",0

.export __str_vital_addr_clobber_detected
__str_vital_addr_clobber_detected: .byte "vital address clobber detected",0

.export __str_saveall
__str_saveall: .byte "save all buffers? ", yes_no, 0

.export __str_assemble_prompt
__str_assemble_prompt: .byte "program out of date, reassemble? ", yes_no, 0

.export __str_tracing
__str_tracing: .byte "tracing...",0

.export __str_watch_triggered
__str_watch_triggered:
.byte "watch triggered",0

.export __str_watches_line
;   $1000 : $10
__str_watches_line:
.byte ESCAPE_BYTE, "  $", ESCAPE_VALUE, ": ", ESCAPE_BYTE, 0

; ! $1000 : $10 > $20
.export __str_watches_changed_line
__str_watches_changed_line:
.byte ESCAPE_BYTE, "! $", ESCAPE_VALUE, ": ", ESCAPE_BYTE, CH_R_ARROW, ESCAPE_BYTE, 0

;*******************************************************************************
; These strings are modified thus are not in RODATA
.DATA
; <filename> l: <line no.> <symbol> : <addr>
.export __str_breakpoints_line
__str_breakpoints_line: .byte "  ", ESCAPE_BYTE," ", ESCAPE_STRING, " l:", ESCAPE_VALUE_DEC, " [", ESCAPE_STRING, "] $", ESCAPE_VALUE,0

.export __str_watch_added
__str_watch_added:
.byte "watch added @ ", $fe, 0

.export __str_memview_title
__str_memview_title: .byte "          memory[$1000]",0

.export __str_symview_title
__str_symview_title: .byte "symbols",0
