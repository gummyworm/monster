.include "config.inc"

.BSS

;******************************************************************************
__mem_keybuff=$277

.export __mem_spare
.export __mem_spareend
__mem_spare=$0500
__mem_spareend = $1000

.export __mem_backbuff
__mem_backbuff = $c000-$f00 	; backup for the screen bitmap

.export __mem_backbuff
.export __mem_progsave
.export __mem_prog00
.export __mem_prog1000
.export __mem_prog9000
.export __mem_prog9400
.export __mem_progsave
.export __mem_debugsave
.export __mem_dbg00
.export __mem_dbg9000
.export __mem_dbg9400

__mem_progsave = __mem_backbuff 	; backup for the user's program during debug
__mem_prog9000 = __mem_progsave		; $9000-$9010
__mem_prog00   = __mem_progsave+$10	; $00-$0400
__mem_prog1000 = __mem_progsave+$410	; $1000-1100
__mem_prog9400 = __mem_progsave+$510	; $9400-$94f0

; we back up less for debug because we can just re-init some state
__mem_debugsave = __mem_progsave+$600
__mem_dbg00     = __mem_debugsave	; $00-$400
__mem_dbg9000   = __mem_debugsave+$400	; $9000-$9010
__mem_dbg9400   = __mem_debugsave+$410	; $9400-$94f0

.export __mem_ctxbuffer
__mem_ctxbuffer = $140+40	; the buffer for the context during assembly

.export __statusline
__statusline = __mem_spare+80

.export __statusinfo
__statusinfo: .res 20

.export __mem_drive_err
__mem_drive_err: .res 20

.export __linesave
__linesave: .res 40

.export __mem_copybuff
__mem_copybuff: .res MAX_COPY_SIZE

.export __mem_coloron
__mem_coloron: .byte 0

.export __mem_rowcolors
__mem_rowcolors: .res 24

.export __mem_rowcolors_save
__mem_rowcolors_save: .res 24

;******************************************************************************
; LINEBUFFER
; The linebuffer must live in lower RAM, which is NOT switched out with the
; upper RAM upon switching banks
; This allows the buffer to be manipulated from any bank
.export __linebuffer
__linebuffer = $0400

.export __linebuffer2
;__linebuffer2: .res 80		; backup buffer for when the linebuffer must be saved
__linebuffer2 = $0400+$80

.export __mem_asmbuffer
__mem_asmbuffer  = __linebuffer2
