;*******************************************************************************
; DAEMON.ASM
; This file contains the code for the daemon process that runs during
; operation of Monster. The daemon waits for changes to the source and
; tries to reassemble the affected compilation unit to update its
; debug information.  If successful, the old debug information is replaced.
;*******************************************************************************

.include "config.inc"

DAEMON_QUEUE_DEPTH = 8

.BSS

;*******************************************************************************
; JOBS
; The stack (LIFO) of the files to reassemble.  If the full, the daemon will
; hog the CPU until it can catch back up.
; A stack is used because the user will likely expect recently edited
; files to be prioritzed.
jobs:     .res DAEMON_QUEUE_DEPTH*MAX_BUFFER_NAME_LEN

num_jobs: .byte 0	; # of items in jobs stack

active_job: .byte 0	; the file being processed (assembled)

.CODE

;*******************************************************************************
; QUEUE
; Enqueues the given file ID, notifying the daemon that it needs to
; be reassembled.
; If the given ID matches the one the daemon is already working on, aborts
; that job and begins the new one.
; IN:
;   - .A: the file to assemble
.export __daemon_queue
.proc __daemon_queue
	ldx num_jobs
	cpx #DAEMON_QUEUE_DEPTH
	bcs @full
@push:	inc num_jobs
	sta jobs,x
	rts

@full:	; hog the CPU until the jobs stack has room again
	; TODO:
.endproc

;*******************************************************************************
; UPDATE
; If there have been changes to a source file, tries to reassemble.
; If reassembly is already in process, continues assembling.
; If reassembly is in process AND a new edit is made, aborts the reassembly
; and restarts again.
; If a file fails to reassemble, the processing will be aborted
.export __daemon_update
.proc __daemon_update
	lda active_job
	bne @continue_job

	; no job in process, check if there are any to process
	ldx num_jobs
	bne @start_job
	rts		; done, nothing to process

@start_job:
	; TODO:

@continue_job:
	; TODO:
.endproc
