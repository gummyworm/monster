## DEBUGGER OVERVIEW

---

https://github.com/gummyworm/monster/assets/4626914/840f5d66-03cb-4daf-9ed2-41a4d37d4c2d

The debugger allows you to step through code, set breakpoints, and watch
data as you execute your program.

Upon entering the debugger, a view of the system state is displayed at the
current step or breakpoint.

This include the state of the registers (A, X, Y, P, SP, and PC) as well as
any effective address that was calculated for reading/writing by the last
instruction.  Note that if the last instruction executed did not read or write to memory,
the effective address field is set to $ffff.

While debugging, most navigation commands work as normal. Breakpoints may
be set as they would in the editor prior to assembly, and they will be installed
in realtime.  Other edits are not allowed, however, while the debugger is active.

Both the debugger and the user program's RAM is saved/restored when control
transfers between the two. That is the screen data ($1000-$2000), the zeropage,
and color RAM.

---

## REQUIREMENTS
In order for the debugger to coexist with your program there are a few small
requirements.

#### DON'T USE $7FEF-$8000

The address range from $7fef to $8000 is used to store the interrupts that
return control to the debugger.
If this range is clobbered, a BRK or NMI will not return to the debugger and the
machine will likely JAM.

#### STACK HAS 6 BYTES FREE

The stack, at its current location for a given step, must have 3 bytes free.
If your program uses an IRQ (correctly) this shouldn't be an issue because the Vic-20's
interrupt sequence pushes 6 bytes (the registers, including status, plus the
interrupt return address).

### DON'T OVERWRITE BRK/NMI VECTORS

The BRK vector is used to return to the debugger during normal execution of your program.

The NMI vector _can_ be used by your program, but it is clobbered by the debugger
when stepping through ROM.  VIA #1's timer is used to trigger the NMI
that returns to the debugger when doing this.

---

### DEBUG COMMANDS

The following commands are supported by the debugger and are accessed by their
respective Key in the table below.

|  Key         | Name            |   Description                                                                        |
|--------------|-----------------|--------------------------------------------------------------------------------------|
|  F1          | Source View     | maximizes the screen area for viewing the source code                                |
|  F2          | Register Editor | enters the register editor                                                           |
|  F3          | Mem View        | activates the memory window, which takes control until `<-` is pressed               |
|  F5          | Break View      | displays the breakpoints that have been set and allows them to be enabled/disabled   |
|  s           | Step Over      | steps to the next instruction. If it is a JSR, continues AFTER the target subroutine |
|  y           | Step Out        | steps until the next RTS instruction                                                 |
|  z           | Step            | steps to the next instruction.                                                       |
|  t           | Trace           | like GO but the debugger takes control between each instruction                      |
|  C= + g      | Go              | begins execution at the cursor                                                       |
|  C= + j      | Jump to         | sets the PC to the address corresponding to the line the cursor is on                |
|  C= + r      | Reset Stopwatch | resets the value of the stopwatch to 0                                               |
| C= + t       | Enter monitor   | enters the text-based debug interface (see the monitor commands section for more info|
| C= + x       | Quit Debugger   | Prompts the user for confirmation then quits the debugger upon receiving it          |
|   <-         | Exit            | exits the debugger and returns to the editor                                         |
| SPACE        | Swap prog       | swaps in the internal memory for the user program (allows user to see screen state)  |
| ^ (up arrow) |  Goto Break     | navigates to the address that the debugger is currently paused at                    |

### REGISTER EDITOR (`F2`)

Pressing F2 moves the cursor to the register contents and allows the user to enter
new values for them.  Pressing `RETURN` will confirm the new register values
and update them to those values the next time the user program is invoked.
Pressing `<-` will abort this process and leave the old register values
intact.

### STOPWATCH

Next to the registers, under the CLK label, is a 24-bit counter that displays the
number of cycles executed by the instructions that have been STEP'd into.
The stopwatch can be reset to 0 with the `C= + r` key combination.

Note that the number of cylces is displayed in decimal unlike the rest of the
information in the debug view, which is displayed in hexadecimal.

---

### STEPPING THROUGH CODE

#### STEP INTO (`z`)

Stepping is a common way to debug a program line-by-line.
Stepping _into_ code will, if possible, return to the debugger
after the next instruction (the one currently highlighted if we have debug
information) is executed. There is a scenario where this is not possible: if
the next instruction is in ROM.  In this case, step _into_ behaves the same
as step _over_: execution begins after the current instruction.  Note that
because we don't know what will happen in ROM, it is possible execution will
never return to the debugger.

Keeping in mind the aforementioned caveat with ROM, stepping _into_ code gives
us a lot of information about the instructions we are executed.  The debugger
behaves almost as a 6502 simulator in this scenario.  When an instruction that
affects a given register, that register is highlighted *even if the register
value hasn't changed*. The same is true of watches.  We can activate a watch
even if we don't store a new value to it. In fact, we can activate them when a
value is loaded.

#### STEP OVER (`s`)

Step _over_ behaves the same as step _into_, but if the next
instruction is a subroutine call (`JSR`), execution continues until the
instruction _after_ the `JSR` (after the subroutine returns).

This command is considered somewhat dangerous because this command doesn't
trace the execution.  If the subroutine that is stepped over runs a JAM
instruction, the processor will halt.  Because the overhead of tracing is
fairly high, this may be worth the risk. Especiallly if you are calling
routines that are trusted such as those in the KERNAL or BASIC ROM.

#### STEP OUT (`y`)

Step _out_ is different from the other STEP commands in that it runs by tracing.
The step out command traces the program until the next RTS instruction.
The RTS instruction is run and control then returns to the debug session.

#### GO (`C= + g`)

The go command begins execution and returns to the debugger only when a
breakpoint is encountered or when RUN/STOP is pressed.

#### TRACE (`t`)

Trace is similar to `GO`, but the debugger executes the program as a series
of STEPs instead of running the program binary directly.
This is useful because it allows the debugger to break if any watched memory
location is accessed or if a JAM would occur.


#### NOTES ON MEMORY SWAPPING

While the debugger and user program have isolated memory banks in the address space
above `$1fff`, the RAM _below_ this, `[$00, $2000)`, is internal to the Vic-20
and cannot be swapped out between debug steps. The debugger has no choice but to
share this address space with the user program as it is also the only RAM
that is visible to the video chip (the VIC-I).  It also contains the stack and
zeropage, which we _could_ avoid, but as long as we need to use the $10th-$20th pages
of RAM, it makes sense to handle them in the same way.

To handle this, the debugger calculates the bytes that need to be saved in
between steps and saves these values in between calls to the program.  Values
that will be used by the user program are then swapped in so that the program
behaves as if the debugger is not running.
The full internal state of the user program and debugger occupy buffers in the
debugger and are available to be swapped in/out on command with the `C= + SPACE`
key combination.  This is useful if you'd like to see what the internal RAM
state, which is the _only_ place that the screen state may live, looks like
at the current step in the program.

If we aren't stepping _into_ code in RAM (_go_, _step over_) we are unable
to calculate the addresses that will be affected when we
hand over control to the user program, we instead save the _entire_ *debugger* state of
the internal RAM and restore the _entire_ *user* state.
Although this is a rather large amount of memory, it is mitigated by being
handled by a mostly unrolled loop and therefore takes only a fraction of a second to occur.
Nonetheless, it is apparent when this is happening if you've changed the setup
of the VIC registers, or anything in the VIC's visible address range, as the
screen will briefly flash with the state of the user program.

---

## AUXILIARY VIEWS

Within the debugger, there are 3 auxiliary views that may be activated with the
function keys.  Each shows information about the machine or debug state.
Each viewer also contains an editor, which is activated with the keys enumerated
below next to their corresponding editor.

Pressing the `<-` key will return the user from the auxiliary editor to the
source code editor.  And `F1` will hide the active view to maximize the
source editor's screen size.

### MEMORY VIEWER (`F3` WHILE DEBUGGING)

The memory viewer displays the contents of RAM at a given address.  The memory
viewer is updated upon reentry to the debugger (if active).
Memory values may be updated by navigating to the value the user wishes to
change and overwriting it with a new hex value. The change occurs immediately.

In addition to hexadecimal keys to edit memory values, the following commands
are supported within the memory viewer:

| Shortcut     | Name      |  Description                                            |
|--------------|-----------|---------------------------------------------------------|
| C= + w       | Add watch | Add watch to the highlighted address                    |
|    /         | Find Value| Seeks from current memory address for given value       |
|   <-         |  Exit     | Returns to the debugger                                 |
| ^ (up-arrow) | Set Addr  | Sets the viewer's address to the given value            |

#### SET WATCH (`C= + W`)

Watches may be placed while navigating in the memory editor.  This is done
by pressing the `C= + w` key-combination while the cursor is on the desired
byte to watch. See the _Watch Viewer_ section for more information on
watches.

#### FIND VALUE (`/`)

Prompts the user for an 8 or 16 bit value (determined by the number of
characters provided) and looks for that value in memory.
If it is found, the memory view is updated to begin at the first address
that was found containing the specified value.

Note that when seeking for a 16 bit value, the value is searched in little-endian
format.  If the input for the search is given as `$1234` the result will be
the first occurrence of the byte value `$34` followed by `$12`.

#### SET ADDRESS (`^`/`UP-ARROW`)

Moves the cursor to the address of the viewer, then prompts the user for a new
value to set the memory viewer to.  Pressing `RETURN` confirms the new address
and `<-` cancels and returns the user to the editor without changing the address

### BREAKPOINT VIEWER (`F5` WHILE DEBUGGING)

The breakpoint viewer displays all the breakpoints that have been set by the
user.  A circle is displayed next to those that are currently active.
The user simply navigates the list with the cursor keys and presses RETURN to
toggle those which he/she wishes to enable/disable.

Note that breakpoints correspond to the debug information generated with
the F4 command.  If the line numbers change after this information is generated,
breakpoints are unlikely to behave in expected ways.

### WATCH VIEWER (`F7` WHILE DEBUGGING)

The watch viewer displays all watches that have been set in the memory
viewer.  The current value of a watch is shown along with its previous
value (if it has changed since the debugger last took over).

A watched address (or range) will also be prefixed with a '!' if it was modified
during the trace or step.  This is especially important for knowing that a range
was modified as ranges do not list the previous or current values for the watch.

The following keys are supported within the watch viewer:

| Shortcut     | Name       |  Description                                            |
|--------------|------------|---------------------------------------------------------|
| C= + w       | Add watch  | Prompt the user for expressions to watch                |
|  RETURN      | Select/Edit| Enters the memory editor at the watch's address         |
|   <-         |  Exit      | Returns to the debugger                                 |

#### ADD WATCH (`C= + W`)

While in the watch editor, the `C= + w` key combination prompts the user for an
address or address range to watch.  These are given as expressions, so you may
provide, for example `myval+3` to set a watch at the address of the label myval plus 3.
To set a watch for an address range, simply provide two expressions, separated by a comma,
at the prompt.  If the expression(s) are invalid, no watch is added.

#### EDIT WATCH (`RETURN`)

Pressing RETURN will invoke the _memory editor_ at the location of the watch
that was selected.  Returning from the memory editor will return the user
back to the watch editor.

---

## BREAKPOINTS

Breakpoints may be set/removed during both normal editing and while debugging.
Setting a breakpoint inserts a special character into the source buffer, which
tells the assembler to generate a breakpoint for the line that this character
resides on.

Because the breakpoint is represented as a character within the source code itself,
it will automatically move as lines are inserted and deleted.  The character itself
is not editable (the cursor will not move to breakpoint characters).  You may remove
it by toggling the breakpoint off _or_ by deleting the entire line.

*NOTE:* Debug information is only generated for instructions _NOT data_.  This means
that, for example, you can set a breakpoint on `LDA #$00` or a macro that expands
to such an instruction, but setting one on `.DB $00` has no effect.

### TOGGLE BREAKPOINT (`C= + B`)
During normal editing, breakpoints may be set and removed  with the `C= + b` key combination.

Pressing the same key combination (`C= + b`) will also remove a breakpoint
if it is pressed while on a line that already has one.

Breakpoints can only be added to buffers that have been named.

---

## WATCHES
Watches are set within the memory editor (`F3`). When the cursor is over the
desired byte to watch, the press `C= + w` to add a watch to the address of the
byte under the cursor.  A beep will confirm that the watch
was added.

The watch editor (`F7`) shows all active watches. This window displays the old
value of a watch and what it was changed to when it is updated.

When a value is changed the watch view is activated to alert the user to the
alteration.  If a read or write is detected while stepping _into_ the code,
the viewer is also activated.


---

## STEP MECHANISMS

The manner in which the program is "stepped" depends on a variety of situations.
There are 3 means of stepping:
 - BRK
 - NMI (timer)
 - NMI (RESTORE)

For a given step, only one of these is used, so we don't need to worry about, for
example, the user pressing RESTORE during a BRK.

#### BRK

When possible, a BRK instruction is inserted at the address after the next
instruction to-be-executed.
This address is calculated by Monster using the current state of the processor.

#### NMI (TIMER)

When a BRK point _cannot_ be inserted and when the instruction to-be-executed is
in ROM, VIA #1 is used to generate an NMI immediately after the next instruction
executes.  Timer 2 is loaded with a value that will count down to 1 at the time
the next instruction begins.

The 6502 waits for the current instruction to complete
before handling interrupts, so, although the timer expires before the next
instruction completes, it still finishes before the interrupt handler takes over.

A handful of ROM routines, specifically those dealing with RS-232 transfer, use
VIA 1's timer 2 themselves.  These cannot be stepped through for this reason.
However, these routine are timing sensitive, so stepping through them would be
mostly useless.

#### NMI (RESTORE KEY)

When the user runs a program with the _GO_ or _TRACE_ commands,
they need a means of returning to the debugger.
In these cases, the debugger enables NMI interrupts on VIA 1's control line 1 (CA1), which
is connected to the RESTORE key.

When the user presses the RESTORE key, control returns to the debugger.

Note that the _TRACE_ command itself utilizes the other "step" methods, BRK and NMI timers,
to execute individual instructions.  In this case, RESTORE simply tells the debugger
to stop automatically stepping into the next instruction.
