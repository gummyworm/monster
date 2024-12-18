## Debugger

---

## Step Mechanisms
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

#### NMI (timer)
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

#### NMI (RESTORE key)
When the user runs a program with the _GO_ or _TRACE_ commands,
they need a means of returning to the debugger.
In these cases, the debugger enables NMI interrupts on VIA 1's control line 1 (CA1), which
is connected to the RESTORE key.

When the user presses the RESTORE key, control returns to the debugger.

Note that the _TRACE_ command itself utilizes the other "step" methods, BRK and NMI timers,
to execute individual instructions.  In this case, RESTORE simply tells the debugger
to stop automatically stepping into the next instruction.
