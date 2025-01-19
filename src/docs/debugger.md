## Debugger

---

## Requirements
In order for the debugger to coexist with your program there are a few small
requirements.

#### Don't Use $7FEF-$8000
The address range from $7fef to $8000 is used to store the interrupts that
return control to the debugger.
If this range is clobbered, a BRK or NMI will not return to the debugger and the
machine will likely JAM.

#### Stack Has 6 Bytes Free
The stack, at its current location for a given step, must have 3 bytes free.
If your program uses an IRQ (correctly) this shouldn't be an issue because the Vic-20's
interrupt sequence pushes 6 bytes (the registers, including status, plus the
interrupt return address).

### Don't overwrite BRK/NMI vectors
The BRK vector is used to return to the debugger during normal execution of your program.

The NMI vector _can_ be used by your program, but it is clobbered by the debugger
when stepping through ROM.  VIA #1's timer is used to trigger the NMI
that returns to the debugger when doing this.

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
