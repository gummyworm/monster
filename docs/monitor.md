## MONITOR OVERVIEW

The monitor is a text based interface for debugging programs and manipulating
program state.  It offers the same functionality as the GUI debugger plus a plethora of other commands to
manipulate the program state.

---

### ACTIVATION

The monitor is activated with the `C= + M` key combination. It can be activated from the editor both during
and not during a debug session.
When activated while debugging, a number of additional commands related to the state of the debugged program
become available.

### FILE REDIRECTION
The output from a given monitor command can be redirected to file instead of the screen by using the
redirect (`>`) operator.  When placed at the end of a command, the redirect operator writes all output from
that command to the following file.

For example:

`r > regs.txt`

Will write the contents of the registers to the file `regs.txt`

### COMMANDS

The below table enumerates the available monitor commands and their parameters (plus optional ones).
Most parameters may be expressions (e.g. `label+10`).

| Command  |  Name                   |  Required Parameters               | Optional Parameters  |                      Description                                                                                              |
|----------|-------------------------|------------------------------------|----------------------|-------------------------------------------------------------------------------------------------------------------------------|
|    a     | assemble                |  instruction                       |                      | assembles the given instruction at the address of the provided expression                                                     |
|    b     | list breakpoints        |                                    |                      | lists all the breakpoints that have been added                                                                                |
|    ba    | add breakpoint @ addr   |  address                           |                      | adds a breakpoint at the given address                                                                                        |
|    bl    | add breakpoint @ line   |  file id, line number              |                      | adds a breakpoint at the given line number within the given file                                                              |
|    br    | remove breakpoint       | breakpoint id                      |                      | removes the given breakpoint                                                                                                  |
|    bt    | backtrace               |                                    | offset               | displays a rendered stack view beginning at the current SP + an optioinal offset, up to $200 (bottom of stack)                |
|    c     | compare                 |  address 1, address 2, number      |                      | compares the given number of bytes at the two given addresses and displays any discrepancies                                  |
|    d     | disassemble             |  start-address                     | end-address          | disassembles from the given start address or, if an end address is given, up to the the given end address                     |
|  dump    | dump memory             |  start-address                     | end-address          | prints a list of bytes (in valid assembleable .DB directives) for the given address range                                     |
|    f     | fill memory             |  start-address, stop-address, value| value list           | fills the given address range with the given list of values, repeating when the list is exhausted                             |
|    g     | go                      |                                    |                      | continues execution of the debugged program without tracing                                                                   |
|    h     | hunt                    |  start-address value               | value list           | hunts for the given list of values beginning at the provided address up to $ffff                                              |
|    m     | show memory             |  start-address                     | end-address          | displays the contents of memory from the given start address or, if given, up to the given end address                        |
|    move  | move memory             |  start-address, end-address, dest  |                      | moves the given range [start, end) to the given destination address                                                           |
|    p     | poke memory             |  address value                     |                      | sets the given address to the provided value                                                                                  |
|    r     | registers               |                                    |                      | displays the current content of the registers in the debugged program                                                         |
|    s     | save memory             |start-address, end-address, filename|                      | saves the given memory range to the specified file                                                                            |
|    zo    | step over               |                                    |                      | runs the next instruction, treating JSR's as a single instruction                                                             |
|    t     | trace                   |                                    |                      | continue tracing the program that is being debugged                                                                           |
|    w     | list watches            |                                    |                      | lists the watches that have been added                                                                                        |
|    wa    | add watch               | start-address                      | end-address          | adds a watch at the given start and (optional) stop address                                                                   |
|    wal   | add watch (load)        | start-address                      | end-address          | adds a LOAD watch at the given start and (optional) stop address                                                              |
|    was   | add watch (store)       | start-address                      | end-address          | adds a STORE watch at the given start and (optional) stop address                                                             |
|    wr    | remove watch            | id                                 |                      | removes the watch with the given id                                                                                           |
|    x     | quit                    |                                    |                      | exits the monitor                                                                                                             |
|    z     | step                    |                                    |                      | runs the next instrcution and returns to the the monitor prompt                                                               |
|    zo    | step-out                |                                    |                      | runs the program that is being debugged until the current subroutine is RTS'd from                                            |
|    F1    | view screen             |                                    |                      | toggles the view of the user-memory (swaps the the  $1000-$2000 range monitor <-> program                                     |
