# Monster
<img width="718" alt="Screenshot 2024-09-17 at 7 52 42 PM" src="https://github.com/user-attachments/assets/c8d3dae7-52f2-420b-95de-56edf8c89d2e">


Table of Contents
=================

  * Getting Started
      * [Overview](https://github.com/gummyworm/monster#overview)
      * [Requirements](https://github.com/gummyworm/monster#requirements)
      * [Building](https://github.com/gummyworm/monster#building)
      * [Running](https://github.com/gummyworm/monster#running)
  * [Editor](https://github.com/gummyworm/monster#editor-overview)
      * [Command Shortcuts](https://github.com/gummyworm/monster#command-shortcuts)
      * [Editor Modes](https://github.com/gummyworm/monster#editor-modes)
          * [Command Mode](https://github.com/gummyworm/monster#command-mode)
          * [Insert Mode](https://github.com/gummyworm/monster#insert-mode-i-a-etc)
          * [Visual Mode](https://github.com/gummyworm/monster#visual-mode-v)
          * [Visual Line Mode](https://github.com/gummyworm/monster#visual-line-mode-v)
      * [UDG Editor](https://github.com/gummyworm/monster#udg-editor)
  * [Assembler](https://github.com/gummyworm/monster#assembler-overview)
      * [Syntax](https://github.com/gummyworm/monster#syntax)
      * [Expressions](https://github.com/gummyworm/monster#expressions)
      * [Formatting](https://github.com/gummyworm/monster#formatting)
      * [Labels](https://github.com/gummyworm/monster#labels)
      * [Best Practices](https://github.com/gummyworm/monster#best-practices)
  * [File I/O](https://github.com/gummyworm/monster#files)
      * [Directory Viewer](https://github.com/gummyworm/monster#directory-viewer-c--l)
  * [Debugger](https://github.com/gummyworm/monster#debugger)
      * [Debug Commands](https://github.com/gummyworm/monster#debug-commands)
      * [Stopwatch](https://github.com/gummyworm/monster#stopwatch)
      * [Stepping through code](https://github.com/gummyworm/monster#stepping-through-code)
      * [Auxiliary Views](https://github.com/gummyworm/monster#auxiliary-views)
          * [Memory View](https://github.com/gummyworm/monster#memory-viewer-f3-while-debugging)
          * [Breakpoint View](https://github.com/gummyworm/monster#breakpoint-viewer-f5-while-debugging)
          * [Watch Viewer](https://github.com/gummyworm/monster#watch-viewer-f7-while-debugging)
      * [Breakpoints](https://github.com/gummyworm/monster#breakpoints)
      * [Watches](https://github.com/gummyworm/monster#watches)
  * [Monitor](https://github.com/gummyworm/monster#monitor)

---

## Overview

Monster is an all-in-one editor/assembler/debugger for the Commodore Vic-20.
The design philosophy is uncompromising maximalism.  This is in contrast to most
existing Vic-20 assemblers (and most native development tools on 8-bit computers),
which mostly designed with memory efficiency in mind.  The primary reason for this
was (or is) to leave the majority of the RAM available to the programmer.

Large RAM expansions have become ubiquitous on the platform, so the philosophy of
this project is to choose feature richness without much concern for the memory footprint.
Virtually any feature that I deem valuable in an editor/assembler is included.

Some of its features are:
 - 40 column bitmap-based editor
 - vi-like keybindings
 - interactive visual debugger
 - breakpoint editor
 - memory viewer/editor
 - file I/O (save/load)
 - directory viewer
 - symbol viewer
 - auto-formatter and realtime syntax checking
 - macro support
 - user program/source/editor isolation
 - many more...

---

## Requirements
For now Monster requires a [Final Expansion](https://github.com/edi-z/FE3) to
function.  It could easily be modified for other 512k+ carts.
Much of this RAM is used to store the multiple source code buffers (up to 8),
but it is also used to store debug info and some code.

The banked memory allows the user program to execute in almost complete
isolation.  This means that, although this environment consumes a vast amount of
memory itself, everything except address $9c02 (the bank select register) and
a couply tiny interrupt handlers is preserved when control moves between the
editor and the user program.  Moreso even than small monitor cartridges,
the program itself is virtually unaware of the resident tooling.

## Building
Building the source requires `ca65`. The easiest way to install this is to
install the latest release of [cc65](https://github.com/cc65/cc65). I've tested
with v2.18.

The build process also requires `python3` (any version should do).

Monster can be built for two targets: disk and cartridge.  The disk format
is useful if you want to test on your Final Expansion 3 without erasing its firmware.

The cartridge is a better choice for emulators or power users on real hardware.
It loads significantly faster

#### Build Steps
Clone this repo `git clone https://github.com/gummyworm/monster.git`

`cd` to the directory you cloned to and run `make` for the appropriate target

To build the disk version, run
`make disk`

To build the cart version, run
`make cart`

## Running

The _Disk_ Makefile generates two PRG's: BOOT.PRG and MASM.PRG.
You may write these to your disk of choice and load Monster as you would any other program on your Vic-20:

```
LOAD "BOOT.PRG",8,1
RUN
```

The _Cart_ Makefile produces a single binary file, which is the cartridge image
for the Final Expansion 3. To flash this to your FE3 for use on real hardware, copy it to
your IEC storage device along with the installer (`install.prg`).

```
LOAD "INSTALLER.PRG",8,1
RUN
```

This will flash Monster to the Final Expansion.  Reset the computer to enter Monster.

If you wish to run Monster in an emulator (VICE), ensure that VICE is installed on your
machine and run `make start-disk` or `make start-cart` from the root of the project to attach the
corresponding disk or cartridge image.

---
## Editor Overview
The editor is a substantial part of this assembler.  In addition to offering
a high-density 40-column display, it has, by 8-bit editor standards, advanced navigation
functionality.

### C= Command
Below are the basic C= + <key> commands along with their corresponding key combination. These
commands are available regardless of editor mode (see the _Editor Modes_ section
below for more info on modes).

|  Key   | Name          |   Description                                                         |
|--------|---------------|-----------------------------------------------------------------------|
| C= + b | Set Breakpoint| sets a breakpoint at the current line                                 |
| C= + c | Refresh       | refrehshes the screen by redrawing the source buffer                  |
|    -   | File Viewer   | list directory, shows the files on the current disk                   |
| C= + n | New buffer    | creates a new source buffer and sets it as the active buffer          |
| C= + q | Close buffer  | closes the current buffer and opens the next one that is open         |
| C= + v | MemView       | enters the memory viewer/editor (press <- to exit)                    |
| C= + y | Show Symbols  | lists the symbol table for the assembled program                      |
| C= + e | Next Error    | if there are errors from the last assembly, navigatest to the next one|
|   F3   | Assemble      | assembles the code in the buffer to memory                            |
|   F4   | Debug         | assembles the code in the buffer to memory _with_ debug info          |
|   F5   | Show buffers  | displays a list of the currently open buffers                         |
| C= + + | Next Drive    | Selects the next drive (limited to #15)                               |
| C= + - | Prev Drive    | Selects the previous drive (limited to #8)                            |
|    :   | Ex Command    | Accepts a command + argument(s) and executes the command              |

### Function (f key) Commands

|  Key   | Name             |   Description                                                         |
|--------|------------------|-----------------------------------------------------------------------|
|   f1   | Assemble Project | assembles current project                                             |
|   f2   | Assemble File    | assembles the current file to memory (must specify .org)              |
|   f3   | Debug            | begins debugging contents of memory at lowest defined origin (.org)   |
|   f4   | Debug BASIC      | initializes BASIC and begins debugging at the lowest defined origin   |
|   f5   | Show buffers     | displays a list of the currently open buffers                         |
|   f6   | Show project     | displays the current project configuration                            |

While debugging, f-keys 1-4 have different functionality, as described in the _Debug Commands_ section

### Ex Commands
The following commands are entered at the "Ex Command" prompt (accessed with the `:` key).
Most accept an argument (as described in each commands description below)

|Key| Name                         |   Args                          | Description                                                                                     |
|---|------------------------------|---------------------------------|-------------------------------------------------------------------------------------------------|
| a | Assemble File                | Filename                        | assembles the given filename                                                                    |
| B | export Binary                | Filename                        | exports the active assembly to a binary file (no .PRG header)                                   |
| d | Start Debugger               | Symbol to debug at (optional)   | begins debugging at the given label                                                             |
| db| Start Debugger (with init)   | Symbol to debug at (optional)   | begins debugging at the given label. Initializes target state with the BASIC cold start handler |
| e | Edit                         | Filename                        | loads the buffer with the contents of the given file                                            |
| g | Goto                         | Symbol to run at (optional)     | executes the program at the address of the given symbol                                         |
| P | export .PRG                  | Filename                        | exports the active assembly to a .PRG file                                                      |
| r | Rename                       | Name                            | renames the buffer to the given name                                                            |
| s | Save                         | Filename                        | saves the buffer to the given filename                                                          |
| S | Save All                     |   N/A                           | saves all modified buffers that are open currently                                              |
| x | Scratch                      | Filename                        | scratches (deletes) the given filename                                                          |

#### Assemble File :a <filename>
Assembles the contents of the given file. This is functionally the same as opening
the given file and assembling it with debug information (F4).

Invoking the debugger will invoke it for the last assembled file (not the current
source buffer) in this scenario.  The debugger cares about the active debug
information _not_ the active file.

Example:
`:a HELLO.S`

#### Export Binary :B <filename>
Exports the active assembly (F3/F4) to the given file as binary.  This means
no load address is prepended to the file.  This can be useful if you are using
Monster to create level data or other code loaded by your main program.  It
can also be used to export things like data tables for use with .INCBIN

#### Start Debugger :d [symbol]
Begins debugging at the given symbol using the active debug information.

If no symbol is given, the program will
begin and the debugger invoked at the _lowest_ defined origin (.ORG) in the
program. See [Debugger](https://github.com/gummyworm/monster#debugger) for more
details on debugging.

Example:
`:d START`

#### Start Debugger (with init) :db [symbol]
Begins debugging at the given symbol using the active debug information, initializing the
system with the BASIC COLD start handler first.

This is useful if your program expects the system to be initialized with the default
VIC-registers, zeropage values , etc.

If no symbol is given, the program will
begin and the debugger invoked at the _lowest_ defined origin (.ORG) in the
program. See [Debugger](https://github.com/gummyworm/monster#debugger) for more
details on debugging.

Example:
`:db START`

#### Edit :e <filename>
Loads the given filename to a new buffer and activates it.

Example:
`:e HELLO.S`

#### Export .PRG :P <filename>
Exports the active assembly (F3/F4) to the given file as a .PRG file.  This means
a load address is prepended to the file prior to export.  This produces a
standalone executable you can use when you are done working on your program.

#### Rename :r <buffername>
Renames the active buffer to the given name.
Example:
`:r TEST2.S`

#### Save :s <filename>
Saves the active buffer to a file with the given name.  If no name is given,
the active buffer's name is used.

Adding an `@` to this command (`s@`) will delete the file before saving. This
allows you to overwrite the existing file if it exists.

Examples:
`:s NEW.S`
`:s@ OLD.S`

#### Save All :S
Saves all buffers that have been modified since they were last saved.
As with the _Save_ command, adding `@` to the command (`S@`) will overwrite
existing files if they exist.

Example:
`:S@`

#### Scratch :x <filename>
Deletes the file of the given name.
Example:
`:x TEST.S`

---

## Editor Modes
The editor is a _modal_ editor, that is, it behaves differently depending on which _mode_ it is
in.  The modes are all accessed from the default mode (called _COMMAND_ mode) and each mode returns
to _COMMAND_ mode when the `<-` key is pressed.  Below is a list of the modes along with
the key that enters that mode, and the editor behavior while in that mode.

### Command Mode (<-)
Command mode is the default mode.  The primary function of command mode is to navigate around the
source code and to enter other modes.
Navigation behaves similar to `vi` and many basic `vi` commands are supported.
The following keys are handled in COMMAND mode.

|  Key       | Name       | Description                                                            |
|------------|------------|------------------------------------------------------------------------|
| HOME       | Home       | moves the cursor to column 0                                           |
| C= + m     | Goto line  | prompts for a line number and moves the cursor to that line            |
| C= + [1-8] | Goto Buffer| opens the buffer corresponding to the number key that is pressed       |
| C= + h     | Prev Buffer| opens the buffer before the active one (if there is one)               |
| C= + l     | Next Buffer| opens the buffer after the active one (if there is one)                |
| C= + i     | Jump up    | jumps forward to the next source position that was "jumped" to         |
| C= + o     | Jump back  | jumps back to the last source position that was "jumped" to            |
|    $       | End of Line| moves the cursor to the end of the current line                        |
|    ;;      | Banner     | inserts a banner (full line of semicolons) below the cursor            |
|    gg      | Top of File| moves the cursor to the first character in the file                    |
|    gd      | Goto Def   | if the cursor is on a label reference, navigates to that label         |
|    G       | End of File| moves the cursor to the last line in the file                          |
|    h       | Left       | moves the cursor left                                                  |
|    j       | Down       | moves the cursor down                                                  |
|    k       | Up         | moves the cursor up                                                    |
|    l       | Right      | moves the cursor right                                                 |
|    H       | Home       | moves the cursor to the top left of the screen                         |
|    L       | Last       | moves the cursor to the bottom left of the screen                      |
|    d0      | Delete To  | deletes everything on the line before the cursor                       |
|    D/d$    | Delete Rest| deletes the contents of the line after the cursor's position           |
|    dd      | Delete Line| deletes the next line                                                  |
|    dw      | Delete Word| deletes the next word                                                  |
|    J       | Join lines | moves the contents of the next line to the end of the current one      |
|    0       | Column 0   | moves the cursor to the first column of the current line               |
|    a       | append char| enters insert mode and moves to the next character                     |
|    A       | append line| enters insert mode and moves to the last character in the current line |
|    o       | open line  | opens a new line below the cursror and moves to it                     |
|    O       | open line ^| opens a new line above the cursor and moves to it                      |
|    p       | paste below| pastes the contents of the copy-buffer to the line below the cursor    |
|    P       | paste above| pastes the contents of the copy-buffer to the line above the cursor    |
|    I       | Insert line| enters insert mode and moves to the first character in the current line|
|    [       | Prev Block | moves to the previous empty line or start of file if there isn't one   |
|    ]       | Next Block | moves to the next empty line or end of file if there isn't one         |

### Insert Mode (i, a, etc.)
Entering insert mode allows the user to enter text at the cursor location.  Keystrokes are
interpreted as their corresponding ASCII character value in this mode, so there are no special
commands accessed via them.

### Visual Mode (v)
In _VISUAL_ mode (accessed via `v` in _COMMAND_ mode), the user can select
a block of text which may then be deleted or copied.  Below is the table of supported commands
while in visual mode. The `<-` key will return the user to to _COMMAND_ mode.

|  Key       | Name       | Description                                                            |
|------------|------------|------------------------------------------------------------------------|
|    d       | delete     | deletes the selected text _and_ copies it to the copy buffer           |
|    y       | yank       | copies the selected text (in VISUAL mode) to the copy buffer           |

### Visual Line Mode (V)
_VISUAL LINE_, which is entered with the `SHIFT - v` key combination from _COMMAND_ mode is similar to _VISUAL_ mode,
but selections include only entire lines.  Upon entering _VISUAL LINE_ mode, the current row is selected.
Navigating to rows above or below will select additional lines.  The delete and yank keys behave the same as they do
in _VISUAL_ mode.

---

### Copy buffer
When text is deleted (delete line, delete word) or _yanked_, it is stored to a buffer where
it may be recalled by the paste commands (`p`, paste below and `P` paste above).
When the paste command is executed, the buffer is cleared.

The copy buffer is $1e00 bytes, which is enough for ~3.5 completely full screens
of text (22 rows and 40 columns).

Because the editor is limited to 40 columns in width, the first and last lines are handled
specially.  If the first line will not fit, the paste is aborted.  If the last line will not
fit, it is broken into two lines.

### Jump Lists
When the user "jumps" to a different position in the source (`gg`, `G`, `goto line`,
`find`, `[`, and `]`) the editor saves the old position.  To recall the positions
that were "jumped" from are two commands: _jump-forward_ (`C= + i`) and _jump-backward_ (`C= + o`).

### Syntax Checking
Lines are checked and formatted according to their contents each time they
are completed (RETURN is pressed).
While this should reduce the number of errors you encounter when assembling,
it does not guarantee it.  The following assumptions are made when checking
the syntax of a line:
    - macros are defined
    - labels may not be defined
    - origin may not be set
This means that lines using undefined labels are treated as valid.  If
the label does not exist at assembly time, of course this will result in an
error.
Macros, however, are expected to be defined.

Although labels aren't _required_ to be defined, they are internally tracked
while editing.  Because their addresses aren't valid til assembly, you cannot
access them (e.g. in the symbol viewer) until then.

### UDG Editor
The UDG (user defined graphics) editor is entered with the `C= + u` key combination.
This editor allows you to visually create simple graphics for your programs.  Navigation
is done with the same vi-like commands used in the main editor and graphics are created using the
following commands:

| Command Name  |   Key   |  Behavior
|---------------|---------|------------------------------------------------------------------------------------------------------|
|  Plot Color 1 |    1    | Sets the selected position to the background color                                                   |
|  Plot Color 2 |    2    | Sets the selected position to the character color (hires mode) or the border color (multicolor mode) |
|  Plot Color 3 |    3    | Multicolor mode only. Sets the selected position to the character color                              |
|  Plot Color 4 |    4    | Multicolor mode only. Sets the selected position to the auxiliary color                              |
|  Clear        |SHIFT+CLR| Multicolor mode only. Sets the selected position to the auxiliary color                              |
|  Done         | RETURN  | Exits the editor and enters (or updates) the .db commands to create the graphic in the editor        |
|  Quit         | STOP    | Exits the editor without creating/updating the graphic contained in the editor                       |
| Toggle Mode   |   M     | If in hires mode, switches to multicolor mode or vise-versa                                          |

Entering the editor while on a line with an 8-byte ".db" definition (e.g. `.db $ff,$00,$ff,$00,$ff,$00,$ff,$00`) will pre-populate the
UDG editor with the character defined by these directives.

---

## Assembler Overview

### Syntax
The assembler syntax is very similar to any other major assembler.  For basic
instructions, the canonical 6502 assembly syntax is supported.  That means '$'
denotes a hex value, '#' and immediate operand, parentheses an indirect address,
etc.

### Expressions
Operands, in addition to basic values and labels, may contain an expression,
which is evaluated at assemble-time to a value.

The supproted operators are: '+', '-', '*', and '/'. Multiplication and division
have higher precedence than addition and subtraction.

Expressions may also contain parentheses, which are evaluated as you would expect,
but note that if the entire expression is enclosed in parentheses, the
assembler will interpret this as indirect addressing. For example:
```
JMP (1+3)	; jump-indirect to the address in memory address (4)
JMP 1+3 	; jump-absolute to address 4
```

Immediate addressing and indirect addressing are mutually exclusive, so the assembler
will allow you to enclose the whole expression in parentheses for immediate expressions
prefixed with a '#' (e.g. `LDA #(2+4)`)

Labels are supported in expressions and will evaluate to their address when assembled.
```
LDA #<LABEL1
```

Hexadecimal and decimal numbers are supported.  Hexadecimal numbers must be prefixed
with a '$'.
```
LDA #(10+$20)
```

Character literals are also supported. These are represented as a character enquoted within
single quotes.
```LDA #'x'```
Character literals must contain exactly one character and always resolve to
a 1 byte value.

## Formatting
Spacing is not important, but instructions are auto-formatted so that they are TAB indented.
Labels and directives are, by convention, not indented. The formatter will also take care of this.

## Labels
Labels begin with either an alpha-character or, in the case of _local_
labels, a '@' character.  They are limited to 16 characters, but it is advisable to keep them shorter (8 characters or less).
Long labels are harder to squeeze onto a line.

They are case-insensitive (`a` and `A` refer to the same label)
and their definitions may end with a colon (':') but are not required to (`A:` and `A` are both valid label definitions)

#### Local Labels
Local labels are defined by prefixing the label with a '@' symbol.  This _does_
count toward the 16 character label limit.
Local labels are valid until the next non-local label is defined as shown in
the following example.
```
PROC0:
@L0:
    DEX
    BNE L0
    RTS
PROC1:
@L0:
    DEY
    BNE L0
    RTS
```
Note that the scope of the `@L0` defined under `PROC0` is valid until the next
non-local label (`PROC1`) at which point the name is recylced and may be used
again.

Because of the way local labels are implemented they are not totally
inaccessible. They _can_ be accessed by
prepending the global label that encapsulates them.  This can be used to
emulate structural data types e.g.
```
PLAYER
@X: .db 0
@Y: .db 0

GAME:
	LDA PLAYER@X
```

#### Anonymous Labels
Anonymous labels can be declared with ':'.
Anonymous labels are useful when you need to do a short branch where
a descriptive label name isn't necessary.

A colon followed by a + or - character is used to reference these
labels.  Pluses (+) refers to the next _forward_ anonymous label and
minuses (-) refer to the previous _backward_ anonymous label.

for example
```
    .ORG $1000
:   JMP :+      ; JMP $1003
:   JMP :-      ; JMP $1003
:   JMP :--     ; JMP $1003
```

Using multiple +'s or -'s will count the same number of references before landing
on the corresponding anonymous label.
for example:
```
    JMP :+++
:   nop
:   nop
:   nop         ; will jump here
```

## Directives
Directives begin with a `.` character and instead of being directly assembled,
as with an instruction, tell the assembler to generate some special code or data
based on the operands.

Some directives (`.MAC` and `.REP`) generate a variable amount of code or data based on the value
of their operands.
For these directives, the expressions used as arguments must be resolvable
in pass 1 of the assembler.  This means any labels used in the expression
must be declared before the directive.

The following example illustrates why this is necessary:
```
.REP NUM, I
  ASL
.ENDREP
.EQU NUM 5
```
Note that `NUM` is not declared until after the `.REP` directive. Because of this
the assembler does not know how many times to repeat the `ASL`. We could assume
the label is an arbitrary 16-bit value as we do with labels that are undefined
in pass 1, but any subsequent labels would have the wrong address if we guessed
any number other than 5.

---
### List of Directives
#### .DB _expression_, ..., _expression_
Defines a sequence of bytes from the comma-separated list that follows.

Examples:
 |       code        |    generated binary  |
 |-------------------|----------------------|
 | .DB $00, $01, $02 | $00 $01 $02          |
 | .DB "HI",0	     | $48 $49 $00          |

#### .DW _expression_, ..., _expression_
deines a sequence of words from the comma-separated list that      |

Examples:
 |       code        |    generated binary     |
 |-------------------|-------------------------|
 | .DW $00, $01, $02 | $00 $00 $01 $00 $02 $00 |

#### .ENDIF
Ends a .IF block

See [.IF](#if-expression)

#### .EQU _name_ _expression_
Defines a constant which may be used in expressions
```
.EQU BITMAP $1100
  LDA #$00
  STA BITMAP+20
```

#### .IF _expression_
Evaluates the expression
Conditionally assembles the lines between this directive and its matching
`.ENDIF`.
```
.IF NTSC
.EQU CYCLES_PER_LINE 65
.EQU LINES 261
.ELSE
.EQU CYCLES_PER_LINE 71
.EQU LINES 312
.ENDIF
```

#### .IFDEF _label_
Evaluates to TRUE if _label_ is defined.  This is different from .IF because
_label_ may be defined to be 0 and this will still evaluate to TRUE.
This can be useful inside macros to determine if a paramter was provided or not.

#### .INC _filename_
Includes a file at the line of the directive. The file is loaded line-by-line
from disk and assembled as if the code was copy/pasted in place of the include directive.
```
.INC "KERNAL.INC"
  LDA #$00
  JSR CHROUT
```

#### .INCBIN _filename_
Includes the binary file. The binary contents are stored at the current location
of the assembly target when this directive is encountered
```
.EQ BITMAP $1100
  LDX #$07
L0:
  LDA SPRITES,X
  STA BITMAP,X
  DEX
  BPL L0

SPRITES:
.INCBIN "SPRITES.BIN"
```

#### .MAC _name_ _param 1_, ..., _param n_
Defines a macro
```
.MAC LDXY VAL
  LDX #<VAL
  LDY #>VAL
.ENDMAC

  LDXY $1234
```
Will generate the following code:
```
  LDX #$34
  LDY #$12
```
Macro definitions begin with the `.MAC` directive followed by the name of the
macro and a comma-separated list of the parameters for the macro.

Macros are invoked with the name of the macro followed by a comma-separated
list of the parameters.


#### .ORG _expression_
Sets the address to assemble code to
```
.ORG $1000
; start up code

.ORG $2000
; main code
```

#### .RORG _expression_
Sets the address the code will run at when executed.
This is useful for code that will be relocated prior to execution.
```
.ORG $1000
.RORG $00
  ; some tight loop
  LDA #$01
  STA *+3
  LDA #$00
  STA $900F
```
Note that the `.RORG` directive must follow the `.ORG` directive in order to
avoid the virtual PC being overwritten.
`.ORG` will set the virtual PC to the same location as the physical PC.

#### .REP _expression_ [, _iterator name_]
Assembles the code between this directive and `.ENDREP` for the given number of
times.
```
.REP 3
  ASL
.ENDREP
```
Becomes
```
  ASL
  ASL
  ASL
```

An optional parameter can be given that will be assigned the value of
the current iteration of repetition during assembly.
```
.REP 5,I
   INC $F0+I
.ENDREP
```
Becomes
```
  INC $F0
  INC $F1
  INC $F2
  INC $F3
  INC $F4
```

---
### Macros
Macros offer a conveinient way to abstract patterns that you find yourself
frequently writing.

They may be recursive as in this example:
```
.MAC LDXY VAL
	LDX VAL
	LDY VAL+1
.ENDMAC

.MAC STXY ADDR
	STX ADDR
	STY ADDR+1
.ENDMAC

.MAC SET DST, SRC
	LDXY SRC
	STXY DST
.ENDMAC
```

You may omit arguments to a macro if your macro knows how to deal with
less than the maximum number it expects as in this example:
```
.MAC SAVEBYTES A, B, C
.IFDEF A
	LDA A
	PHA
.ENDIF
.IFDEF B
	LDA B
	PHA
.ENDIF
.IFDEF C
	LDA C
	PHA
.ENDIF
.ENDMAC
```

### Macro Limitations

There are some limitations on the number of macros and overall size of the
macros per assembly.  The source for all macros must be less than $1400 bytes.
There is also a 128 macro limit.

Each macro can be at most 16 lines or 512 bytes, whichever is lower. This restriction also applies to .REP.

Comments are excluded from the internal context buffer, so using them will not count toward the byte limit.

---

### Limitations

#### Memory Usage
The user program may use all available memory from $00 to $7fef. The $11 bytes above $7fef are reserved
for the debugger.  If your program needs these, you can still assemble to them, but you will not be
able to use the debugger.

### Best Practices
Although Monster strives to feel like unbothered by the physical limits of the
Vic-20, certain usage patterns can cause issues. By following these practices,
you should have a smooth experience without running into some of the
limitations that you may hit if you are unconcious about them.

#### Avoid using many files
When possible, try to stick to 8 or fewer files for your project.  This will
allow all of them to be stored in memory during assembly, which will greatly
speed up your edit/debug loop.  When debugging, this practice will also allow
all files to be available in RAM when you are stepping through the program,
which will be a much smoother experience than programs that continuously must
page to disk.

#### Use anonymous labels
Anonymous labels take up no space for the label names, only address.  Using
them is much more efficient than labels, and so this should be done for short
branches that don't require much description.  Using too many labels, in the
extreme case, can push your program over the symbol limit.

#### Keep control within the debugger
You will be warned when transferring control out of the debugger, so try to heed this.
If the CPU encounters a JAM instruction there is no way to recover.
When stepping or tracing, the debugger will take care to ensure that no JAMs are executed.

For things that cannot be traced, like cycle-based effects, you will need to
give full control to the user program to get rid of the the overhead tracing introuduces.
In these cases, you can press RESTORE to return to the debugger (assuming the user program
has not overwritten the NMI vector at $0318 or disabled NMIs).

In the event of a JAM or an unreoverable state (NMI disabled or vector overwritten), you
will need to reset the machine.  Save often.

---

### Example program
Here is a basic hello world program to demonstrate some of the assembler's
features
```
.ORG $1400
MSG:
.DB "HELLO WORLD!",0
START:
  JSR $E5B5
  LDX #$00
  LDA #' '
CLR:
  STA $1000,X
  STA $1100,X
  DEX
  BNE CLR
DISP:
  LDA MSG,X
  BEQ DONE
  JSR $FFD2
  INX
  BNE DISP
DONE:
  JMP DONE
```

## Files
Monster holds the active source file in memory (for editing), but assembles
all included files directly from file.
Files are stored with $0d line endings, but if you save your file with UNIX
style line-endings, they will be automatically converted when the file is
read in.

As with any work done with Commodore disk I/O, it is wise to regularly back up your files

### Drive Selection (`C= + +` and `C= + -`)
Selects the next or previous drive (within the valid range of 8-15).
`C= + + (plus)` selects the _next_ available drive and `C= + - (minus)` selects
the _previous_ available drive.

### Directory Viewer (`-` in command mode)
The directory viewer offers a paginated view of all files on the disk.
Pressing `RETURN` while the row of the desired file is highlighted will load
that file into a new buffer and activate that buffer.

---

## Symbol Viewer (`C= + Y`)
The symbol viewer displays all the labels in the program
along with their corresponding address.
The up/down cursor keys navigate between pages of symbols. The back-arrow
key returns to the debugger.

---

## Debugger

https://github.com/gummyworm/monster/assets/4626914/840f5d66-03cb-4daf-9ed2-41a4d37d4c2d

The debugger allows you to step through code, set breakpoints, and watch
data as you execute your program.

Upon entering the debugger, a view of the system state is displayed at the
current step or breakpoint.
This include the state of the registers (A, X, Y, P, SP, and PC) as well as
any effective address that was calculated for reading/writing by the last
instruction.

While debugging, most navigation commands work as normal. Breakpoints may
be set as they would in the editor prior to assembly, and they will be installed
in realtime.  Other edits are not allowed, however, while the debugger is active.

Both the debugger and the user program's RAM is saved/restored when control
transfers between the two. That is the screen data ($1000-$2000), the zeropage,
and color RAM.

---

### Debug Commands
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

### Register Editor (`F2`)
Pressing F2 moves the cursor to the register contents and allows the user to enter
new values for them.  Pressing `RETURN` will confirm the new register values
and update them to those values the next time the user program is invoked.
Pressing `<-` will abort this process and leave the old register values
intact.

### Stopwatch

Next to the registers, under the CLK label, is a 24-bit counter that displays the
number of cycles executed by the instructions that have been STEP'd into.
The stopwatch can be reset to 0 with the `C= + r` key combination.

Note that the number of cylces is displayed in decimal unlike the rest of the
information in the debug view, which is displayed in hexadecimal.

---

### Stepping through code
#### Step Into (`z`)
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

#### Step Over (`s`)
Step _over_ behaves the same as step _into_, but if the next
instruction is a subroutine call (`JSR`), execution continues until the
instruction _after_ the `JSR` (after the subroutine returns).

This command is considered somewhat dangerous because this command doesn't
trace the execution.  If the subroutine that is stepped over runs a JAM
instruction, the processor will halt.  Because the overhead of tracing is
fairly high, this may be worth the risk. Especiallly if you are calling
routines that are trusted such as those in the KERNAL or BASIC ROM.

#### Step Out (`y`)
Step _out_ is different from the other STEP commands in that it runs by tracing.
The step out command traces the program until the next RTS instruction.
The RTS instruction is run and control then returns to the debug session.

#### Go (`C= + g`)
The go command begins execution and returns to the debugger only when a
breakpoint is encountered or when RUN/STOP is pressed.

#### Trace (`t`)
Trace is similar to `GO`, but the debugger executes the program as a series
of STEPs instead of running the program binary directly.
This is useful because it allows the debugger to break if any watched memory
location is accessed or if a JAM would occur.


#### Notes on memory swapping
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

## Auxiliary Views
Within the debugger, there are 3 auxiliary views that may be activated with the
function keys.  Each shows information about the machine or debug state.
Each viewer also contains an editor, which is activated with the keys enumerated
below next to their corresponding editor.

Pressing the `<-` key will return the user from the auxiliary editor to the
source code editor.  And `F1` will hide the active view to maximize the
source editor's screen size.

### Memory Viewer (`F3` while debugging)
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

#### Set Watch (`C= + w`)
Watches may be placed while navigating in the memory editor.  This is done
by pressing the `C= + w` key-combination while the cursor is on the desired
byte to watch. See the _Watch Viewer_ section for more information on
watches.

#### Find Value (`/`)
Prompts the user for an 8 or 16 bit value (determined by the number of
characters provided) and looks for that value in memory.
If it is found, the memory view is updated to begin at the first address
that was found containing the specified value.

Note that when seeking for a 16 bit value, the value is searched in little-endian
format.  If the input for the search is given as `$1234` the result will be
the first occurrence of the byte value `$34` followed by `$12`.

#### Set Address (`^`/`up-arrow`)
Moves the cursor to the address of the viewer, then prompts the user for a new
value to set the memory viewer to.  Pressing `RETURN` confirms the new address
and `<-` cancels and returns the user to the editor without changing the address

### Breakpoint Viewer (`F5` while debugging)
The breakpoint viewer displays all the breakpoints that have been set by the
user.  A circle is displayed next to those that are currently active.
The user simply navigates the list with the cursor keys and presses RETURN to
toggle those which he/she wishes to enable/disable.

Note that breakpoints correspond to the debug information generated with
the F4 command.  If the line numbers change after this information is generated,
breakpoints are unlikely to behave in expected ways.

### Watch Viewer (`F7` while debugging)
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

#### Add Watch (`C= + w`)
While in the watch editor, the `C= + w` key combination prompts the user for an
address or address range to watch.  These are given as expressions, so you may
provide, for example `myval+3` to set a watch at the address of the label myval plus 3.
To set a watch for an address range, simply provide two expressions, separated by a comma,
at the prompt.  If the expression(s) are invalid, no watch is added.

#### Edit Watch (`RETURN`)
Pressing RETURN will invoke the _memory editor_ at the location of the watch
that was selected.  Returning from the memory editor will return the user
back to the watch editor.

---

## Breakpoints
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

### Toggle Breakpoint (`C= + b`)
During normal editing, breakpoints may be set and removed  with the `C= + b` key combination.

Pressing the same key combination (`C= + b`) will also remove a breakpoint
if it is pressed while on a line that already has one.

Breakpoints can only be added to buffers that have been named.

---

## Watches
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

## Monitor (`C= + t`)
The monitor is a text based interface for debugging programs and manipulating
program state.  It offers the same functionality as the GUI debugger plus a plethora of other commands to
manipulate the program state.

### File Redirection
The output from a given monitor command can be redirected to file instead of the screen by using the
redirect (`>`) operator.  When placed at the end of a command, the redirect operator writes all output from
that command to the following file.

For example:

`r > regs.txt`

Will write the contents of the registers to the file `regs.txt`

### Monitor Commands
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
