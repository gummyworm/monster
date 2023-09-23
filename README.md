# MONster
![helloworld](https://github.com/gummyworm/monster/assets/4626914/ba7345da-db81-4603-b381-7bc5cab56b52)

MONster is an all-in-one editor/assembler/debugger for the Commodore Vic-20.

Some of its features are:
 - 40 column bitmap-based editor
 - vi-like keybindings
 - breakpoint editor
 - interactive visual debugger
 - memory viewer/editor
 - file I/O (save/load)
 - directory viewer
 - auto-formatter and realtime syntax checking
 - improved keyboard routine (3-key rollover)
 - macro support
 - user program/source/editor isolation
 - many more...

The source code is stored in a gap buffer to allow for efficient insertion/deletion.

## Requirements
MONster requires a _completely_ expanded (BLK 2,3, and 5) RAM configuration to function.

Debugging requires a [Final Expansion](https://github.com/edi-z/FE3).
This is because it is very memory-expensive to store all the debug info.
When this project matures to a fairly stable point, I'd like to distribute it on a cartridge based on the FE design, but for now...

Building the source requires `ca65`. The easiest way to install this is to 
install the latest release of [cc65](https://github.com/cc65/cc65). I've tested 
with v2.18.

## Building
 1. Clone this repo `git clone https://github.com/gummyworm/monster.git`
 2. `cd` to the directory you cloned to and run `make` 

## Running
The Makefile will generate a PRG. You may write this to your disk of choice
and load it as you would any other program on your Vic-20: `LOAD "MONSTER.PRG",8,1`

If you wish to run it in an emulator (VICE), ensure that VICE is installed on your 
machine and run `make start` from the root of the project.

## Usage
The builtin HELP menu (C= + H) provides basic usage details
Here are some of the supported commands.  When given a prompt the '<-' key
will exit the prompt and cancel the command

#### Command shortcuts
|  Key   | Name    |   Description                                                               |
|--------|---------|-----------------------------------------------------------------------------|
| C= + a | Assemble File | prompts for a filename and assembles it.                              | 
| C= + b | Set Breakpoint| sets a breakpoint at the current line                                 | 
| C= + c | Refresh       | refrehshes the screen by redrawing the source buffer                  | 
| C= + d | Start Debugger| prompts for a label and begins debugging at it                        | 
| C= + g | Goto          | prompts for a label name and executes the program at its address      | 
| C= + h | Help          | displays the help menu                                                | 
| C= + l | List          | list directory, shows the files on the current disk                   |
| C= + o | Open          | prompts for a filename and loads the buffer with its contents         |
| C= + n | New buffer    | creates a new source buffer and sets it as the active buffer          |
| C= + q | Close buffer  | closes the current buffer and opens the next one that is open         |
| C= + r | Rename        | prompts for a filename. this name will be used for future saves       |
| C= + s | Save          | save file, prompts for a filename and saves the buffer contents to it |
| C= + v | MemView       | enters the memory viewer/editor (press <- to exit)                    |
| C= + x | Scratch       | prompts for a filename and deletes the file                           |
| C= + y | Show Symbols  | lists the symbol table for the assembled program                      | 
|   F3   | Assemble      | assembles the code in the buffer to memory                            |
|   F4   | Debug         | assembles the code in the buffer to memory _with_ debug info          |
|   F5   | Show buffers  | displays a list of the currently open buffers                         |


#### Navigation/Text keys
Navigation behaves similar to `vi` and many basic `vi` commands are supported.
The following keys are handled in COMMAND mode.  Entering insert mode (`i`)
allows characters to be entered into the source file at the current cursor
position.
The back arrow (`<-`) key returns to COMMAND mode.
|  Key       | Name       | Description                                                            |
|------------|------------|------------------------------------------------------------------------|
| HOME       | Home       | moves the cursor to column 0                                           |
| C= + m     | Goto line  | prompts for a line number and moves the cursor to that line            |
| C= + [1-8] | Goto Buffer| opens the buffer corresponding to the number key that is pressed       |
| C= + <     | Prev Buffer| opens the buffer before the active one (if there is one)               |
| C= + >     | Next Buffer| opens the buffer after the active one (if there is one)                |
|    $       | End of Line| moves the cursor to the end of the current line                        |
|    ;;      | Banner     | inserts a banner (full line of semicolons) below the cursor            |
|    gg      | Top of File| moves the cursor to the first character in the file                    |
|    G       | End of File| moves the cursor to the last line in the file                          |
|    h       | Left       | moves the cursor left                                                  |
|    j       | Down       | moves the cursor down                                                  |
|    k       | Up         | moves the cursor up                                                    |
|    l       | Right      | moves the cursor right                                                 |
|    H       | Home       | moves the cursor to the top left of the screen                         |
|    L       | Last       | moves the cursor to the bottom left of the screen                      |
|    dw      | Delete Word| deletes the next word                                                  |
|    dd      | Delete Line| deletes the next line                                                  |
|    0       | Column 0   | moves the cursor to the first column of the current line               |
|    a       | append char| enters insert mode and moves to the next character                     |
|    A       | append line| enters insert mode and moves to the last character in the current line |
|    o       | open line  | opens a new line below the cursror and moves to it                     |
|    O       | open line ^| opens a new line above the cursor and moves to it                      |
|    I       | Insert line| enters insert mode and moves to the first character in the current line|
|    [       | Prev Block | moves to the previous empty line or start of file if there isn't one   |
|    ]       | Next Block | moves to the next empty line or end of file if there isn't one         |


## Assembler Syntax
The assembler syntax is very similar to any other major assembler.  For basic
instructions, the canonical 6502 assembly syntax is supported.  That means '$'
denotes a hex value, '#' and immediate operand, parentheses an indirect address,
etc.
:vs
## Expressions
Operands, in addition to basic values and labels, may contain an expression,
which is evaluated to a value to generate the operand for the generated
binary.

('+', '-', '\*', '/' respectively), which are evaluated with proper operator 
precedence.

Expressions may contain parentheses, which are evaluated as you would expect,
but note that if the entire expression is enclosed in parentheses, the 
assembler will interpret this as indirect addressing. For example: 
```
JMP (1+3)	; jump-indirect to the address in memory address (4)
JMP 1+3 	; jump-absolute to address 4
```

Immediate-addressing makes no sense with indirect addressing mode, so the assembler
will allow you to enclose the whole expression in parentheses for expressions
that are defined with a '#' prefix (e.g. `LDA #(2+4)`)

In addition to hexadecimal values, decimal values, and labels, character literals
may be used in expressions. These are represented as a character enquoted within
single quotes.
```LDA #'x'```
Character literals must contain exactly one character and always resolve to
a 1 byte value.

## Formatting
Spacing is not important, but instructions are auto-formatted to be indented
by two spaces.  Labels and directives are, by convention, not indented.  The
formatter will also take care of this.

## Files
Monster holds the active source file in memory (for editing), but assembles
all included files directly from file.
Files are stored with $0d line endings, but if you save your file with UNIX
style line-endings, they will be automatically converted when the file is
read in.

As with any work done with Commodore disk I/O, it is wise to regularly back up your files

### Directory Viewer (`C= + L`)
The directory viewer offers a paginated view of all files on the disk.
Pressing `RETURN` while the row of the desired file is highlighted will load
that file into a new buffer and activate that buffer.

## Labels
Labels begin with either an alpha-character or, in the case of _local_
labels, a '@' character.
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


## List of Directives
### .DB _expression_, ..., _expression_
Defines a sequence of bytes from the comma-separated list that follows.

Examples:
 |       code        |    generated binary  |
 |-------------------|----------------------|
 | .DB $00, $01, $02 | $00 $01 $02          |
 | .DB "HI",0	     | $48 $49 $00          |

### .DW _expression_, ..., _expression_
deines a sequence of words from the comma-separated list that      |

Examples:
 |       code        |    generated binary     |
 |-------------------|-------------------------|
 | .DW $00, $01, $02 | $00 $00 $01 $00 $02 $00 |

### .ENDIF 
Ends a .IF block

See [.IF](#if-expression)

### .EQU _name_ _expression_
Defines a constant which may be used in expressions
```
.EQU BITMAP $1100
  LDA #$00
  STA BITMAP+20
```

### .IF _expression_
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

### .IFDEF _label_
Evaluates to TRUE if _label_ is defined.  This is different from .IF because
_label_ may be defined to be 0 and this will still evaluate to TRUE.
This can be useful inside macros to determine if a paramter was provided or not.

### .INC _filename_
Includes a file at the line of the directive. The file is loaded line-by-line
from disk and assembled as if the code was copy/pasted in place of the include directive.
```
.INC "KERNAL.INC"
  LDA #$00
  JSR CHROUT
```

### .MAC _name_ _param 1_, ..., _param n_
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


### .ORG _expression_
Sets the address to assemble code to 
```
.ORG $1000
; start up code

.ORG $2000
; main code
```

### .RORG _expression_
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

### .REP _expression_ [, _iterator name_]
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

## Macros
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

## Example program
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

## Symbol Viewer
The symbol viewer, activated with C= + Y, displays all the labels in the program
along with their corresponding address.
The up/down cursor keys navigate between pages of symbols. The back-arrow
key returns to the debugger.

## Debugger
The debugger allows you to step through code, set breakpoints, and watch
data as you execute your program.  Due to the size of the data needed to 
store the debug information, this feature requires a Final Expansion
(512k RAM expansion).

The debugger is enabled by pressing CTRL-D
This will prompt the user for a label name, which will be used as the start
address for debugging.  If no label name is provided, execution will begin
at the base origin of the program (the _lowest_ value set by any `.ORG`
directive)

While debugging, most navigation commands work as normal. Breakpoints may
be set as they would in the editor prior to assembly, and they will be installed
in realtime.

Both the debugger and the user program's RAM is saved/restored when control
transfers between the two. That is the screen data ($1000-$2000), the zeropage,
and color RAM.

Editor navigation behaves as normal while debugging (albeit with slightly less
real estate due to the debug information displayed at the bottom of the screen.)

### Debug Commands
The following commands are supported by the debugger and are accessed by their
respective Key in the table below.

|  Key   | Name          |   Description                                                                        |
|--------|---------------|--------------------------------------------------------------------------------------|
|  F1      | Source View | maximizes the screen area for viewing the source code                                |
|  F3      | Mem View    | activates the memory window, which takes control until `<-` is pressed               |
|  F5      | Break View  | displays the breakpoints that have been set and allows them to be enabled/disabled   |
|  C=+g    | Go          | begins execution at the cursor                                                       |
|  C=+s    | StepOver    | steps to the next instruction. If it is a JSR, continues AFTER the target subroutine |
|  C=+z    | Step        | steps to the next instruction.                                                       | 
|   <-     | Exit        | exits the debugger and returns to the editor                                         |

### Breakpoints
During normal editing, breakpoints may be set with the `C= + b` key combination.
A breakpoint symbol (a filled circle) is placed at the beginning of a line to
indicate that a breakpoint has been added.
Pressing the same key combination (`C= + b`) will also remove a breakpoint
if it is pressed while on a line that already has one.

#### Breakpoint Viewer (`F5` while debugging)
The breakpoint viewer displays all the breakpoints that have been set by the
user.  A circle is displayed next to those that are currently active.
The user simply navigates the list with the cursor keys and presses RETURN to
toggle those which he/she wishes to enable/disable.

Note that breakpoints correspond to the debug information generated with 
the F4 command.  If the line numbers change after this information is generated,
breakpoints are unlikely to behave in expected ways.

As with all editors the back-arrow key (`<-`) exits and returns to the editor.

### Memory Viewer (`F3` while debugging)
The memory viewer displays the contents of RAM at a given address.  The memory
viewer is updated upon reentry to the debugger (if active).
Memory values may be updated by navigating to the value the user wishes to
change and overwriting it with a new hex value. The change occurs immediately.

As with all editors the back-arrow key (`<-`) exits and returns to the editor.
