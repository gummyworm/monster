# MONster
MONster is an all-in-one editor/assembler/memory editor for the Commodore Vic-20.
In its current form it is primarily intended for native development of applications for the
unexpanded Commodore Vic-20.

Features include:
 - 40 column bitmap-based editor
 - interactive debugger
 - memory viewer/editor
 - file support (save/load)
 - directory viewer
 - auto-formatter and realtime syntax checking
 - improved keyboard routine (3-key rollover)
 - many more...

The source buffer is stored in a gap buffer to allow for efficient insertion/deletion.

## Requirements
MONster requires a _completely_ expanded (BLK 2,3, and 5) RAM configuration to function.

I do plan to support configurations with banked memory so that one can isolate the 
development memory from the programs, however there are limited hardware options that support
this available (only the discontinued Ultimem to my knowledge). 

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

|  Key   | Description                                                           |
|--------|-----------------------------------------------------------------------|
| C= + C | refrehshes the screen by redrawing the source buffer                  | 
| C= + H | displays the help menu                                                | 
| C= + S | save file, prompts for a filename and saves the buffer contents to it |
| C= + L | list directory, shows the files on the current disk                   |
| C= + O | prompts for a filename and loads the buffer with its contents         |
| C= + V | enters the memory viewer/editor (press <- to exit)                    |
| C= + X | prompts for a filename and deletes the file                           |
| C= + G | prompts for a label name and executes the program at its address      | 
|   F3   | assembles the code in the buffer to memory                            |

Navigation keys
|  Key   | Description                                                           |
|--------|-----------------------------------------------------------------------|
| HOME   | moves the cursor to column 0                                          | 
| C= + M | prompts for a line number and moves the cursor to that line           |


## Assembler Syntax
The assembler syntax is very similar to any other major assembler.  For basic
instructions, the canonical 6502 assembly syntax is supported.  That means '$'
denotes a hex value, '#' and immediate operand, parentheses an indirect address,
etc.

### Expressions
Operands, in addition to basic values and labels, may contain an expression,
which is evaluated to a value to generate the operand for the generated
binary.

Expressions may contain addition, subtraction, multiplication, and division 
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

### Formatting
Spacing is not important, but instructions are auto-formatted to be indented
by two spaces.  Labels and directives are, by convention, not indented.  The
formatter will also take care of this.

## Directives
The following directives are supported
### .DB      
Defines a sequence of bytes from the comma-separated list that follows.

Examples:
 |       code        |    generated binary  |
 |-------------------|----------------------|
 | .DB $00, $01, $02 | $00 $01 $02          |
 | .DB "HI",0	     | $48 $49 $00          |

### .DW
deines a sequence of words from the comma-separated list that      |

Examples:
 |       code        |    generated binary     |
 |-------------------|-------------------------|
 | .DW $00, $01, $02 | $00 $00 $01 $00 $02 $00 |

### .EQU
Defines a constant which may be used in expressions
```
.EQU BITMAP $1100
  LDA #$00
  STA BITMAP+20
```

### .INC
Includes a file at the line of the directive. The file is loaded line-by-line
from disk and assembled as if the code was copy/pasted in place of the include directive.
```
.INC "KERNAL.INC"
  LDA #$00
  JSR CHROUT
```


### .ORG
Sets the address to assemble code to 
```
.ORG $1000
; start up code

.ORG $2000
; main code
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

