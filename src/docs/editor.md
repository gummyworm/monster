## Editor
The editor provideds powerful facilities for editing programs for Monster.
Text is displayed in a 40 column bitmap to provide a higher density interface.

Navigation will be familiar to vi users.  There are also a variety of commands to handle things like assembly, disassembly, etc.

### Command shortcuts
Below are the basic commands along with their associated key combinations. These
commands are available regardless of insertion mode (see the _Editor Modes_ section
below for more info on modes).

|  Key   | Name          |   Description                                                         |
|--------|---------------|-----------------------------------------------------------------------|
| C= + b | Set Breakpoint| sets a breakpoint at the current line                                 |
| C= + c | Refresh       | refrehshes the screen by redrawing the source buffer                  |
| C= + h | Help          | displays the help menu                                                |
| C= + l | List          | list directory, shows the files on the current disk                   |
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

### Ex Commands
The following commands are entered at the "Ex Command" prompt (accessed with the `:` key).
Most accept an argument (as described in each commands description below)

|Key| Name              |   Args                          | Description                                                  |
|---|-------------------|---------------------------------|--------------------------------------------------------------|
| a | Assemble File     | Filename                        | assembles the given filename                                 |
| B | export Binary     | Filename                        | exports the active assembly to a binary file (no .PRG header)|
| d | Start Debugger    | Symbol to debug at (optional)   | begins debugging at the given label                          |
| D | Disassemble       | Start address, End address      | Disassembles the given address range                         |
| e | Edit              | Filename                        | loads the buffer with the contents of the given file         |
| g | Goto              | Symbol to run at (optional)     | executes the program at the address of the given symbol      |
| F | Disassemble File  | Filename                        | disassembles the given file to a new source buffer           |
| P | export .PRG       | Filename                        | exports the active assembly to a .PRG file                   |
| r | Rename            | Name                            | renames the buffer to the given name                         |
| s | Save              | Filename                        | saves the buffer to the given filename                       |
| S | Save All          |   N/A                           | saves all modified buffers that are open currently           |
| x | Scratch           | Filename                        | scratches (deletes) the given filename                       |

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

#### Disassemble :D <start address>, <end address>
Disassembles the contents of the _virtual_ memory between the given range.
e.g. `:D $1001, $1040`.
Expressions may be used in addtion to literal addresses when defining the disassembly range.

This could be useful, for example, if your program generates code and you want to view
the results.

The result of the disassembly is opened in a new buffer, where you can edit it
as you would any of your handwritten source.

Example:
`:D PROC, PEND`

#### Edit :e <filename>
Loads the given filename to a new buffer and activates it.

Example:
`:e HELLO.S`

#### Disassemble from File :F <filename>
Disassembles the contents of the given binary file.
e.g. `:F EXAMPLE.PRG`

The result of the disassembly is opened in a new buffer, where you can edit it
as you would any of your handwritten source.

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


### Editor Modes
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
| C= + <     | Prev Buffer| opens the buffer before the active one (if there is one)               |
| C= + >     | Next Buffer| opens the buffer after the active one (if there is one)                |
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
|    dw      | Delete Word| deletes the next word                                                  |
|    dd      | Delete Line| deletes the next line                                                  |
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

### Line Mapping
Line mapping relies on successful assembly.  A program that
cannot be assembled will not have access to the features
that rely on line mapping.  This includes the symbol viewer,
and the "goto definiton" (gd) command.
