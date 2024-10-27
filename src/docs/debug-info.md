## DEBUG INFORMATION

Debug information is stored in a few tables as described below. At a high level, these are:
  - a "file table" for mapping file id's to file name 
  - a series of "blocks" that store ranges of addresses and lines for blocks of code
  - a series of _line programs_, state machines that resolve to addresses and line numbers when executed

### File Table
The file IDs table maps filenames (implicitly) to an ID.  The ID is simply
the index of the filename in this table

|  size    | description                     |
|----------|---------------------------------|
|   16     | filename 0                      |
|   16     | filename 1                      |
|   16     | ...                             |

### Blocks
To simplify the storage of lines in cases like noncontiguous addresses (although these can be handled in a single block) and 
multi-file programs, mappings are broken down into "blocks".

Each block defines a file id (blocks will always reference one file only), and a range of lines and addresses.
The table below describes the layout of a block

|  Field       | Size  | Description                                     |
|--------------|-------|-------------------------------------------------|
| base         |  2    | the address that this block begins at           |
| top address  |  2    | the highest address represented by the block + 1|
| base line    |  2    | the lowest line number represented by the block |
| # of lines   |  2    | the highest line number represented by the block|
| file id      |  1    | filename 0                                      |
| program      |  2    | address of the line mapping program             |
| program end  |  2    | address of the end of the line program          |

Note that _top address_ is the last address in the block + 1.
In other words, the range represented starts at the base address (inclusive)
and ends at the top address (exclusive): `[base, top)`.

In the assembler, the two psuedo-ops that force the creation of a block are:
  - including a file (`.INC`)
  - setting the address (`.ORG`)

### Line Program
The line program facilitates compact mapping of line numbers to addresses.
It is a state machine with the following state:
 - line number
 - address (or program counter)
 - filename (as file id)

Commands modify this state in order to produce the address <-> line mapping.
There are two types of instructions to handle this process: "basic" and "extended".
Each type is detailed below.

### Basic Instructions

The most basic operation, which is so common that it requires no special opcode, adds a small
offset to the current line number AND program counter.
Bits 0-3 contain the line offset to add and bits 3-7 contain the PC offset.

### Extended Instructions

For cases when a small offset to either the line or address is not enough (e.g. a macro 
that expands to more than 16 bytes), these instructions are required.
The extended instrutions begin with a $00 value prefix, which is not a valid basic instruction
because at least one of the line or address states must be advanced for each entry in the line table.

Below is the list of extended commands and their effects.

| Command Name  |  Operand  | Code | Operand Size| Effect                                            |
|---------------|-----------|------|-------------|---------------------------------------------------|
| `SET_ADDRESS` | address   |  $01 | 2           | Sets the address to the given absolute address    |
| `SET_FILE`    | file id   |  $02 | 1           | Sets the file to the given file                   |
| `SET_LINE`    | offset    |  $03 | 2           | Sets the line to the given line number            |
| `ADVANCE_LINE`| offset    |  $04 | 2           | Moves the line by the given signed offset         |
| `ADVANCE_PC`  | offset    |  $05 | 2           | Moves the address by the given signed offset      |
| `SET_PC`      | offset    |  $06 | 2           | Moves the address by the given signed offset      |
| `END`         |  -        |  $00 | 0           | Marks the end of the program (block)              |

`ADVANCE_LINE` and `ADVANCE_PC` can be used to handle bigger "jumps" in the line mapping, but they're
also useful when adding a line to the existing mapping without fully recompiling the line program.
This is the case when editing a file, which produces realtime updates to the debug info.
When the program is assembled, the line-program is recompiled into a more optimal format.

### Example

```
.org $1000
    lda #$00
    sta $900f
loop:
    jmp loop
```

The block header for this file sets up the file ID, line number, and (base) address
This program begins at line 1, starts at address $1000, and has 5 lines.

| Instruction | Description           |
|-------------|-----------------------|
| $00 $10     | base address          |
| $09 $10     | top address           |
| $00 02      | base line             |
| $00 05      | number of lines       |
| $01         | file ID               |
| $00 $20     | line program address  |
 
The state machine is initialized with values from the block header, so we enter the program with:
  - line number: 2
  - file ID: 1
  - address: $1000

Note that this corresponds to `lda #$00` in our code.  If we are looking for the address $1000 or line
number 2, we never actually execute any instructions in the line program as the initialization has 
already landed us on our target.

Our program is defined as:

| Value | Description                                                 |
|-------|-------------------------------------------------------------|
| $22   | move line by 2 and address by 2 (we are now at `sta $900f`) |
| $32   | move line by 2 and address by 3 (we are now at `jmp loop`)  |

## Generation

The flow for generating debug information is:
 1. begin block
 2. add lines
 3. end block

Below are the specific routines used to accomplish this:

1. `dbgi::startblock_<symbol/file/addr>`
`startblock` along with the appropriate suffix is called upon encountering a global label (symbol), including a file (file), or setting the ORG (addr).

This routine copies the block to the work buffer, where lines can be inserted or removed.

2. `dbgi::addline` / `dbgi::removeline`
These routines add or remove commands from the line program for the active block.  For both adding and removing, the first step is finding the command in the line program that references the line to be added/deleted.

2a. `dbgi::addline`
Appends command(s) that maps to the newly added line. During assembly of a full buffer these will likey be regular "basic" commands as lines/addresses will be small offsets from one line to the next.
If the user is inserting lines, these will likely be extended commands (ADVANCE PC and ADVANCE LINE) as the user may be adding lines in the middle of the block, which will require a signed offset.

2b. `dbgi::removeline`
Deletes the command that moves to the line being deleted.

3. `dbgi::endblock`
This routine copies the block back to its original location.  If necessary (the block has
grown to overlap with another block), the other blocks are shifted to make room for the new block.

## Hot Reloading
When a line is inserted or deleted, the base line for all subsequent lines must be incremented or decremented. The algorithms for doing this are described below

#### Insert
Find all blocks with a start line greater than the line being inserted and increment them

#### Delete Line
Find all blocks with a start line greater than the line being inserted and decrement them

---

## Using Debug Information
Once debug information is generated, its primary function is to map lines to addresses and vise-versa.

### Mapping Address to Line

To map a given address to its corresponding line number, the line program for the block that contains
its address is executed.  Once the line program reaches a PC value equal to the one that is sought,
the current line number is returned.

### Mapping Line to Address

The line to address mapping works just address to line mapping.  The first block containing the
file/address range being sought is executed.  When the line program's line number is equal to the one
requested, the PC value for the program at that point is returned.

