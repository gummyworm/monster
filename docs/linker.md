## LINKER TECHNICAL DETAILS

The linker is responsible for taking a number of _object_ files and turning them into a
single executable binary file.

### OBJECT FORMAT
Object files are composed of four main blocks.

A header, a symbol map, and finally the object code: a list of instructions that tell the linker
what to emit to the binary output file.

### LINK FILE FORMAT
The link file is responsible for producing the desired layout for the binary program.
Segment usage is defined in the object files and is always relative.  The section
definition is absolute and tells the linker where to place the segments.

The link file contains two blocks of definitions for these two concepts.
The `MEMORY` block defines all the _sections_.
The `SEGMENTS` block defines how the segments defined in the object code map to the memory sections.

The link file must always be named "LINK". Therefore, only 1 link file
may exist on a given disk.  This is the file that the linker will
look for to set up the MEMORY and SEGMENTS prior to linking the object files.

#### EXAMPLE
Below is a simple example of the link configuration file format

```
MEMORY [
SECTIONA:
 START=$0400
 END=$1000
 FILL=1
SECTIONB:
 START=$1000
 END=$1200
]

SEGMENTS [
 SEGA:
   LOAD=SECTIONA
   RUN=SECTIONB
   TYPE=BSS
]
```

### SECTION FLAGS
In the above example, we declared the key "fill" with the value of "1" for SECTIONA.
This is called a "section flag".  The _FILL_ flag tells the linker how to handle unused
memory in a section.  The table below describes the available flags and their names.

Note that any nonzero value for these flags will enable them while the zero value disables them.

| name | description
|------|--------------------------------------------------------------
| FILL |  if '1' fills unused memory in the section with 0's


----

## OBJECT FILE FORMAT
Below is a description of the object file's components.  These are listed in the order they appear in the object file.  These are described in further depth in the rest of this document.

| field          | description
|----------------|-----------------------------------------------
| OBJ HEADER     | basic info (# of sections, # of symbols)
| SECTION HEADER | info about sections (name and size)
| SYMBOLS        | the symbol table
| SECTIONS       | .CODE, .REL, and .DEBUGINFO, tables (for each SECTION)


### HEADER
At thet beginning of the object file is the _header_, which gives basic details about the object file.  The header
simply tells us how many sections are used in the object file and how many symbols are defined.
This makes it easy to determine how much space to allocate at link time.

| size |  description
|------|---------------------------------------------------------
|   1  | number of sections used
|   2  | number of symbols in object file


#### SECTION HEADER
After the header is the SECTION header. This describes the section usage for the object file.  It details which sections
are used (by name) and how many bytes each section contains within the object file.
Whenever a new SEGMENT is defined in the assembly code, a new SECTION is created with a unique ID.

Here is a simple example of the internal sections created for a program that activates the CODE segment, then the DATA one,
followed by the CODE segment again.  Note that each .CODE directive creates a new section referencing the same SEGMENT
```
.CODE   ;SEGMENT("CODE", 1)
asl
.DATA   ;SEGMENT("DATA", 3)
jmp $f00d
.CODE   ;SEGMENT("DATA", 2)
lda #$00
```

Each section has its own block of object code, relocation table, and debug information table.  The offset of the section name is also the ID (index) for these tables.
The SECTION. The linker will concatenate all the SECTIONs that reference the same SEGMENT when the program is linked


| size |  description
|------|---------------------------------------------------------
|  16  | SEGMENT name (where to write SECTION to)
|   2  | size in bytes


#### SYMBOLS
Next is the _symbol_ table. This table contains all labels that are used in the object file.
Symbols that are marked as "GLOBAL" must be resolved at link time.

The symbol table begins with a metadata table followed by the symbols themselves.

| size   | field   | description
|--------|---------|------------------------------
|   1    | type    | (binding information: GLOBAL, LOCAL, ABSOLUTE)
|   1    | section | ID (index in SECTIONS block)
|   2    | address | (absolute or offset from section within its object file)


"type" is set to GLOBAL if an "IMPORT" was found for the symbol in pass 1 of the assembly, LOCAL if it was
defined in pass 1, and ABSOLUTE if a `.eq` directive was found for it in pass 1.

"section ID" is set to the ID of the SECTION that corresponds to the latest `.SEG` directive

And "address" contains the offset from that section (or the value if "type" is `ABSOLUTE`)

### CODE SECTION
Following the symbol table is the object code. Object code is a primitive instruction set that the linker uses to generate the final binary program.
This section contains "records".  See below for more information on these.
A new _section_ is created any time a .SEGMENT directive is encountered (even if the segment alaready exists).

### RELOCATION TABLE DEFINITION
For each _section_, the linker uses an RLE list of offsets that must be adjusted by the current relocation adjustment.
The address at the relocation address is added to the relocation adjustment to get the final address
A 0 indicates the end of this table.

### OTHER SECTIONS
In addition to the required sections, some additional information can be stored in the object files

#### `DEBUG INFO`
This stores the program to evaluate line numbers and addresses within the object file as well as references to which source files were used to create the object file.  This information allows the linker to produce a single mega debug file (or .D file) that contains all the information for the linked program, which allows for source level debugging.

---

### RECORDS
The process of producing an object file is similar to assembling the file.
The `.SEGMENT` directive is allowed. This directive will close the current _SECTION_ in the object file and create a new
one. It will also create a new debug-information section (as the `.ORG` directive does) if debug information is enabled.

Object files also emit "records" instead of the actual program binary. Below are the types of records.
See the tables below for the format of each.
- BYTES: tells the linker to write literal bytes to the resulting binary
- EXPR: tells the linker how to resolve a given symbol to produce an instruction that references one
- FILL: tells the linker to fill n bytes with a given pattern

#### BYTES record format

| field    |  size  | description                                                                         |
|----------|--------|-------------------------------------------------------------------------------------|
|  descr   |   1    | BYTES record identifier                                                             |
|  length  |   1    | the number of bytes to write out                                                    |
|  bytes   | length | the literal bytes to write out at link time                                         |

#### EXPR record format

| field    | size | description                                                                         |
|----------|------|-------------------------------------------------------------------------------------|
|  descr   |  1   | EXPR record identifier                                                              |
|  opcode  |  1   | the opcode of the instruction - e.g. $AD                                            |
|  symbol  |  2   | the symbol target for the instruction                                               |
|  addend  |  2   | the offset from the symbol of the target                                            |
|  info    |  1   | MSB contains post processing info ('<' or '>) and LSB contains operand size (0-2)   |

#### FILL record format

| field    |  size  | description                                                                         |
|----------|--------|-------------------------------------------------------------------------------------|
|  descr   |   1    | FILL record identifier                                                              |
|  length  |   2    | the number of bytes to write out                                                    |
|  patlen  |   1    | the length of the pattern to repeat                                                 |
|  pattern | patlen | the pattern of bytes to fill the record's length with                               |

---

### LINK PROCESS OVERVIEW
#### To link multiple object files the linker follows the following procedure:
* Parse link file
 * get section addresses (where to assemble the object code)
* Read SEGMENT header
  * for each section used in object file: calculate start addresses per object file and total section sizes
  * in order defined by link file, set section start addresses to SECTION start + size of all preceding sections
* Build global symbol table from symbol tables in each object file
   * if a new symbol is encountered, add it to global symbol table
   * if a an existing symbol is encountered, update its status: unresolved, resolved, conflicting (two defintions)
* Assemble object code
   * walk object code and assemble (write BYTES records or resolve EXPR records using symbol table and write them out)
* Peroform relocatation
   * for each section, walk respective relocation table, adding relocation address to the offset at each address to relocate
* Validate
  * make sure sections don't overlap
