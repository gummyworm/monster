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
|   1  | number of exports in object file
|   2  | number of imports in object file


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


### SYMBOLS
Next is the _symbol_ table. This table contains all labels that are used in the object file.

The symbol table contains three parts: locals, imports, and exports.

#### LOCAL SYMBOLS
Local symbols are the ones referenced in the relocation table(s) for the object file.
The name "local" is somewhat misleading because imported (global) symbols will reside in here as well.  These are identifiable by the "section" value: `SEC_UNDEF`.

The local symbol table begins with a metadata table followed by the symbols themselves.

| size | field   | description
|------|---------|--------------------------------------------------------------
|   1  | section | ID (index in SECTIONS block)
|   2  | address | (absolute or offset from section within its object file)

"section ID" is set to the ID of the SECTION that corresponds to the latest `.SEG` directive.
The value $ff means `ABSOLUTE`.  Symbols with this section are constants- their address is not relative to any section.

"address" contains the offset from that section (or the value if section is `ABSOLUTE`)

#### IMPORTS
The "IMPORTS" block of symbols contains the names of all symbols imported by the objet file and their index in the "LOCALS" table.

Since these are external, we don't know what their index will be when we generate the object code.  Instead they're identified by name.

Resolving these names to their section/offset occurs by reading the EXPORTS block for all object files, which is done in the linker's _first_ pass.
The linker will then map the resolved section/address to the corresponding index for the symbol in the "LOCALS" block.  This map index is stored in this table as well.  This mapping happens in the linker's _second_ pass.

| size  |  field   |  description
|-------|----------|----------------------------------------------
| 1-33  |  name    | the symbol name as a 0-terminated string
|   2   |  index   | the index used for this symbol in the "locals" table

#### EXPORTS
The "EXPORTS" block defines symbols that are used (or may be used) in other object files.  They tell the linker where to define the labels that will be used during linkage.

| size   | field  | description
|--------|---------|------------------------------------------------
|  1-33  | name    | the name of the symbol as a 0-terminated string
|  2     | section | the index of the section in this object file the symbol is defined in
|  2     | offset  | the offset of the symbol within the section

### SECTIONS
Following the symbol table is a list of one or more SECTIONs (the number is defined in the .O file header).
These contain the object code, the relocation table, and the debug information needed to produce the linked
debug or program file.

SECTIONs begin with their own header, which defines the size and position of these various tables.

| field           | size | description
|-----------------|------|-------------------------------------------------------
|  name           |  8   | name of the SEGMENT for this section
|  info           |  1   | info byte: zeropage/absolute etc.
|  code size      |  2   | size of the object-code binary table for the section
|  reloc size     |  2   | size of the relocation table for the section

The info bitfield for the section is in the following format
| field           | bit(s) | description
|-----------------|------|-------------------------------------------------------
|  size           |  0   | 0=zeropage, 1=absolute

A new _section_ is created any time a .SEGMENT directive is encountered (even if the segment alaready exists).

### RELOCATION TABLES
For each _section_, the linker contains a table of _relocation info_.
This table is made up of a number of entries, each describes how to relocate a byte or word
in the section.  Relocations can either be _section-relative_ or _symobl-relative_.

The following table describes the relocation format in detail.

| field             | size | description
|-------------------|------|-------------------------------------------------------------------------------------
| info              |  1   | bitfield of information about the relocation entry
| offset            |  2   | offset from section to relocate
| symbol/section id |  2   | the symbol index in the symbol table (for symbol-relative relocation) or section for section-relative
| addend MSB*       |  1   | explicit MSB for addend (only when applying post-processing)

\* see details below for when this field is included

`info` is a bitfield with the following values:

| field      | bit(s) | description
|------------|--------|---------------------------------------------------------------------
| size       |   0    | size of target value to modify 0=1 byte, 1=2 bytes
| mode       |   1    | what to relocate relative to: 0=section relative, 1=symbol relative
| postproc   |  2-3   | post-processing to apply after adding addend (0=NONE, 1=LSB, 2=MSB)

To apply the relocation table to a section, we walk the table, go to the address of that section's
base + the offset for each table entry, and depending on the value of "mode" in the "info" field:
 - "section relative" (0) -look up section base address and add addend to it
 - "symbol relative" (1) - look up the symbol address and add addend to it

Finally, we apply post-processing (bits 2-3) in the info byte, if necessary.

The "addend" is the value stored in the object code as the operand for the instruction we are
relocating.

The exception is for relocation entries that contain post-processing.  For these,
the intermediate value may be greater than $ff, so we need to encode a full 16-bit addend for the
the 1 byte target.  For example: `LDA #<(LABEL + 500)` requires a 16-bit addend (500) to calculate
the final 8-bit target.  The LSB of this addend is stored in the instruction stream, but the
MSB is stored in an extra byte at the end of the relocation entry for that record.

### DEBUG INFO
This table stores the program to evaluate line numbers and addresses within the object file as well as references to which source files were used to create the object file.  This information allows the linker to produce a single mega debug file (or .D file) that contains all the information for the linked program, which allows for source level debugging.

The format for debug information is described in further detail in debug-info.md.

---

### LINK PROCESS OVERVIEW
To link multiple object files the linker follows the following procedure:
* Parse link config file
 * Get section addresses (where to assemble the object code)
* Pass 1: build link context
  * Read all SECTION headers from object files
      * In order defined by link file, set section start addresses to SECTION start + size of all preceding sections Read SEGMENT header
    * Build global symbol table
       * Read EXPORT table in each object file
       * Add name, section, and section offset for each EXPORT
* Pass 2: link objects
    * Build object-local context
       * Build map of symbol index to their values using global symbol table
    * Store object code to the current address for each section
    * Walk relocation table, for each section, and apply the the relocations described using the global symbol table
* Validate
  * make sure sections don't overlap
