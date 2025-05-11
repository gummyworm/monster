## LINKER TECHNICAL DETAILS

The linker is responsible for taking a number of _object_ files and turning them into a
single executable binary file.

### OBJECT FORMAT
Object files are composed of four main blocks.

A header, a symbol map, and finally the object code: a list of instructions that tell the linker
what to emit to the binary output file.

### LINK FILE FORMAT
The link file is responsible for the mapping of _segments_ to _sections_.
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


### HEADER
The first block, the header, gives basic details about the object file.  This tells the linker how
many segments are used and which symbols are imported (_available_ in other object files) or exported (_used_ in other object files).

| size |  description
|------|---------------------------------------------------------
|   1  | number of segments used
|   2  | number of symbols in object file

The next parts of the header can be broken into logical blocks.  These are stored in the following order:

|  FIELD         | DESCRIPTION                   |
|----------------|-------------------------------|
| SEGMENTS       | segments used in the file     |
| SYMBOLS        | symbols required to link      |

Further details on each of these is described in the sections below.

#### SEGMENTS
After the object header, the SEGMENTS block describes the segment usage for the object file.  It details which segments
are used (by name) and how many bytes each segment contains within the object file.

The offset of the segment name is also the ID used in the object code.  For example, the
link directive "set segment 0" will switch the active segment to the first one defined (index 0) in
the object file being assembled

| size |  description
|------|---------------------------------------------------------
|  16  | segment 0 name
|  16  | segment 1 name...
|   2  | segment 0 usage (in bytes)
|   2  | segment 1 usage (in bytes)

#### SYMBOLS
The next block in the object file, after segments, is the _symbols_.
This table contains all labels that are imported and exported from the object file.

All imports must have a coresponding export of the same name in another
object file at link-time in order to produce the ouput binary.

| size   | description
|--------|---------------------------------------
|  1-32  | symbol 0 name
|   1    | symbol 0 type (IMPORT, RELATIVEEXPORT, RELATIVE IMPORT)
|   1    | symbol 0 segment ID (index in SEGMENTS block)
|   2    | symbol 0 offset from segment within this object file
|  1-32  | symbol 1 name
|   1    | symbol 1 type (IMPORT, RELATIVEEXPORT, RELATIVE IMPORT)
|   1    | symbol 1 segment ID (index in SEGMENTS block)
|   2    | symbol 1 offset from segment within this object file

### LINK PROCESS OVERVIEW
#### To link multiple object files the linker follows the following procedure, starting with
the linker configuration file.

* Parse link file
  * get section addresses (where to assemble the object code)

#### Then, to build the global link context, the linker opens all the object files, one-by-one, and builds the global context.

* Read all object file's headers
* Calculate segment start addresses in each object file and total segment sizes
  * sum segment usage in each object file to get total size of each segment
  * in order defined by link file, set segment start addresses to SECTION start + size of all
     preceding segments
* Build global symbol table from symbol tables in each object file
   * each IMPORT symbol that was found should contain a corresponding EXPORT

#### Finally, for each object file, the linker uses the global link context to link it to the output file via the following steps.

* Map IMPORTs/EXPORTs (generate global SYMBOL map)
  * read names of all symbols in object header and map them to their EXPORT's address
* Map SEGMENTs (generate SEGMENT map)
  * read names of all segments in object header and map them to their current address
* Assemble object code (see object code definition in this doc below)
* Validate
  * make sure sections don't overlap

### OBJECT CODE DEFINITION
Object code is a primitive instruction set that the linker uses to generate
the final binary program.  The below table describes the opcodes and operands that make up
this instruction set.

|  Instruction   |Opcode| Operands (size)             | Description
|----------------|------|-----------------------------|------------------------------------------------------------------------------------------------------|
| Set Segment    |  1   | name (16)                   | Sets the current segment to the operand.                                                             |
| Emit Byte      |  2   |                             | Outputs one byte, which immediately follows this instruction opcode                                  |
| Emit Bytes     |  3   | num (1)                     | Outputs the given number of bytes (up to 255) The byte sequence immediately follows this instruction |
| Relative Byte  |  4   | symbol id (2), addend (2)   | Outputs a byte that's value is the given symbol plus the given addend                                |
| Relative Word  |  5   | symbol id (2), addend (2)   | Outputs a _word_that's value is the given symbol plus the given addend                               |
| Relative ZP Op |  6   | symbol id (2), addend (2)   | Outputs an _instruction_, one absolute byte and one _byte_, whose value is the given symbol + addend |
| Relative Abs Op|  7   | symbol id (2), addend (2)   | Outputs an _instruction_, one absolute byte and one _word_, whose value is the given symbol + addend |
