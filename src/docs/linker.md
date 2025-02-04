## LINKER TECHNICAL DETAILS

The linker is responsible for taking a number of _object_ files and turning them into a
single executable binary file.

### Object Format
Object files are composed of four main blocks.

A header, a list of imported symbols, a map of exported symbols to their relative
addresses, and finally the object code: a list of instructions that tell the linker
what to emit to the binary output file.

### Link File Format
The link file is responsible for the mapping of _segments_ to _sections_.
Segment usage is defined in the object files and is always relative.  The section
definition is absolute and tells the linker where to place the segments.

The link file contains two blocks of definitions for these two concepts.
The `MEMORY` block defines all the _sections_.
The `SEGMENTS` block defines how the segments defined in the object code map to the memory sections.

#### Example
Below is a simple example of the link configuration file format

```
MEMORY [
SECTIONA:
 start=$0000
 end=$1000
SECTIONB:
 ...
]
SEGMENTS [
 SEGA: load = SECTIONA, run = SECTIONB
]
```

### Header
The first block, the header, gives basic details about the object file.  This helps the
linker determine the

#### Segments
The first block describes the segment usage for the object file.  It details which segments
are used (by name) and how many bytes each segment contains within the object file.

The offset of the segment name is also the ID used in the object code.  For example, the
link directive "set segment 0" will switch the active segment to the first one defined (index 0) in
the object file being assembled

| size |  description
|------|---------------------------------------------------------
|   1  | number of segments used
|  16  | segment 0 name
|  16  | segment 1 name...
|   2  | segment 0 usage (in bytes)

#### Imports
The second block in the object file, after segments, is the _imports_.
Imports enumerate the labels that are referenced in the object file but defined in another
one.

All imports must have a coresponding export of the same name in another
object file at link-time in order to produce the ouput binary.

| size | description
|------|---------------------------------------
|  2   | number of imports
|  32  | import 0 name
|  32  | import 1 name...

### Link Process Overview
To link multiple object files the linker follows the following procedure.

1. Parse link file
  a. get section addresses (where to assemble the object code)
2. Read all object file's headers
3. Calculate segment start addresses in each object file and total segment sizes
  b. sum segment usage in each object file to get total size of each segment
  c. in order defined by link file, set segment start addresses to SECTION start + size of all
     preceding segments
4. Calculate label addresses
  a. absolute exports: define labels at their absolute address
  b. relative exports: define labels at the SEGMENT base address within the object file + relative offset
5. Assemble object files
  a. in order provided, open object file
  b. read imports from header for this object file (skip rest of header)
  c. map imports to their corresponding labels
  d. assemble object code (see object code definition in this doc below)
6. Validate
  a. make sure sections don't overlap

### Object Code Definition
Object code is a primitive instruction set that the linker uses to generate
the final binary program.  The below table describes the opcodes and operands that make up
this instruction set.

|  Instruction   |Opcode| Operands (size)             | Description
|----------------|------|-----------------------------|------------------------------------------------------------------------------------------------------|
| Set Segment    |  1   | name (16)                   | Sets the current segment to the operand.                                                             |
| Emit Bytes     |  2   | num (1)                     | Outputs the given number of bytes (up to 255) The byte sequence immediately follows this instruction |
| Relative Byte  |  3   | symbol id (2), offset (2)   | Outputs a byte that's value is the given symbol plus the given offset                                |
| Relative Word  |  4   | symbol id (2), offset (2)   | Outputs a _word_that's value is the given symbol plus the given offset                               |
| Relative ZP Op |  5   | symbol id (2), offset (2)   | Outputs an _instruction_, one absolute byte and one _byte_, whose value is the given symbol + offset |
| Relative Abs Op|  6   | symbol id (2), offset (2)   | Outputs an _instruction_, one absolute byte and one _word_, whose value is the given symbol + offset |
