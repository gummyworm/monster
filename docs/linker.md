## LINKER TECHNICAL DETAILS

The linker is responsible for taking a number of _object_ files and turning them into a
single executable binary file.

### OBJECT FORMAT
Object files are composed of four main blocks.

A header, a list of imported symbols, a map of exported symbols to their relative
addresses, and finally the object code: a list of instructions that tell the linker
what to emit to the binary output file.

### LINK FILE FORMAT
The link file is responsible for the mapping of _segments_ to _sections_.
Segment usage is defined in the object files and is always relative.  The section
definition is absolute and tells the linker where to place the segments.

The link file contains two blocks of definitions for these two concepts.
The `MEMORY` block defines all the _sections_.
The `SEGMENTS` block defines how the segments defined in the object code map to the memory sections.

#### EXAMPLE
Below is a simple example of the link configuration file format

```
MEMORY [
SECTIONA:
 start=$0400
 end=$1000
 fill=1
SECTIONB:
 start=$1000
 end=$1200
]

SEGMENTS [
 SEGA: load = SECTIONA, run = SECTIONB
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
many segments are used and which symbols are imported (used in this object file) and exported (made
available to be imported in _other_ object files).

| size |  description
|------|---------------------------------------------------------
|   1  | number of segments used
|   2  | number of symbols imported
|   2  | number of symbols exported

The next parts of the header can be broken into logical blocks.  These are stored in the following order:

|  FIELD         | DESCRIPTION                   |
|----------------|-------------------------------|
| SEGMENTS       | segments used in the file     |
| IMPORTS        | symbols required to link      |
| EXPORTS        | symbols exported by this file |

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

#### IMPORTS
The next block in the object file, after segments, is the _imports_.
Imports enumerate the labels that are referenced in the object file but defined in another
one.

All imports must have a coresponding export of the same name in another
object file at link-time in order to produce the ouput binary.

| size | description
|------|---------------------------------------
|  32  | import 0 name
|  32  | import 1 name...

### EXPORTS
The final block in the object file before the object code itself is the EXPORTS block.
This block tells the linker the relative offset that each symbol exported by the object file
is from its respective SEGMENT _within this object file_.

| size | description
|------|---------------------------------------------------------
|  32  | symbol 0 name
|  32  | symbol 1 name
|  2   | symbol 0 segment ID (index in SEGMENTS block)
|  2   | symbol 1 segment ID (index in SEGMENTS block)
|  2   | symbol 0 offset from segment within this object file
|  2   | symbol 1 offset from segment within this object file

### LINK PROCESS OVERVIEW
To link multiple object files the linker follows the following procedure, starting with
the linker configuration file.

* Parse link file
  * get section addresses (where to assemble the object code)

Then, to build the global link context, the linker opens all the object files, one-by-one, and builds the global context.

* Read all object file's headers
* Calculate segment start addresses in each object file and total segment sizes
  * sum segment usage in each object file to get total size of each segment
  * in order defined by link file, set segment start addresses to SECTION start + size of all
     preceding segments

Finally, for each object file, the linker uses the global link context to link it to the output file via the following steps.

* Store global EXPORTs
  * absolute exports: define labels at their absolute address
  * relative exports: define labels at the SEGMENT base address within the object file + relative offset
* Map IMPORTs (generate SYMBOL map)
  * read names of all imported symbols in object header and map them to their EXPORT's address
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
| Emit Bytes     |  2   | num (1)                     | Outputs the given number of bytes (up to 255) The byte sequence immediately follows this instruction |
| Relative Byte  |  3   | symbol id (2), offset (2)   | Outputs a byte that's value is the given symbol plus the given offset                                |
| Relative Word  |  4   | symbol id (2), offset (2)   | Outputs a _word_that's value is the given symbol plus the given offset                               |
| Relative ZP Op |  5   | symbol id (2), offset (2)   | Outputs an _instruction_, one absolute byte and one _byte_, whose value is the given symbol + offset |
| Relative Abs Op|  6   | symbol id (2), offset (2)   | Outputs an _instruction_, one absolute byte and one _word_, whose value is the given symbol + offset |
