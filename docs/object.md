### OBJECT FILE FORMAT TECHNICAL DETAILS

This document describes the layout of the .O (object) file.

The high level overview of the object file is shown in the tables below.  Note
that there are two types of object files: executable (debuggable) and linkable.
The differences between the two are in there headers.  A linkable object file
includes section headers, which describe the section usage for a particular
file.  A debug object file omits this information, but also contains debug
information (line numbers, files, etc.)

#### LINK FORMAT (.O)
Refer to [the linker](linker.md) document for details on the object format.

|  FIELD         | DESCRIPTION                   |
|----------------|-------------------------------|
| Segments       | segments used in the file     |
| Imports        | symbols required to link      |
| Exports        | symbols exported by this file |
| Section Header | # of headers                  |
| Section Table  | info about each section       |
| Sections       | .CODE, .RODATA, .DATA         |

#### DEBUG FORMAT (.D)
|  FIELD         | DESCRIPTION                 |
|----------------|-----------------------------|
| Run address    | .PRG header                 |
| Segments       | the binary (linked) program |
| Section Header | # of headers                |
| Section Table  | info about each section     |
| Sections       | .CODE, .RODATA, .DATA       |
| Debug Sections | .LINE, .FILES, .SYM         |

#### HEADER
.O files begin with a header, which defines things like the number of
sections in the program.  The format of this header is described in this table:

| field        | size | description                        |
|--------------|------|------------------------------------|
| num sections |  1   | the number of sections in the file |

#### SECTIONS HEADERS
Following the main object file header is a table of metadata about each section.
This table contains an entry of the below format for each section:

| field  | size |description                                |
|--------|------|-------------------------------------------|
| offset |   2  | section offset from base in bytes         |
| size   |   2  | the size of the section in bytes          |
| flags  |   1  | R ($01), W ($02), X ($04), or Debug ($00  |

.O files
There are two types of sections: executable and debug.  Executable
sections, when concatenated, comprise the .PRG executable that will
ultimately be distributed when you are done with your program.

The debug sections are _not_ used in the final binary.  They are
only useful to the debugger while you're still working on your code.

### EXECUTABLE SECTIONS

#### .CODE
The code section contains the instructions for the executable
program.  Combined with the data sections (.DATA and .RODATA), it makes
up the program that will be loaded into memory and is the first of the
"executable" sections.

The format of the .CODE sections is very simple: it is the literal
assembled binary.

| field  |  description                                |
|--------|---------------------------------------------|
| binary | the binary instruction data of the program  |

#### .RELCODE
Contains relocation data for the code section.  This section contains
a table with the following layout:

| index | size | field  |  description                                                       |
|-------|------|--------|--------------------------------------------------------------------|
|   0   |  2   | offset | offset to the address to update                                    |
|   2   |  1   | type   | the type of the entry (used to determine how to apply the value)   |
|   4   |  2   | value  | the value to apply at the offset for this entry                    |

The valid values for "type" are defined in this table

| value |   name  |          description                                                |
|-------|---------|---------------------------------------------------------------------|
|   0   |  ADD    |  Adds value to the address at the offset                            |
|   1   |  NEG    |  Negates (twos-complement) the value to the address at the offset   |
|   2   |  MUL    |  Multiplies value with the one at the address at the offset         |
|   3   |  DIV    |  Divides the address at the offset by value                         |
|   4   |  AND    |  ANDs the value at the offset with value                            |
|   5   |  OR     |  ORs the value at the offset with value                             |
|   6   |  EOR    |  Exclusive ORs the value at the offset with value                   |

#### .DATA
The data section contains mutable data (variable) definitions.

Much like the .CODE section, it has a very simple layout: a single
contiguous block representing the state of the variables at load-time

| index | field  |  description                                     |
|-------|--------|--------------------------------------------------|
|   0   | binary | the binary of the initial state of the variables |

#### .RODATA
.RODATA is very similar to .DATA. The only difference is that it
should not be modified by the program.  The debugger can assert that
this is the case, but there are no runtime checks to enforce this
guarantee.

| index | field  |  description                                     |
|-------|--------|--------------------------------------------------|
|   0   | binary | the binary of the state of data constants        |

#### .BSS
The .BSS section contains no data at all. The header, however,
defines where uninitialized user variables are stored.

### DEBUG SECTIONS
Debug sections are stripped when building the .PRG file, but they
contain useful information for the debugger.

#### .SYM
Contains the symobls in the file
|  field  | size  | description               |
|---------|-------|---------------------------|
|  name   |   8   | the name of the symbol    |
| address |   2   | the address of the symbol |

#### .FILES
Contains a list of all the files that are represented in the .LINE
section.  The .LINE section's file ID's represent the offset to
the file in this table

| field | size     |            description            |
|-------|----------|-----------------------------------|
| name  |  1-16    | the 0-terminated name of the file |

#### .LINE
Contains information about how to map lines to/from an address.
Each entry in the table defined in this section represents one line
in the source code.

|  field  | size | description
|---------|------|---------------------------------------|
| address |   2  | the address corresponding to the line |
| file id |   1  | the ID of the file the line is in     |
| line #  |   2  | the line number within the file       |
