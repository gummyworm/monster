<p align="center">
  <img width="718" alt="Screenshot 2024-09-17 at 7 52 42 PM" src="https://github.com/user-attachments/assets/8a0b14ef-3b51-4dbf-b85f-b272051a85e9">
</p>

## OVERVIEW

<img width="718" alt="Screenshot 2024-09-17 at 7 52 42 PM" src="https://github.com/user-attachments/assets/c8d3dae7-52f2-420b-95de-56edf8c89d2e">


Monster is an all-in-one editor/assembler/debugger for the Commodore Vic-20.
The design philosophy is uncompromising maximalism.  This is in contrast to most
existing Vic-20 assemblers (and most native development tools on 8-bit computers),
which are mostly designed with memory efficiency in mind.


Large RAM expansions have become ubiquitous on the platform, so the philosophy of
this project is to choose feature richness without much concern for the cost to implement.
Virtually any feature that I deem valuable in an assembly IDE is included.

Some of its features are:
 - 40 column bitmap-based editor
 - vi-like keybindings
 - interactive visual debugger
 - breakpoint editor
 - memory viewer/editor
 - file I/O (save/load)
 - directory viewer
 - symbol viewer
 - auto-formatter and realtime syntax checking
 - macro support
 - user program/source/editor isolation
 - many more...

For more information about the major components of Monster, refer to the documents
linked below.

 **[Assembler](docs/assembler.md)**

 **[Editor](docs/editor.md)**

 **[Debugger](docs/debugger.md)**

 **[Monitor](docs/monitor.md)**

---

## REQUIREMENTS

Monster requires a [Final Expansion](https://github.com/edi-z/FE3) to
function.  Much of this RAM is used to store the multiple source code buffers (up to 8),
but it is also used to store debug info and some code.

The banked memory allows the user program to execute in almost complete
isolation.  This means that, although this environment consumes a vast amount of
memory itself, everything except address $9c02 (the bank select register) and
a couply tiny interrupt handlers is preserved when control moves between the
editor and the user program.  Moreso even than small monitor cartridges,
the program itself is virtually unaware of the resident tooling.

## BUILD INSTRUCTIONS

### DEPENDENCIES

 - [cc65](https://github.com/cc65/cc65) (tested with version 2.18)
 - make
 - python3 (any version should do)

The build process also requires `python3` (any version should do).

### BUILD STEPS

Clone this repo `git clone https://github.com/gummyworm/monster.git`

`cd` to the directory you cloned to and run `make` for the appropriate target

Monster can be built for two targets: disk and cartridge.  The disk format
is useful if you want to test on your Final Expansion 3 without erasing its firmware.

The cartridge is a better choice for emulators or power users on real hardware.
It loads significantly faster (although JiffyDOS mostly closes the gap between the two)

To build the disk version, run
`make disk`

To build the cart version, run
`make cart`

## RUNNING

The _Disk_ Makefile generates two PRG's: BOOT.PRG and MASM.PRG.
You may write these to your disk of choice and load Monster as you would any other program on your Vic-20:

```
LOAD "BOOT.PRG",8,1
RUN
```

The _Cart_ Makefile produces a single binary file, which is the cartridge image
for the Final Expansion 3. To flash this to your FE3 for use on real hardware, copy it to
your IEC storage device along with the installer (`install.prg`).

```
LOAD "INSTALLER.PRG",8,1
RUN
```

This will flash Monster to the Final Expansion.  Reset the computer to enter Monster.

If you wish to run Monster in an emulator (VICE), ensure that VICE is installed on your
machine and run `make start-disk` or `make start-cart` from the root of the project to attach the
corresponding disk or cartridge image.

---

### EXAMPLE PROGRAM

Here is a basic hello world program to demonstrate some of the Monster assembler's
features:

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

---

### SOURCE FILES

Monster holds the active source file in memory (for editing), but assembles
all included files directly from file.
Files are stored with $0d line endings, but if you save your file with UNIX
style line-endings ($0a), they will be automatically converted when the file is
read in.

As with any work done with Commodore disk I/O, it is wise to regularly back up your files

---

### LIMITATIONS

#### MEMORY USAGE

The user program may use all available memory from $00 to $7fef. The $11 bytes above $7fef are reserved
for the debugger.  If your program needs these, you can still assemble to them, but you will not be
able to use the debugger.

### BEST PRACTICES

Although Monster is designed to feel modern in all ways, it is, at the end of
the day, running on a computer from 1981.

By following the following practices,
you should have a smooth experience without running into some of the
limitations that you may hit if you are unconcious about them.

#### USE ANONYMOUS LABELS

Anonymous labels take up no space for the label names, only address.  Using
them is much more efficient than labels, and so this should be done for short
branches that don't require much description.  Using too many labels, in the
extreme case, can push your program over the symbol limit.

#### BE CAREFUL WITH "GO"

When debugging, you may be tempted to use the "GO" command to test your program.
Be careful when doing so.
If the CPU encounters a JAM instruction there is no way to recover.
When stepping or tracing, the debugger will take care to ensure that no JAMs are executed.

For things that cannot be traced, like cycle-based effects, you will need to
give full control to the user program to get rid of the the overhead tracing introuduces.
In these cases, you can press RESTORE to return to the debugger (assuming the user program
has not overwritten the NMI vector at $0318 or disabled NMIs).

In the event of a JAM or an unreoverable state (NMI disabled or vector overwritten), you
will need to reset the machine.  Save often.
