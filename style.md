## STYLE GUIDE

There is no one standard for formatting of 6502 assembly.  This guide details the chosen
style for the Monster codebase along with some justification (or at least _my_ thought
process) for it.

### INDENTATION

Some people use levels of indentation to correspond with the level of iteration
.e.g.
```
my_proc:
	ldy #40
	outer:
		ldx #30
		inner:
			sta $100,x
			dex
			bne inner
		dey
		bne outer
```

I used to use this style, but over time I found the nested indentation to be more annoying than useful.
It looks fine for basic nested loops, but it makes scanning the left side of the source awkward.
It also makes comments (which are especially useful in arcane assembly inner loops) very cramped.
On the other hand, it can be nice for visually skimming a routine's complexity, but for most routines,
in my opinion, this is not the major readability concern.

Below is the above loop without indentation.  You can decide yourself which you like, but Monster
prefers this flattened version.

```
my_proc
	ldy #40
outer:
	ldx #30
inner:
	sta $100,x
	dex
	bne inner
	dey
	bne outer
```

#### TABS

Tabs are preferred to spaces.  Monster tabs are 8 characters wide and a single tab should
indent all opcodes.  Comments should be indented by as many tabs are needed so that the comment
does not overlap another instruction in a given "block".  A "block" is a group of instructions
separated by newlines on either side.

For example
Good block:

```
@loop:
        asl	        ; okay
	rol hello,x
```

Bad block:

```
@loop:
        asl	; okay
	rol hello,x
```

### COMMENTS

Comments are essential in assembly and this is one area where I think a strong opinion
can make a big difference in improving the usability of a 6502 codebase.

Many programmers use a convention of writing the inputs, outputs, and "clobbered" registers
and/or memory locations.  Since there is no calling convention or ABI in assembly (and,
while you can try to implement one you will almost certainly want to break it at
some point - the whole point of using ASM is doing tricky things) you will need
to rely on accurate and detailed comments to provide the user of your routines the
information they need to call your procedure and protect any state it might affect.

As I said, this is something that can be hard to be consistent with.  There is
not standard for formatting the above information even if most have some general
understanding that such info is a very good thing to provide.

```
;*******************************************************************************
; MVSPR
; moves a sprite to a given position
; IN:
;  - .A: id of sprite to move
;  - .Y: Y position to move sprite to
;  - .X: X position to move sprite to
; OUT:
;  - .C: clear if sprite was successfully moved
; CLOBBERS:
;  - .A, .X, .Y
;  - [sprite0, sprite0+4)
.proc mvspr
```

The comment begins with a banner.  Banners have been standard practice in
assembly language since time began, and I still find them useful in dividing
routines.  Unlike high level programming languages, there are no braces or
indentation to clearly separate code, so the banner makes it clear that the
label that follows (or `.proc` declaration or whatever other sugar your assembler
offers) is a special entrypoint to the following code and not _just_ another
label.

I have made the banner 80 characters wide here.  This is also the maximum width
for any single line of code.

Following is a brief description of the routine.  This should be a plain English
description of what the routine does.

Below that, the registers and memory locations used as parameters to the
function are defined in what we may call the "in" block.

Below _that_ is a similar block defining the data of interest that is
returned to the caller and in what registers or memory addresses it is
done.

Finally, the comment block ends with a section called "clobbers".  This
lists the registers and meomory locations that are affected by routine. This
may be RAM locations that the procedure uses to perform intermediate calculations, etc.
This `CLOBBERS` section is optional.  It is useful, but unfortunately I rarely use it
in the Monster code.
