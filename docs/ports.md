## PORTING GUIDE

Porting Monster to another 6502-based platform requires implementing a few components.

Here is a basic list of the implementation required:

 - SCREEN:    routines to setup, clear, and save/restore the contents of the screen
 - TEXT:      routines to draw text to the screen
 - AUDIO:     routines to produce beeps
 - MEMORY:    implementations to call procedures throughout the 24 bit address space
 - FILE:      routines to save/load files
 - INTERRUPT: the main IRQ that handles platform specific visual, audio, etc. functionality

### SCREEN
The screen routines provide the implementation for things like clearing the screen or changing its
color. Below list of procedures that must be implemented to do this.

|  PROCEDURE              | DESCRIPTION
|-------------------------|---------------------------------------------------------------------------------
| `__screen_init`         | initialize the display for future screen procedures
| `__screen_clr`          | clear the whole screen
| `__screen_clrcolor`     | reset the entire screen to its default color state
| `__screen_clrpart`      | clear all character rows below the one given in `.A`
| `__screen_clrline`      | clear the character row given in `.A`
| `__screen_rvsline`      | reverse all characters in the row given in `.A`
| `__screen_rvsline_part` | reverse the characters between columns `[.Y, .X]` in the row given in `.A`
| `__screen_save`         | save the contents of the screen to be later restored by `__screen_restore`
| `__screen_restore`      | restore the contents last saved by `__screen_save`

### TEXT
Rendering text only requires one procedure to be implemented.

|  PROCEDURE              | DESCRIPTION
|-------------------------|---------------------------------------------------------------------------------
| `__text_puts`           | renders the text string whose address is given in `.YX` at the row in `.A`

### DRAW
The _DRAW_ routines serve mostly to set the colors for given rows of characters.

Besides `draw::line` these still operate on characters, so you may, for example, reverse the "colored"
characters instead of coloring them.

To implement `draw::line` on a a target that cannot render to a bitmap, you may devise some other way of
representing this.  Note that this routine is used to emphasize the current line while debugging, so
it should be implemented somehow.

|  PROCEDURE              | DESCRIPTION
|-------------------------|---------------------------------------------------------------------------------
| `__draw_hline`          | set the color for the row in `.X` to the color given in `.A`
| `__draw_rvs_underline`  | EOR the _pixel_ row given in `.A`
| `__draw_scrollcolorsu`  | Scrolls all character colors within the rows `[X., .Y]` up by `.A` characters
| `__draw_scrollcolorsd`  | Scrolls all character colors within the rows `[X., .Y]` down by `.A` characters
| `__draw_coloroff`       | Disables color until reenabled by calling another draw routine

### CURSOR
There is only one cursor routine that needs to be implemented.

|  PROCEDURE              | DESCRIPTION
|-------------------------|--------------------------------------------------------------------------------------
| `__cur_toggle`          | toggles the cursor state (on/off) for the cursor position in `(zp::curx, zp::cury)`

### IRQ
The IRQ routines are intended to setup a stable interrupt that runs at 60 Hz.
Stability is only necessary if you choose to do raster-based effects to achieve some of the draw (color)
functionality.

|  PROCEDURE              | DESCRIPTION
|-------------------------|--------------------------------------------------------------------------------------
| `__irq_on`              | Initializes and enables the IRQ for Monster
| `__irq_off`             | Disables the IRQ until reenabled

### FILE
At the moment, the file routines are built for Commodore computers.
See `file.asm` for the routines that must be modified to support other targets.  Also note that
the `$FFD2` KERNAL vector is used (sparingly).  This routine, which outputs a character to the current
file, also needs to be implemented/replaced to support the target.

### AUDIO
Simple audio routines will produce basic beeps when implemented.  These can be implemented as no-ops safely.

Producing the sound should be done in a non-blocking way, so you will probably want to implement a counter/timer
and update the sound registers for the target machine's IRQ.

|  PROCEDURE              | DESCRIPTION
|-------------------------|---------------------------------------------------------------------------------
| `__beep_short`          | Produces a "short" beep sound
| `__long_short`          | Produces a "long" beep sound

