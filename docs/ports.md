r# PORTING GUIDE

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
