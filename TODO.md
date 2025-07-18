### TODO V2
- [ ] Update debug info while editing
- [ ] Goto Definition for local labels
  - [ ] Track current scope in editor
  - [ ] Show current scope in status bar (why not)
- [ ] Support variable width characters (8x8 UDG's)
- [ ] Add settings / project configuration file
- [ ] History in monitor
- [ ] Support illegal opcodes

### TODO V1.1
- [ ] Increase copy buffer size to max size of source buffer
- [ ] Save/restore VIA timers while debugging
- [ ] Save/restore raster position while debugging

### TODO V1
- [x] Startup animation
- [x] Symbolic disassembly
- [x] Multicolor support for UDG editor
- [x] Don't paste when it would create line > 40 columns
- [x] Cleanup hack that requires "jmp gui::ret" in GUI (call text::render instead of pushing format strings)
- [x] Allow setting breakpoints in ROM
- [x] debug information rework
  - [x] store relative line #'s (label + x)
  - [x] update debug line #'s in realtime (upon line insertion/delete)
- [x] Finish installer
- [x] yank line (yy)
- [x] symbol viewer improvements
  - [x] sort by address
  - [x] show filename / line
  - [x] navigate to symbol
- [x] Add "Go" at cursor command in debugger
- [x] Add "Return" command in debugger (trace until RTS)
- [x] Fix disassembly
- [x] Support break via NMI in debugger
- [x] Create cartridge target
- [x] Disassemble file command
- [x] Fix visual mode
- [x] Optimize source up/down/goto
  - [x] Do one copy instead of repeated moves (next/prev)
