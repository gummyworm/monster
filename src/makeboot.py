# MAKEBOOT
# This script splits the app and boot files out from the input "mega-binary"
# The boot segments are combined to make the bootloader and the remaining
# segments are combined to make the app file.
#
# A label file is required to get the start addresses and sizes of each
# segment.  This can be produced with the `-Ln <file>` argument from `cl65`.
#
# Example:
# `python3 makeboot.py labels.txt monster-disk.prg bootloader.prg masm.prg`
# This will take the given "mega-binary" called "monster-disk.prg" and produce
# a boot file named "bootloader.prg" and an application binary named "masm.prg"
# The "labels.txt" file is used to source info about the segments that are
# used to do this.
# In this file, the bootsegments list contains the specific segments that are
# taken out of the mega-binary and placed in the bootloader.

import sys

# segments to store in the bootloader (everything else will be written to
# the app file)
bootsegments = ["BANKCODE", "BANKCODE2", "DEBUGINFO_CODE" "SETUP", "FASTTEXT", "MACROCODE", "VSCREEN", "IRQ", "DATA", "LABELS", "FASTCOPY", "UDGEDIT", "CONSOLE", "OBJCODE", "COPYBUFF", "RODATA"]

if len(sys.argv) != 5:
    print('extracts the boot segments and writes them to a bootloader .PRG file')
    print(f'usage: {sys.argv[0]} <label-file> <infile> <bootfile> <appfile>')
    exit(1)

labelfile = sys.argv[1]
infile = sys.argv[2]
bootfile = sys.argv[3]
appfile = sys.argv[4]

HIGHLIGHT = "\033[32m"+"\033[1m"
RESET = "\033[0m"
print(f'{HIGHLIGHT}creating bootloader and app files...')

# open map file and extract the segment to crunch in the bootloader
segments = {}
f = open(labelfile)
for line in f:
    parts = line.split()
    if len(parts) != 3:
        continue

    if parts[2].endswith('_LOAD__'):
        seg = parts[2].removesuffix('_LOAD__').removeprefix('.__')
        if seg not in bootsegments:
            continue
        segments[seg] = {
            'load': int(parts[1], 16),
            'size': 0,
        }

    if parts[2].endswith('_SIZE__'):
        seg = parts[2].removesuffix('_SIZE__').removeprefix('.__')
        if seg not in bootsegments:
            continue
        segments[seg]['size'] = int(parts[1], 16)

start_addr = 0x1201
stop_addr = 0x1201
for s in segments.values():
    if s['load'] >= stop_addr:
        stop_addr = s['load'] + s['size']

size = stop_addr - start_addr + 2 # +2 for load address

print(f'writing bootloader from ${start_addr:02x} to ${stop_addr:02x} (${size:02x}) bytes')

with open(infile, 'rb') as file:
    buf = file.read()
    bootloader = buf[:size]
    app = buf[size:]

    with open(bootfile, 'wb') as prg:
        prg.write(bootloader)

    appstart = 0x2000
    print(f'writing application file to ${appstart:02x}-${appstart+(len(buf)-size):02x} (${(len(buf)-size):02x} bytes)')
    with open(appfile, 'wb') as prg:
        # write load address
        prg.write(appstart.to_bytes(2, 'little'))
        prg.write(buf[size:])

print(f'DONE{RESET}\n')
