import sys

BASE_ADDR = 0x1000

HIGHLIGHT = "\033[32m"+"\033[1m"
RESET = "\033[0m"

# set to true, will produce a cart binary file (.bin) instead of a
# bootloader .prg
CART = True

print(f'{HIGHLIGHT}creating bootloader and app files...')

if len(sys.argv) != 5:
    print('extracts the boot segments and writes them to a bootloader .PRG file')
    print(f'usage: {sys.argv[0]} <label-file> <infile> <bootfile> <appfile>')
    exit(1)

labelfile = sys.argv[1]
infile = sys.argv[2]
bootfile = sys.argv[3]
appfile = sys.argv[4]

# segments to crunch in the bootloader
bootsegments = ["BANKCODE", "SETUP", "FASTTEXT", "MACROCODE", "SAVESCR", "IRQ", "DATA", "LABELS", "UDGEDIT", "CONSOLE", "COPYBUFF", "RODATA"]
cartsegmentnames = ["CART"]

# open map file and extract the segment to crunch in the bootloader
cartsegments = {}
segments = {}

# read the label file to find the load addresses and sizes of the segments to
# write to their respective target file
f = open(labelfile)
for line in f:
    parts = line.split()
    if len(parts) != 3:
        continue

    if parts[2].endswith('_LOAD__'):
        seg = parts[2].removesuffix('_LOAD__').removeprefix('.__')
        if seg in cartsegmentnames:
            cartsegments[seg] = {
                'load': int(parts[1], 16),
                'size': 0,
            }
        elif seg in bootsegments:
            segments[seg] = {
                'load': int(parts[1], 16),
                'size': 0,
            }

    # add the size to our segment structure
    if parts[2].endswith('_SIZE__'):
        seg = parts[2].removesuffix('_SIZE__').removeprefix('.__')
        if seg in cartsegments:
            cartsegments[seg]['size'] = int(parts[1], 16)
        elif seg in bootsegments:
            segments[seg]['size'] = int(parts[1], 16)

start_addr = BASE_ADDR
stop_addr = BASE_ADDR

cart_header_size = 0

# find the highest load address and set the total size to it + its size
for s in segments.values():
    if s['load'] >= stop_addr:
        stop_addr = s['load'] + s['size']

for s in cartsegments.values():
    cart_header_size = s['size']

# get the total size of the boot segments and cart-boot segments
size = stop_addr - start_addr

if CART:
    print(f'writing cart header ${cart_header_size:02x} bytes');
    print(f'writing cartridge from ${start_addr:02x} to ${stop_addr:02x} (${size:02x}) bytes')
else:
    print(f'writing bootloader from ${start_addr:02x} to ${stop_addr:02x} (${size:02x}) bytes')

with open(infile, 'rb') as file:
    # read the entire mega-file
    buf = file.read()

    bootloader = [];
    if CART:
        start_padding = 0x6000 - size;
        cart = buf[:cart_header_size]
        buf = buf[cart_header_size:]
        end_padding = 0x80000 - 0x8000 - len(buf)+size

        # [ boot code |   padding  | cart header  |  padding  ]
        bootloader = (
            buf[:size] +  # $0000-$xxxx:   main boot code
            bytes([0]*start_padding) + # $xxxx-$6000:   padding
            (cart) +        # $6000-$xxxx:   the cartridge header/boot code
            bytes([0]*(0x2000-cart_header_size)) +   # $6xxx-$8000 padding
            buf[size:] +  # $8000-~$10000 application
            bytes([0]*end_padding)     # $xxxx-$80000: padding (rest of flash image)
        )
    else:
        bootloader = buf[:size]

    print(bootloader[-1])
    with open(bootfile, 'wb') as prg:
        prg.write(bootloader)

if not CART:
    app = buf[size:]
    appstart = 0x2000 
    print(f'writing application file to ${appstart:02x}-${appstart+(len(buf)-size):02x} (${(len(buf)-size):02x} bytes)')
    with open(appfile, 'wb') as prg:
        # write load address
        prg.write(appstart.to_bytes(2, 'little'))
        # write app
        prg.write(buf[size:])

print(f'DONE{RESET}\n')
