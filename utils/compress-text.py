import sys

SPACE=ord('z')+1
DOT=SPACE+1
SLASH=SPACE+2

if len(sys.argv) != 2:
    print("usage {prog}: <string-to-convert>".format(prog=sys.argv[0]))
    exit(0)

text = bytearray(sys.argv[1], "utf-8")

for i in range(3-len(text)%3):
    text += bytes([ord('a')-1])

print("\n.byte ", end='')

size = 0
for i in range(0, len(text), 3):
    # transform supported special chars
    for j in range(0, 3):
        if(text[i+j] == ord(' ')):
            text[i+j] = SPACE
        elif(text[i+j] == ord('.')):
            text[i+j] = DOT
        elif(text[i+j] == ord('/')):
            text[i+j] = SLASH

    ch0 = (text[i]) - ord('a')+1
    ch1 = (text[i+1]) - ord('a')+1
    ch2 = (text[i+2]) - ord('a')+1

    out0 = (ch0 << 3) | ((ch1 & 0x1c) >> 2)
    out1 = ((ch1 & 0x03) << 6) | (ch2 & 0x1f)

    end = "\n" if (i+3 >= len(text)) else ", "
    print("${a:x}, ${b:x}".format(a=out0, b=out1), end=end)

    size += 2

print("")
print("input size: {size}".format(size=len(sys.argv[1])))
print("output size: {size}".format(size=size))
