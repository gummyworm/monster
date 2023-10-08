SPACE=ord('z')+1
DOT=SPACE+1
SLASH=SPACE+2

text = bytearray("hello world", "utf-8")

for i in range(5-len(text)%5):
    text += bytes([ord('a')-1])

print(".byte ", end='')
for i in range(0, len(text), 5):
    # transform supported special chars
    for j in range(0, 5):
        if(text[i+j] == ord(' ')):
            text[i+j] = SPACE
        elif(text[i+j] == ord('.')):
            text[i+j] = DOT
        elif(text[i+j] == ord('/')):
            text[i+j] = SLASH

    ch0 = (text[i]) - ord('a')+1
    ch1 = (text[i+1]) - ord('a')+1
    ch2 = (text[i+2]) - ord('a')+1
    ch3 = (text[i+3]) - ord('a')+1
    ch4 = (text[i+4]) - ord('a')+1

    out0 = (ch0 << 3) | ((ch1 & 0x1c) >> 2)
    out1 = ((ch1 & 0x03) << 6) | ch2 << 1 | ((ch3) >> 4)
    out2 = ((ch3 & 0x07) << 5) | ch4 & 0x1f

    end = "\n" if (i+5 >= len(text)) else ", "
    print("${a:x}, ${b:x}, ${c:x}".format(a=out0, b=out1, c=out2), end=end)
