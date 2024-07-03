TARGET=vic20

XVIC=xvic
FE3BIN=fe3vice.bin
PYTHON=python3
CL65=cl65
C1541=c1541
EXOMIZER=exomizer

BOOTSRC=boot.asm 
SRC=$(filter-out $BOOTSRC, $(wildcard *.asm))
TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
TEST_TXTS=$(wildcard tests/*.txt)
TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS)

all: test.d64 

# main app binary
monster.prg: $(BOOTSRC) $(SRC) 
	$(CL65) -t $(TARGET) -o $@ --asm-define $(TARGET) -C link.config $^ -Ln labels.txt -v -m map.txt

# create the test disk image
test.d64: monster.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	$(PYTHON) makeboot.py labels.txt monster.prg bootloader.prg masm.prg
	$(EXOMIZER) sfx basic -t55 -n -o boot.prg bootloader.prg   
	#$(EXOMIZER) sfx 0x3028 -t55 -o masm.prg monster.prg -f -n -Di_load_addr=0x1400
	$(C1541) -format test,1 d64 test.d64 -attach test.d64 -write boot.prg -write masm.prg $(addprefix -write , $(TEST_FILES))

start: test.d64
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1541 -10 test.d64

vstart: 
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc +drive10truedrive -virtualdev10

clean:
	rm -f *.prg *.o test.d64
