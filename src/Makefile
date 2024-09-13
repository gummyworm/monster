TARGET=vic20
REGION=NTSC

XVIC=xvic
FE3BIN=fe3vice.bin
PYTHON=python3
CL65=cl65
C1541=c1541
EXOMIZER=exomizer

BOOTSRC=boot.asm 
SRC=$(filter-out $BOOTSRC, $(wildcard *.asm))
TEST_TXTS=$(wildcard tests/*.txt)
TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS)

TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
#TESTS=$(wildcard tests/wg/*.s)
#TEST_INCS=$(wildcard tests/wg/*.inc)

all: test.d64 

# main app binary
monster.prg: $(BOOTSRC) $(SRC) 
	$(CL65) -t $(TARGET) -o $@ --asm-define $(TARGET) --asm-define $(REGION) -C link-$(TARGET).config $^ -Ln labels.txt -v -m map.txt

# create the test disk image
test.d64: monster.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	$(PYTHON) makeboot.py labels.txt monster.prg bootloader.prg masm.prg
	$(EXOMIZER) sfx basic -t55 -n -o boot.prg bootloader.prg   
	#$(EXOMIZER) sfx 0x3028 -t55 -o masm.prg monster.prg -f -n -Di_load_addr=0x1400
	$(C1541) -format test,1 d64 test.d64 -attach test.d64 -write boot.prg -write masm.prg $(addprefix -write , $(TEST_FILES))

start: test.d64
	# $(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -dos1541II JiffyDOS_1541-II.bin -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1542 -10 test.d64
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1542 -10 test.d64

vstart: 
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc +drive10truedrive -virtualdev10

clean:
	rm -f *.prg *.o test.d64
