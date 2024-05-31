SRC_FILES=$(wildcard *.asm)
BOOTSRC=boot.asm 
SRC=$(filter-out exodecrunch.asm $BOOTSRC, $(SRC_FILES))
TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
TEST_TXTS=$(wildcard tests/*.txt)

XVIC=xvic
FE3BIN=fe3vice.bin

all: monster.prg udg.prg

# main binary
monster.prg: $(BOOTSRC) $(SRC) 
	cl65 -t vic20 -o $@ -C link.config $^ -Ln labels.txt -v -m map.txt

# udg editor module
udg.prg: modules/udgedit.asm
	cl65 -t vic20 -o $@ -C modules/link.config $^ -Ln labelsudgedit.txt -v -m udgeditmap.txt

# create the test disk image
test.d64: monster.prg udg.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	# split the main binary into bootloader and app
	python3 makeboot.py labels.txt monster.prg boot.prg monster.prg
	# write the bootloader, followed by new app binary, modules, and all tests
	c1541 -format test,1 d64 test.d64 -attach test.d64 -write boot.prg $(addprefix -write ,$^)

test: test.d64
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1541 -10 test.d64 

start: monster.prg
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1541 -10 test.d64 

clean:
	rm -f *.prg *.o test.d64
