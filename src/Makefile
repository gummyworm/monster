SRC_FILES=$(wildcard *.asm)
SRC=$(filter-out boot.asm test.asm, $(SRC_FILES))
TESTS=$(wildcard tests/*.s)
TEST_INCS=$(wildcard tests/*.inc)
TEST_TXTS=$(wildcard tests/*.txt)

# XVIC=xvic
# FE3BIN=fe3vice.bin

all: monster.prg udg.prg

################################################################################
# main binary
monster.prg: boot.asm $(SRC) 
	cl65 -t vic20 -o $@ -C link.config $^ -Ln labels.txt -v -m map.txt
	rm *.o


################################################################################
# modules
udg.prg: modules/udgedit.asm
	cl65 -t vic20 -o $@ -C modules/link.config $^ -Ln labelsudgedit.txt -v -m udgeditmap.txt
	rm *.o

################################################################################
# create the test disk image
test.d64: monster.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS) udg.prg
	c1541 -format test,1 d64 test.d64 -attach test.d64 $(addprefix -write ,$^)

################################################################################
# test commands
# run the assembler with the test disk image
test: test.d64
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1541 -10 test.d64 

start: monster.prg
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1541 -10 test.d64 
	#xvic +warp -memory all -ntsc -drive9type 1541 -truedrive -9 test.d64 -autostart monster.prg 

################################################################################
# clean
clean:
	rm test.d64
	rm monster.prg
	rm udg.prg
	rm *.o
