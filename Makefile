SRC_FILES=$(wildcard *.asm)
SRC=$(filter-out boot.asm test.asm, $(SRC_FILES))
TESTS=$(wildcard tests/*.s)

monster.prg: boot.asm $(SRC) 
	cl65 -o $@ -C link.config $^ -Ln labels.txt -v -m map.txt
	rm *.o

# create the test disk image
test.d64: $(TESTS)
	c1541 -format test,1 d64 test.d64 -attach test.d64 $(addprefix -write ,$^)

# run the assembler with the test disk image
test: test.prg test.d64
	xvic +warp -cartfe fe3vice.bin -memory all -ntsc -drive10type 1541 -truedrive -10 test.d64 -autostart monster.prg 

start: monster.prg
	xvic +warp -cartfe fe3vice.bin -memory all -ntsc -drive10type 1541 -truedrive -10 test.d64 -autostart monster.prg 
	#xvic +warp -memory all -ntsc -drive9type 1541 -truedrive -9 test.d64 -autostart monster.prg 
clean:
	rm monster.prg
	rm *.o
