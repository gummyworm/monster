SRC_FILES=$(wildcard *.asm)
SRC=$(filter-out boot.asm test.asm, $(SRC_FILES))
TESTS=$(wildcard tests/*.asm )

monster.prg: boot.asm $(SRC)
	cl65 -o $@ -C link.config $^ -Ln labels.txt -v
	rm *.o

test.prg: test.asm $(SRC) $(TESTS)
	cl65 -o $@ -C link.config $^ -Ln labels.txt
	rm *.o

test: test.prg
	xvic +warp -ntsc -drive9type 1581 -9 testdisk.d81 -autostart test.prg 
start: monster.prg
	# xvic +warp -cartfe fe3vice.bin -memory all -ntsc -drive9type 1541 -truedrive -9 test.d64 -autostart monster.prg 
	xvic +warp -memory all -ntsc -drive9type 1541 -truedrive -9 test.d64 -autostart monster.prg 
clean:
	rm monster.prg
	rm *.o
