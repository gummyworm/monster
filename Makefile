SRC=$(wildcard *.asm)
monster.prg: $(SRC)
	cl65 -o $@ -C link.config $^ -Ln labels.txt
	rm *.o

test:
	xvic -memory all -ntsc monster.prg
clean:
	rm monster.prg
	rm *.o
