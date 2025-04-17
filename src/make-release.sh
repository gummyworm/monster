# make the installer
make clean; make installer.prg
cp installer.prg ../bin/

# make the NTSC cart binary
make clean; make cart REGION=NTSC
cp monster.bin ../bin/monster-ntsc.bin

# make the PAL cart binary
make clean; make cart REGION=PAL
cp monster.bin ../bin/monster-pal.bin

# make the NTSC disk image
make clean; make disk REGION=NSTC
c1541 -format monster,1 d81 monster.d81 -attach monster.d81 -write boot.prg -write masm.prg
cp monster.d81 ../bin/monster-ntsc.d81

# make the PAL disk image
make clean; make disk REGION=PAL
c1541 -format monster,1 d81 monster.d81 -attach monster.d81 -write boot.prg -write masm.prg
cp monster.d81 ../bin/monster-pal.d81
