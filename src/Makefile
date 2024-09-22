TARGET=vic20
REGION=NTSC

# default device # (for cart image)
DEFAULT_DEVICE=10

XVIC=xvic
FE3BIN=fe3vice.bin
CART_IMAGE=monster.bin	# name of cart binary to produce
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

all: test.d81

installer: install.asm
	$(CL65) -t $(TARGET) -o $@ -C link-installer.config $^

monster-disk.prg: $(BOOTSRC) $(SRC) 
	$(CL65) -t $(TARGET) -o $@ --asm-define $(TARGET) --asm-define $(REGION) -C link-$(TARGET)-disk.config $^ -Ln labels.txt -v -m map.txt

monster-cart.prg: $(BOOTSRC) $(SRC) 
	$(CL65) -t $(TARGET) -o $@ --asm-define $(TARGET) --asm-define $(REGION) --asm-define CART --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE) -C link-$(TARGET)-cart.config $^ -Ln labels.txt -v -m map.txt

$(CART_IMAGE): monster-cart.prg
	$(PYTHON) makecart.py labels.txt monster-cart.prg $(CART_IMAGE)

# create the test disk image
test.d81: monster-disk.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	$(PYTHON) makeboot.py labels.txt monster-disk.prg bootloader.prg masm.prg
	$(EXOMIZER) sfx basic -t55 -n -o boot.prg bootloader.prg
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write boot.prg -write masm.prg $(addprefix -write , $(TEST_FILES)) -write fe3firmware

start-cart: $(CART_IMAGE) test.d81
	$(XVIC) +warp -cartfe $(CART_IMAGE) -memory all -ntsc -drive10type 1581 -10 test.d81

start-disk: test.d81
	# $(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -dos1541II JiffyDOS_1541-II.bin -cartfe $(FE3BIN) -memory all -ntsc -drive10type 1542 -10 test.d81
	$(XVIC) +warp -cartfe $(FE3BIN) -memory all -ntsc -drive8type 1581 -8 test.d81

vstart: 
	# $(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -cartfe $(FE3BIN) -memory all -ntsc +drive10truedrive -virtualdev10 -10 test.d81

clean:
	rm -f *.prg *.o test.d81
