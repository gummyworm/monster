TARGET=vic20
REGION=NTSC

# default device # (for cart image)
DEFAULT_DEVICE=10

XVIC=xvic
FE3BIN=fe3vice.bin
CART_IMAGE=monster.bin	# name of cart binary to produce
TEST_IMAGE=test.bin	# name of test suite cart binary to produce
PYTHON=python3
CL65=cl65
C1541=c1541
EXOMIZER=exomizer

BOOTSRC=vic20/boot.asm
TARGET_SRC=$(filter-out $BOOTSRC, $(wildcard $(TARGET)/*.asm))	# platform specific source files
PI_SRC=$(wildcard *.asm)					# platform independent source files
SRC=$(TARGET_SRC) $(PI_SRC)
TEST_TXTS=$(wildcard tests/*.txt)
TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS)

UNIT=$(wildcard *.a65)
#TESTS=$(wildcard tests/*.s)
#TEST_INCS=$(wildcard tests/*.inc)
TESTS=$(wildcard tests/wg/*.s)
TEST_INCS=$(wildcard tests/wg/*.inc)

all: test.d81

disk: monster-disk.prg test.d81

cart: $(CART_IMAGE)

suite: $(TEST_IMAGE)

installer.prg: installer/install.asm
	$(CL65) -t $(TARGET) -o $@ -C installer/link-installer.config $^

monster-disk.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ --asm-define $(TARGET) --asm-define $(REGION) -C link-$(TARGET)-disk.config $^ -Ln labels.txt -v -m map.txt

monster-cart.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ --asm-define $(TARGET) --asm-define $(REGION) --asm-define CART --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE) -C link-$(TARGET)-cart.config $^ -Ln labels.txt -v -m map.txt

monster-cart-test.prg: $(BOOTSRC) $(SRC) $(UNIT)
	$(CL65) -t $(TARGET) -o $@ --asm-define TEST --asm-define $(TARGET) --asm-define $(REGION) --asm-define CART --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE) -C link-$(TARGET)-cart.config $^ -Ln labels.txt -v -m map.txt

$(CART_IMAGE): monster-cart.prg
	$(PYTHON) makecart.py labels.txt monster-cart.prg $(CART_IMAGE)

$(TEST_IMAGE): monster-cart-test.prg
	$(PYTHON) makecart.py labels.txt monster-cart-test.prg $(TEST_IMAGE)

# create the test disk image
test.d81: monster-disk.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS)
	$(PYTHON) makeboot.py labels.txt monster-disk.prg bootloader.prg masm.prg
	$(EXOMIZER) sfx basic -t55 -n -o boot.prg bootloader.prg
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write boot.prg -write masm.prg $(addprefix -write , $(TEST_FILES)) -write fe3firmware

install.d81: installer.prg $(CART_IMAGE)
	$(C1541) -format install,1 d81 install.d81 -attach install.d81 -write installer.prg -write $(CART_IMAGE)

start-cart: $(CART_IMAGE) test.d81
	$(C1541) -format disk,1 d81 empty.d81
	$(XVIC) +warp -sound -cartfe $(CART_IMAGE) -memory all -ntsc -drive10type 1581 -10 test.d81 -drive8type 1581 -8 empty.d81 -moncommands moncommands

start-cart-jiffy: $(CART_IMAGE) test.d81
	$(C1541) -format disk,1 d81 empty.d81
	$(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -sound -cartfe $(CART_IMAGE) -memory all -ntsc -drive10type 1581 -10 test.d81 -drive8type 1581 -dos1581 JiffyDOS_1581.bin -8 empty.d81 -moncommands moncommands

start-disk: test.d81
	$(XVIC) +warp -sound -cartfe $(FE3BIN) -memory all -ntsc -drive8type 1581 -8 test.d81

start-disk-jiffy: test.d81
	$(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -sound -cartfe $(FE3BIN) -memory all -ntsc -drive8type 1581 -dos1581 JiffyDOS_1581.bin -8 test.d81

start-install: install.d81
	cp fe3vice.bin fw-test.bin
	$(XVIC) +warp -sound -cartfe fw-test.bin -fewriteback -memory all -ntsc -drive8type 1581 -8 install.d81

start-test: $(TEST_IMAGE) test.d81
	$(XVIC) +warp -sound -cartfe $(TEST_IMAGE) -memory all -ntsc -drive10type 1581 -10 test.d81 -moncommands moncommands

vstart:
	# $(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -cartfe $(FE3BIN) -memory all -ntsc +drive10truedrive -virtualdev10 -10 test.d81

clean:
	rm -f *.prg *.o test.d81 $(CART_IMAGE)
	rm -rf vic20/*.o
	rm -rf c64/*.o
