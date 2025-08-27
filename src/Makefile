TARGET ?= vic20
REGION ?= NTSC

# default device # (for cart image)
DEFAULT_DEVICE=10

# emulators
XVIC=xvic
X64=x64sc

SCREEN_MODE=soft4x8
#SCREEN_MODE=hard8x8

ASMFLAGS=--asm-define $(TARGET) --asm-define $(REGION) --asm-define $(SCREEN_MODE)

# set KERNAL based on configured TARGET and REGION
ifeq ($(TARGET),vic20)
	ifeq ($(REGION),NTSC)
		KERNAL=JiffyDOS_VIC-20_6.01_NTSC.bin
	endif
	ifeq ($(REGION),PAL)
		KERNAL=JiffyDOS_VIC-20_PAL.bin
	endif
endif
ifeq ($(TARGET),c64)
	KERNAL=JiffyDOS_C64.bin
endif

# get emulator flags based on REGION
ifeq ($(REGION),PAL)
	EMU_REGION="-pal"
endif
ifeq ($(REGION),NTSC)
	EMU_REGION="-ntsc"
endif

# set emulator based on the configured TARGET
ifeq ($(TARGET),vic20)
	EMU=$(XVIC)
endif
ifeq ($(TARGET),c64)
	EMU=$(X64)
endif

FE3BIN=fe3vice.bin
CART_IMAGE=monster.bin	# name of cart binary to produce
TEST_IMAGE=test.bin	# name of test suite cart binary to produce
PYTHON=python3
CL65=cl65
C1541=c1541
EXOMIZER=exomizer

BOOTSRC=$(TARGET)/boot.asm

# platform specific source files
TARGET_SRC=$(filter-out $BOOTSRC, $(wildcard $(TARGET)/*.asm))
MODE_SRC=$(wildcard $(TARGET)/$(SCREEN_MODE)/*.asm)

# platform independent source files
PI_SRC=$(wildcard *.asm)

SRC=$(TARGET_SRC) $(PI_SRC) $(MODE_SRC)
TEST_TXTS=$(wildcard tests/*.txt)

UNIT=$(wildcard *.a65)
TEST_INCS=$(wildcard tests/*.inc)
TESTS=$(wildcard tests/*.s)
#TESTS=$(wildcard tests/wg/*.s)
#TEST_INCS=$(wildcard tests/wg/*.inc)

TEST_FILES=$(TESTS) $(TEST_INCS) $(TEST_TXTS) tests/link tests/hello.o

all: test.d81

disk: monster-disk.prg test.d81

cart: monster.bin

suite: $(TEST_IMAGE)

installer.prg: installer/install.asm
	$(CL65) -t $(TARGET) -o $@ -C installer/link-installer.config $^

monster-cart.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ --asm-define CART --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE) $(ASMFLAGS) -C link-$(TARGET)-cart.config $^ -Ln labels.txt -v -m map.txt

monster-cart-test.prg: $(BOOTSRC) $(SRC) $(UNIT)
	$(CL65) -t $(TARGET) -o $@ --asm-define TEST --asm-define CART --asm-define DEFAULT_DEVICE=$(DEFAULT_DEVICE) $(ASMFLAGS) -C link-$(TARGET)-cart.config $^ -Ln labels.txt -v -m map.txt

$(CART_IMAGE): monster-cart.prg
	$(PYTHON) makecart.py labels.txt monster-cart.prg $(CART_IMAGE)

$(TEST_IMAGE): monster-cart-test.prg
	$(PYTHON) makecart.py labels.txt monster-cart-test.prg $(TEST_IMAGE)

monster-disk.prg: $(BOOTSRC) $(SRC)
	$(CL65) -t $(TARGET) -o $@ $(ASMFLAGS) -C link-$(TARGET)-disk.config $^ -Ln labels.txt -v -m map.txt

boot.prg: monster-disk.prg
ifeq ($(TARGET),vic20)
	$(PYTHON) makeboot.py labels.txt monster-disk.prg bootloader.prg masm.prg
	$(EXOMIZER) sfx basic -t55 -n -o $@ bootloader.prg
endif
ifeq ($(TARGET),c64)
	#$(PYTHON) makeboot.py labels.txt monster-disk.prg bootloader.prg masm.prg
	#$(EXOMIZER) sfx basic -t64 -n -o $@ bootloader.prg masm.prg
	$(EXOMIZER) sfx basic -t64 -n -o $@ monster-disk.prg
endif

test.d81: boot.prg $(TESTS) $(TEST_INCS) $(TEST_TXTS)
ifeq ($(TARGET),vic20)
	# create the test disk image
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write boot.prg -write masm.prg $(addprefix -write , $(TEST_FILES)) -write fe3firmware
endif
ifeq ($(TARGET),c64)
	$(C1541) -format test,1 d81 test.d81 -attach test.d81 -write monster-disk.prg $(addprefix -write , $(TEST_FILES))
endif

install.d81: installer.prg $(CART_IMAGE)
	# create the FE3 installer image
	$(C1541) -format install,1 d81 install.d81 -attach install.d81 -write installer.prg -write $(CART_IMAGE)

start-cart: $(CART_IMAGE) $(KERNAL) test.d81
	# test the cartridge binary
	$(C1541) -format disk,1 d81 empty.d81
	$(XVIC) +warp -sound -cartfe $(CART_IMAGE) -memory all $(EMU_REGION) -drive10type 1581 -10 test.d81 -drive8type 1581 -dos1581 JiffyDOS_1581.bin -8 empty.d81 -moncommands moncommands

start-cart-jiffy: $(CART_IMAGE) $(KERNAL) test.d81
	$(C1541) -format disk,1 d81 empty.d81
	$(XVIC) +warp -kernal $(KERNAL) -sound -cartfe $(CART_IMAGE) -memory all $(EMU_REGION) -drive10type 1581 -10 test.d81 -drive8type 1581 -dos1581 JiffyDOS_1581.bin -8 empty.d81 -moncommands moncommands

start-disk: test.d81
	$(XVIC) $(EMU_REGION) -memory all -cartfe $(FE3BIN) -sound +warp -drive8type 1581 -8 test.d81

start-disk-jiffy20: test.d81
	$(XVIC) +warp -kernal JiffyDOS_VIC-20_6.01_NTSC.bin -sound -cartfe $(FE3BIN) -memory all $(EMU_REGION) -drive8type 1581 -dos1581 JiffyDOS_1581.bin -8 test.d81

start-disk-jiffy64: test.d81
	$(X64) +warp -kernal JiffyDOS_C64.bin -sound $(EMU_REGION) -drive8type 1581 -dos1581 JiffyDOS_1581.bin -reu -reusize 16384 monster-disk.prg

start-install: install.d81
	cp fe3vice.bin fw-test.bin
	$(XVIC) +warp -sound -cartfe fw-test.bin -fewriteback -memory all $(EMU_REGION) -drive8type 1581 -8 install.d81

start-test: $(TEST_IMAGE) test.d81
	$(XVIC) +warp -sound -cartfe $(TEST_IMAGE) -memory all $(EMU_REGION) -drive10type 1581 -10 test.d81 -moncommands moncommands

vstart:
	$(XVIC) +warp -default -cartfe $(FE3BIN) -memory all $(EMU_REGION) +drive10truedrive -virtualdev10 -10 test.d81

clean:
	rm -f *.prg *.o test.d81 $(CART_IMAGE) monster.d81
	rm -rf vic20/*.o
	rm -rf vic20/hard8x8.o/*.o
	rm -rf vic20/soft4x8.o/*.o
	rm -rf c64/*.o
