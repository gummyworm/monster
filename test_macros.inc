.ifndef TEST_MACROS_INC
TEST_MACROS_INC=1

__test_fail:
	inc $900f
	jmp *-3

.macro assertz
.local @skip
	beq @skip
	jmp __test_fail
@skip:
.endmacro

.macro assertnz
.local @skip
	bne @skip
	jmp __test_fail
@skip:
.endmacro
.endif
