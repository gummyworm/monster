.ifdef vic20
.ifdef hard8x8
	.include "vic20/hard8x8/layout.inc"
.else
	.include "vic20/soft4x8/layout.inc"
.endif
.endif

.ifdef c64
	.include "c64/layout.inc"
.endif
