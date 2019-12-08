  ldx #$fe

_main:
  lda #$00
  inx
  sta $0400,X
  sta $0401,X
  sta $0402,X
  sta $0403,X
  dex
  dex
  lda #$ff
  sta $0400,X
  sta $0401,X
  sta $0402,X
  sta $0403,X
  jmp _main
