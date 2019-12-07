_main:
  lda #$08
  tax

_loop:
  dex
  bne $0200
  jmp _loop
