_main:
  lda #$03
  tax

  jsr _loop
  lda #$10

  brk
  nop

_loop:
  iny
  dex
  beq _done
  jmp _loop

_done:
  rts
