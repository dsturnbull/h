_install_isr:
  lda <_isr
  sta $0314
  lda >_isr
  sta $0315

_main:
  lda #$fe
  sta $41
  sta $42
  sta $43
  lda #$f0
  sta $44

_loop:
  ldx #$04
  lda $40,X
  clc
  adc #$01
  sta $40,X
  dex

_roll:
  lda $40,X
  adc #$00
  sta $40,X
  dex
  bne _roll
  jmp _main

_isr:
  pha
  lda $0300
  sta $0301
  pla
  rti
