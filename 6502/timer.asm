_install_isr:
  lda <_isr
  sta $0314
  lda >_isr
  sta $0315

_start_timer:
  lda #$ff
  sta $0382
  lda #$ff
  sta $0381

  lda #%00000001
  sta $0380

  lda #$00 ; exit condition
  sta $30

_main:
  lda $30
  beq _main
  jmp _start_timer

_isr:
  pha
  pla
  lda #$01
  sta $30
  lda #$2e
  sta $0301
  rti
