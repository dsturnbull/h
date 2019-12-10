_install_isr:
  lda <_isr
  sta $0314
  lda >_isr
  sta $0315

_start_timer:
  lda #$00
  sta $0322
  lda #$0f
  sta $0321

  lda #%00000001
  sta $0320

  lda #$00 ; exit condition
  sta $40

_main:
  lda $40
  beq _main
  jmp _start_timer

_isr:
  lda #$01
  sta $40
  ;lda #$2e
  ;sta $0301
  ;cli
  rti
