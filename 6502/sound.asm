; f0SID= f0 ⋅ 16.94 = 440 ⋅ 16.94 ≈ 7454

_main:
  lda #%00000001    ; volume 1
  sta $0418

  ;     76543210
  lda #%11110010    ; attack 8 (100ms), decay 8 (300ms)
  sta $0405

  ;     76543210
  lda #%11111111    ; sustain 8 (100ms), release 8 (300ms)
  sta $0406

  lda >#$7454       ; note h
  sta $0400
  lda <#$7454       ; note l
  sta $0401

  ;     76543210
  lda #%00010001    ; gate + triangle
  sta $0404         ; v1 ctrl

  ldx #$0f
_hold:
  dex
  bne _hold

  lda #%00000000    ; off gate
  sta $0404         ; v1 ctrl

  ldx #$1f
_wait:
  dex
  bne _wait
  jmp _main
