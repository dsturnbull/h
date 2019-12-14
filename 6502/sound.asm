; f0SID= f0 ⋅ 16.94 = 440 ⋅ 16.94 ≈ 7454

_main:
  lda #%00000001    ; volume 1
  sta $0418

  ;     76543210
  lda #%11100110    ; attack 8 (100ms), decay 8 (300ms)
  sta $0405

  ;     76543210
  lda #%11111111    ; sustain 8 (100ms), release 8 (300ms)
  sta $0406

; $1d1e is 440 Hz
  lda <#$1d1e       ; note l
  sta $0400
  lda >#$1d1e       ; note h
  sta $0401

  ;     76543210
  lda #%00010001    ; gate + triangle
  sta $0404         ; v1 ctrl

  lda <_isr
  sta $0314
  lda >_isr
  sta $0315

  ; count $000f
  lda #$01
  sta $0322
  lda #$a4
  sta $0321

  lda #$00
  sta $60           ; break condition

  lda #%00000001    ; enable timer
  sta $0320

_wait:
  lda $60
  bne _exit
  jmp _wait

_isr:
  lda #%00000000    ; off gate
  sta $0404         ; v1 ctrl

  lda #$01          ; exit 1
  sta $60

  rti

_exit:
  ;     76543210
  lda #%00000000    ; gate + triangle
  sta $0404         ; v1 ctrl
_noop:
  jmp _noop