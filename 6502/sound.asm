; f0SID= f0 ⋅ 16.94 = 440 ⋅ 16.94 ≈ 7454

_main:
  jsr _start
_noop:
  jmp _noop

_start:
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

  lda #%00000001    ; enable timer
  sta $0320

  rts

_isr:
  ; kbd?
  lda $0302
  bne _isr_kbd

  ; timerA?
  lda $0323
  bne _isr_timer

  rti

_isr_kbd:
  ; space -> restart
  lda $0300
  cmp #' '
  bne _isr_kbd_s
  jsr _start

_isr_kbd_s:
  ; s -> stop sound
  cmp #'s'
  bne _isr_kbd_inc
  lda #%00000000    ; gate off
  sta $0404
  jmp _isr_kbd_ret

_isr_kbd_inc:
  cmp #'.'
  bne _isr_kbd_dec
  lda $0400
  clc
  adc #$f0          ; some note-like amount up
  sta $0400
  lda $0401
  adc #$00
  sta $0401
  jmp _isr_kbd_ret

_isr_kbd_dec:
  cmp #','
  bne _isr_kbd_ret
  lda $0400
  sec               ; no borrow
  sbc #$f0          ; some note-like amount down
  sta $0400
  lda $0401
  sbc #$00
  sta $0401
  jmp _isr_kbd_ret

  ; multiply to sid freq (*16.94)

  ; lda <#$b901
  ; sta $0400
  ; lda >#$b901
  ; sta $0401

_isr_kbd_ret:
  clc
  ror $0302         ; clear kbd
  rti

_isr_timer:
  lda #%00000000    ; off gate
  sta $0404         ; v1 ctrl

  clc
  ror $0323         ; clear timer

  rti

_exit:
  ;     76543210
  lda #%00000000    ; gate + triangle
  sta $0404         ; v1 ctrl