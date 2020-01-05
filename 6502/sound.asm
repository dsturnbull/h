; f0SID= f0 ⋅ 16.94 = 440 ⋅ 16.94 ≈ 7454

.data
_mag:
  .byte $10

.code
_main:
  ; $1d1e is 440 Hz
  lda <#$1d1e       ; note l
  sta $d400
  lda >#$1d1e       ; note h
  sta $d401

  lda $01
  sta _mag          ; initial magnitude of frequency changes

  jsr _start
_noop:
  jmp _noop

_start:
  lda #%00000001    ; volume 1
  sta $d418

  ;     76543210
  lda #%11100110    ; attack 8 (100ms), decay 8 (300ms)
  sta $d405

  ;     76543210
  lda #%11111111    ; sustain 8 (100ms), release 8 (300ms)
  sta $d406

  ;     76543210
  lda #%00010001    ; gate + triangle
  sta $d404         ; v1 ctrl

  lda <_isr
  sta $fffe
  lda >_isr
  sta $ffff

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
  bne _kbd_s
  jsr _start

_kbd_s:
  ; s -> stop sound
  cmp #'s'
  bne _kbd_inc_mag
  lda #%00000000     ; gate off
  sta $b404
  jmp _kbd_ret

_kbd_inc_mag:
  cmp #']'
  bne _kbd_dec_mag
  rol _mag           ; some note-like amount up
  jmp _kbd_ret

_kbd_dec_mag:
  cmp #'['
  bne _kbd_inc
  ror _mag           ; some note-like amount down
  jmp _kbd_ret

_kbd_inc:
  cmp #'.'
  bne _kbd_dec
  lda $d400
  clc
  adc _mag          ; some note-like amount up
  sta $d400
  lda $d401
  adc #$00
  sta $d401
  jmp _kbd_ret

_kbd_dec:
  cmp #','
  bne _kbd_ret
  lda $d400
  sec               ; no borrow
  sbc _mag          ; some note-like amount down
  sta $d400
  lda $d401
  sbc #$00
  sta $d401
  jmp _kbd_ret

_kbd_ret:
  clc
  ror $0302         ; clear kbd
  cli
  rti

_isr_timer:
  lda #%00000000    ; off gate
  sta $d404         ; v1 ctrl

  clc
  ror $0323         ; clear timer

  rti