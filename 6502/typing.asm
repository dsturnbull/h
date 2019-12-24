.data

_posH:
  .byte $04
_posL:
  .byte $00

.code

  lda <_isr
  sta $fffe
  lda >_isr
  sta $ffff

  ldx #$ff
_write_1:
  lda #$6e
  sta $d7ff,X
  dex
  bne _write_1

_loop:
  jmp _loop

;   ldx #$ff      ; 2
; _inc:
;   inc $d7ff,X   ; 7
;   dex           ; 2
;   bne _inc      ; 2
;   jmp _loop     ; 3

_isr:
  pha
  txa
  pha
  tya
  pha

  lda $0300
;   sta _posH
  ldx _posL
  sta $0400,X
  clc
  inc _posL

  pla
  tay
  pla
  tax
  pla

  rti