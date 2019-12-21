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

_loop:
  jmp _loop

_isr:
  lda $0300
;   sta _posH
  ldx _posL
  sta $0400,X
  clc
  inc _posL
  rti