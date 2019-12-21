.data

.code
_fill:
  ldx #$ff
  lda #$ff
  stx $2000
  bne _fill
