.data

test8 = $20
test16 = $7020

.code
_main:
  lda #test8
  jmp (test16)
  ldx #test8
  ldx #test8+1
  bne _sep
  jmp _sep
  jmp test16
  lda test8,X
  lda test8,Y
  lda test16,X
  lda test16,Y

  lda <#test16
  lda >#test16
  sta (test8,X)
  sta (test8),Y

_sep:
  brk