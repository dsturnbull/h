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

  lda #$01
  sta $d015 ; enable sprite 0
  lda #$00
  sta $d01b ; draw sprite in front
  lda #$05
  sta $d027 ; sprite is green
  lda #$80
  sta $d000 ; set sprite to row 128
  lda #$40
  sta $d001 ; set sprite to col 64

  ; write sprite
  !bin "hikaty.spr"
  ; [255,255,255,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,128,0,1,255,255,255]

  ; set spr01 ptr to start of zpg
  lda #$03
  sta $07f8

_loop:
  ldx #$ff
  ldy #$ff
_x:
  dex
  bne _x
_y:
  dey
  bne _y

  inc $d000
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