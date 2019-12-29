.data

_posH:
  .byte $04
_posL:
  .byte $00
_fill:
  .byte $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff

_sprite_0:
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00011000,%00000000,%00000000
.byte %00001100,%00000000,%00000000
.byte %00000010,%00000000,%00000000
.byte %00000011,%00000000,%00000000
.byte %00000001,%10000000,%00000000
.byte %00000000,%11000000,%00000000
.byte %00000000,%00100000,%00000000
.byte %00000000,%00011000,%00000000
.byte %00000000,%00001100,%00000000
.byte %00000000,%00000011,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000
.byte %00000000,%00000000,%00000000

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
  lda #%00000001
  sta $d027
  lda #%00000011
  sta $d028
  lda #%00000100
  sta $d029
  lda #%00001000
  sta $d02a
  lda #%00001111
  sta $d02b
  lda #%00000010
  sta $d02c
  lda #%00000000
  sta $d02d
  lda #%00000110
  sta $d02e
  lda #$80
  sta $d000 ; set sprite to row 128
  lda #$40
  sta $d001 ; set sprite to col 64

  ; write sprite
  ; !bin "6502/hikaty.spr"

  ; set spr01 ptr
  lda #$81
  sta $07f8
  sta $07f9
  sta $07fa
  sta $07fb
  sta $07fc
  sta $07fd
  sta $07fe
  sta $07ff

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
  inc $d001
  dec $d002
  dec $d003
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