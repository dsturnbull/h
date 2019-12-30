.data

_posH:
  .byte $04
_posL:
  .byte $00

.org $2000
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
.byte $00
!bin "6502/hikaty.spr"
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

  ldx #$00
  ldy #$00
  lda #$80
_set_sprite_pos:
  sta $d000,X
  sta $d001,X
  inx
  inx
  iny
  cpy #$10
  bne _set_sprite_pos

  ; set spr ptrs
  ldx #$00
_load_spr_ptr:
  lda #$80
  sta $07f8,X
  inx
  cpx #$08
  bne _load_spr_ptr

_main:
  lda #$00
_loop:
  ldx #$ff
  ldy #$ff
_x:
  dex
  bne _x
_y:
  dey
  bne _y

  ; spr0 nw
  dec $d000
  dec $d001
  ; spr1 ne
  inc $d002
  dec $d003
  ; spr2 e
  inc $d004
  ;   $d005
  ; spr3 se
  inc $d006
  inc $d007
  ; spr4 s
  ;   $d008
  inc $d009
  ; spr5 sw
  dec $d00a
  inc $d00b
  ; spr6 w
  dec $d00c
  ;   $d00d
  ; spr7 n
  ;   $d00e
  dec $d00f

  jsr _change_colour
  adc #$01
  jmp _loop

_change_colour:
  ; set spr0 colour
  ldx #$00
_set_colour:
  sta $e000,X
  sta $e040,X
  sta $e080,X
  sta $e0c0,X
  sta $e100,X
  sta $e140,X
  sta $e180,X
  sta $e1c0,X
  inx
  cpx #$40
  bne _set_colour
  rts
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
