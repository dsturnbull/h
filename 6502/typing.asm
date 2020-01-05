.data

pos_h:
  .byte $04
pos_l:
  .byte $00

.org $2000
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00
!bin "6502/hithere.spr"
.byte $00

isr_v = $fffe

.code

  lda <isr
  sta isr_v
  lda >isr
  sta $ffff

  ldx #$ff
write_1:
  lda #$6e
  sta $d7ff,X
  dex
  bne write_1

  lda #$01
  sta $d015 ; enable sprite 0
  lda #$00
  sta $d01b ; draw sprite in front

  ldx #$00
  ldy #$00
  lda #$80
set_sprite_pos:
  sta $d000,X
  sta $d001,X
  inx
  inx
  iny
  cpy #$10
  bne set_sprite_pos

  ; set spr ptrs
  ldx #$00
load_spr_ptr:
  lda #$80
  sta $07f8,X
  inx
  cpx #$08
  bne load_spr_ptr

main:
  lda #$00
loop:
;   ldx #$ff
;   ldy #$ff
; x:
;   dex
;   bne x
; y:
;   dey
;   bne y

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

  jsr change_colour
  adc #$01
  jmp loop

change_colour:
  ; set spr0 colour
  ldx #$00
set_colour:
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
  bne set_colour
  rts
isr:
  pha
  txa
  pha
  tya
  pha

  lda $0300
;   sta _pos_h
  ldx pos_l
  sta $0400,X
  clc
  inc pos_l

  pla
  tay
  pla
  tax
  pla

  rti
