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
colour_ram = $d800
sper = $d015
sprite_base = $d000
sprite_ptr_base = $07f8

screen_ram = $0400

colour_ram_0 = $e000
colour_ram_1 = $e040
colour_ram_2 = $e080
colour_ram_3 = $e0c0
colour_ram_4 = $e100
colour_ram_5 = $e140
colour_ram_6 = $e180
colour_ram_7 = $e1c0

kbd = $0300

.code

  lda <#isr
  sta isr_v
  lda >#isr
  sta isr_v+1

  ldx #$ff
write_1:
  lda #$6e
  sta colour_ram,X
  dex
  bne write_1

  lda #$ff
  sta sper ; enable sprites

  ldx #$00
  ldy #$00
  lda #$80
set_sprite_pos:
  sta sprite_base,X
  sta sprite_base+1,X
  inx
  inx
  iny
  cpy #$10
  bne set_sprite_pos

  ; set spr ptrs
  ldx #$00
load_spr_ptr:
  lda #$80
  sta sprite_ptr_base,X
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
  dec sprite_base
  dec sprite_base+1
  ; spr1 ne
  inc sprite_base+2
  dec sprite_base+3
  ; spr2 e
  inc sprite_base+4
  ;   $d005
  ; spr3 se
  inc sprite_base+6
  inc sprite_base+7
  ; spr4 s
  ;   $d008
  inc sprite_base+9
  ; spr5 sw
  dec sprite_base+10
  inc sprite_base+11
  ; spr6 w
  dec sprite_base+12
  ;   $d00d
  ; spr7 n
  ;   $d00e
  dec sprite_base+15

  jsr change_colour
  adc #$01
  jmp loop

change_colour:
  ; set spr0 colour
  ldx #$00
set_colour:
  sta colour_ram_0,X
  sta colour_ram_1,X
  sta colour_ram_2,X
  sta colour_ram_3,X
  sta colour_ram_4,X
  sta colour_ram_5,X
  sta colour_ram_6,X
  sta colour_ram_7,X
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

  lda kbd
;   sta _pos_h
  ldx pos_l
  sta screen_ram,X
  clc
  inc pos_l

  pla
  tay
  pla
  tax
  pla

  rti
