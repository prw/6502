.PC02      ; enable 65C02
.linecont	+ ; enable line continuations with backslash

.DATA
;.org $6000
DDRA := $A003
OUTA := $A001
LEDCHARMAP: .byte $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$67, \
		  $F7,$FC,$B9,$DE,$F9,$F1

.CODE
;.org $e000
MAIN:
	jsr initLed
	ldx #$00
LOOP:
	jsr displayLed
	inx
	cpx #$10
	beq MAIN
	jsr wait
	jsr wait
	jmp LOOP

initLed:
	lda #$FF
	sta DDRA
	lda #$00
	sta OUTA
	rts
displayLed:
; pass the hex value to be printed in X (needs to be less than $10)
	lda LEDCHARMAP, X
	sta OUTA
	rts

wait: ; saves registers, counts 0-2^16
	pha
	txa
	pha
	tya
	pha
	ldy #$00
waitloop:
	clc
	ldx #$00
	lda #$00
waitinnerloop:
	inx
	sta $00
	lda #$02
	sta $01
	lda #$AA
	sta ($00), Y
	lda ($00), Y
	cpx #$00
	bne waitinnerloop
	iny
	cpy #$00
	bne waitloop

	pla
	tay
	pla
	tax
	pla
	rts
; end wait
