;; Code to send and recieve data via serial connection
.PC02
.linecont +
; parameters: $00-$03
; return:     $04-$05
Param1 := $00
Param2 := $01
Param3 := $02
Param4 := $03
Return1 := $04
Return2 := $05

; ============ SERIAL STUFF =============
; *** EXTERNAL *** TODO
Drec            := $90
Dsend           := $91
; *** EXTERNAL *** TODO
OUTB            := $A000
OUTA            := $A001
DDRB            := $A002
DDRA            := $A003
ACR             := $A00B

.DEFINE LISTEN          #%00000010 ; listening pb1 = 1
.DEFINE READY           #%00000001 ; data ready = 1
; bits are 3[CLK] 2[DATA] 1[LISTENING] 0[READY]
.DEFINE DDRB_RECV       #%11110011 ; waiting for data, pb2-data in, pb3 - clk in
.DEFINE DDRB_SEND       #%11111111 ; waiting to send, pb2 - data out, pb3 - clk out
.DEFINE ZERO            #$00
.DEFINE CHECKCLK        #%00001000 ; clock bit
.DEFINE CHECKDATA       #%00000100 ; data bit

; ================ DATA TRANSFER STUFF =================
Addrl 			:= $92
Addrh			:= $93
Tstate			:= $94
Dcounth			:= $95

.DEFINE STATE_Command	#$30	; ascii 0
.DEFINE STATE_Addrl 	#$31 	;   1
.DEFINE STATE_Addrh		#$32	;	2
.DEFINE STATE_Dcountl	#$33	;	3
.DEFINE STATE_Dcounth	#$34	;	4
.DEFINE STATE_D			#$35	;	5

.DEFINE COMMAND_A		#$41	; ascii A
.DEFINE COMMAND_D		#$44	;  	D
.DEFINE COMMAND_E		#$45	;	E
.DEFINE COMMAND_R		#$52	;	R

;;;;;;;;;;;;;;;;;;;;;;
;;;; DEBUG MACROS ;;;;
;;;;;;;;;;;;;;;;;;;;;;
.DEFINE _DEBUG 1

.MACRO DEBUGOUT val
.if _DEBUG = 1
        phx
        ldx val
        lda LEDCHARMAP, X
        sta OUTA
        plx
.endif
.ENDMACRO

.MACRO DEBUGCLEAR
.if _DEBUG = 1
        phx
        ldx #$10
        lda LEDCHARMAP, X
        sta OUTA
        plx
.endif
.ENDMACRO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.data
hellomsg: .asciiz "===my 6502==="

LEDCHARMAP: .byte $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$67, \
                  $F7,$FC,$B9,$DE,$F9,$F1,$00

.code
MAIN:
        cld 		; clear decimal mode on startup
        sei 		; disable interrupts (nothing connected anyway)

        jsr setupVIA

        DEBUGOUT #$0f
        jsr waitLoop
        jsr waitLoop

        jsr SendString

        lda #$30 			; send initial data
        sta Dsend
@processLoop:
        jsr bitbangSend
        jsr bitbangRecv
        lda Drec
        inc
        sta Dsend 			; echo back char+1
        jmp @processLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
setupVIA: ; *** EXTERNAL *** TODO
        lda #$FF
        sta DDRB
        STA DDRA
        lda ZERO
        sta OUTB
        sta OUTA
        sta ACR  ; disable latching etc..
        rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bitbangRecv:
        DEBUGOUT #$00
; gets bits MSBFIRST
        lda ZERO
        tax 				; zero x - bit counter
        sta Drec 			; zero out receive byte

        lda DDRB_RECV 		; set up pins for receive
        sta DDRB
        DEBUGOUT #$01
        lda LISTEN 			; output listen bit
        sta OUTB
							; start looping through bits
@pollHigh:
        lda OUTB         	; get bits
        tay              	; store value for later (faster)
        and CHECKCLK 		; check clock bit
        beq @pollHigh 		; goto pollHigh if 0 (clk low)
        ; here clock is high, data is good
        lda ZERO
        sta OUTB
        tya               	; restore value when it was good
        and CHECKDATA 		; get data bit 2 CxLR
        beq @zero
        asl Drec
        inc Drec
        jmp @one
@zero:
        asl Drec 		; shift data left
@one:
        inx
        cpx #$08 		; if x = 8, then we're done
        beq @done

        DEBUGOUT #$02
@pollLow: 				; begin waiting for clock to go low
        lda OUTB
        and CHECKCLK
        bne @pollLow 	; loop while clock is 1

        DEBUGOUT #$01
        jmp @pollHigh
@done: 					; shifting is finished
        lda ZERO
        sta OUTB		; not listening anymore

        jsr waitLoop
        DEBUGCLEAR
        rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
waitLoop:
        phy
        ldy #$08 ; adjust length of delay 0f = .05, 08 = .02
        lda ZERO
@loop:
        clc              	; 2
        adc #$01 			; 2
        bne @loop 			; 3
        dey               	; 2
        bne @loop 			; 3
        ply
        rts		            ; delay is ~0.05 sec (1MHz/(256*$0F*12))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bitbangSend:				; initialize port
        ldx ZERO
        lda DDRB_SEND
        sta DDRB 			; port config

        lda READY
        sta OUTB 			; ready signal
        jsr waitLoop
        jsr waitLoop
        jsr waitLoop
; start the send loop!
@loop:
        lda Dsend
; need to set clk, ready = 1, D = 1 otherwise, MSBFIRST
        bpl @dataLow    	; bit 7 contains data to send, pl = 0, mi = 1
        lda #%00001100  	; since its negative, D=1
        bne @controlWrite 	; go write it out
@dataLow:
        lda #%00001000 		; its positive here, D=0
@controlWrite:
        sta OUTB 			; write the data, CLK HIGH

        DEBUGOUT #$03

        jsr waitLoop 		; wait for a bit while high

        lda ZERO
        sta OUTB 			; CLK LOW

        DEBUGOUT #$04

        jsr waitLoop 		; wait a bit while low
        asl Dsend 			; shift bit left for next round
        inx               	; bit counter
        cpx #$08  			; if shifted 8 then done
        bne @loop

        lda DDRB_RECV 		; set them as inputs
        sta DDRB 			; port config
        lda ZERO
        sta OUTB        	; reset the port

        DEBUGCLEAR
        rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dataProcess:
	; function takes Drec, processes, puts response in Dsend
	; A -> BB, D->BB->B,B,B,....., E -> Execute @ A, R -> recv byte @ A
	lda Tstate
	cmp STATE_Command
	bne @processCommand
getCommand:					; waiting on command, figure out what it is
	lda Drecv
	cmp COMMAND_A
	bne @checkD
	lda STATE_Addrl
	sta Dsend
	sta Tstate			; its A, so next is get the addrl
	rts
@checkD:
	cmp COMMAND_D
	bne @checkE
	lda STATE_Dcountl
	sta Dsend
	sta Tstate
	rts
@checkE
	cmp COMMAND_E
	bne @checkR
	jmp (Addrl)
@checkR:
	cmp COMMAND_R
	bne @defaultCommand
	lda (Addrl)
	sta Dsend
	inc Addrl
	bne @RaddrhGood
	inc Addrh			; increment addrh since addrl looped
@RaddrhGood:
	rts
@defaultCommand:		; unknown state
	lda STATE_Command	; reset state to recv command
	sta Dsend
	sta Tstate
	rts

@processCommand:		; state is A,D,E,R
	cmp STATE_Addrl
	bne @checkAddrh
	lda Drecv			; get addrl byte
	sta Addrl
	lda STATE_Addrh
	sta Dsend
	sta Tstate			; next state is get addrh, store away addrl
	rts	
@checkAddrh:
	cmp STATE_Addrh
	bne @checkDcountl
	lda Drecv
	sta Addrh
	lda STATE_Command	; no command 
	sta Dsend
	sta Tstate
	rts
@checkDcountl:
	cmp STATE_Dcountl
	bne @checkDcounth
	lda Drecv
	sta Dcountl			; get low counter byte
	lda STATE_Dcounth
	sta Dsend
	sta Tstate
	rts
@checkDcounth:
	cmp STATE_Dcounth
	bne @checkD
	lda Drecv
	sta Dcounth			; get high counter byte
	lda STATE_D
	sta Dsend
	sta Tstate
	rts
@checkD:
	cmp STATE_D
	bne @defaultCommand
	
	lda Dcounth			; get high count value
	bne @counterGood	; branch if non-zero
	lda Dcountl			;
	bne @counterGood	; branch if non-zero	
						; counter is 0 here
	lda STATE_Command
	sta Dsend
	sta Tstate
	rts 				; counter expired

@counterGood:
	lda #$Drecv
	sta (Addrl)
	inc Addrl
	bne @DaddrGood
	inc Addrh
@DaddrGood:
	dec Dcountl			; decrement the counter
	lda Dcountl
	cmp #$ff
	bne @DcountGood
	dec Dcounth
@DcountGood:
	rts
@defaultCommand:
	lda STATE_Command
	sta Dsend
	sta Tstate
	rts

; =====================================
; experimental string send function
; probably good to make this external as well TODO
SendString:
        ; Param1,Param2 is pointer
        ; bytes written passed into Return1
        clc
        ldx #$00
@loop:
        lda hellomsg,X
        beq @quit ; quit if null
        sta Dsend
        phx
        jsr bitbangSend
        plx
        inx
        bne @loop ; loop while y doesn't wrap - 256 chars max
@quit:
        sty Return1 ; bytes sent into $02
        rts
; ======================================
;displayLed:
;; pass the hex value to be printed in X
;       lda LEDCHARMAP, X
;       sta OUTA
;       rts
.END
