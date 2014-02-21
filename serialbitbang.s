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

; *** EXTERNAL *** TODO
Drec 		:= $90
Dsend 		:= $91
; *** EXTERNAL *** TODO
OUTB		:= $A000
OUTA		:= $A001
DDRB		:= $A002
DDRA		:= $A003
SR			:= $A00A
ACR			:= $A00B
IFR			:= $A00D
; LOCAL
ADDRH 		:= $A1 ; current address
ADDRL 		:= $A0 ; needs to be zero page
PROCESSSTATE:= $A2 ; status of data processing
TMPBYTE 	:= $A3

; ============states==============
.DEFINE GETCOMMAND  #$43 ; C
.DEFINE GETADDRHH   #$30 ; 0
.DEFINE GETADDRHL   #$31 ; 1
.DEFINE GETADDRLH   #$32 ; 2
.DEFINE GETADDRLL   #$33 ; 3
.DEFINE GETDATAH    #$34 ; 4  
.DEFINE GETDATAL    #$35 ; 5
.DEFINE STARTSYMBOL #$53 ; S

; *** EXTERNAL *** TODO
; ==========serial console init reg values============
.DEFINE LISTEN		#%00000010 ; listening pb1 = 1
.DEFINE READY		#%00000001 ; data ready = 1
; bits are 3[CLK] 2[DATA] 1[LISTENING] 0[READY]
.DEFINE DDRB_RECV	#%11110011 ; waiting for data, pb2-data in, pb3 - clk in
.DEFINE DDRB_SEND	#%11111111 ; waiting to send, pb2 - data out, pb3 - clk out
.DEFINE ZERO		#$00
.DEFINE	CHECKCLK	#%00001000 ; clock bit
.DEFINE CHECKDATA	#%00000100 ; data bit

.DEFINE NEWLINE 	#$0D	   ; newline char

;;;;;;;;;;;;;;;;;;;;;;
;;;; DEBUG MACROS ;;;;
;;;;;;;;;;;;;;;;;;;;;;
.DEFINE _DEBUG 1

.MACRO DEBUGOUT val
.if _DEBUG = 1
	ldx val
	lda LEDCHARMAP, X
	sta OUTA
.endif
.ENDMACRO

.MACRO DEBUGCLEAR
.if _DEBUG = 1
	ldx #$10
	lda LEDCHARMAP, X
	sta OUTA
.endif
.ENDMACRO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.data
hellomsg: .asciiz "===6502 serial program loader==="
;hello2msg: .asciiz "Axxxx Dxx R E"

LEDCHARMAP: .byte $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$67, \
                  $F7,$FC,$B9,$DE,$F9,$F1,$00

.code
MAIN:
	cld ; clear decimal mode on startup
	clc ; (clear carry)
	sei ; disable interrupts (nothing connected)
	
	lda #$00
	sta ADDRH
	sta ADDRL
	
	lda GETCOMMAND
	sta PROCESSSTATE
	
	jsr setupVIA
	
	lda .LOBYTE(hellomsg)
	sta Param1
	lda .HIBYTE(hellomsg)
	sta Param2
	jsr SendString
	
	lda STARTSYMBOL ; send initial data
	sta Dsend
@processLoop:
	jsr bitbangSend
	jsr bitbangRecv
	jsr processData ; possible reentry location
	jmp @processLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
setupVIA: ; *** EXTERNAL *** TODO
	lda #$FF
	sta DDRB
	STA DDRA
	lda ZERO
	sta OUTB
	sta OUTA
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bitbangRecv:
	DEBUGOUT #$00
; gets bits MSBFIRST
	lda ZERO
	tax ; zero x - bit counter
	sta Drec ; zero out receive byte
	lda DDRB_RECV ; set up pins for receive
	sta DDRB
	lda LISTEN ; output listen bit
	sta OUTB
	; start looping through bits
	DEBUGOUT #$01
@pollHigh:
	lda OUTB	 ; get bits
	tay 		 ; store value for later (faster)
	and CHECKCLK ; check clock bit
	beq @pollHigh ; goto pollHigh if 0 (clk low)
	; here clock is high, data is good
	tya 		  ; restore value when it was good
	and CHECKDATA ; get data bit 2 CxLR
	lsr ; bit 1
	lsr ; bit 0 - put data in bit 0
	asl Drec ; shift data left
	eor Drec ; add new bit
	sta Drec ; store it away
	inx
	cpx #$08 ; if x = 8, then we're done
	beq @done
	
	DEBUGOUT #$02
@pollLow: ; begin waiting for clock to go low
	lda OUTB
	and CHECKCLK
	bne @pollLow ; loop while clock is 1
	
	DEBUGOUT #$01
	beq @pollHigh
@done: ; shifting is finished
	lda ZERO
	sta OUTB ; not listening anymore
	
	DEBUGCLEAR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
waitLoop:
	ldx #$0F ; adjust length of delay 0f = .05, 08 = .02
	lda ZERO
@loop:
	clc		 ; 2
	adc #$01 ; 2
	bne @loop ; 3
	dex		  ; 2
	bne @loop ; 3
	rts		; delay is ~0.05 sec (1MHz/(256*$0F*12))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bitbangSend:
	; initialize port
	ldx ZERO
	lda DDRB_SEND
	sta DDRB ; port config
	
	lda READY
	sta OUTB ; ready signal
; start the send loop!
@loop:
	lda Dsend
; need to set clk, ready = 1, D = 1 otherwise, MSBFIRST
	bpl @dataLow	; bit 7 contains data to send, pl = 0, mi = 1
	lda #%00001101  ; since its negative, D=1
	bne @controlWrite ; go write it out
@dataLow:
	lda #%00001001 ; its positive here, D=0
@controlWrite:
	sta OUTB ; write the data, CLK HIGH
	
	DEBUGOUT #$03
	
	jsr waitLoop ; wait for a bit while high
	
	lda READY
	sta OUTB ; CLK LOW

	DEBUGOUT #$04
	
	jsr waitLoop ; wait a bit while low
	asl Dsend ; shift bit left for next round
	inx		  ; bit counter
	cpx #$08  ; if shifted 8 then done
	bne @loop
	
	lda ZERO
	sta OUTB	; reset the port
	
	DEBUGCLEAR
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ===================================================
; procedure: recv data, call processData
; processData -> state = 0, check if it's A D E R
; 				 A -> 10 -> 11 ; E -> execute at ADDR
;				 D -> 20 ; R -> send back byte at ADDR
;			     state = 10 then get high addr
;				 state = 11 then get low addr
; 				 state = 20 then get data
processData:
	lda PROCESSSTATE ; load state
	cmp GETCOMMAND
	bne @processStatus ;; if state != command then branch
	;; state is zero, decide what we got 
@checkA:
	lda Drec ; load command from serial
	cmp #$41 ; A
	bne @checkD
	;; its A, set state to ADDRH, $10 - get first byte
	lda GETADDRHH
	sta PROCESSSTATE 
	sta Dsend 
	rts 	; return
@checkD:
	cmp #$44 ; D
	bne @checkE
	;; its D, set state to DATA, $20
	lda GETDATAH
	sta PROCESSSTATE
	sta Dsend
	rts
@checkE:
	cmp #$45 ; E
	bne @checkR
	DEBUGOUT #$0E
	;; its E, jmp to addr
	jmp (ADDRL) ; addrl execute the loaded code
@checkR: ; sends two consecutive bytes, hex of value at addr
	cmp #$52 ; R
	bne @checkError
	
	lda (ADDRL) ; load A with value at addrl
	lsr
	lsr
	lsr
	lsr ; shift right 4 bits to get high nibble
	jsr ValToHex
	sta Dsend ; store into Dsend
	jsr bitbangSend ; send the high nibble
	
	lda (ADDRL) ; load A with value at addrl
	and #$0F
	jsr ValToHex
	sta Dsend ; store into Dsend

	jsr incAddr
	rts
@checkError:
	lda Drec
	sta Dsend ; just echo
	rts
	
; =============================
@processStatus:
	;; here state is not zero, need to find out what to do
	;; coming into this point with some data in Rrec, state already in A
@checkADDRHH: ; not used, for readability!
	cmp GETADDRHH 
	bne @checkADDRHL
	; got ADDRHH nibble
	jsr AsciiByteHigh
	lda GETADDRHL ; put new state 
	sta PROCESSSTATE
	sta Dsend
	rts
@checkADDRHL:
	cmp GETADDRHL
	bne @checkADDRLH
	; got ADDRHL nibble
	jsr AsciiByteLow
	lda TMPBYTE
	sta ADDRH  ; store the high byte
	lda GETADDRLH ; set state
	sta PROCESSSTATE
	sta Dsend
	rts
@checkADDRLH:
	cmp GETADDRLH
	bne @checkADDRLL
	; got ADDRLH byte
	jsr AsciiByteHigh
	lda GETADDRLL    ; next is to get low nibble
	sta PROCESSSTATE
	sta Dsend
	rts
@checkADDRLL:
	cmp GETADDRLL
	bne @checkDATAH
	jsr AsciiByteLow
	lda TMPBYTE
	sta ADDRL    ; got byte for addrl, put it there
	lda GETCOMMAND ; next state is wait for command
	sta PROCESSSTATE
	sta Dsend
	rts
@checkDATAH:
	cmp GETDATAH
	bne @checkDATAL
	; got the data high nibble
	jsr AsciiByteHigh
	lda GETDATAL
	sta PROCESSSTATE
	sta Dsend
	rts
@checkDATAL:
	cmp GETDATAL
	bne @checkStatusError
	jsr AsciiByteLow
	lda TMPBYTE
	sta (ADDRL) ; store the value at the address
	jsr incAddr
	lda GETCOMMAND
	sta PROCESSSTATE
	sta Dsend
	rts
@checkStatusError:
	;; oh no, unrecognized status, should never get here
	sta Dsend ; send back the status to indicate this
	lda GETCOMMAND
	sta PROCESSSTATE ; reset the status to 0
	rts
; ===========================
	
incAddr:
	inc ADDRL ; increment low byte
	bne @incAddrDone
	inc ADDRH ; if result wasnt 0, then increment high byte
@incAddrDone:
	rts
; =====================================
AsciiByteHigh:
	; gets high nibble of byte, always call this first
	lda Drec
	jsr HexToVal
	asl
	asl
	asl
	asl ; shift 4 bits over
	sta TMPBYTE ; clobber tmp byte with 0xN0
	rts

AsciiByteLow:
	lda Drec
	jsr HexToVal
	clc
	adc TMPBYTE ; add high and low byte
	sta TMPBYTE ; store back
	rts

HexToVal:
	; expects ascii val in A, lowercase
	; converts 0-9a-f to nibble
	sec
	sbc #$30 ; subtract 0x30; '0' ascii == 0x30
	cmp #$10 ; if val < 0x10 then branch
	bmi @done ; subtract 0x27 to get offset from a-f
	sbc #$27
@done:
	rts
	
ValToHex:
	; takes 0x0N format
	clc
	adc #$30
	cmp #$3a
	bmi @done
	adc #$27
@done:
	rts
; =====================================
; experimental string send function
; probably good to make this external as well TODO
SendString:
	; Param1,Param2 is pointer
	; bytes written passed into Return1
	clc
	ldy #$00
@loop:
	lda (Param1),Y
	beq @quit ; quit if null
	sta Dsend
	jsr bitbangSend
	iny
	bne @loop ; loop while y doesn't wrap - 256 chars max
@quit:
	lda NEWLINE
	sta Dsend
	jsr bitbangSend
	iny
	sty Return1 ; bytes sent into $02
	rts
; ======================================
.END
