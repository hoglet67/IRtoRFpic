
	list      p=16f628            
	#include <p16f628.inc>        

    #include "serial2.inc"
	
	__CONFIG _CP_OFF & _WDT_ON & _BODEN_ON & _PWRTE_ON & _XT_OSC & _LVP_OFF  

;-----------------------------------------------------------------------------
;
; Constants
;
;-----------------------------------------------------------------------------


TIMER_UPDATE    EQU     .159 ; -100+3           ;Timer update value (2 dead cycles)
MS5_COUNT       EQU     .50 ; 100             ;Number of counts to 5ms
CLR_TIME        EQU     .50 ; 100             ;Display clear timeout (20ms/count)
HALF_TIME       EQU     .9 ; 900/100          ;Number of timer counts per half bit


HE_PORT			EQU PORTA
HE_PBIT			EQU 0

HE_ADDRESS		EQU 	0x0125BE6		; a 26 bit address

HE_ADDR_0		EQU		(HE_ADDRESS >> .18) & 0xFF;
HE_ADDR_1		EQU		(HE_ADDRESS >> .10) & 0xFF;
HE_ADDR_2		EQU		(HE_ADDRESS >> .2)  & 0xFF;
HE_ADDR_3		EQU		(HE_ADDRESS << .6)  & 0xFF;


HE_HEAD_H_TIME	EQU		.3
HE_HEAD_L_TIME	EQU		.27

HE_DATA_T0		EQU		.3		; High
HE_DATA_T1		EQU		.3		; Data
HE_DATA_T2		EQU		.3		; High
HE_DATA_T3		EQU		.4		; ~Data
HE_DATA_T4		EQU		.3		; High
HE_DATA_T5		EQU		.3		; Low

HE_TAIL_H_TIME	EQU		.3
HE_TAIL_L_TIME	EQU		.99

ALL_LEDS_ON		EQU		0x3f
ALL_LEDS_OFF	EQU		0x00

;------ variable registers

VARS        UDATA

LEDS        res 	1	;What's currently on the LEDs - bits 0-4 used; 0 = off, 1 = on
SCAN_DELAY  res		1	;Scan delay counter (5ms pace)
CLR_DELAY  	res		1	;Display clear delay counter
IR_STATE    res		1   ;IR receiver state machine
BIT_TIMER   res     1   ;Bit timer
IR_SHIFT    res     4   ;IR shift register




HE_COMMAND	res 1		; the current Home Easy Command (bit 4 = on-off, bits 3-0 device)
HE_STATE	res 1		; Home Easy Transmit State Machine
HE_COUNT	res 1		; Home Easy Transmit Counter (within a state)
HE_REPEAT	res 1		; Home Easy Command Repeat Counter (also triggers start of sent)
HE_BIT		res	1		; Home Easy Bit Counter
HE_SHIFT	res 4		; Home Easy Shift Register


CMD_STATE	res 1		; command state machine
CMD_TMP		res 1		; a tempory register, used for building up the command
CMD_VAL		res 1		; the command to be sent

;------ reset vector

RESET_VECTOR  CODE    0x000         ;processor reset vector

  		goto    INIT
		nop
		nop
		nop

        return


PROG		CODE 5

;-----------------------------------------------------------------------------
;
; Scan display routine
;
;-----------------------------------------------------------------------------

SCAN_DISP       MOVLW   MS5_COUNT       ; Reload scan delay
                MOVWF   SCAN_DELAY
				
										; PB7 = LEDS3
										; PB6 = LEDS2
										; PB5 = LEDS1
										; PB4 = LEDS0
										; PB3 = LEDS5
										; PB0 = LEDS4

				SWAPF	LEDS, W			; map LEDS3..0 to PB7..4
				BTFSC	LEDS, 4			; map LEDS4 to PB0
				IORLW	b'00000001'		
				BTFSC	LEDS, 5			; map LEDS5 to PB3
				IORLW	b'00001000'
				XORLW   b'11111111'		; Invert
				IORLW	b'00000110'		; don't affect RS232 bits PB2/PB1
				MOVWF	PORTB

                DECF    CLR_DELAY,F     ; Decrement CLR_DELAY every 5ms
                BTFSS   STATUS,Z
                RETURN                  ; We're done if counter <> 0 !

                MOVLW   ALL_LEDS_OFF
                MOVWF   LEDS
                RETURN


;-----------------------------------------------------------------------------
;
; Command processing state machine
;
;-----------------------------------------------------------------------------

CMD_MACHINE	    CLRF	PCLATH
				MOVF    CMD_STATE,W      ;Jump to present state
                MOVWF   PCL


; process the command character (0..1)
CMD_STATE_0		CALL	uart_rx			; Z will be 0 if there is a character
				BTFSC	STATUS, Z
				RETURN

				MOVWF	CMD_TMP

				MOVF	CMD_TMP, W
				SUBLW	0x0A			; compare with LF 
				BTFSC	STATUS, Z		
				RETURN					; treat as a noop

				MOVF	CMD_TMP, W
				SUBLW	0x0D			; compare with CR 
				BTFSC	STATUS, Z		
				RETURN					; treat as a noop

				MOVF	CMD_TMP, W
				ANDLW	0xfe			; ignore the command bit
				SUBLW	0x30			; compare with '0' 
				BTFSS	STATUS, Z		
				GOTO	CMD_ERROR		; not a valid command

				SWAPF	CMD_TMP, W		; move the command bit to bit 4
				ANDLW	0x10
				MOVWF	CMD_VAL

				MOVLW 	CMD_STATE_1
				MOVWF	CMD_STATE

				RETURN

; process the device character (0..7)
CMD_STATE_1		CALL	uart_rx			; Z will be 0 if there is a character
				BTFSC	STATUS, Z
				RETURN

				MOVWF	CMD_TMP

				MOVF	CMD_TMP, W
				ANDLW	0xf8			; ignore the device bits
				SUBLW	0x30			; compare with '0' 
				BTFSS	STATUS, Z		
				GOTO	CMD_ERROR		; not a valid device
	
				MOVF	CMD_TMP, W
				ANDLW	0x07
				IORWF	CMD_VAL, F
		
				MOVLW 	CMD_STATE_2
				MOVWF	CMD_STATE

				RETURN
				
; process the terminating character (CR or LF)

CMD_STATE_2		CALL	uart_rx			; Z will be 0 if there is a character
				BTFSC	STATUS, Z
				RETURN

				MOVWF	CMD_TMP

				MOVF	CMD_TMP, W
				SUBLW	0x0A			; compare with LF 
				BTFSC	STATUS, Z		
				GOTO	CMD_SEND		; a valid command 

				MOVF	CMD_TMP, W
				SUBLW	0x0D			; compare with CR
				BTFSC	STATUS, Z		
				GOTO	CMD_SEND		; a valid command 

CMD_ERROR
				MOVLW 	'e'
				CALL	uart_tx
				MOVLW 	'r'
				CALL	uart_tx
				MOVLW 	'r'
				CALL	uart_tx
				MOVLW 	0x0a
				CALL	uart_tx
				MOVLW 	0x0d
				CALL	uart_tx

				MOVLW 	CMD_STATE_0
				MOVWF	CMD_STATE
				RETURN

CMD_SEND

				MOVLW 	CMD_STATE_3
				MOVWF	CMD_STATE
				RETURN

; actually send the command

CMD_STATE_3		

				MOVLW 	'o'
				CALL	uart_tx
				MOVLW 	'k'
				CALL	uart_tx
				MOVLW 	' '
				CALL	uart_tx

				MOVF	CMD_VAL, W
				CALL 	COMMAND_SEND
				MOVLW 	CMD_STATE_0
				MOVWF	CMD_STATE
				RETURN



;-----------------------------------------------------------------------------
;
; IR receiver state machine
;
;-----------------------------------------------------------------------------

IR_MACHINE     	CLRF	PCLATH
				MOVF    IR_STATE,W      ;Jump to present state
                MOVWF   PCL

;---------------------------------------STATE 0, WAIT FOR BEGIN OF START BIT--

IR_STATE_0      BTFSC   PORTA,4         ;Input still high?
                RETURN                  ;Yes! Nothing to do

                MOVLW   HALF_TIME/2-1   ;Wait until we're in the center of the
                MOVWF   BIT_TIMER       ; start pulse
                MOVLW   IR_STATE_1      ;Next stop is state 1
                MOVWF   IR_STATE
                RETURN

;---------------------------STATE 1, START BIT DETECTED, CHECK IF IT IS REAL--

IR_STATE_1      DECFSZ  BIT_TIMER,F       ;Wait until center of start pulse
                RETURN                  ;Time's not up yet!

                BTFSC   PORTA,4         ;Is the input still low?
                GOTO    IR_ERROR_1      ;Nope! Exit with error

                MOVLW   HALF_TIME       ;Set interval to the center of the
                MOVWF   BIT_TIMER       ; first half of the next bit
                MOVLW   0x08; %0000.1000      ;Prepare the shift register
                MOVWF   IR_SHIFT
                CLRF    IR_SHIFT+1
                MOVLW   IR_STATE_2      ;Prepare for next stop
                MOVWF   IR_STATE
                RETURN

;-----------------------------------IR STATE 2, WAIT FOR FIRST HALF OF A BIT--

IR_STATE_2      DECFSZ  BIT_TIMER,F       ;Wait until center of first half of bit
                RETURN                  ;Keep waiting!

                MOVLW   IR_STATE_3      ;Next state is 3 if input is high
                BTFSS   PORTA,4
                MOVLW   IR_STATE_4      ;Input is low, next state is 4
                MOVWF   IR_STATE
                MOVLW   HALF_TIME       ;Restart bit timer
                MOVWF   BIT_TIMER
                RETURN

;---------------IR STATE 3, FIRST HALF WAS HIGH NOW IT MUST BE LOW FOR A "1"--

IR_STATE_3      DECFSZ  BIT_TIMER,F       ;Wait until center of 2nd half of bit
                RETURN                  ;Keep waiting!

                BTFSC   PORTA,4         ;Is input high now?
                GOTO    ERR             ;Nope! It's an error!

                BSF     STATUS,C        ;A 1 was received, shift it in result
                RLF     IR_SHIFT,F
                RLF     IR_SHIFT+1,F
                MOVLW   HALF_TIME       ;Restart bit timer
                MOVWF   BIT_TIMER
                MOVLW   IR_STATE_2      ;In case we need some more bits
                BTFSC   STATUS,C        ;We're done when Carry is 1
                MOVLW   IR_STATE_5      ;Carry is 1, received entire message
                MOVWF   IR_STATE
                RETURN

ERR             MOVLW   IR_ERROR_0      ;Wait until input gets high before
                MOVWF   IR_STATE        ; returning to state 0
                RETURN

;---------------IR STATE 4, FIRST HALF WAS LOW NOW IT MUST BE HIGH FOR A "0"--

IR_STATE_4      DECFSZ  BIT_TIMER,F       ;Wait until center of 2nd half of bit
                RETURN                  ;Keep waiting!

                BTFSS   PORTA,4         ;Is input high now?
                GOTO    IR_ERROR_1      ;Nope! It's an error!

                BCF     STATUS,C        ;A 0 was received, shift it in result
                RLF     IR_SHIFT,F
                RLF     IR_SHIFT+1,F
                MOVLW   HALF_TIME       ;Restart bit timer
                MOVWF   BIT_TIMER
                MOVLW   IR_STATE_2      ;In case we need some more bits
                BTFSC   STATUS,C        ;We're done when Carry is 1
                MOVLW   IR_STATE_5      ;Carry is 1, received entire message
                MOVWF   IR_STATE
                RETURN

;--------------------------IR STATE 5, MESSAGE RECEIVED, START PROCESSING IT--

IR_STATE_5      
                MOVF    IR_SHIFT,W      ;Get IR command
                ANDLW   0x3F; %0011.1111      ;The command is only 6 bits wide
                BTFSS   IR_SHIFT+1,4    ;Copy inverted extended bit to b6 of
                IORLW   0x40; %0100.0000      ; command

                RLF     IR_SHIFT,F      ;Shift the entire IR address in
                RLF     IR_SHIFT+1,F    ; 2nd byte of shift register
                RLF     IR_SHIFT,F
                RLF     IR_SHIFT+1,F
                MOVWF   IR_SHIFT        ;Save cleaned up hex IR command number

                MOVLW   IR_STATE_6      ;We've done enough in this state
                MOVWF   IR_STATE        ;Let's do the rest in state 6
                RETURN

;------------------------------IR STATE 6, CONVERT HEX MESSAGE TO 7 SEGMENTS--

IR_STATE_6

				MOVF	IR_SHIFT+1, W
                ANDLW   0x1F
				XORLW	0x19
				BTFSS	STATUS, Z
				GOTO	SKIP

				MOVF	IR_SHIFT,W
				CALL 	COMMAND_SEND

SKIP
                MOVLW   IR_STATE_7      ;Done enough for now. Let's finish it
                MOVWF   IR_STATE        ; in the last state
                RETURN

;----------------------------------IR STATE 7, WAIT FOR INPUT TO RETURN HIGH--

IR_STATE_7
IR_ERROR_0      MOVLW   IR_STATE_0      ;Reset state machine only if input is
                BTFSC   PORTA,4         ; high
                MOVWF   IR_STATE
                RETURN

;-----------------------------------------------------------IR ERROR STATE 1--

IR_ERROR_1      MOVLW   IR_STATE_0      ;Return to IR state 0
                MOVWF   IR_STATE

                RETURN


COMMAND_SEND	MOVWF	HE_COMMAND		; the command that will be sent over HE

				MOVWF	LEDS			; LEDS 0..3 are the device address, LED 4 is the on command

				BTFSS	HE_COMMAND, 4	; LED5 is the off command
				BSF		LEDS, 5

				MOVLW   CLR_TIME        ;Set display clear timer
                MOVWF   CLR_DELAY

				MOVLW	4
				MOVWF	HE_REPEAT		; trigger the HE State Machine

				SWAPF	HE_COMMAND, W	; queue the command to rhe serial port
				ANDLW	0x0F
				IORLW	0x30
				CALL	uart_tx
				MOVF	HE_COMMAND, W
				ANDLW	0x0F
				IORLW	0x30
				CALL	uart_tx
				MOVLW	.13
				CALL	uart_tx
				MOVLW	.10
				CALL	uart_tx

				RETURN

;-----------------------------------------------------------------------------
;
; Home Easy Transmit State Machine
;
;-----------------------------------------------------------------------------

				ORG	0x100

HE_MACHINE      MOVLW	1
				MOVWF	PCLATH
				MOVF    HE_STATE,W      ;Jump to present state
                MOVWF   PCL

HE_STATE_IDLE	BCF		HE_PORT, HE_PBIT

				MOVF	HE_REPEAT,W		; Loading HE_REPEAT with a non-zero count starts things off...
                BTFSC   STATUS,Z
				RETURN
				MOVLW	HE_STATE_INIT
				MOVWF	HE_STATE
				RETURN
		

HE_STATE_INIT	BSF		HE_PORT, HE_PBIT
				MOVLW	HE_HEAD_H_TIME
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_HEAD_H
				MOVWF	HE_STATE
				RETURN


HE_STATE_HEAD_H	DECFSZ	HE_COUNT, F
				RETURN
				BCF		HE_PORT, HE_PBIT
				MOVLW	HE_HEAD_L_TIME
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_HEAD_L
				MOVWF	HE_STATE
				RETURN


HE_STATE_HEAD_L	DECFSZ	HE_COUNT, F
				RETURN
				BSF		HE_PORT, HE_PBIT		
				MOVLW	0x20		; number of data bits to send
				MOVWF	HE_BIT

				MOVLW	HE_ADDR_0		
				MOVWF	HE_SHIFT
				MOVLW	HE_ADDR_1		
				MOVWF	HE_SHIFT+1
				MOVLW	HE_ADDR_2		
				MOVWF	HE_SHIFT+2

									; bits 7-6 are address
									; bit 5 is the group bit
									; bit 4 is the command
									; bits 3-0 are the device
				MOVLW	HE_ADDR_3		
				IORWF	HE_COMMAND,W
				MOVWF	HE_SHIFT+3

				MOVLW	HE_DATA_T0
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA0
				MOVWF	HE_STATE
				RETURN

HE_STATE_DATA0	DECFSZ	HE_COUNT, F
				RETURN
				BCF		HE_PORT, HE_PBIT
				MOVLW	HE_DATA_T1
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA1
				MOVWF	HE_STATE
				RETURN

HE_STATE_DATA1	DECFSZ	HE_COUNT, F
				RETURN
				BTFSS	HE_SHIFT,7
				BSF		HE_PORT, HE_PBIT
				BTFSC	HE_SHIFT,7
				BCF		HE_PORT, HE_PBIT
				MOVLW	HE_DATA_T2
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA2
				MOVWF	HE_STATE
				RETURN

HE_STATE_DATA2	DECFSZ	HE_COUNT, F
				RETURN
				BCF		HE_PORT, HE_PBIT
				MOVLW	HE_DATA_T3
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA3
				MOVWF	HE_STATE
				RETURN

HE_STATE_DATA3	DECFSZ	HE_COUNT, F
				RETURN
				BTFSS	HE_SHIFT,7
				BCF		HE_PORT, HE_PBIT
				BTFSC	HE_SHIFT,7
				BSF		HE_PORT, HE_PBIT
				MOVLW	HE_DATA_T4
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA4
				MOVWF	HE_STATE
				RETURN

HE_STATE_DATA4	DECFSZ	HE_COUNT, F
				RETURN
				BCF		HE_PORT, HE_PBIT
				MOVLW	HE_DATA_T5
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA5
				MOVWF	HE_STATE
				RETURN

HE_STATE_DATA5	DECFSZ	HE_COUNT, F
				RETURN
				BSF		HE_PORT, HE_PBIT
                RLF     HE_SHIFT+3,F ; shift the shift register by one bit
                RLF     HE_SHIFT+2,F
                RLF     HE_SHIFT+1,F
                RLF     HE_SHIFT,F
				DECFSZ	HE_BIT, F
				GOTO	HE_DATA_MORE
				MOVLW	HE_TAIL_H_TIME
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_TAIL_H
				MOVWF	HE_STATE
				RETURN

HE_DATA_MORE	MOVLW	HE_DATA_T0
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_DATA0
				MOVWF	HE_STATE
				RETURN

HE_STATE_TAIL_H	DECFSZ	HE_COUNT, F
				RETURN
				BCF		HE_PORT, HE_PBIT
				MOVLW	HE_TAIL_L_TIME
				MOVWF	HE_COUNT
				MOVLW	HE_STATE_TAIL_L
				MOVWF	HE_STATE
				RETURN


HE_STATE_TAIL_L	DECFSZ	HE_COUNT, F
				RETURN

				MOVLW	HE_STATE_IDLE
				DECFSZ	HE_REPEAT, F
				MOVLW	HE_STATE_INIT
				MOVWF	HE_STATE
				RETURN

;-----------------------------------------------------------------------------
;
; Get started
;
;-----------------------------------------------------------------------------


				ORG 0x200

INIT

                BSF     STATUS,RP0      ;Select bank 1 of register file
                MOVLW   0xdf; #%1101.1111     ;Timer mode without pre-scaler
                OPTION
                MOVLW   0x10; #%0001.0000     ;Only PA4 is an input
                MOVWF   TRISA
                MOVLW   0x02; #%0000.0010     ;Only PB1 is an input
                MOVWF   TRISB
                BCF     STATUS,RP0      ;Select bank 0 of register file

				MOVLW	0x07			; Turn comparators off and enable pins for I/O functions
				MOVWF	CMCON

                CLRF   INTCON 			;Disable all interrupts

		        MOVLW 	b'00000000'       ; important to set PA0 (Home Easy Transmitter) to 0
		        MOVWF 	PORTA

		        MOVLW 	b'00000100'       ; RB2(TX)=1 others are 0
		        MOVWF 	PORTB

                MOVLW   IR_STATE_0      ;Init IR receiver state machine
                MOVWF   IR_STATE

                MOVLW  	CMD_STATE_0      ;Init command state machine
                MOVWF  	CMD_STATE

				CLRF	HE_COMMAND
				CLRF	HE_REPEAT
                MOVLW   HE_STATE_IDLE      ;Init Home Easy receiver state machine
                MOVWF   HE_STATE



				CALL 	uart_init
                MOVLW   0x00; 			;Disable all interrupts
                MOVWF   INTCON

				MOVLW	'h'
				CALL	uart_tx
				MOVLW	'e'
				CALL	uart_tx
				MOVLW	'l'
				CALL	uart_tx
				MOVLW	'l'
				CALL	uart_tx
				MOVLW	'o'
				CALL	uart_tx
				MOVLW	.13
				CALL	uart_tx
				MOVLW	.10
				CALL	uart_tx

                MOVLW   TIMER_UPDATE    ;Get the timer started
                MOVWF   TMR0
                MOVLW   MS5_COUNT       ;Start 5ms counter for display scan
                MOVWF   SCAN_DELAY

				MOVLW	ALL_LEDS_ON
                MOVWF   LEDS
                MOVLW   .200            ;Set clear time-out to 4 seconds
                MOVWF   CLR_DELAY


;-----------------------------------------------------------------------------
;
; Main program loop
;
;-----------------------------------------------------------------------------

MAIN

;-----------------------------------------RESET THE WATCHDOG TIMER--

				CLRWDT


;-----------------------------------------CALL THE HOME EASY STATE MACHINE--

                CALL    HE_MACHINE

;-----------------------------------------CALL THE IR RECEIVER STATE MACHINE--

                CALL    IR_MACHINE

;-----------------------------------------CALL THE COMMAND STATE MACHINE--

                CALL    CMD_MACHINE

;-----------------------------------------POLL THE SERIAL DRIVER --
;; todo: should be OK to alternate calls
                CALL    uart_int_tx
                CALL    uart_int_rx

;-----------------------------------------------------SCAN DISPLAY EVERY 5MS--

                DECF    SCAN_DELAY,F    ;Decrement scan delay counter
                BTFSC   STATUS,Z        ;Only scan display if counter = 0
                CALL    SCAN_DISP

;-------------------------------------------------------SYNC MAIN WITH TIMER--

SYNCH           BTFSC   TMR0,7          ;Wait until bit 7 of TMR = 0
                GOTO    SYNCH           ;Not 0 yet!
                MOVLW   TIMER_UPDATE    ;Reload timer again
                ADDWF   TMR0,F

                GOTO    MAIN



            END

	
;------ program end
