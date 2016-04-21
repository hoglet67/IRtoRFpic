;**********************************************************************
;                                                                     *
;    Filename:	    serial2.asm                                       *
;    Date:                                                            *
;    Author:        el@jap.hu                                         *
;    Company:       http://jap.hu/electronic/                         *
;                                                                     *
;    Description:   interrupt-driven hardware serial driver           *
;                                                                     *
;**********************************************************************
; NOTES:
; rewitten from picuart.asm (16C74) for the 16F628
; by Jap 2002/01
;
; Based on picuart.asm
;  picuart.asm is written by Fr. Thomas McGahee, tom_mcgahee@sigmais.com
;  primary web site http://redrival.com/mcgahee
;  secondary website http://mcgahee.freeservers.com
;
;
; 001: first working version
; 002: start interrupt driven serial engine
;      - no framing error, hw/sw overflow error counting
; 003: library version
;

	list		p=16f628

#include <p16f628.inc>
	GLOBAL uart_init, uart_int_handler, uart_int_tx, uart_int_rx
	GLOBAL uart_tx, uart_rx, uart_rx_wait

; * constants

BPBITS	EQU 0x07 	; buffer length-1 (blen must be 2^n)
XTAL_FREQ = d'4000000'	; crystal frequency in Hertz.
baudrate = d'19200'	; desired baudrate.

spbrg_value = (((d'10'*XTAL_FREQ/(d'16'*baudrate))+d'5')/d'10')-1

; * variables

	UDATA
rxbuf	res 0x08	; receive buffer (blen bytes)
txbuf	res 0x08	; transmit buffer (blen bytes)

txps	res 1		; pointers to buffer start/end
txpe	res 1
rxps	res 1
rxpe	res 1
temp	res 1

	; for debugging purposes only:
	GLOBAL txps, txpe, rxps, rxpe

	CODE

uart_init
	; initalize UART
	; pre-requisites: initialize TRISB <2> as 0(out) <1> as 1(in)
	;

gie04	bcf	INTCON,GIE
	btfsc	INTCON,GIE
	goto 	gie04

	bsf STATUS, RP0
	movlw (1<<TXEN)|(1<<BRGH)
	movwf TXSTA

	;txsta=Transmit STAtus and control register.
	; CSRC <7> (0) don't care in asynch mode
	; TX9  <6>  0  select 8 bit mode
	; TXEN <5>  1  enable transmit function 
	;      *MUST* be 1 for transmit to work!!!
	; SYNC <4>  0 asynchronous mode. 
	;      *MUST* be 0 !!!
	;      If NOT 0 the async mode is NOT selected!
	;      <3>  (0) not implemented
	; BRGH <2>  0 disable high baud rate generator !!!
	; !!!  errata sheet says NOT to set high for 
	;      16C74A due to excessive receive errors!
	; 1    (0) trmt is read only.
	; TX9D <0>  (0)  tx9d data cleared to 0.

;   For brgh=0       baudrate=Fosc/(64(spbrg+1))
;   So when brgh=0   spbrg_value = (xtal_freq/(baudrate*d'64'))-1

;   For brgh=1       baudrate=Fosc/(16(spbrg+1)) 
;   So when brgh=1   spbrg_value = (xtal_freq/(baudrate*d'16'))-1

;* ; calculates baudrate when BRGH = 1, adjust for rounding errors
;* spbrg_value = (((d'10'*XTAL_FREQ/(d'16'*baudrate))+d'5')/d'10')-1
;*
;* ; calculates baudrate when BRGH = 0, adjust for rounding errors
;* spbrg_value = (((d'10'*XTAL_FREQ/(d'64'*baudrate))+d'5')/d'10')-1

	movlw	spbrg_value	;set baud rate generator value
	movwf	SPBRG

	bcf	STATUS,RP0

	movlw (1<<SPEN)|(1<<CREN)
	movwf RCSTA

	;RCSTA=ReCeive STATUS and control register

	; 7 spen 1=rx/tx set for serial uart mode
	;   !!! very imPORTAnt to set spen=1
	; 6 rc8/9 0=8 bit mode
	; 5 sren 0=don't care in uart mode
	; 4 cren 1=enable constant reception
	; !!! (and low clears errors)
	; 3 not used / 0 / don't care
	; 2 ferr input framing error bit. 1=error
	; 1 oerr input overrun error bit. 1=error
	;!!! (reset oerr by neg pulse clearing cren)
	;you can't clear this bit by using bcf.
	;It is only cleared when you pulse cren low. 
	; 0 rx9d input (9th data bit). ignore.

	; delay here to wait for MAX232 charge-pump normal voltage

	movf	RCREG,w		;clear uart receiver
	movf	RCREG,w		; including fifo
	movf	RCREG,w		; which is three deep.

	movlw	0		;any character will do.
	movwf	TXREG		;send out dummy character
				; to get transmit flag valid!

; if you forget to send out an initial dummy 
; character, then the txif flag never goes high and your serial input
; routine will go round and round in circles forever waiting for txif
; to go high

	clrf txps
	clrf txpe
	clrf rxps
	clrf rxpe

	bsf STATUS, RP0
	movf PIE1, W
	iorlw (1<<RCIE)
	movwf PIE1 ; enable receive & transmit interrupt flags
	bcf STATUS, RP0

	clrf PIR1 ; clear pending peripherial interrupts

	bsf INTCON, PEIE	; enable peripherial interrupts
	bsf INTCON, GIE		; enable interrupts
	return

uart_int_handler
	call uart_int_tx

;to receive:
;to enable reception of a byte, cren must be =1. on any error, recover by
;pulsing cren low then back to high. when a byte has been received the 
;rcif flag will be set. rcif is automatically cleared when RCREG is read
;and empty. RCREG is double buffered, so it is a two byte deep fifo. if a
;third byte comes in, then oerr is set. you can still recover the two bytes
;in the fifo, but the third (newest) is lost. cren must be pulsed negative
;to clear the oerr flag. on a framing error ferr is set. ferr is 
;automatically reset when RCREG is read, so errors must be tested for 
;*before* RCREG is read. it is *NOT* recommended that you ignore the 
;error flags. eventually an error will cause the receiver to hang up 
;if you don't clear the error condition.

uart_int_rx
	btfsc	RCSTA,OERR
	goto	overerror	;if overflow error...
	btfsc	RCSTA,FERR
	goto	frameerror	;if framing error...

uart_ready
	btfss	PIR1,RCIF
	return			; not ready

uart_gotit			;eventually we get something!

	movlw rxbuf		; rxbuf[rxpe] = RCREG
	addwf rxpe, W
	movwf FSR
	incf rxpe, W		; rxpe++
	andlw BPBITS
	movwf rxpe

	movf	RCREG,w		;recover uart data
	movwf	INDF		;save for later
	return

overerror	   		;over-run errors are usually
				;caused by the incoming data
				;building up in the fifo.
				;this is often the case when
				;the program has not read the
				;uart in a while.
				;flushing the fifo will
				;allow normal input to resume.
				;note that flushing the fifo
				;also automatically clears 
				;the ferr flag.
				;pulsing cren resets the oerr flag

	bcf	RCSTA,CREN	;pulse cren off...
	movf	RCREG,w		;flush fifo
	movf	RCREG,w		; all three elements.
	movf	RCREG,w
	bsf	RCSTA,CREN	;turn cren back on.
				;this pulsing of cren
				;will clear the oerr flag.
	goto	uart_int_rx	;try again...

frameerror			;framing errors are usually
				;due to wrong baud rate
				;coming in.

	movf	RCREG,w		;reading RCREG clears ferr flag.
	goto	uart_int_rx	;try again...


;you may load transmit data when txif=1. this is reset automatically
;when data is loaded. can not be reset by user using any other method!
;data transmission occurs when TXREG is loaded with data and txen is set=1.
;!!! GOTCHA !!! normally just keep txen=1 all the time.

;data transmission occurs when TXREG is loaded with data and txen is set=1
;txmt and txif are set/reset automatically. txmt shows the state of the 
;shift reg. But. You guessed it. !!! GOTCHA !!! txmt is in bank 1. 
;Don't you just LOVE switching banks? I detest it.
;In my routines I use txif exclusively. Works like a charm.

uart_int_tx
	btfss PIR1, TXIF
	goto uart_bf		; set TXIE, no room in HW buffer this time

	; there is room in the hw buffer to load data
	; check if there's data waiting to be sent in sw buffer

	movf txps, W
	xorwf txpe, W
	btfsc STATUS, Z	; if (txps == txpe) { clear TXIE; return }
	goto uart_be	; (sw buffer is empty...)

	movlw txbuf	; TXREG=txbuf[txps]
	addwf txps, W
	movwf FSR
	incf txps, W	; txps++
	andlw BPBITS
	movwf txps

	movf INDF, W
	movwf TXREG	; load data to be sent...

	; if the shift register was empty, the TXREG is in 1 ck time
	; moved to the shift reg, so TXIF will indicate TXREG is empty

	; DMB: only do one character at a time....
	; goto uart_int_tx; check again for room in HW buffer

uart_be	bsf STATUS, RP0
	bcf PIE1, TXIE	; don't trigger TX interrupt
	bcf STATUS, RP0
	return

uart_tx	movwf temp

	movlw txbuf	; txbuf[txpe] = W
	addwf txpe, W
	movwf FSR

	movf temp, W
	movwf INDF

	incf txpe, W	; txpe++
	andlw BPBITS
	movwf txpe

uart_bf	bsf STATUS, RP0
	bsf PIE1, TXIE	; trigger a TX interrupt when
	bcf STATUS, RP0	; TXREG gets empty
	return

uart_rx_wait
	movf rxps, W
	xorwf rxpe, W
	btfsc STATUS, Z
	goto uart_rx_wait

uart_rx
	movf rxps, W
	xorwf rxpe, W
	btfsc STATUS, Z	; if (rxps == rxpe) return Z=1
	return		;  (sw buffer is empty...)

	movlw rxbuf	; W=rxbuf[rxps]
	addwf rxps, W
	movwf FSR
	incf rxps, W	; rxps++
	andlw BPBITS
	movwf rxps

	movf INDF, W
	bcf STATUS, Z
	return		; return Z=0

	end

