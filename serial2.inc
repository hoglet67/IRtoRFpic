;**********************************************************************
;                                                                     *
;    Filename:	    serial2.inc                                       *
;    Date:                                                            *
;    Author:        el@jap.hu                                         *
;    Company:       http://jap.hu/electronic/                         *
;                                                                     *
;    Description:   interrupt-driven hardware serial driver           *
;                                                                     *
;**********************************************************************

	EXTERN uart_init, uart_int_handler, uart_int_rx, uart_int_tx, uart_tx, uart_rx, uart_rx_wait

