;***************************************************************************
;*
;* Title: CHILLED MIRROR HYGROMETER
;* Author: Sheryar Shah
;* Last updated: 2-7-14
;* Target: ATmega16 @ 1 MHz
;*
;*
;* PROGRAM DESCRIPTION: Measures the dewpoint direcly. Controls the
;* Heating/cooling of a TEC DPH board to heat/cool a mirror. Once
;* the mirror is cool enough, condensation appears on it blocking
;* the rays of an infared LED. When the LED is block photo transitor
;* turns off signalling the dew point has been reched. Has three modes:
;* 1) When pushbuton 2 is pressed, reads and displays the ambient temperature 
;* of room in degrees F 
;* 2) When pushbutton 0 is pressed reads and displays mirror temperature 
;* constantly until dew is reached, and than heats the mirror to eliminate
;* contaminants on mirror and than cools again, repeating the process
;* 3) When pushbutton 1 is pressed, it displays the last known dewpoint
;* PORT DESCRIPTION:
;* PORTB - data outputs to 7-segment display
;* PORTA - PA0-PA2 control DIG0-DIG2 tansistors respectivly
;* PA5 measures the AMBIENT, PA6 polls for DEWPOINT
;* PORTD - PD3 (INT1) used to call display_post
;* - PD4 16 bit PWM signal with changable duty cycle
;* controls H-bridge
;* - PD5 selects heating (0) or coolint (1)
;*
;* IMPORTANT REGISTERS:
;* r0 - DIG0 displays data for digit 0
;* r1 - DIG1 displays data for digit 1
;* r2 - DIG2 displays data for digit 2
;* r29 - temporary holding register
;*
;* Included Interrupt Service Routines:
;* -1) keypress_isr: activated when a rising edge is observed
;* at PD3. Used to call display-post

;* Intenal voltage reference is used of ATMega16 for ADC conversion

;* IMPORTANT EXTERNAL DEVICE[S]:
;* - LM34: temparature to voltage converter conncter to PD5. takes
;* Ambient temperature reading and convertes it to a voltage signal
;* to be converted by the ATMega16's adc and store as a reference
;* - TEC (Thermo Electric cooler): Heats/cools a mirror to force
;* condensation. Pin 1 controls H-bridge, Pin 3 controls Heat/cool,
;* Pin 9 sends the "DP found signal" when photo transistor is blocked
;* Pin 8 is connected to an LM34 attached to the mirror.
;* Pin connections to micro controller:
;* *Pin1 - PD4 Pin3 - PD5 Pin9 - PD6 Pin8 - PA6

;*****************************************************************************
.nolist
.include"m16def.inc"
.list

reset:
.org RESET              					 ;reset interrupt vector
    jmp start		        				 ;program starts here at reset

.org INT1addr								 ;INT1 interrupt vector set
	jmp key_press_isr			

start:
;*********************INITIALIZATION***********************************
	ldi r16, 0b10110000						;PD0-PD3=PB, PD4=EN, PD5=C/H, PD6=DPS
	out DDRD, r16				

	ldi r17, $FF 							;Outputs for LEDs
	out DDRB, r17	

	ldi r17, 0b00000111 					;PA0-PA3 are outputs for transistors, PA5-PA7 for ADC
	out DDRA, r17 					

	ldi r16, 0b11100000						;PC7 for Fan, and PC6 for LED
	out DDRC, r16

	ldi r20, LOW(RAMEND) 					;load low byte of stack pointer
	out SPL, r20
	ldi r20, HIGH(RAMEND) 					;load high byte of stack pointer
	out SPH, r20

	clr r16								   	;clear random value in r16

;*******************LED FUNCTIONALITY TEST******************************
	ldi r17, $f8							;turn transistors on 
	out PORTA, r17
	ldi r25, $00							;display all 8's on LEDs
	out PORTB, r25		
	sbi PORTC, 6							;turn diode LED on 
	call test_delay							;keep LEDS on for 1 sec

	cbi PORTC, 6							;turn LED off
	ldi r17, $ff							;turn transisitors off
	out PORTA, r17

;*****************INT1 SETUP**************************
	ldi r20, (1 << ISC11) | (1 << ISC10)    ;interrupt sense control bits
    out MCUCR, r20     						;rising edge at INT0 requests interrupt
    ldi r20, 1<<INT1   						;enable interrupt request at INT1   
    out GICR, r20

;******************TIMER/COUNTER1 SETUP*****************

	ldi r20, $FF
	out OCR1AH, r20
	ldi r20, $FF
	out OCR1AL, r20 						;sets $0FFF as TOP value

	;PWM Initialization of 100% duty cycle

	ldi r20, $ff
	out OCR1BH, r20
	ldi r20, $ff
	out OCR1BL, r20 						;duty cycle starts at 100% 


	ldi r20, $23 			
	out TCCR1A, r20 						;non inverting and mode 15
	ldi r20, $19 			
	out TCCR1B, r20 						;wgm=mode 15

;****************ADC SETUP******************************
	ldi r20, $87							;set prescale to 128
	out ADCSRA, r20
	ldi r20, $C5							;internal VREF=2.56V, for ambient temperature
	out ADMUX, r20

    sei      								;set global interrupt

	clr r0									;clear random value in r0
	clr r1									;clear random value in r1
	clr r2									;clear random value in r2
	clr r18									;clear random value in r18
	clr r21									;clear random value in r21
	clr r25									;clear random value in r25		
	clr r26									;clear random value in r26
	clr r27									;clear random value in r27
	clr r28									;clear random value in r28
	clr r29									;clear random value in r29
	
	;*********** test code added task 2
	ldi r25, $50
	mov r14, r25 

ADC_loop:
	sei										;re-enable global interrupt
	sbi ADCSRA, ADSC						;start convertion

ADC_polling:	
	sbis ADCSRA,ADIF						;check if converstion is done
	rjmp ADC_polling						;if not jump back and keep converting untill done
	sbi ADCSRA, ADIF						;write 1 to clear ADIF flag

	in r26, ADCL							;read the first 8 ls bits
	in r27, ADCH							;read the 2 ms bits
	ldi r25, 0b0000000 						
	
;***********************Division by 4************************

	lsr r27									;first right shift
	ror r26
	ror r25

	lsr r27									;second right shift
	ror r26
	ror r25
	nop
	
;**********estimatation for decimal BCD value***************
	swap r25
	lsr r25
	lsr r25
	clc
	
;*****************BCD conversion****************************
	call BCD_conversion
	call BCD_conversion1

	ldi r20, $D3
	out OCR1BH, r20
	ldi r20, $B0
	out OCR1BL, r20							;slow down pwm to 70%

;******* test code added task 2
	cp r26, r14								;if temperature reaches 80(ambient) start cooling
	breq cooling1							;if set

;the problem might be in wrong place
	cpi r26, 60								;if temperature=60, slow down using PWM
	breq slow_down

;*****************TEMPERATURE DISPLAY*************************
TEM_display:
	call display_post
	call display_post1
	
	sbic PIND, 6							;poll, if dew point reached turn LED off and TEC off
	call dew_reached
	
	rjmp ADC_loop							;restart the process

cooling1:
	;save temperature for pb1
	sbi PORTC, 7							;rapidly cool the mirror, by activating fan
	sbi PORTD, 5							;cool the mirror
	cbi PORTC, 6							;turn LED off
	rjmp ADC_loop							;restart the process

dew_reached:
	mov r8, r0								;save the dew value to be displayed on pb1
	mov	r9, r1								;save the dew value to be displayed on pb1
	mov r10, r2								;save the dew value to be displayed on pb1

	sbi PORTC, 6							;turn LED on
	;cbi PORTD, 4							;turn TEC off
	cbi PORTD, 5							;start heating
	cbi PORTC, 7							;turn fan off
	;call delay_15sec
	ret 

slow_down:
	ldi r20, $D3
	out OCR1BH, r20
	ldi r20, $b0
	out OCR1BL, r20
	rjmp ADC_loop							;slow down to 70%

;***********************FOR DISPLAYING***********************************
display_post:
	in r18, SREG								;save SREG
	push r18								 	;push that value onto stack

	ldi r17, 0b00000110							;turn transitor on for first LED
	mov r29, r22								;move to TEMP
	andi r29, $0f
	call hex_2_7seg

	mov r2, r29									;for simulation purposes
	out PORTB, r2
	
	ldi r17, 0b00000101							;turn transistor on for second LED
	mov r29, r21								;move to TEMP
	andi r29, $0f

	call hex_2_7seg

	mov r0, r29									;for simulation purposes
	out PORTB, r0
	
	pop r18										;restore SREG
	out SREG, r18

	ret

display_post1:
	ldi r17, 0b00000011							;turn transistor on for third LED
	mov r29, r25								;move to TEMP
	andi r29, $03
	call hex_2_7seg

	mov r1, r29									;for simulation purposes
	out PORTB, r1

	ret	

;*************WHEN PB1 PRESSED(TO DISPLAY DEW TEMPERATURE*********************								
display_post3:
	sei
	in r18, SREG								;save SREG
	push r18								 	;push that value onto stack

	ldi r17, 0b00000110							;turn transitor on for first LED
	mov r29, r8									;move to TEMP
	;andi r29, $0f
	call display

	;mov r2, r29								;for simulation purposes
	;out PORTB, r2
	
	ldi r17, 0b00000101							;turn transistor on for second LED
	mov r29, r9									;move to TEMP
	;andi r29, $0f

	call display

;	mov r0, r29									;for simulation purposes
;	out PORTB, r0
	
	pop r18										;restore SREG
	out SREG, r18

	ret

display_post4:
	ldi r17, 0b00000011							;turn transistor on for third LED
	mov r29, r10								;move to TEMP
	;andi r29, $03
	call display

;	mov r1, r29									;for simulation purposes
;	out PORTB, r1

	ret	
;***************************************************************************
;*
;* "keypress_isr" - interupt service routine 
;*
;* Description: Interrupt occurs when a rising edge is observed at PD3.
;* The keypress_isr is used to either display the ambient temperature of room
;* when pushbutton 2 is pressed, shows the monitoring mode of TEC when pb 0 is 
;* pressed or operational mode when pb 1 is pressed
;* Author: Sheryar Shah
;* Version 1.1
;* Last updated: 2-7-14
;***************************************************************************

key_press_isr:

	in r21, PIND							;read inputs
	andi r21, $07							;mask the 5 ms bits	

	cpi r21, $00							;if 0 start dew point measurement
	breq monitoring_mode

	cpi r21, $01							;if 1, only look at dew point
	breq operational_mode

	cpi r21, $02							;if 2, read ambient temp of room
	breq ambient_roomtemp

monitoring_mode:
	ldi r18, $C7							;for ADC 7, mirror temperature	
	out ADMUX, r18
	sbi PORTD, 4							;activate EN/TEC	
	cbi PORTD, 5							;heat the mirror

cooling:
	sbi PORTC, 7							;rapidly cool the mirror, by activating fan
	sbi PORTD, 5							;cool the mirror
	cbi PORTC, 6							;turn LED off
	
	rjmp returni

operational_mode:
	ldi r18, $C7							;read from the same port,pin
	out ADMUX, r18

	rjmp TEM_display1

ambient_roomtemp:
	ldi r18, $C5							;read LM34 connected to ADC5
	out ADMUX, r18
	
	rjmp returni	

returni:
	reti									;return back to where the interrupt was called
	
TEM_display1:
	call display_post3
	call display_post4
	call dew_reached1

	reti

dew_reached1:
	cbi PORTC, 6							;turn LED off
	cbi PORTD, 4							;turn TEC off
	cbi PORTC, 7							;turn fan off

	ret

;***************************************************************************
;*
;* "hex_2_7seg" - Looks Up Hextable value for r16 (HEX)
;*
;* Description: when the subroutine is called, it will lookup the equivalent
;* HEX pattern for r29 (HEX)to output to the seven seven display 
;*
;* Author: Sheryar Shah
;* Version 1.0
;* Last updated: 10-23-13
;*
;*
;* Parameters: r29 is the input to be converted to a HEX pattern
;*
;***************************************************************************

hex_2_7seg:
	ldi ZH, high (hextable * 2) 			;set Z to point to start of table
	ldi ZL, low (hextable * 2)
	ldi r16, $00 							;add offset to Z pointer
	add ZL, r29
	adc ZH, r16
	lpm r29, Z 								;load byte from table pointed to by Z

;Table of segment values to display digits 0 - F
hextable: .db $01, $4F, $12, $06, $4C, $24, $20, $0F, $00, $0C, $08, $60, $31, $42, $30, $38

display:	
	out PORTB, r29							;output image to LED
	out PORTA, r17   			   		    ;ENABLE transistor for digX.				      
	call var_delay	
	ret

;***************************************************************************
;*
;* "var_delay" - helps get rid of ghosting
;*
;* Author: Sheryar Shah
;* Version 1.0
;* Last updated: 10-23-13
;*
;*
;* Parameters:
;* r16 is used as the delay parameter
;* r17 is used as a the multiplying delay parameter
;*
;*
;***************************************************************************

var_delay:									;delay for keeping the LEDs on, and to get rid of ghosting
		ldi r17, 45			
	outer_loop4:
		ldi r16, 45
	inner_loop5:
		dec r16
		brne inner_loop5
		dec r17
		brne outer_loop4
		ret

;***************************************************************************
;*
;* "test_delay" - testing all LEDs for 1 sec
;*
;* Description: When powered on, it turns all LEDs on for 1 sec and turns them
;* off afterwards
;*
;* Author: Sheryar Shah
;* Version 1.0
;* Last updated: 12-5-13
;*
;*
;*
;*
;***************************************************************************

test_delay:									;to turn on intial value 8 for 1 sec and than turn off and continue with program
		ldi r18, 6
	outer_loop6:
		ldi r17, 246		
	outer_loop7:
		ldi r16, 246
	inner_loop8:
		dec r16
		brne inner_loop8
		dec r17
		brne outer_loop7
		dec r18
		brne outer_loop6
		ret

;***************************************************************************
;* Atmel Corporation
;*
;* Title:		BCD Arithmetics
;* Version:		1.4
;* Last updated: 2004.02.02
;* Target:		AT90Sxxxx (All AVR Devices)
;* "bin2BCD8" - 8-bit Binary to BCD conversion

;* Support E-mail:	avr@atmel.com
;*
;* This subroutine converts an 8-bit number (fbin) to a 2-digit
;* BCD number (tBCDH:tBCDL).
;*
;* Number of words	:6 + return
;* Number of cycles	:5/50 (Min/Max) + return
;* Low registers used	:None
;* High registers used  :2 (fbin/tBCDL,tBCDH)
;*
;* Included in the code are lines to add/replace for packed BCD output.
	
;* VERSION HISTORY
;* 1.1 Original version
;* 1.2 Fixed error in BCDADD routine
;* 1.3 Fixed error in BCD2BIN8 routine (packed version)
;* 1.4 Fixed missing Carrige Returns for windows
;***************************************************************************

BCD_conversion:

	;***** Subroutine Register Variables

.def	fbin	=r21		;8-bit binary value
.def	tBCDL	=r21		;BCD result MSD
.def	tBCDH	=r22		;BCD result LSD

;***** Code

mov	fbin, r26
rcall	bin2BCD8	;result: tBCDH:tBCDL = 0505
ret

bin2bcd8:
	clr	tBCDH		;clear result MSD
bBCD8_1:subi	fbin,10		;input = input - 10
	brcs	bBCD8_2		;abort if carry set
	inc	tBCDH		;inc MSD
;---------------------------------------------------------------------------
;				;Replace the above line with this one
;				;for packed BCD output				
;	subi	tBCDH,-$10 	;tBCDH = tBCDH + 10
;---------------------------------------------------------------------------
	rjmp	bBCD8_1		;loop again
bBCD8_2:subi	fbin,-10	;compensate extra subtraction
;---------------------------------------------------------------------------
;				;Add this line for packed BCD output
;	add	fbin,tBCDH	
;---------------------------------------------------------------------------	
	ret

;***************************************************************************
;* Atmel Corporation
;*
;* Title:		BCD Arithmetics
;* Version:		1.4
;* Last updated: 2004.02.02
;* Target:		AT90Sxxxx (All AVR Devices)
;* "bin2BCD8" - 8-bit Binary to BCD conversion

;* Support E-mail:	avr@atmel.com
;*
;* This subroutine converts an 8-bit number (fbin) to a 2-digit
;* BCD number (tBCDH:tBCDL).
;*
;* Number of words	:6 + return
;* Number of cycles	:5/50 (Min/Max) + return
;* Low registers used	:None
;* High registers used  :2 (fbin/tBCDL,tBCDH)
;*
;* Included in the code are lines to add/replace for packed BCD output.
	
;* VERSION HISTORY
;* 1.1 Original version
;* 1.2 Fixed error in BCDADD routine
;* 1.3 Fixed error in BCD2BIN8 routine (packed version)
;* 1.4 Fixed missing Carrige Returns for windows
;***************************************************************************

BCD_conversion1:

	;***** Subroutine Register Variables

.def	fbin	=r23		;8-bit binary value
.def	tBCDL	=r23		;BCD result MSD
.def	tBCDH	=r24		;BCD result LSD

;***** Code

mov	fbin, r25
rcall	bin2BCD81	;result: tBCDH:tBCDL = 0505
ret

bin2bcd81:
	clr	tBCDH		;clear result MSD
bBCD8_11:subi	fbin,10		;input = input - 10
	brcs	bBCD8_21		;abort if carry set
	inc	tBCDH		;inc MSD
;---------------------------------------------------------------------------
;				;Replace the above line with this one
;				;for packed BCD output				
;	subi	tBCDH,-$10 	;tBCDH = tBCDH + 10
;---------------------------------------------------------------------------
	rjmp	bBCD8_11		;loop again
bBCD8_21:subi	fbin,-10	;compensate extra subtraction
;---------------------------------------------------------------------------
;				;Add this line for packed BCD output
;	add	fbin,tBCDH	
;---------------------------------------------------------------------------	
	ret




