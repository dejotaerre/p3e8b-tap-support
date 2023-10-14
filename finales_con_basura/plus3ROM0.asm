		OUTPUT	"p3t_rom0.rom"

		ORG	$0000

; **************************************************
; *** SPECTRUM +3 ROM 0 DISASSEMBLY (EDITOR ROM) ***
; **************************************************

; The Spectrum ROMs are copyright Amstrad, who have kindly given permission
; to reverse engineer and publish Spectrum ROM disassemblies.


; =====
; NOTES
; =====

; ------------------------
; Disassembly Contributors
; ------------------------
; Garry Lancaster
;
; The ROM disassembly was created with the aid of dZ80 V1.10, and incorporates work from
; "The canonical list of +3 oddities" by Ian Collier.

; -----------------
; Assembler Details
; -----------------

; This file can be assembled to produce a binary omp348k.romage of the ROM
; with Interlogic's Z80ASM assembler (available for Z88, QL, DOS and Linux).
; Note that the defs directive is used and this causes a block of $00 bytes to be created.

		ORG	$0000

l0000:		DI
		LD	BC,$6C03

l0004:		DEC	BC		; Delay for approx 0.2s
		LD	A,B
		OR	C
		JR	NZ,l0004
		JP	l010f		; go to test memory

		DB	"ED"		; Editor ROM ID
		DS	2

; RST 10: call the RST 10 routine in ROM 3 to print a character

l0010:		RST	28H
		DW	$0010		; call RST 10 in ROM 3
		RET

		DS	4

; RST 18: call the RST 18 routine in ROM 3 to collect a character

l0018:		RST	28H
		DW	$0018		; call RST 18 in ROM 3
		RET

		DS	4

; RST 20: call the RST 20 routine in ROM 3 to collect next character

l0020:		RST	28H
		DW	$0020		; call RST 20 in ROM 3
		RET

		DS	4

; RST 28 : Call a routine in ROM 3, then return to ROM 0
; The address following the RST 28 instruction is called, then control
; is returned to the instruction following the address

l0028:		EX	(SP),HL		; save HL, get return address
		PUSH	AF		; save AF
		LD	A,(HL)		; A=low byte of address to call
		INC	HL
		INC	HL		; HL=address of instruction to return to
		LD	(RETADDR),HL	; save

l0030:		DEC	HL
		LD	H,(HL)
		LD	L,A		; HL=address to call in ROM 3
		POP	AF		; restore AF
		JP	l00ae		; jump on

		DS	1

; The maskable interrupt routine, called every 50ms while in IM1

l0038:		PUSH	HL		; save HL
		LD	HL,o0048
		PUSH	HL
		LD	HL,SWAP
		PUSH	HL
		LD	HL,o0038
		PUSH	HL
		JP	SWAP		; call MASK-INT and KEY-INT in ROM 3
		POP	HL		; restore HL
		DI			; disable interrupts again
		CALL	l0074
		EI			; re-enable interrupts
		RET

FREE_ROM0_0:	EQU	$

		;...
		;...

R0_FREE_0:	EQU	$17-($-FREE_ROM0_0)
ROM0_SPARE0:	DS	R0_FREE_0

; NMI routine

l0066:		PUSH	AF		; save AF & HL
		PUSH	HL
		LD	HL,(NMIADD)
		LD	A,H
		OR	L
		JR	Z,l0070		; skip if no routine (NMIADD=0)
		JP	(HL)		; else execute
l0070:		POP	HL		; restore registers
		POP	AF
		RETN

; Disk motor timeout subroutine
; Called by maskable interrupt to turn off disk motor when timeout occurs

l0074:		PUSH	AF		; save AF & BC
		PUSH	BC
		LD	BC,PBANKM
		LD	A,(BANKM)
		OR	$07
		OUT	(C),A		; get page 7 (+3DOS workspace)
		LD	A,($E600)	; check motor off timeout
		OR	A
		JR	Z,l00a3		; move on if already off
		LD	A,(FRAMES)
		BIT	0,A
		JR	NZ,l00a3	; only decrement every other time
		LD	A,($E600)
		DEC	A
		LD	($E600),A	; decrement motor off timeout
		JR	NZ,l00a3	; move on if still should be on
		LD	BC,PBANK678
		LD	A,(BANK678)
		AND	$F7
		LD	(BANK678),A
		OUT	(C),A		; turn motor off
l00a3:		LD	BC,PBANKM
		LD	A,(BANKM)
		OUT	(C),A		; page previous memory back in
		POP	BC		; restore registers
		POP	AF
		RET

; Continuation of RST 28: call a routine in ROM 3

l00ae:		LD	(TARGET),HL	; save ROM 3 address in TARGET
		LD	HL,YOUNGER
		EX	(SP),HL		; stack YOUNGER address beneath TOS
		PUSH	HL
		LD	HL,(TARGET)	; get HL=target address in ROM 3
		EX	(SP),HL		; restore HL & save target address on stack
		JP	SWAP		; jump to SWAP - pages in ROM 3, returns to
					; target routine which returns to YOUNGER
					; where ROM 0 is paged back and jump made
					; back to RETADDR


; Here follows the five paging subroutines which are copied into
; the system variables on startup

; Enter at SWAP to change ROM 0<->3 or ROM 1<->2

l00bd:		PUSH	AF		; save AF & BC
		PUSH	BC
		LD	BC,PBANKM
		LD	A,(BANKM)	; get copy of last OUT to $7FFD
		XOR	$10		; change ROM 0<->1 or ROM 2<->3
		DI			; disable interrupts
		LD	(BANKM),A
		OUT	(C),A		; page new ROM

; Enter at STOO with interrupts disabled and AF/BC stacked
; to change ROM 0<->2 or ROM 1<->3

l00cd:		LD	BC,PBANK678
		LD	A,(BANK678)	; get copy of last OUT to $1FFD
		XOR	$04		; change ROM 0<->2 or ROM 1<->3
		LD	(BANK678),A
		OUT	(C),A		; page new ROM
		EI			; re-enable interrupts
		POP	BC		; restore registers
		POP	AF
		RET

; Enter at YOUNGER with return address in RETADDR to swap
; ROM 0<->3 or ROM 1<->2 and return there

l00de:		CALL	SWAP		; swap ROM 0<->3 or ROM 1<->2
		PUSH	HL		; save HL
		LD	HL,(RETADDR)	; get return address from system vars
		EX	(SP),HL		; restore return address & HL
		RET

; Enter at REGNUOY with return address in RETADDR to swap
; ROM 0<->2 or ROM 1<->3 and return there

l00e7:		PUSH	HL		; save HL
		LD	HL,$5B34
		EX	(SP),HL		; place $5B34 as return address
		PUSH	AF		; save AF & BC
		PUSH	BC
		JP	STOO		; swap ROM 0<->2 or ROM 1<->3 and return here
		PUSH	HL		; save HL
		LD	HL,(RETADDR)	; get return address from system vars
		EX	(SP),HL		; restore return address & HL
		RET

; Enter at ONERR to page in Syntax ROM (ROM 1) and jump to error handler

l00f7:		DI
		XOR	A
		LD	BC,PBANK678
		OUT	(C),A		; ensure ROM 0 or 1 is paged
		LD	(BANK678),A
		SET	4,A
		LD	BC,PBANKM
		OUT	(C),A		; ensure ROM 1 is paged
		LD	(BANKM),A
		EI			; enable interrupts
		JP	m253a		; jump to error handler in ROM 1

; Test memory at startup & initialise

l010f:		LD	B,$08		; 8 pages to clear

l0111:		LD	A,B
		EXX
		DEC	A
		LD	BC,PBANKM
		OUT	(C),A		; page next RAM page to $C000
		LD	HL,$C000
		LD	DE,$C001
		LD	BC,$3FFF
		LD	(HL),$00
		LDIR			; clear it
		EXX
		DJNZ	l0111		; back for more pages
		XOR	A
		LD	HL,$DCBA	; an address in top 16K of ROM
		LD	BC,PBANKM	; memory paging address

l0130:		LD	DE,$0108	; E=8 bits to test, D=bit 0
		OUT	(C),A		; get next page to segment 3
		EX	AF,AF'		; save A'=page

l0136:		LD	A,D		; test to see if bit can be set
		LD	(HL),A
		LD	A,(HL)
		AND	D
		JP	Z,l0367		; jump if memory not re-read correctly
		CPL			; test to see if bit can be reset
		LD	(HL),A
		LD	A,(HL)
		AND	D
		JP	NZ,l0367	; jump if memory not re-read correctly
		RLC	D
		DEC	E
		JR	NZ,l0136	; loop back to test other bits
		EX	AF,AF'
		INC	A
		CP	$08
		JR	NZ,l0130	; loop back to test other pages
		LD	C,$FD
		LD	D,$FF
		LD	E,$BF
		LD	B,D
		LD	A,$0E
		OUT	(C),A		; select AY register 14 (RS232/AUX)
		LD	B,E
		LD	A,$FF
		OUT	(C),A		; set all RS232/AUX lines high
		JR	l0167		; move on, with page 7 at $C000


; Apparently unused section, possibly originally intended to
; flag a memory error

		EXX
		LD	A,B
		OUT	($FE),A
l0165:		JR	l0165


; More initialisation (with page 7 at $C000)

l0167:		XOR	A
		EX	AF,AF'		; A' clear to show reset, not NEW
		LD	SP,$6000	; set stack within page 5

l016c:		LD	B,D
		LD	A,$07
		OUT	(C),A		; select AY register $07
		LD	B,E
		LD	A,$FF
		OUT	(C),A		; initialise AY chip (?)
		LD	DE,SWAP
		LD	HL,l00bd
		LD	BC,$0052
		LDIR			; copy paging subroutines to system vars
		LD	A,$CF
		LD	(RAMRST),A	; place RST 8 instruction at RAMRST
		LD	HL,$FFFF
		LD	(P_RAMT),HL	; set P RAMT to 64K
		LD	DE,$3EAF	; prepare to copy chars A-U from ROM 3
		LD	BC,$00A8	; to UDG area
		EX	DE,HL
		RST	28H		; execute a LDDR from ROM 3 to copy them
		DW	o1661
		EX	DE,HL
		INC	HL
		LD	(UDG),HL	; store address of first UDG
		DEC	HL
		LD	BC,$0040
		LD	(RASP),BC	; set RASP and PIP
		LD	(RAMTOP),HL	; set RAMTOP below UDGs
		LD	HL,FLAGS3
		res	7,(HL)		; reset bit 7 of FLAGS3 (??)
		LD	HL,DUMPLF
		LD	(HL),$09	; set DUMPLF

; The NEW command enters here

l01b0:		LD	HL,$3C00
		LD	(CHARS),HL	; set CHARS
		IM	1		; set interrupt mode 1
		LD	IY,ERR_NR	; IY points to ERR NR
		SET	4,(IY+$01)	; set "+3 BASIC mode"
		LD	HL,FLAGS3
		res	3,(HL)		; set "print to Centronics"
		SET	2,(HL)		; set "print expanded tokens"
		LD	HL,$000B
		LD	(BAUD),HL	; set BAUD
		XOR	A
		LD	(SERFL),A	; clear SERFL
		LD	(COL),A		; clear COL
		LD	(TVPARS),A	; clear TVPARS
		LD	HL,$EC00
		LD	($FF24),HL	; ???
		LD	A,$50
		LD	(WIDTH),A	; set WIDTH
		LD	HL,$000A
		LD	(RC_START),HL	; set RCSTART
		LD	(RC_STEP),HL	; set RCSTEP
		LD	A,$54
		LD	(LODDRV),A	; set LODDRV to "T"
		LD	(SAVDRV),A	; set SAVDRV to "T"
		LD	HL,$5CB6
		LD	(CHANS),HL	; set CHANS immediately after system vars
		LD	DE,l03b8
		LD	BC,$0015
		EX	DE,HL
		LDIR			; copy initial channel information
		EX	DE,HL
		DEC	HL
		LD	(DATADD),HL	; set DATADD after CHANS
		INC	HL
		LD	(PROG),HL	; set PROG after DATADD
		LD	(VARS),HL	; set VARS
		LD	(HL),$80	; store end of variables marker
		INC	HL
		LD	(E_LINE),HL	; set ELINE after VARS
		LD	(HL),$0D	; store end of line marker
		INC	HL
		LD	(HL),$80	; store end of ELINE marker
		INC	HL
		LD	(WORKSP),HL	; set WORKSP after ELINE
		LD	(STKBOT),HL	; set STKBOT
		LD	(STKEND),HL	; set STKEND
		LD	A,defattr
		LD	(ATTR_P),A	; set ATTR P
		LD	(ATTR_T),A	; set ATTR T
		LD	(BORDCR),A	; set BORDCR
		XOR	A
		LD	(ed_P_FLAG),A	; set editor's P_FLAG
		LD	A,defborder
		OUT	($FE),A		; white border
		LD	HL,$0523
		LD	(REPDEL),HL	; set REPDEL and REPPER
		DEC	(IY-$3A)	; set two bytes of KSTATE to $FF
		DEC	(IY-$36)
		LD	HL,l03cd
		LD	DE,STRMS
		LD	BC,$000E
		LDIR			; copy initial stream addresses to STRMS
		res	1,(IY+$01)	; reset bit 1 of FLAGS
		LD	(IY+$00),$FF	; set ERR NR to no error
		LD	(IY+$31),$02	; set DF SZ
		EX	AF,AF'
		CP	$52

		IF garry
		NOP
		NOP
		NOP
		ELSE
		JP	Z,l2675		; move on if in self-test program
		ENDIF

		LD	HL,(RAMTOP)
		INC	HL
		LD	SP,HL		; set SP to RAMTOP+1
		EI			; enable interrupts
		RST	28H
		DW	o0D6B		; CLS using ROM 3
		CALL	l02aa		; display test image if BREAK held down

		IF garry
		LD	DE,l3834
		ELSE
		LD	DE,l03db
		ENDIF

		CALL	l029e		; display copyright message
		LD	HL,TSTACK
		LD	(OLDSP),HL	; set OLDSP to TSTACK area
		CALL	l05cc		; switch in page 7 with stack in TSTACK
		LD	A,defattr
		LD	(ed_ATTR_T),A	; set editor's ATTR_T
		LD	(ed_ATTR_P),A	; set editor's ATTR_P
		CALL	l05a7		; switch back page 0
		CALL	l3e80
		DW	m2410		; initialise DOS & display drive info
		CALL	l05cc		; switch in page 7 with stack in TSTACK
		LD	(IY+$31),$02	; set DFSZ
		SET	5,(IY+$02)	; set bit 5 of TVFLAG
		CALL	l0633		; ???
		CALL	l05a7		; switch back page 0
		JP	l064e		; move on

; Print string subroutine
; Displays a string terminated by a byte with bit 7 set
; Entry: DE=address of string
; Exit: DE=address after string, A corrupted

l029e:		LD	A,(DE)		; get next character
		AND	$7F		; mask high bit
		PUSH	DE
		RST	10H		; print it
		POP	DE
		LD	A,(DE)
		INC	DE		; increment address
		ADD	A,A
		JR	NC,l029e	; loop back if bit 7 wasn't set
		RET

; Check to see if BREAK is held down, entering the test image if so

l02aa:		LD	A,$7F
		IN	A,($FE)
l02ae:		RRA
		RET	C		; exit if SPACE not held down
		LD	A,$FE
		IN	A,($FE)
		RRA
		RET	C		; exit if CAPS SHIFT not held down
		LD	A,$07
		OUT	($FE),A		; white border
		LD	A,$02
		RST	28H
		DW	o1601		; open stream 2 for output
		XOR	A
		LD	(TV_FLAG),A	; clear TV FLAG
		LD	A,$16
		RST	10H
		XOR	A
		RST	10H
		XOR	A
		RST	10H		; AT 0,0
		LD	E,$08		; E=8, used many times in routine
		LD	B,E		; B=8 messages per line
		LD	D,B		; D=8 lines
l02ce:		LD	A,B
		DEC	A
		RL	A
		RL	A
		RL	A		; A=paper colour from position along line
		ADD	A,D
		DEC	A		; add in ink colour from line number
		LD	(ATTR_T),A	; set ATTR T
		LD	HL,l03b0	; address of '1987' test message
		LD	C,E		; C=8=length of message
l02df:		LD	A,(HL)
		RST	10H		; display next character
		INC	HL
		DEC	C
		JR	NZ,l02df	; loop back for more characters
		DJNZ	l02ce		; loop back for more messages
		LD	B,E		; B=8 messages per line
		DEC	D
		JR	NZ,l02ce	; loop back for more lines
		LD	HL,$4800	; start of middle third of screen
		LD	D,H
		LD	E,L
		INC	DE
		XOR	A
		LD	(HL),A
		LD	BC,$0FFF
		LDIR			; clear bottom two thirds of screen
		EX	DE,HL
		LD	DE,$5900
		LD	BC,$0200
		LDIR			; copy attribs of top third to rest of screen
		DI			; disable interrupts
l0302:		LD	DE,$0370
		LD	L,$07
l0307:		LD	BC,$0099
l030a:		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,l030a	; delay
		LD	A,L
		XOR	$10
		LD	L,A
		OUT	($FE),A		; generate tone
		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,l0307	; loop back for tone

; Here we test for sets of keys pressed at the test image, and jump
; to routines to handle them if necessary

		IF garry
		JR	l0302		; sound tone again
		CALL	l089f
		LD	(HL),A
		RET
		CALL	l089f
		LD	A,(HL)
		RET
		INC	B
		DJNZ	l032e
		LD	DE,15
		JR	l0352
l032e:		DJNZ	l034f
		LD	A,D
		OR	E
		JP	NZ,$38D5
		PUSH	HL
		LD	HL,(CURCHL)
		LD	DE,13
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		DEC	DE
		INC	HL
		EX	DE,HL
		POP	BC
		AND	A
		SBC	HL,BC
		EX	DE,HL
		JP	C,$38D5
		LD	(HL),C
		INC	HL
		LD	(HL),B
		RET
l034f:		LD	DE,13
l0352:		LD	HL,(CURCHL)
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		LD	DE,0
		RET
l035e:		DS	9

		ELSE

		LD	DE,$2000	; DE=number of times to check for keysets
		LD	IX,l03a8	; IX=start of keyset table
l0321:		LD	L,(IX+$00)	; HL=next keyset start address-1
		LD	H,(IX+$01)
		INC	IX
		INC	IX		; IX points to next entry in keyset table
		LD	A,H
		OR	L
		JR	NZ,l0335	; test keyset unless at end of table
		LD	IX,l03a8	; if so, start again at the beginning
		JR	l0321

l0335:		INC	HL		; HL points to next keyboard scan address
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=next keyboard scan address
		INC	HL
		LD	A,B
		OR	C
		JR	Z,l034c		; move on if scanned all for this keyset
		IN	A,(C)
		AND	$1F		; mask keyboard (bits 0-4)
		CP	(HL)		; check against required value
		JR	Z,l0335		; continue checking if OK
		DEC	DE		; decrement number of checks counter
		LD	A,D
		OR	E
		JR	NZ,l0321	; loop back to scan again
		JR	l0302		; sound tone again

l034c:		LD	C,(HL)		; get address of routine to execute
		INC	HL
		LD	B,(HL)
		PUSH	BC		; stack address
		RET			; and "return" to it

; Self-test keyset table
; Program accessed with "QAZPLM" held down on test screen

l0351:		DW	$FBFE
		DB	$1E		; "Q"
		DW	$FDFE
		DB	$1E		; "A"
		DW	$FEFE
		DB	$1D		; "Z"
		DW	$DFFE
		DB	$1E		; "P"
		DW	$BFFE
		DB	$1D		; "L"
		DW	$7FFE
		DB	$1B		; "M"
		DW	0		; end of keys to scan
		DW	l21df		; routine address

		ENDIF

; Jump here if there is a memory test error:
; if the bit couldn't be set, a border is set to the bit number,
; if it couldn't be reset, an alternating bit number/bit number XOR 7
; border is set.

l0367:		LD	A,8
		SUB	E
		EX	AF,AF'		; A'=bit number failed on
		AND	A
		JR	NZ,l0373	; jump on if bit could be set
		EX	AF,AF'
		OUT	($FE),A		; else halt with border set to bit number
l0371:		JR	l0371

l0373:		EX	AF,AF'
		LD	C,A
		LD	B,$07
		XOR	B
		LD	B,A		; B=bit number XOR 7

l0379:		LD	A,C
		OUT	($FE),A		; set bit number border
		LD	DE,$0000
l037f:		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,l037f	; pause for approx 0.5s
		LD	A,B
		OUT	($FE),A		; set bit number XOR 7 border
		LD	DE,$2AAA
l038a:		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,l038a	; pause for approx 0.1s
		JR	l0379		; loop back

		IF garry

FREE_ROM0_1:	EQU	$

		;...
		;...

R0_FREE_1:	EQU	31-($-FREE_ROM0_1)
ROM0_SPARE1:	DS	R0_FREE_1

		ELSE
; Pretty EAR monitor keyset table
; Program accessed with "EUA" held down on test screen
l0391:		DW	$FBFE
		DB	$1B		; "E"
		DW	$DFFE
		DB	$17		; "U"
		DW	$FDFE
		DB	$1E		; "A"
		DW	0		; end of keys to scan
		DW	l22d0		; routine address

; Reboot keyset table
; Spectrum rebooted with "BV" held down on test screen

l039e:		DW	$7FFE
		DB	$0F		; "B"
		DW	$FEFE
		DB	$0F		; "V"
		DW	0		; end of keys to scan
		DW	$0000		; routine address

; The table of keyset addresses-1 scanned at startup

l03a8:		DW	l0351-1		; self-test keyset table-1
		DW	l0391-1		; pretty EAR monitor keyset table-1
		DW	l039e-1		; reboot keyset table-1
		DW	0		; end of table marker
					; Text used for the test display
		ENDIF

l03b0:		DB	$13,$0,"19", $13, $1, "87"

; Here is the initial channel information, copied to CHANS

l03b8:		DW	o09F4
		DW	o10A8
		DB	"K"		; keyboard/lower screen channel
		DW	o09F4
		DW	o15C4
		DB	"S"		; main screen channel
		DW	o0F81
		DW	o15C4
		DB	"X"		; workspace channel
		IF garry
		DW	$5B00
		DW	$5B00
		ELSE
		DW	o3A05
		DW	o3A00
		ENDIF
		DB	"P"		; printer channel
		DB	$80		; end of channel information

; Here is the initial stream addresses, copied to STRMS

l03cd:		DW	$0001		; stream -3, 'K'
		DW	$0006		; stream -2, 'S'
		DW	$000B		; stream -1, 'X'
		DW	$0001		; stream 0, 'K'
		DW	$0001		; stream 1, 'K'
		DW	$0006		; stream 2, 'S'
		DW	$0010		; stream 3, 'P'

; Copyright message

l03db:		DB	$7F, "1982, 1986, 1987 Amstrad Plc.", $8D

; Subroutine to ???

l03fa:		LD	HL,$EEF5
		res	0,(HL)		; ???
		SET	1,(HL)		; ???
l0401:		LD	HL,(E_PPC)	; get current line
		LD	A,H
		OR	L
		JR	NZ,l040b	; move on unless 0
l0408:		LD	($EC06),HL	; ???
l040b:		LD	A,(nr_above)	; ???
		PUSH	AF
		LD	HL,($FC9A)	; ???
		CALL	l1418		; get number of line before (or 0)
		LD	($F9D7),HL	; ???
		CALL	l12f0		; ???
		CALL	l11a4		; ???
		POP	AF
l041f:		OR	A
		JR	Z,l042e		; move on if ???
		PUSH	AF
l0423:		CALL	l11ad		; ???
		EX	DE,HL
		CALL	l1338
		POP	AF
		DEC	A
		DB	$18
		DB	-15
l042e:		LD	C,$00
		CALL	l1182
		LD	B,C
		LD	A,(ed_N_ROWS)
		LD	C,A
		PUSH	BC
		PUSH	DE
l043a:		CALL	l11ad
		LD	A,($EEF5)
		BIT	1,A
		DB	$28
		DB	29
		PUSH	DE
		PUSH	HL
l0446:		LD	DE,$0020
		ADD	HL,DE
		BIT	0,(HL)
		DB	$28
		DB	17
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	E,(HL)
		OR	A
		LD	HL,(E_PPC)
		SBC	HL,DE
		DB	$20
		DB	5
		LD	HL,$EEF5
		SET	0,(HL)
l045f:		POP	HL
		POP	DE
l0461:		PUSH	BC
		PUSH	HL
		LD	BC,$0023
		LDIR
		POP	HL
		POP	BC
		PUSH	DE
		PUSH	BC
		EX	DE,HL
		LD	HL,$EEF5
		BIT	0,(HL)
		DB	$28
		DB	42
		LD	B,$00
l0476:		LD	HL,($EC06)
		LD	A,H
		OR	L
		DB	$28
		DB	14
		PUSH	HL
		CALL	l0f0f
		POP	HL
		DB	$30
		DB	18
		DEC	HL
		INC	B
		LD	($EC06),HL
		DB	$18
		EX	DE,HL
		CALL	l0f0f
		CALL	NC,l0f31
		LD	HL,$EEF5
		LD	(HL),$00
l0496:		LD	A,B
		POP	BC
		PUSH	BC
		LD	C,B
		LD	B,A
		CALL	l0adc
l049e:		POP	BC
		POP	DE
		LD	A,C
		INC	B
		CP	B
		DB	$30
		DB	-107
		LD	A,($EEF5)
		BIT	1,A
		DB	$28
		DB	33
		BIT	0,A
		DB	$20
		DB	29
		LD	HL,(E_PPC)
		LD	A,H
		OR	L
		DB	$28
		DB	8
		LD	($FC9A),HL
		CALL	l12f0
		DB	$18
		DB	9
l04bf:		LD	($FC9A),HL
		CALL	l1420
		LD	(E_PPC),HL
l04c8:		POP	DE
		POP	BC
		JP	l0401

l04cd:		POP	DE
		POP	BC
		CP	A

l04d0:		PUSH	AF
		LD	A,C
		LD	C,B
		CALL	l1182
		EX	DE,HL

l04d7:		PUSH	AF
		CALL	l16fe
		POP	AF
		LD	DE,$0023
		ADD	HL,DE
		INC	C
		CP	C
		JR	NC,l04d7	; (-13)
		POP	AF
		RET	Z
		CALL	l0ad2

l04e9:		CALL	l0c43
		LD	HL,($EC06)
		DEC	HL
		LD	A,H
		OR	L
		LD	($EC06),HL
		JR	NZ,l04e9	; (-14)
		JP	l0adc
		RET

l04fb:		LD	B,$00
		LD	A,(ed_N_ROWS)
		LD	D,A
		JP	l1d6b

l0504:		LD	B,$00
		PUSH	HL
		LD	C,B
		CALL	l1182
		CALL	l1338
		POP	HL
		RET	NC
		CALL	l11ad

l0513:		PUSH	BC
		PUSH	HL
		LD	HL,$0023
		ADD	HL,DE
		LD	A,(ed_N_ROWS)
		LD	C,A
		CP	B
		JR	Z,l052e		; (14)
		PUSH	BC

l0521:		PUSH	BC
		LD	BC,$0023
		LDIR
		POP	BC
		LD	A,C
		INC	B
		CP	B
		JR	NZ,l0521	; (-12)
		POP	BC

l052e:		POP	HL
		CALL	l1712
		LD	BC,$0023
		LDIR
		SCF
		POP	BC
		RET

l053a:		LD	B,$00
		CALL	l12f9
		RET	NC

l0540:		PUSH	BC
		PUSH	HL
		LD	A,(ed_N_ROWS)
		LD	C,A
		CALL	l1182
		CALL	l11ec
		JR	NC,l0574	; (38)
		DEC	DE
		LD	HL,$0023
		ADD	HL,DE
		EX	DE,HL
		PUSH	BC
		LD	A,B
		CP	C
		JR	Z,l0565		; (12)

l0559:		PUSH	BC
		LD	BC,$0023
		LDDR
		POP	BC
		LD	A,B
		DEC	C
		CP	C
		JR	C,l0559		; (-12)

l0565:		EX	DE,HL
		INC	DE
		POP	BC
		POP	HL
		CALL	l1726
		LD	BC,$0023
		LDIR
		SCF
		POP	BC
		RET

l0574:		POP	HL
		POP	BC
		RET

l0577:		PUSH	DE
		LD	H,$00
		LD	L,B
		ADD	HL,DE
		LD	D,A
		LD	A,B

l057e:		LD	E,(HL)
		LD	(HL),D
		LD	D,E
		INC	HL
		INC	A
		CP	$20
		JR	C,l057e		; (-9)
		LD	A,E
		CP	$00
		POP	DE
		RET

l058c:		PUSH	DE
		LD	HL,$0020
		ADD	HL,DE
		PUSH	HL
		LD	D,A
		LD	A,$1F
		JR	l059e		; (7)

l0597:		LD	E,(HL)
		LD	(HL),D
		LD	D,E
		CP	B
		JR	Z,l05a1		; (4)
		DEC	A

l059e:		DEC	HL
		JR	l0597		; (-10)

l05a1:		LD	A,E
		CP	$00
		POP	HL
		POP	DE
		RET

; Subroutine to page in normal memory (page 0) and swap SP with OLDSP

l05a7:		EX	AF,AF'		; save AF
		LD	A,$00
		DI
		CALL	l05c1		; page in page 0
		POP	AF		; AF holds return address
		LD	(TARGET),HL	; save HL in TARGET
		LD	HL,(OLDSP)	; get OLDSP
		LD	(OLDSP),SP	; save SP in OLDSP
		LD	SP,HL		; SP now holds what was in OLDSP
		EI
		LD	HL,(TARGET)	; restore HL
		PUSH	AF		; push back return address
		EX	AF,AF'		; restore AF
		RET

; Subroutine to page in page A

l05c1:		PUSH	BC		; save BC
		LD	BC,PBANKM
		OUT	(C),A		; change page
		LD	(BANKM),A	; save copy of OUT
		POP	BC		; restore BC
		RET

; Subroutine to page in DOS workspace (page 7) and swap SP with OLDSP

l05cc:		EX	AF,AF'		; save AF
		DI
		POP	AF		; AF holds return address
		LD	(TARGET),HL	; save HL in TARGET
		LD	HL,(OLDSP)	; get OLDSP
		LD	(OLDSP),SP	; save SP in OLDSP
		LD	SP,HL		; SP now holds what was in OLDSP
		LD	HL,(TARGET)	; restore HL
		PUSH	AF		; push back return address
		LD	A,$07
		CALL	l05c1		; page in page 7
		EI
		EX	AF,AF'		; restore AF
		RET

; The editing keys table
; Most of these keys are produced by the external keypad, which was not
; made available in the UK.

l05e6:		DB	$15
		DB	$0B		; cursor up
		DW	l0b5f
		DB	$0A		; cursor down
		DW	l0b80
		DB	$08		; cursor left
		DW	l0ba2
		DB	$09		; cursor right
		DW	l0bae
		DB	$AD		; TAB (up 10)
		DW	l0b1a
		DB	$AC		; AT (down 10)
		DW	l0af0
		DB	$AF		; CODE (left word)
		DW	l0a9f
		DB	$AE		; VAL$ (right word)
		DW	l0aac
		DB	$A6		; INKEY$ (top)
		DW	l0a4e
		DB	$A5		; RND (bottom)
		DW	l0a76
		DB	$A8		; FN (start of line)
		DW	l0b52
		DB	$A7		; PI (end of line)
		DW	l0b45
		DB	$AA		; SCREEN$ (delete char right)
		DW	l09e6
		DB	$0C		; delete
		DW	l09f6
		DB	$B3		; COS (delete word right)
		DW	l10e5
		DB	$B4		; TAN (delete word left)
		DW	l108a
		DB	$B0		; VAL (delete line right)
		DW	l1140
		DB	$B1		; LEN (delete line left)
		DW	l110c
		DB	$0D		; enter
		DW	l0a0f
		DB	$A9		; POINT (screen)
		DW	l0748
		DB	$07		; edit
		DW	l07b1

; The menu keys table

l0626:		DB	$04
		DB	$0B		; cursor up
		DW	l07dc
		DB	$0A		; cursor down
		DW	l07df
		DB	$07		; edit
		DW	l07c5
		DB	$0D		; enter
		DW	l07c5

; Subroutine to ????

l0633:		CALL	l0989
		LD	HL,$0000
		LD	($FC9A),HL
		LD	A,$82
		LD	(ed_flags),A
		LD	HL,$0000
		LD	(E_PPC),HL
		CALL	l16b6
		CALL	l1758
		RET

; Routine to display main menu & go to process it

l064e:		LD	HL,TSTACK
		LD	(OLDSP),HL	; set "OLDSP" to temporary stack area
		CALL	l05cc		; page in DOS workspace
		LD	A,$02
		RST	28H
		DW	o1601		; open channel to stream 2
l065c:		LD	HL,l07f2
		LD	(men_rout),HL	; store main menu routine table address
		LD	HL,l07ff
		LD	(men_text),HL	; store main menu text address
		PUSH	HL		; save menu address
		LD	HL,ed_flags
		SET	1,(HL)		; signal "processing menu"
		res	4,(HL)		; ???
		DEC	HL
		LD	(HL),$00	; set men_high=0
		POP	HL		; restore menu address
		XOR	A
		CALL	l189a		; display main menu
		JP	l0703		; move to process menu


l067b:		LD	IX,$FD98
		LD	HL,TSTACK
		LD	(OLDSP),HL
		CALL	l05cc
		LD	A,$02
		RST	28H
		DW	o1601
		CALL	l185a
		LD	HL,FLAGS

l0693:		BIT	5,(HL)
		JR	Z,l0693		; (-4)
		LD	HL,ed_flags
		res	3,(HL)
		BIT	6,(HL)
		JR	NZ,l06b4	; (20)
		LD	A,(process)
		CP	$04
		JR	Z,l06b1		; (10)
		CP	$00
		JP	NZ,l0992
		CALL	l1a5a
		JR	l06b4		; (3)

l06b1:		CALL	l1a5f		; display "Calculator" bar
l06b4:		CALL	l11a4
		CALL	l12f0
		LD	A,(process)	; get current process
		CP	$04
		JR	Z,l0703		; move on if its the calculator
		LD	HL,(E_PPC)	; get number of current line
		LD	A,H
		OR	L
		JR	NZ,l06dd	; move on if not zero
		LD	HL,(PROG)
		LD	BC,(VARS)
		AND	A
		SBC	HL,BC		; get length of BASIC program
		JR	NZ,l06da	; move on if not zero
		LD	HL,$0000
		LD	($EC08),HL	; ??? last line
l06da:		LD	HL,($EC08)	; ??? last line
l06dd:		CALL	l05a7		; page in normal memory
		RST	28H
		DW	o196E		; ???
		RST	28H
		DW	o1695
		CALL	l05cc		; page in DOS workspace
		LD	(E_PPC),DE
		LD	HL,ed_flags
		BIT	5,(HL)
		JR	NZ,l0703	; (15)
		LD	HL,$0000
		LD	($EC06),HL
		CALL	l03fa
		CALL	l0abd
		CALL	l0a0f

; Main routine to process menus & editing functions

l0703:		LD	SP,TSTACK	; set SP in temporary stack
l0706:		CALL	l1871		; get a key
		PUSH	AF
		LD	A,(PIP)
		CALL	l0799		; sound a 'PIP'
		POP	AF
		CALL	l0716		; "do" the key
		JR	l0706		; loop back
l0716:		LD	HL,ed_flags
		BIT	1,(HL)		; check editing/menu flag
		PUSH	AF
		LD	HL,l0626	; use menu keys table
		JR	NZ,l0724
		LD	HL,l05e6	; or editing keys table
l0724:		CALL	l2166		; perform pressed key action
		JR	NZ,l072e
		CALL	NC,l0794	; sound a RASP if action failed
		POP	AF		; restore editing/menu flag status
		RET
l072e:		POP	AF		; restore editing/menu flag status
		JR	Z,l0736		; move on if editing
		XOR	A
		LD	(MODE),A	; else in menu, so set MODE=0
		RET
l0736:		LD	HL,ed_flags
		BIT	0,(HL)
		JR	Z,l0741		; move on if ???
		CALL	l0794		; sound a RASP
		RET
l0741:		CP	$A3
		IF garry
		RET	NC
		NOP
		ELSE
		JR	NC,l0706	; loop back if ???
		ENDIF
		JP	l09bc

; Editing keys: SCREEN

l0748:		LD	A,(process)
		CP	$04
		RET	Z		; exit if in Calculator
		CALL	l04fb
		LD	HL,ed_flags
		res	3,(HL)
		LD	A,(HL)
		XOR	$40
		LD	(HL),A
		AND	$40
		JR	Z,l0763		; (5)
		CALL	l0768
		JR	l0766		; (3)
l0763:		CALL	l077b
l0766:		SCF
		RET

l0768:		CALL	l1a8e
		LD	HL,ed_flags
		SET	6,(HL)
		CALL	l0efb
		CALL	l1c95
		CALL	l09aa
		JR	l0786		; (11)

l077b:		LD	HL,ed_flags
		res	6,(HL)
		CALL	l0989
		CALL	l1a5a
l0786:		LD	HL,($FC9A)
		LD	A,H
		OR	L
		CALL	NZ,l1418
		CALL	l03fa
		JP	l0abd

; Subroutine to sound a PIP or RASP
; Enter at l0799 with A=PIP, or at l0794 for RASP

l0794:		LD	A,(RASP)
		SRL	A		; A=RASP/2
l0799:		PUSH	IX		; save IX
		LD	D,$00
		LD	E,A		; DE=f*t

		IF alternative || garry = 0
		LD	HL,$0C80	; HL=timing constant
					; (same original)
		ELSE
		LD	HL,$00C8
		ENDIF

l07a1:		RST	28H
		DW	o03B5		; call BEEPER
		POP	IX		; restore IX
		RET

; Another sound

l07a7:		PUSH	IX
		LD	DE,$0030
		LD	HL,$0300
		JR	l07a1

; Editing keys: EDIT

l07b1:		CALL	l0ab7		; remove cursor
		LD	HL,ed_flags
		SET	1,(HL)		; set "processing menu"
		DEC	HL
		LD	(HL),$00	; highlight on line 0
		LD	HL,(men_text)
		XOR	A
		CALL	l189a		; display menu
		SCF
		RET

; The menu ENTER/EDIT routine

l07c5:		LD	HL,ed_flags
		res	1,(HL)		; signal editing mode
		DEC	HL
		LD	A,(HL)		; A=currently highlighted line
		LD	HL,(men_rout)	; HL=menu routines table
		PUSH	HL
		PUSH	AF
		CALL	l1950		; copy saved area back to screen
		POP	AF
		POP	HL
		CALL	l2166		; execute required routine
		JP	l0abd		; move on

; The menu cursor up/down routines
; Enter at l07dc for up, l07df for down

l07dc:		SCF
		JR	l07e0
l07df:		AND	A		; clear carry for cursor down
l07e0:		LD	HL,men_high
		LD	A,(HL)		; get currently highlighted line number
		PUSH	HL
		LD	HL,(men_text)	; point to menu
		CALL	C,l19b9		; move highlight up
		CALL	NC,l19c8	; or down
		POP	HL
		LD	(HL),A		; replace highlighted line number
l07f0:		SCF
		RET

; The main menu routine address table

l07f2:		DB	$04
		DB	$00
		DW	l08e8		; Loader
		DB	$01
		DW	l0937		; +3 BASIC
		DB	$02
		DW	l0950		; Calculator
		DB	$03
		DW	l08df		; 48K BASIC

; The main menu

l07ff:		DB	$05		; 5 lines total

		IF alternative
		DB	"128 +3  ", $FF
		ELSE
		IF garry
		DB	"128 +3e ", $FF
		ELSE
		DB	"128 +3  ", $FF
		ENDIF
		ENDIF

		IF spanish

l0809:		DB	"Cargado","r"           + $80
l080f:		DB	"+3 BASI","C"           + $80
l0817:		DB	"Calculador","a"        + $80
		DB	"48 BASI","C"           + $80
		DB	" "                     + $80

; The editor menu routine address table

l082a:		DB	$05
		DB	$00
		DW	l07f0		; +3 BASIC
		DB	$01
		DW	l0917		; Renumber
		DB	$02
		DW	l08c5		; Screen
		DB	$03
		DW	l0928		; Print
		DB	$04
		DW	l08ca		; Exit

; The editor menu

l083a:		DB	$06
		DB	"Opciones",$FF

		DB	"+3 BASI","C"           + $80
		DB	"Renumera","r"          + $80
		DB	"Pantall","a"           + $80
		DB	"Imprimi","r"           + $80
		DB	"Salid","a"             + $80
		DB	" "                     + $80

; The calculator menu routines table

l0864:		DB	$02
		DB	$00
		DW	l07f0		; Calculator
		DB	$01
		DW	l08ca		; Exit

; The calculator menu

l086b:		DB	$03
		DB	"Opciones",$FF

		DB	"Calculador","a"        + $80
		DB	"Salid","a"             + $80
		DB	" "                     + $80

; Cassette loader message

		ELSE

l0809:		DB	"Loade","r"             + $80
l080f:		DB	"+3 BASI","C"           + $80
l0817:		DB	"Calculato","r"         + $80
		DB	"48 BASI","C"           + $80
		DB	" "                     + $80

; The editor menu routine address table

l082a:		DB	$05
		DB	$00
		DW	l07f0		; +3 BASIC
		DB	$01
		DW	l0917		; Renumber
		DB	$02
		DW	l08c5		; Screen
		DB	$03
		DW	l0928		; Print
		DB	$04
		DW	l08ca		; Exit

; The editor menu

l083a:		DB	$06
		DB	"Options ",$FF

		DB	"+3 BASI","C"           + $80
		DB	"Renumbe","r"           + $80
		DB	"Scree","n"             + $80
		DB	"Prin","t"              + $80
		DB	"Exi","t"               + $80
		DB	" "                     + $80

; The calculator menu routines table

l0864:		DB	$02
		DB	$00
		DW	l07f0		; Calculator
		DB	$01
		DW	l08ca		; Exit

; The calculator menu

l086b:		DB	$03
		DB	"Options ",$FF

		DB	"Calculato","r"         + $80
		DB	"Exi","t"               + $80
		DB	" "                     + $80

; Cassette loader message

		ENDIF

		IF garry

FREE_ROM0_2:	EQU	$

l0884:		;...
		;...

R0_FREE_2:	EQU	27-($-FREE_ROM0_2)
ROM0_SPARE2:	DS	R0_FREE_2

l089f:		LD	HL,(CURCHL)
		LD	DE,13
		ADD	HL,DE
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		PUSH	HL
		AND	A
		SBC	HL,BC
		POP	HL
		EX	DE,HL
		JP	NC,$38D5
		INC	DE
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		INC	HL
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		EX	DE,HL
		ADD	HL,BC
		DEC	HL
		SCF
		RET

		ELSE

l0884:		DB	$16,$00,$00
		DB	$10,$00,$11,$07
		DB	$13,$00

		IF spanish

		DB	"Introduzca la cinta y pulse PLAY", $0D
		DB	"Cancelar: pulse BREAK dos veces", '.'+$80

		ELSE

		DB	"Insert tape and press PLAY", $0D
		DB	"To cancel - press BREAK twic", "e"+$80

		ENDIF

		ENDIF

; The Screen menu option

l08c5:		CALL	l0748		; call SCREEN editing key routine
		JR	l093f		; ???

; The "Exit" from submenu option

l08ca:		LD	HL,ed_flags
		res	6,(HL)		; ???
		CALL	l0989		; ???
		LD	B,$00
		LD	D,$17
		CALL	l1d6b		; clear whole screen to editor colours
		CALL	l05a7		; page in normal memory
		JP	l064e		; display main menu & process it

; The 48K BASIC menu option

l08df:		CALL	l05a7		; page in normal memory
		CALL	l3e80
		DW	m1488		; enter 48K BASIC via ROM 1
		RET

; The Loader menu option

l08e8:		CALL	l1a64		; display "Loader" bar
		LD	HL,TV_FLAG
		SET	0,(HL)		; signal "using lower screen"

		IF garry
		LD	DE,$37EB
		NOP
		NOP
		LD	A,(LODDRV)
		CP	$54
		ELSE
		LD	DE,l0884
		PUSH	HL
l08f4:		LD	HL,FLAGS3
		BIT	4,(HL)
		POP	HL
		ENDIF

		JR	NZ,l08ff	; move on if disk interface present
		CALL	l029e		; display cassette loader message

l08ff:		res	0,(HL)		; ???
		SET	6,(HL)		; ???
		LD	A,$07
		LD	(process),A	; signal "current process is Loader"
		LD	BC,$0000
		CALL	l191d		; output "AT 0,0"
l090e:		CALL	l05a7		; page in normal memory
		CALL	l3e80
		DW	m12e8
		RET

l0917:		CALL	l1a95
		CALL	NC,l0794
		LD	HL,$0000
		LD	(E_PPC),HL
		LD	($EC08),HL
		JR	l0930		; (8)

; The Print menu option

l0928:		CALL	l05a7		; page in normal memory
		CALL	l3e80
		DW	m1451
l0930:		LD	HL,ed_flags
		BIT	6,(HL)		; ???
l0935:		JR	NZ,l093f

; The +3 BASIC routine - called from the main menu

l0937:		LD	HL,TV_FLAG
		res	0,(HL)		; signal "main screen"
		CALL	l1a5a		; display "+3 BASIC" bar
l093f:		LD	HL,ed_flags
		res	5,(HL)		; ???
		res	4,(HL)		; ???
		LD	A,$00		; ???
		LD	HL,l082a	; +3 BASIC menu addresses
		LD	DE,l083a	; +3 BASIC menu
		JR	l097c		; go to set menu

; The Calculator routine - called from the main menu

l0950:		LD	HL,ed_flags
		SET	5,(HL)		; ???
		SET	4,(HL)		; ???
		res	6,(HL)		; ???
		CALL	l0989
		CALL	l1a5f		; display "Calculator" bar
		LD	A,$04
		LD	(process),A
		LD	HL,$0000
		LD	(E_PPC),HL
		CALL	l03fa
		LD	BC,$0000
		LD	A,B
		CALL	l0ac3
		LD	A,$04
		LD	HL,l0864
		LD	DE,l086b

; Routine to set new menu and ???

l097c:		LD	(process),A	; ???
		LD	(men_rout),HL	; store routine address table
		LD	(men_text),DE	; store menu address
		JP	l06b4		; ???

; Subroutine to ???

l0989:		CALL	l0eed
		CALL	l1c8c
		JP	l09b3

l0992:		LD	B,$00
		LD	D,$17
		CALL	l1d6b		; clear screen to editor colours
		JP	l065c

l099c:		DB	$06
		DB	0
		DB	0
		DB	0
		DB	$04
		DB	$10
		DB	$14

l09a3:		DB	$06
		DB	0
		DB	0
		DB	0
		DB	0
		DB	$01
		DB	$01

l09aa:		LD	HL,l09a3
		LD	DE,$F6EE
		JP	l2152

l09b3:		LD	HL,l099c
		LD	DE,$F6EE
		JP	l2152

; Subroutine to ???

l09bc:		LD	HL,ed_flags
		OR	A
		OR	A
		BIT	0,(HL)
		JP	NZ,l0abd
		res	7,(HL)
		SET	3,(HL)
		PUSH	HL
		PUSH	AF
		CALL	l0ab7
		POP	AF
		PUSH	AF
		CALL	l0f4f
		POP	AF
		LD	A,B
		CALL	l0c43
		POP	HL
		SET	7,(HL)
		JP	NC,l0abd
		LD	A,B
		JP	C,l0ac3
		JP	l0abd


; Editing keys: DELETE RIGHT

l09e6:		LD	HL,ed_flags
		SET	3,(HL)
		CALL	l0ab7
		CALL	l0fe0
		SCF
		LD	A,B
		JP	l0ac3

; Editing keys: DELETE

l09f6:		LD	HL,ed_flags
		res	0,(HL)
		SET	3,(HL)
		CALL	l0ab7
		CALL	l0c26
		CCF
		JP	C,l0abd
l0a07:		CALL	l0fe0
		SCF
		LD	A,B
		JP	l0ac3

; Editing keys: ENTER

l0a0f:		CALL	l0ab7
		PUSH	AF
		CALL	l1182
		PUSH	BC
		LD	B,$00
		CALL	l0f0f
		POP	BC
		JR	C,l0a29		; (10)
		LD	HL,$0020
		ADD	HL,DE
		LD	A,(HL)
		CPL
		AND	$09
		JR	Z,l0a45		; (28)
l0a29:		LD	A,(ed_flags)
		BIT	3,A
		JR	Z,l0a35		; (5)
		CALL	l0d59
		JR	NC,l0a4a	; (21)
l0a35:		CALL	l0d17
		CALL	l0c43
		CALL	l0f9c
		LD	B,$00
		POP	AF
		SCF
		JP	l0ac3
l0a45:		POP	AF
		SCF
		JP	l0abd
l0a4a:		POP	AF
		JP	l0abd

; Editing keys: TOP

l0a4e:		LD	A,(process)
		CP	$04
		RET	Z		; exit if in Calculator
		CALL	l0ab7		; remove cursor
		LD	HL,$0000	; line 0
		CALL	l05a7		; page in normal memory
		RST	28H
		DW	o196E		; get address of first line in HL
		RST	28H
		DW	o1695		; get line number in DE
		CALL	l05cc		; page in DOS workspace
		LD	(E_PPC),DE	; set as current line
		LD	A,$0F
		CALL	l1ca3		; set colours to blue ink, white paper
		CALL	l03fa		; ???
		SCF			; success
		JP	l0abd		; place cursor & exit

; Editing keys: BOTTOM

l0a76:		LD	A,(process)
		CP	$04
		RET	Z		; exit if in Calculator
		CALL	l0ab7		; remove cursor
		LD	HL,9999		; last possible line
		CALL	l05a7		; page in normal memory
		RST	28H
		DW	o196E		; get last line address in DE
		EX	DE,HL
		RST	28H
		DW	o1695		; get last line number in DE
		CALL	l05cc		; page in DOS workspace
		LD	(E_PPC),DE	; set as current line
		LD	A,$0F
		CALL	l1ca3		; set colours to blue ink, white paper
		CALL	l03fa		; ???
		SCF			; success
		JP	l0abd		; place cursor & exit

; Editing keys: LEFT WORD

l0a9f:		CALL	l0ab7
		CALL	l0cb5
		JP	NC,l0abd
		LD	A,B
		JP	l0ac3

; Editing keys: RIGHT WORD

l0aac:		CALL	l0ab7
		CALL	l0cd4
		JR	NC,l0abd	; (9)
		LD	A,B
		JR	l0ac3		; (12)

; Subroutine to remove cursor

l0ab7:		CALL	l0ad2		; get cursor position
		JP	l1749		; remove it

; Subroutine to place cursor

l0abd:		CALL	l0ad2		; get cursor position
		JP	l173a		; place it

; Subroutine to set cursor to line C, column B, ??? A
; and set colours & place it

l0ac3:		CALL	l0adc		; set cursor details
		PUSH	AF
		PUSH	BC
		LD	A,$0F
		CALL	l1ca3		; set colours to blue INK, white PAPER
		POP	BC
		POP	AF
		JP	l173a		; place cursor

; Subroutine to get cursor line (C), column (B), and ??? (A)

l0ad2:		LD	HL,$F6EE
		LD	C,(HL)		; get line (within editing area)
		INC	HL
		LD	B,(HL)		; get column
		INC	HL
		LD	A,(HL)		; get ???
		INC	HL
		RET

; Subroutine to set cursor line (C), column (B), and ??? (A)

l0adc:		LD	HL,$F6EE
		LD	(HL),C		; set line
		INC	HL
		LD	(HL),B		; set column
		INC	HL
		LD	(HL),A		; set ???
		RET



l0ae5:		PUSH	HL
		CALL	l1182
		LD	H,$00
		LD	L,B
		ADD	HL,DE
		LD	A,(HL)
		POP	HL
		RET

; Editing keys: DOWN 10 LINES

l0af0:		CALL	l0ab7
		LD	E,A
		LD	D,$0A
l0af6:		PUSH	DE
		CALL	l0bfb
		POP	DE
		JR	NC,l0abd	; (-64)
		LD	A,E
		CALL	l0adc
		LD	B,E
		CALL	l0bc4
		JR	NC,l0b0d	; (6)
		DEC	D
		JR	NZ,l0af6	; (-20)
		LD	A,E
		JR	C,l0ac3		; (-74)
l0b0d:		PUSH	DE
		CALL	l0bd6
		POP	DE
		LD	B,E
		CALL	l0bc4
		LD	A,E
		OR	A
		JR	l0ac3		; (-87)

; Editing keys: UP 10 LINES

l0b1a:		CALL	l0ab7
		LD	E,A
		LD	D,$0A
l0b20:		PUSH	DE
		CALL	l0bd6
		POP	DE
		JR	NC,l0abd	; (-106)
		LD	A,E
		CALL	l0adc
		LD	B,E
		CALL	l0bcd
		JR	NC,l0b38	; (7)
		DEC	D
		JR	NZ,l0b20	; (-20)
		LD	A,E
		JP	C,l0ac3
l0b38:		PUSH	AF
		CALL	l0bfb
		LD	B,$00
		CALL	l0c9f
		POP	AF
		JP	l0ac3

; Editing keys: END OF LINE

l0b45:		CALL	l0ab7
		CALL	l0d17
		JP	NC,l0abd
		LD	A,B
		JP	l0ac3

; Editing keys: START OF LINE

l0b52:		CALL	l0ab7
		CALL	l0cfc
		JP	NC,l0abd
		LD	A,B
		JP	l0ac3

; Editing keys: CURSOR UP

l0b5f:		CALL	l0ab7		; remove cursor
		LD	E,A
		PUSH	DE
		CALL	l0bd6
		POP	DE
		JP	NC,l0abd
		LD	B,E
		CALL	l0bcd
		LD	A,E
		JP	C,l0ac3
		PUSH	AF
		CALL	l0bfb
		LD	B,$00
		CALL	l0bc4
		POP	AF
		JP	l0ac3

; Editing keys: CURSOR DOWN

l0b80:		CALL	l0ab7		; remove cursor
		LD	E,A
		PUSH	DE
		CALL	l0bfb
		POP	DE
		JP	NC,l0abd
		LD	B,E
		CALL	l0bcd
		LD	A,E
		JP	C,l0ac3
		PUSH	DE
		CALL	l0bd6
		POP	DE
		LD	B,E
		CALL	l0bc4
		LD	A,E
		OR	A
		JP	l0ac3

; Editing keys: CURSOR LEFT

l0ba2:		CALL	l0ab7		; remove cursor
		CALL	l0c26
		JP	C,l0ac3
		JP	l0abd

; Editing keys: CURSOR RIGHT

l0bae:		CALL	l0ab7		; remove cursor
		CALL	l0c43
		JP	C,l0ac3
		PUSH	AF
		CALL	l0bd6
		LD	B,$1F
		CALL	l0caa
		POP	AF
		JP	l0ac3



l0bc4:		PUSH	DE
		CALL	l0c9f
		CALL	NC,l0caa
		POP	DE
		RET

l0bcd:		PUSH	DE
		CALL	l0caa
		CALL	NC,l0c9f
		POP	DE
		RET

l0bd6:		CALL	l0d47
		JR	NC,l0bfa	; (31)
		PUSH	BC
		CALL	l1182
		LD	B,$00
		CALL	l0f0f
		CALL	NC,l104e
		POP	BC
		LD	HL,$F6F1
		LD	A,(HL)
		CP	C
		JR	C,l0bf8		; (9)
		PUSH	BC
		CALL	l053a
		POP	BC
		RET	C
		LD	A,C
		OR	A
		RET	Z

l0bf8:		DEC	C
		SCF

l0bfa:		RET

l0bfb:		PUSH	BC
		CALL	l1182
		LD	B,$00
		CALL	l0f0f
		POP	BC
		JR	C,l0c0a		; (3)
		JP	l104e

l0c0a:		CALL	l0d33
		JR	NC,l0c25	; (22)
		LD	HL,$F6F1
		INC	HL
		LD	A,C
		CP	(HL)
		JR	C,l0c23		; (12)
		PUSH	BC
		PUSH	HL
		CALL	l0504
		POP	HL
		POP	BC
		RET	C
		INC	HL
		LD	A,(HL)
		CP	C
		RET	Z

l0c23:		INC	C
		SCF

l0c25:		RET

l0c26:		LD	D,A
		DEC	B
		JP	m,l0c31
		LD	E,B
		CALL	l0caa
		LD	A,E
		RET	C

l0c31:		PUSH	DE
		CALL	l0bd6
		POP	DE
		LD	A,E
		RET	NC
		LD	B,$1F
		CALL	l0caa
		LD	A,B
		RET	C
		LD	A,D
		LD	B,$00
		RET

l0c43:		LD	D,A
		INC	B
		LD	A,$1F
		CP	B
		JR	C,l0c50		; (6)
		LD	E,B
		CALL	l0c9f
		LD	A,E
		RET	C

l0c50:		DEC	B
		PUSH	BC
		PUSH	HL
		LD	HL,ed_flags
		BIT	7,(HL)
		JR	NZ,l0c8b	; (49)
		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		LD	A,(HL)
		BIT	1,A
		JR	NZ,l0c8b	; (37)
		SET	1,(HL)
		res	3,(HL)
		LD	HL,$0023
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		POP	BC
		PUSH	AF
		CALL	l0bfb
		POP	AF
		CALL	l1182
		LD	HL,$0023
		ADD	HL,DE
		EX	DE,HL
		res	0,A
		SET	3,A
		CALL	l0fa1
		CALL	l16ee
		LD	A,B
		SCF
		RET

l0c8b:		POP	HL
		POP	BC
		PUSH	DE
		CALL	l0bfb
		POP	DE
		LD	A,B
		RET	NC
		LD	B,$00
		CALL	l0c9f
		LD	A,B
		RET	C
		LD	A,E
		LD	B,$00
		RET

l0c9f:		PUSH	DE
		PUSH	HL
		CALL	l1182
		CALL	l0f0f
		JP	l0d30

l0caa:		PUSH	DE
		PUSH	HL
		CALL	l1182
		CALL	l0f31
		JP	l0d30

l0cb5:		PUSH	DE
		PUSH	HL
l0cb7:		CALL	l0c26
		JR	NC,l0cd2	; (22)
		CALL	l0ae5
		CP	$20
		JR	Z,l0cb7		; (-12)
l0cc3:		CALL	l0c26
		JR	NC,l0cd2	; (10)
		CALL	l0ae5
		CP	$20
		JR	NZ,l0cc3	; (-12)
		CALL	l0c43

l0cd2:		JR	l0d30		; (92)

l0cd4:		PUSH	DE
		PUSH	HL

l0cd6:		CALL	l0c43
		JR	NC,l0cf6	; (27)
		CALL	l0ae5
		CP	$20
		JR	NZ,l0cd6	; (-12)

l0ce2:		CALL	l0c43
		JR	NC,l0cf6	; (15)
		CALL	l0f0f
		JR	NC,l0cf6	; (10)
		CALL	l0ae5
		CP	$20
		JR	Z,l0ce2		; (-17)
		SCF
		JR	l0d30		; (58)

l0cf6:		CALL	NC,l0c26
		OR	A
		JR	l0d30		; (52)

l0cfc:		PUSH	DE
		PUSH	HL

l0cfe:		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	0,(HL)
		JR	NZ,l0d10	; (7)
		CALL	l0bd6
		JR	C,l0cfe		; (-16)
		JR	l0d30		; (32)

l0d10:		LD	B,$00
		CALL	l0c9f
		JR	l0d30		; (25)

l0d17:		PUSH	DE
		PUSH	HL

l0d19:		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	3,(HL)
		JR	NZ,l0d2b	; (7)
		CALL	l0bfb
		JR	C,l0d19		; (-16)
		JR	l0d30		; (5)

l0d2b:		LD	B,$1F
		CALL	l0caa

l0d30:		POP	HL
		POP	DE
		RET

l0d33:		LD	A,(ed_flags)
		BIT	3,A
		SCF
		RET	Z
		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	3,(HL)
		SCF
		RET	Z
		JR	l0d59		; (18)

l0d47:		LD	A,(ed_flags)
		BIT	3,A
		SCF
		RET	Z
		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	0,(HL)
		SCF
		RET	Z

l0d59:		LD	A,$02

l0d5b:		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	0,(HL)
		JR	NZ,l0d6e	; (8)
		DEC	C
		JP	p,l0d5b
		LD	C,$00
		LD	A,$01

l0d6e:		LD	HL,$EC00
		LD	DE,$EC03
		OR	$80
		LD	(HL),A
		LD	(DE),A
		INC	HL
		INC	DE
		LD	A,$00
		LD	(HL),A
		LD	(DE),A
		INC	HL
		INC	DE
		LD	A,C
		LD	(HL),A
		LD	(DE),A
		LD	HL,$0000
		LD	($EC06),HL
		CALL	l142d
		CALL	l1dfa
		PUSH	IX
		CALL	l05a7
		CALL	l3e80
		DW	m24f0
		CALL	l05cc
		EI
		POP	IX
		LD	A,(ERR_NR)
		INC	A
		JR	NZ,l0dbd	; (24)
		LD	HL,ed_flags
		res	3,(HL)
		CALL	l1758
		LD	A,(process)
		CP	$04
		CALL	NZ,l03fa
		CALL	l07a7
		CALL	l0ad2
		SCF
		RET

l0dbd:		LD	HL,$EC00
		LD	DE,$EC03
		LD	A,(DE)
		res	7,A
		LD	(HL),A
		INC	HL
		INC	DE
		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		LD	A,(DE)
		LD	(HL),A
		CALL	l1df6
		JR	C,l0dd8		; (4)
		LD	BC,($EC06)

l0dd8:		LD	HL,($EC06)
		OR	A
		SBC	HL,BC
		PUSH	AF
		PUSH	HL
		CALL	l0ad2
		POP	HL
		POP	AF
		JR	C,l0df8		; (17)
		JR	Z,l0e13		; (42)

l0de9:		PUSH	HL
		LD	A,B
		CALL	l0c26
		POP	HL
		JR	NC,l0e13	; (34)
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,l0de9	; (-13)
		JR	l0e13		; (27)

l0df8:		PUSH	HL
		LD	HL,ed_flags
		res	7,(HL)
		POP	HL
		EX	DE,HL
		LD	HL,$0000
		OR	A
		SBC	HL,DE

l0e06:		PUSH	HL
		LD	A,B
		CALL	l0c43
		POP	HL
		JR	NC,l0e13	; (5)
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,l0e06	; (-13)

l0e13:		LD	HL,ed_flags
		SET	7,(HL)
		CALL	l0adc
		LD	A,$17
		CALL	l1ca3
		OR	A
		RET

l0e22:		LD	HL,$EC00
		BIT	7,(HL)
		JR	Z,l0e30		; (7)
		LD	HL,($EC06)
		INC	HL
		LD	($EC06),HL

l0e30:		LD	HL,$EC00
		LD	A,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	C,(HL)
		PUSH	HL
		AND	$0F
		LD	HL,l0e53
		CALL	l2166
		LD	E,L
		POP	HL
		JR	Z,l0e47		; (2)
		LD	A,$0D

l0e47:		LD	(HL),C
		DEC	HL
		LD	(HL),B
		DEC	HL
		PUSH	AF
		LD	A,(HL)
		AND	$F0
		OR	E
		LD	(HL),A
		POP	AF
		RET
l0e53:		DB	$03,$02
		DW	l0e7a
		DB	$04
		DW	l0eb7
		DB	$01
		DW	l0e5d
l0e5d:		CALL	l1385
l0e60:		CALL	l0edc
		JR	NC,l0e6c	; (7)
		CP	$00
		JR	Z,l0e60		; (-9)
		LD	L,$01
		RET

l0e6c:		INC	C
		LD	B,$00

		IF garry
		LD	HL,nr_above
		ELSE
		LD	HL,(nr_above)
		ENDIF

		LD	A,C
		CP	(HL)
		DB	$38
		DB	-25
		LD	B,$00
		LD	C,$00

l0e7a:		PUSH	HL
		LD	HL,$F6EE
		LD	A,(HL)
		CP	C
		JR	NZ,l0e8c	; (10)
		INC	HL
		LD	A,(HL)
		CP	B
		JR	NZ,l0e8c	; (5)
		LD	HL,$EC00
		res	7,(HL)

l0e8c:		POP	HL

l0e8d:		CALL	l1182
		CALL	l0edc
		JR	NC,l0e9c	; (7)
		CP	$00
		JR	Z,l0e7a		; (-31)
		LD	L,$02
		RET

;End of row reached - no editable characters in the Screen Line Edit Buffer row

l0e9c:		LD	HL,$0020	;
		ADD	HL,DE		; Point to the flag byte for the row.
		BIT	3,(HL)		; Is it the last row of the BASIC line?
		JR	Z,l0ea9		; Jump if not.

;On last row of the BASIC line and finished fetching characters from the line

		LD	L,$08		; L=Signal at the end of the last row of the BASIC line.
		LD	A,$0D		; A='Enter' character.
		RET			; Return with zero flag reset to indicate match found.

;Not on the last row of the BASIC line so move to the beginning of the next, if it is on screen.

l0ea9:		LD	HL,ed_area	; Point to the 'top row scroll threshold' value.
		INC	C		; Next row of the BASIC line in the Screen Line Edit Buffer.
		LD	A,(HL)		; Fetch the number of the last row in the Screen Line Edit Buffer.
		CP	C		; Exceeded the upper scroll threshold?
		LD	B,$00		; Column 0.
		JR	NC,l0e8d	; Jump back if not to retrieve the character from the next row.

;The upper row threshold for triggering scrolling the screen has been reached so proceed to scroll up one line

		LD	B,$00		; Column 0. [Redundant byte]
		LD	C,$01		; Row 1. (Row 0 holds a copy of the last row visible on screen)

; Table entry point - Using lower screen and only top row of a multi-row BASIC line is visible
; --------------------------------------------------------------------------------------------

l0eb7:		CALL	l1291		; Find the address of the row specified by C in Below-Screen Line Edit Buffer, into DE.

l0eba:		CALL	l0edc		; Fetch character from Below-Screen Line Edit Buffer row, incrementing the column number.
		JR	NC,l0ec6	; Jump if end of row reached.

		CP	$00		; Is the character a null, i.e. not editable?
		JR	Z,l0eba		; Jump back if null to keep fetching characters until a character is found or the end of the row is reached.

		LD	L,$04		; L=Signal a character was returned from the Below-Screen Line Edit Buffer row, with A holding the character.
		RET			; Return with zero flag reset to indicate match found.

;End of row reached - no editable characters in the (below screen) Below-Screen Line Edit Buffer row

l0ec6:		LD	HL,$0020	;
		ADD	HL,DE		; Point to the flag byte for the row.
		BIT	3,(HL)		; Is it the last row of the BASIC line?
		JR	NZ,l0ed7	; Jump if so.

		INC	C		; Next row.
		LD	B,$00		; Column 0.

		LD	A,(row_below)	; Fetch number of rows in the Below-Screen Line Edit Buffer.
		CP	C		; Exceeded last line in Below-Screen Line Edit Buffer?
		JR	NC,l0eb7	; Jump back if not to retrieve the character from the next row.

;All characters from rows off bottom of screen fetched so return an 'Enter'

; [Note it is not possible to have more than 20 rows off the bottom of the screen]

l0ed7:		LD	L,$08		; L=Signal at the end of the last row of the BASIC line.
		LD	A,$0D		; A='Enter' character.
		RET			; Return with zero flag reset to indicate match found.
					; ------------------------------------
					; Fetch Character from Edit Buffer Row
					; ------------------------------------
					; Entry: B =Column number.
					;        DE=Start address of row in Edit Buffer.
					; Exit : Carry flag set indicates character fetched, reset if column out of range.

l0edc:		LD	A,$1F		; Column 31.
		CP	B		; Is column
		CCF			;
		RET	NC		; Return if B is greater than 31.

		LD	L,B		;
		LD	H,$00		; HL=Column number.
		ADD	HL,DE		;
		LD	A,(HL)		; Fetch the character at the specified column.
		INC	B		; Increment the column number.
		SCF			; Signal character fetched.
		RET			;

l0ee9:		DB	$01
		DB	$14

l0eeb:		DB	$01
		DB	$01

; Subroutine to ???

l0eed:		LD	HL,TV_FLAG
		res	0,(HL)		; signal "not using lower screen"
		LD	HL,l0ee9
		LD	DE,ed_N_ROWS
		JP	l2152		; copy $14 into ed_N_ROWS and exit

l0efb:		LD	HL,TV_FLAG
		SET	0,(HL)		; signal "using lower screen"
		LD	BC,$0000
		CALL	l191d		; output "AT 0,0"
		LD	HL,l0eeb
		LD	DE,ed_N_ROWS
		JP	l2152		; copy $01 into ed_N_ROWS and exit

l0f0f:		LD	H,$00
		LD	L,B
		ADD	HL,DE
		LD	A,(HL)
		CP	$00
		SCF
		RET	NZ
		LD	A,B
		OR	A
		JR	Z,l0f29		; (13)
		PUSH	HL
		DEC	HL
		LD	A,(HL)
		CP	$00
		SCF
		POP	HL
		RET	NZ

l0f24:		LD	A,(HL)
		CP	$00
		SCF
		RET	NZ

l0f29:		INC	HL
		INC	B
		LD	A,B
		CP	$1F
		JR	C,l0f24		; (-12)
		RET

l0f31:		LD	H,$00
		LD	L,B
		ADD	HL,DE
		LD	A,(HL)
		CP	$00
		SCF
		RET	NZ

l0f3a:		LD	A,(HL)
		CP	$00
		JR	NZ,l0f46	; (7)
		LD	A,B
		OR	A
		RET	Z
		DEC	HL
		DEC	B
		JR	l0f3a		; (-12)

l0f46:		INC	B
		SCF
		RET
		LD	H,$00
		LD	L,B
		ADD	HL,DE
		LD	A,(HL)
		RET

l0f4f:		LD	HL,ed_flags
		OR	A
		BIT	0,(HL)
		RET	NZ
		PUSH	BC
		PUSH	AF
		CALL	l1182
		POP	AF

l0f5c:		CALL	l0577
		PUSH	AF
		EX	DE,HL
		CALL	l16fe
		EX	DE,HL
		POP	AF
		CCF
		JR	Z,l0f9a		; (49)
		PUSH	AF
		LD	B,$00
		INC	C
		LD	A,(ed_N_ROWS)
		CP	C
		JR	C,l0f96		; (35)
		LD	A,(HL)
		LD	E,A
		AND	$D7
		CP	(HL)
		LD	(HL),A
		LD	A,E
		SET	1,(HL)
		PUSH	AF
		CALL	l1182
		POP	AF
		JR	Z,l0f90		; (13)
		res	0,A
		CALL	l0fa1
		JR	NC,l0f9a	; (16)
		CALL	l16ee
		POP	AF
		JR	l0f5c		; (-52)

l0f90:		CALL	l0f0f
		POP	AF
		JR	l0f5c		; (-58)

l0f96:		POP	AF
		CALL	l123c

l0f9a:		POP	BC
		RET

l0f9c:		CALL	l1182
		LD	A,$09

l0fa1:		PUSH	BC
		PUSH	DE
		LD	B,C
		LD	HL,l0fbd
		LD	C,A
		PUSH	BC
		CALL	l0540
		POP	BC
		LD	A,C
		JR	NC,l0fba	; (10)
		LD	C,B
		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		LD	(HL),A
		SCF

l0fba:		POP	DE
		POP	BC
		RET

l0fbd:		DS	$20

		ADD	HL,BC

		DS	2

l0fe0:		PUSH	BC
		CALL	l1182
		PUSH	BC

l0fe5:		LD	HL,$0020
		ADD	HL,DE
		BIT	1,(HL)
		LD	A,$00
		JR	Z,l0fff		; (16)
		INC	C
		LD	HL,$0023
		ADD	HL,DE
		EX	DE,HL
		LD	A,(ed_N_ROWS)
		CP	C
		JR	NC,l0fe5	; (-22)
		DEC	C
		CALL	l1297

l0fff:		POP	HL

l1000:		PUSH	HL
		CALL	l1182
		POP	HL
		LD	B,A
		LD	A,C
		CP	L
		LD	A,B
		PUSH	AF
		JR	NZ,l100f	; (3)
		LD	B,H
		JR	l1018		; (9)

l100f:		PUSH	AF
		PUSH	HL
		LD	B,$00
		CALL	l0f0f
		POP	HL
		POP	AF

l1018:		PUSH	HL
		LD	HL,$F6F4
		SET	0,(HL)
		JR	Z,l1022		; (2)
		res	0,(HL)

l1022:		CALL	l058c
		PUSH	AF
		PUSH	BC
		PUSH	DE
l1028:		LD	HL,$F6F4
		BIT	0,(HL)
		JR	NZ,l103d	; (14)
		LD	B,$00
		CALL	l0c9f
		JR	C,l103d		; (7)
		CALL	l104e
		POP	DE
		POP	BC
		JR	l1042		; (5)

l103d:		POP	HL
		POP	BC
l103f:		CALL	l16fe

l1042:		POP	AF
		DEC	C

l1044:		LD	B,A
		POP	HL
		POP	AF
		LD	A,B
		JP	NZ,l1000
		SCF
		POP	BC
		RET

l104e:		LD	HL,$0020
		ADD	HL,DE
		LD	A,(HL)
		BIT	0,(HL)
		JR	NZ,l1080	; (41)
		PUSH	AF
		PUSH	BC
		LD	A,C
		OR	A
		JR	NZ,l1072	; (21)
		PUSH	BC
		LD	HL,($FC9A)
		CALL	l1418
		LD	($FC9A),HL
		LD	A,(nr_above)
		LD	C,A
		DEC	C
		CALL	l1385
		POP	BC
		JR	l1076		; (4)

l1072:		DEC	C
		CALL	l1182

l1076:		POP	BC
		POP	AF
		LD	HL,$0020
		ADD	HL,DE
		res	1,(HL)
		OR	(HL)
		LD	(HL),A

l1080:		LD	B,C
		CALL	l1182
		CALL	l11ad
		JP	l0513


; Editing keys: DELETE WORD LEFT

l108a:		CALL	l1152
l108d:		PUSH	HL
		CALL	l1163
		JR	Z,l10c5		; (50)
		CALL	l0c26
		POP	HL
		JR	NC,l10c6	; (45)
		CALL	l0ae5
		PUSH	AF
		PUSH	HL
		CALL	l0fe0
		POP	HL
		POP	AF
		CP	$20
		JR	Z,l108d		; (-26)

l10a7:		PUSH	HL
		CALL	l1163
		JR	Z,l10c5		; (24)
		CALL	l0c26
		POP	HL
		JR	NC,l10c6	; (19)
		CALL	l0ae5
		CP	$20
		JR	Z,l10c1		; (7)
		PUSH	HL
		CALL	l0fe0
		POP	HL
		JR	l10a7		; (-26)

l10c1:		PUSH	HL
		CALL	l0c43

l10c5:		POP	HL

l10c6:		LD	A,B
		PUSH	AF
		PUSH	HL
		LD	HL,$EEF5
		res	2,(HL)
		LD	A,(ed_N_ROWS)
		PUSH	BC
		LD	B,$00
		LD	C,A
		CP	A
		CALL	l04d0
		POP	BC
		LD	HL,ed_flags
		SET	3,(HL)
		POP	HL
		CALL	l0ac3
		POP	AF
		RET


; Editing keys: DELETE WORD RIGHT

l10e5:		CALL	l1152
l10e8:		PUSH	HL
		CALL	l0ae5
		POP	HL
		CP	$00
		SCF
		JR	Z,l10c6		; (-44)
		PUSH	AF
		PUSH	HL
		CALL	l0fe0
		POP	HL
		POP	AF
		CP	$20
		JR	NZ,l10e8	; (-21)

l10fd:		CALL	l0ae5
		CP	$20
		SCF
		JR	NZ,l10c6	; (-63)
		PUSH	HL
		CALL	l0fe0
		POP	HL
		JR	l10fd		; (-15)


; Editing keys: DELETE LINE LEFT

l110c:		CALL	l1152
l110f:		PUSH	HL
		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	0,(HL)
		JR	NZ,l1127	; (12)
		CALL	l0c26
		JR	NC,l113b	; (27)
		CALL	l0fe0
		POP	HL
		JR	l110f		; (-23)
		PUSH	HL

l1127:		LD	A,B
		CP	$00
		JR	Z,l113b		; (15)
		DEC	B
		CALL	l0ae5
		INC	B
		CP	$00
		JR	Z,l113b		; (6)
		DEC	B
		CALL	l0fe0
		JR	l1127		; (-20)

l113b:		POP	HL

l113c:		SCF
		JP	l10c6


; Editing keys: DELETE LINE RIGHT

l1140:		CALL	l1152
l1143:		CALL	l0ae5
		CP	$00
		SCF
		JR	Z,l113c		; (-15)
		PUSH	HL
		CALL	l0fe0
		POP	HL
		JR	l1143		; (-15)

l1152:		LD	HL,ed_flags
		res	0,(HL)
		CALL	l0ab7
		LD	HL,$EEF5
		SET	2,(HL)
		LD	HL,$F6F1
		RET

l1163:		CALL	l1182
		LD	HL,$0020
		ADD	HL,DE
		BIT	0,(HL)
		JR	Z,l117c		; (14)
		LD	A,B
		CP	$00
		JR	Z,l1180		; (13)
		DEC	B
		CALL	l0ae5
		INC	B
		CP	$00
		JR	Z,l1180		; (4)

l117c:		LD	A,$01
		OR	A
		RET

l1180:		XOR	A
		RET

l1182:		LD	HL,$EC16

l1185:		PUSH	AF
		LD	A,C
		LD	DE,$0023

l118a:		OR	A
		JR	Z,l1191		; (4)
		ADD	HL,DE
		DEC	A
		JR	l118a		; (-7)

l1191:		EX	DE,HL
		POP	AF
		RET
		PUSH	DE
		CALL	l1182
		LD	H,$00
		LD	L,B
		ADD	HL,DE
		POP	DE
		RET


l119e:		DB	$05
		DB	$00,$00,$00,$F8,$F6

; Subroutine to ???

l11a4:		LD	HL,l119e
		LD	DE,row_below
		JP	l2152

; Subroutine to ???

l11ad:		PUSH	BC
		PUSH	DE
		LD	HL,row_below	; ???
		PUSH	HL
		LD	A,(HL)
		OR	A
		JR	NZ,l11cf	; move on if ???
		PUSH	HL
		CALL	l142d		; setup token routines in RAM
		LD	HL,($F9D7)
		CALL	l1420		; get line number after ???
		JR	NC,l11c6	; move on if none
		LD	($F9D7),HL	; store it
l11c6:		LD	B,H
		LD	C,L
		POP	HL
		CALL	l13a4		; ???
		DEC	A
		JR	l11e4
l11cf:		LD	HL,ed_flags
		res	0,(HL)
		LD	HL,$F6F8
		LD	D,H
		LD	E,L
		LD	BC,$0023
		ADD	HL,BC
		LD	BC,$02BC
		LDIR
		DEC	A
		SCF
l11e4:		POP	DE
		LD	(DE),A
		LD	HL,$F6F8
		POP	DE
		POP	BC
		RET


l11ec:		PUSH	BC
		PUSH	DE
		LD	HL,$0020
		ADD	HL,DE
		LD	A,(HL)
		CPL
		AND	$11
		JR	NZ,l120d	; (21)
		PUSH	HL
		PUSH	DE
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	E,(HL)
		PUSH	DE
		CALL	l142d
		POP	HL
		CALL	l1418
		JR	NC,l120b	; (3)
		LD	($F9D7),HL

l120b:		POP	DE
		POP	HL

l120d:		BIT	0,(HL)
		LD	HL,row_below
		PUSH	HL
		JR	Z,l121a		; (5)
		LD	A,$00
		SCF
		JR	l11e4		; (-54)

l121a:		LD	A,(HL)
		CP	$14
		JR	Z,l11e4		; (-59)
		LD	BC,$0023
		LD	HL,$F6F8
		EX	DE,HL
		LDIR
		LD	HL,$F9D6
		LD	D,H
		LD	E,L
		LD	BC,$0023
		OR	A
		SBC	HL,BC
		LD	BC,$02BC
		LDDR
		INC	A
		SCF
		JR	l11e4		; (-88)

l123c:		PUSH	BC
		PUSH	DE
		PUSH	AF
		LD	B,$00
		LD	C,$01
		PUSH	HL
		CALL	l1291
		POP	HL
		BIT	3,(HL)
		res	3,(HL)
		JR	NZ,l126e	; (32)

l124e:		CALL	l0f0f
		POP	AF

l1252:		CALL	l0577
		JR	Z,l1288		; (49)
		PUSH	AF
		LD	B,$00
		INC	C
		LD	A,C
		CP	$15
		JR	C,l126e		; (14)
		DEC	HL
		LD	A,(HL)
		INC	HL
		CP	$00
		JR	Z,l126e		; (7)
		PUSH	HL
		LD	HL,ed_flags
		SET	0,(HL)
		POP	HL

l126e:		BIT	1,(HL)
		SET	1,(HL)
		res	3,(HL)
		CALL	l1291
		JR	NZ,l124e	; (-43)
		PUSH	BC
		PUSH	DE
		CALL	l16e0		; pad line at DE with nulls to length 32
		LD	(HL),$08
		POP	DE
		POP	BC
		CALL	l16ee
		POP	AF
		JR	l1252		; (-54)

l1288:		LD	A,C
		LD	(row_below),A
		SET	3,(HL)
		POP	DE
		POP	BC
		RET

l1291:		LD	HL,$F6F8
		JP	l1185

l1297:		PUSH	BC
		PUSH	DE
		LD	HL,ed_flags
		res	0,(HL)
		LD	A,(row_below)
		LD	C,A
		OR	A
		LD	A,$00
		JR	Z,l12e9		; (66)

l12a7:		CALL	l1291
		PUSH	AF
		LD	B,$00
		CALL	l0f0f
		JR	NC,l12c0	; (14)
		POP	AF
		CALL	l058c
		PUSH	AF
		PUSH	BC
		LD	B,$00
		CALL	l0f0f
		POP	BC
		JR	C,l12e4		; (36)

l12c0:		INC	HL
		LD	A,(HL)
		PUSH	AF
		PUSH	BC
		LD	A,C
		CP	$01
		JR	NZ,l12d2	; (9)
		LD	A,(ed_N_ROWS)
		LD	C,A
		CALL	l1182
		JR	l12d6		; (4)

l12d2:		DEC	C
		CALL	l1291

l12d6:		POP	BC
		POP	AF
		LD	HL,$0020
		ADD	HL,DE
		res	1,(HL)
		OR	(HL)
		LD	(HL),A
		LD	HL,row_below
		DEC	(HL)

l12e4:		POP	AF
		DEC	C
		JR	NZ,l12a7	; (-65)
		SCF

l12e9:		POP	DE
		POP	BC
		RET

; ???

l12ec:		DB	$03
		DB	$00,$DE,$F9

; Subroutine to ???

l12f0:		LD	HL,l12ec
		LD	DE,nr_above
		JP	l2152

l12f9:		PUSH	BC
		PUSH	DE
		LD	HL,nr_above
		PUSH	HL
		LD	A,(HL)
		OR	A
		JR	NZ,l1321	; (30)
		PUSH	HL
		CALL	l142d
		LD	HL,($FC9A)
		CALL	l1418
		JR	NC,l1312	; (3)
		LD	($FC9A),HL

l1312:		LD	B,H
		LD	C,L
		POP	HL
		INC	HL
		INC	HL
		INC	HL
		JR	NC,l132b	; (17)
		CALL	l13a4
		DEC	A
		EX	DE,HL
		JR	l132b		; (10)

l1321:		LD	HL,($F9DC)
		LD	BC,$0023
		SBC	HL,BC
		SCF
		DEC	A

l132b:		EX	DE,HL
		POP	HL
		JR	NC,l1330	; (1)
		LD	(HL),A

l1330:		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		EX	DE,HL
		POP	DE
		POP	BC
		RET

l1338:		PUSH	BC
		PUSH	DE
		LD	HL,$0020
		ADD	HL,DE
		LD	A,(HL)
		CPL
		AND	$11
		JR	NZ,l1350	; (12)
		PUSH	DE
		PUSH	HL
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	E,(HL)
		LD	($FC9A),DE
		POP	HL
		POP	DE

l1350:		BIT	3,(HL)
		LD	HL,nr_above
		PUSH	HL
		JR	Z,l136e		; (22)
		PUSH	HL
		CALL	l142d
		LD	HL,($FC9A)
		CALL	l1420
		LD	($FC9A),HL
		POP	HL
		INC	HL
		INC	HL
		INC	HL
		LD	A,$00
		SCF
		JR	l132b		; (-67)

l136e:		LD	A,(HL)
		CP	$14
		JR	Z,l1381		; (14)
		INC	A
		LD	HL,($F9DC)
		LD	BC,$0023
		EX	DE,HL
		LDIR
		EX	DE,HL
		SCF
		JR	l132b		; (-86)

l1381:		POP	HL
		POP	DE
		POP	BC
		RET

l1385:		LD	HL,$F9DE
		JP	l1185

; Table of routine addresses for printing chars in EDITOR

l138b:		DB	$08
		DB	$0D
		DW	l16c6		; ???
		DB	$01
		DW	l16d4		; ???
		DB	$12
		DW	l1428		; for colour codes, skip embedded code
		DB	$13
		DW	l1428
		DB	$14
		DW	l1428
		DB	$15
		DW	l1428
		DB	$10
		DW	l1428
		DB	$11
		DW	l1428

; Subroutine to ???

l13a4:		LD	D,H
		LD	E,L
		INC	DE
		INC	DE
		INC	DE
l13a9:		PUSH	DE
		LD	HL,$0020
		ADD	HL,DE
		LD	(HL),$01	; store ???
		INC	HL
		LD	(HL),B
l13b2:		INC	HL
		LD	(HL),C		; store line number
		LD	C,$01		; coloumn 1 ???
		LD	B,$00		; character position on screen line
l13b8:		PUSH	BC
		PUSH	DE
		LD	A,(process)
		CP	$04
		CALL	NZ,l1611	; unless in calculator, get next line char
		POP	DE
		POP	BC
		JR	C,l13d5		; move on if found character
		LD	A,C
		CP	$01
		LD	A,$0D
		JR	NZ,l13d5	; do CR unless no chars output
		LD	A,B
		OR	A
		LD	A,$01
		JR	Z,l13d5		; ???
		LD	A,$0D		; CR
l13d5:		LD	HL,l138b
		CALL	l2166		; perform actions for special chars
		JR	C,l13fa		; if successful action, move on
		JR	Z,l13b8		; loop back if need new char
		PUSH	AF
		LD	A,$1F
		CP	B
		JR	NC,l13f4	; move on unless need to start new line
		LD	A,$12
		CALL	l13ff		; ???
		JR	C,l13f1		; ???
		POP	AF
		LD	A,$0D
		JR	l13d5
l13f1:		CALL	l16ee		; ???
l13f4:		POP	AF
		CALL	l16bf		; ???
		JR	l13b8
l13fa:		POP	HL
		LD	A,C		; ???
		RET	Z
		SCF
		RET

; Subroutine to ???

l13ff:		PUSH	AF
		CALL	l16e0		; pad line at DE to length 32 with nulls
		POP	AF
		XOR	(HL)		; ??
		LD	(HL),A
		LD	A,C
		CP	$14
		RET	NC
		INC	C
		LD	HL,$0023
		ADD	HL,DE
		EX	DE,HL
		LD	HL,$0020
		ADD	HL,DE
		LD	(HL),$00
		SCF
		RET

; Subroutine to get number (HL) of line before HL (or 0 if none)

l1418:		CALL	l15b0		; find line number before HL
		RET	C		; exit if found
		LD	HL,$0000	; else use 0
		RET

; Subroutine to get number (HL) of line after HL (or 0 if none)

l1420:		CALL	l152a		; get line number after HL
		RET	C		; exit if found
		LD	HL,$0000	; else use 0
		RET

; Subroutine to skip an embedded colour code in a line

l1428:		CALL	l1611		; get next char (skip colour code)
		CCF
		RET	NC		; exit if success, else set no data left

; Subroutine to setup token scanning/expanding routines

l142d:		LD	HL,$0000
		LD	($FC9F),HL	; set "no line data"
		LD	($FCA1),HL	; set "no expanded token"
		LD	HL,l1442
		LD	DE,$FCAE
		LD	BC,$00E8
		LDIR			; copy token routines into RAM
		RET

; Routine executed in RAM at $FCAE
; On entry, A=zero-based token number (code-$A5), and on exit $FCA1
; contains address of expanded token

l1442:		DI
		PUSH	AF
		LD	BC,PBANKM
		LD	A,$17
		OUT	(C),A		; page in ROM 1 & RAM 7
		LD	BC,PBANK678
		LD	A,(BANK678)
		SET	2,A
		OUT	(C),A		; page in ROM 3
		POP	AF
		CP	$50
		JR	NC,l148b	; these parts start off search at every
		CP	$40		; 16th token for speed
		JR	NC,l1484
		CP	$30
		JR	NC,l147d
		CP	$20
		JR	NC,l1476
		CP	$10
		JR	NC,l146f
		LD	HL,$0096	; RND in token table
		JR	l1490
l146f:		SUB	$10
		LD	HL,$00CF	; ASN in token table
		JR	l1490
l1476:		SUB	$20
		LD	HL,$0100	; OR in token table
		JR	l1490
l147d:		SUB	$30
		LD	HL,$013E	; MERGE in token table
		JR	l1490
l1484:		SUB	$40
		LD	HL,$018B	; RESTORE in token table
		JR	l1490
l148b:		SUB	$50
		LD	HL,$01D4	; PRINT in token table
l1490:		LD	B,A		; B=offset of token from current
		OR	A
l1492:		JR	Z,l149d
l1494:		LD	A,(HL)		; get next character
		INC	HL
		AND	$80
		JR	Z,l1494		; loop back until end of token found
		DEC	B		; decrement token offset
		JR	l1492		; loop back
l149d:		LD	DE,$FCA3
		LD	($FCA1),DE	; set expanded token address
		LD	A,($FC9E)
		OR	A		; test "leading space" flag
		LD	A,$00
		LD	($FC9E),A	; and set to zero
		JR	NZ,l14b3
		LD	A," "
		LD	(DE),A		; insert space if necessary
		INC	DE
l14b3:		LD	A,(HL)
		LD	B,A
		INC	HL
		LD	(DE),A		; copy token byte
		INC	DE
		AND	$80
		JR	Z,l14b3		; back until end of token
		LD	A,B
		AND	$7F
		DEC	DE
		LD	(DE),A		; mask off high bit in last char
		INC	DE
		LD	A," "+$80	; and add terminating space
		LD	(DE),A
		LD	BC,PBANKM
		LD	A,(BANKM)
		OUT	(C),A		; restore original ROM/RAM configuration
		LD	BC,PBANK678
		LD	A,(BANK678)
		OUT	(C),A
		EI
		RET

; Routine executed in RAM at $FD43
; On entry, bit-7 terminated word to check for is at $FDA0
; On exit, if carry set, A=token code, else A=0

l14d7:		DI
		PUSH	AF
		LD	BC,PBANKM
		LD	A,$17
		OUT	(C),A		; page in ROM 1 & RAM 7
		LD	BC,PBANK678
		LD	A,(BANK678)
		SET	2,A
		OUT	(C),A		; page in ROM 3
		POP	AF
		LD	HL,$0096	; token table start
		LD	B,$A5		; first token number
l14f0:		LD	DE,$FDA0	; start of word to test
l14f3:		LD	A,(DE)		; get next letter of word to test
		AND	$7F
		CP	$61
		LD	A,(DE)
		JR	C,l14fd
		AND	$DF		; mask lowercase letters to uppercase
l14fd:		CP	(HL)		; test against current token letter
		JR	NZ,l1509	; move on if no match
		INC	HL
		INC	DE
		AND	$80
		JR	Z,l14f3		; loop back unless last token character
		SCF			; success
		JR	l1515
l1509:		INC	B		; increment token number
		JR	Z,l1514		; exit if all checked
l150c:		LD	A,(HL)
		AND	$80
		INC	HL
		JR	Z,l150c		; loop back until current token finished
		JR	l14f0		; back to check more tokens
l1514:		OR	A		; failure
l1515:		LD	A,B
		LD	D,A
		LD	BC,PBANKM
		LD	A,(BANKM)
		OUT	(C),A		; page back original ROM/RAM configuration
		LD	BC,PBANK678
		LD	A,(BANK678)
		OUT	(C),A
		LD	A,D		; A=token number
		EI
		RET

; Subroutine to form ASCII line number for next line after HL. Exits
; with DE=address of line data & HL=line number

l152a:		CALL	l15e4		; set "no line" addresses
		OR	A
		LD	($FC9E),A	; set "leading spaces"
		CALL	l05a7		; page in normal memory
		CALL	l15f0		; get address of line HL
		JR	NC,l158b	; exit if not found
		JR	NZ,l1547
		LD	A,B
		OR	C
		JR	Z,l1547		; move on if first line was required
		CALL	l15c9		; get address of next line
		CALL	l15d3
		JR	NC,l158b	; exit if end of program

; Subroutine to form ASCII line number for line at HL.
; Exits with HL=line number, DE=address of line data

l1547:		LD	D,(HL)
		INC	HL
		LD	E,(HL)		; get line number
		CALL	l05cc		; page in DOS workspace
		PUSH	DE
		PUSH	HL
		PUSH	IX
		LD	IX,$FCA3
		LD	($FCA1),IX	; set ASCII line number address
		EX	DE,HL
		LD	B,$00		; don't form leading zeros
		LD	DE,$FC18
		CALL	l158f		; form 1000s
		LD	DE,$FF9C
		CALL	l158f		; form 100s
		LD	DE,$FFF6
		CALL	l158f		; form 10s
		LD	DE,$FFFF
		CALL	l158f		; form units
		DEC	IX
		LD	A,(IX+$00)
		OR	$80		; set bit 7 of last digit
		LD	(IX+$00),A
		POP	IX
		POP	HL
		POP	DE
		INC	HL
		INC	HL
		INC	HL		; HL=address of line data
		LD	($FC9F),HL	; save it
		EX	DE,HL
		SCF			; success
		RET
l158b:		CALL	l05cc		; page in DOS workspace
		RET			; exit

; Subroutine to form next digit of line number at IX
; Line number in HL, unit size in DE, and print zero flag in B

l158f:		XOR	A		; count 0
l1590:		ADD	HL,DE		; reduce line number by unit size
		INC	A		; increment count
		JR	C,l1590		; loop until overflow
		SBC	HL,DE		; add back last try
		DEC	A
		ADD	A,"0"		; form ASCII digit
		LD	(IX+$00),A	; store it
		CP	"0"
		JR	NZ,l15ab	; set flag if not zero
		LD	A,B
		OR	A
		JR	NZ,l15ad	; if flag set, leave 0 digits
		LD	A,$00
		LD	(IX+$00),A	; else replace with $00
		JR	l15ad
l15ab:		LD	B,$01		; set "print 0 digits" flag
l15ad:		INC	IX		; increment pointer
		RET

; Subroutine to get number (HL) and address (DE) of line before line HL
; forming ASCII line number in page 7. Carry reset if no prior line.

l15b0:		CALL	l15e4		; initialise "no line" addresses
		OR	A
		LD	($FC9E),A	; ???
		CALL	l05a7		; page in normal memory
		CALL	l15f0		; find address of line HL
		JR	NC,l158b	; if not found, exit with error
		EX	DE,HL
		LD	A,L
		OR	H		; was there a previous line?
		SCF
		JP	NZ,l1547	; form line number & address of data if so
		CCF
		JR	l158b		; else exit with error

; Subroutine to get address of next program line in HL (current is
; saved in DE)

l15c9:		PUSH	HL
		INC	HL
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; DE=line length
		INC	HL
		ADD	HL,DE		; HL=next line
		POP	DE		; DE=previous line
		RET

; Subroutine to check for end of program (carry reset if so)

l15d3:		LD	A,(HL)		; check next program byte
		AND	$C0
		SCF
		RET	Z		; exit with carry set if not end-of-program
		CCF			; clear carry if end-of-program
		RET

; Subroutine to check if line number at HL is equal to BC (carry set if so)

l15da:		LD	A,B
		CP	(HL)
		RET	NZ
		LD	A,C
		INC	HL
		CP	(HL)
		DEC	HL
		RET	NZ
		SCF
		RET

; Subroutine to set "no line" addresses

l15e4:		PUSH	HL
		LD	HL,$0000
		LD	($FCA1),HL	; set "no ASCII line number"
		LD	($FC9F),HL	; set "no line data"
		POP	HL
		RET

; Subroutine to search for address of line HL, returning in HL with
; carry set. If HL=0 first line address is returned. Carry reset if
; line not found. DE=address of previous line (or 0)

l15f0:		PUSH	HL
		POP	BC		; BC=line to find
		LD	DE,$0000	; no previous line
		LD	HL,(PROG)	; get start of program
		CALL	l15d3		; check if end

		IF garry

		JP	l3873
l15fe:		LD	HL,(CURCHL)
l1601:		JP	l387f

		ELSE

		RET	NC		; exit if so with failure
		CALL	l15da		; is it line BC
		RET	C		; exit if so with success
		LD	A,B
l1601:		OR	C
		SCF
		RET	Z		; exit with first line if line 0 specified

		ENDIF

l1604:		CALL	l15c9		; get to next line
		CALL	l15d3
		RET	NC		; exit if program end
		CALL	l15da
		JR	NC,l1604	; loop back if not line BC yet
		RET

; Subroutine to get next character (A) from line. Carry reset if none left

l1611:		LD	HL,($FCA1)	; get address of ASCII text
		LD	A,L
		OR	H
		JR	Z,l1636		; move on if none
		LD	A,(HL)
		INC	HL
		CP	$A0		; test for terminating space
		LD	B,A
		LD	A,$00		; set "print leading space" if none
		JR	NZ,l1623
		LD	A,$FF		; else suppress
l1623:		LD	($FC9E),A	; set flag
		LD	A,B
		BIT	7,A
		JR	Z,l162e
		LD	HL,$0000	; if last character, set "no text left"
l162e:		LD	($FCA1),HL	; update address
		AND	$7F		; mask high bit
		JP	l1689		; exit with success
l1636:		LD	HL,($FC9F)	; get address of line data
		LD	A,L
		OR	H
		JP	Z,l168b		; exit with fail if none
		CALL	l05a7		; page in normal memory
l1641:		LD	A,(HL)
		CP	$0E		; check for embedded number
		JR	NZ,l164e
		INC	HL		; if found, skip & loop back
		INC	HL
		INC	HL
		INC	HL
		INC	HL
		INC	HL
		JR	l1641
l164e:		CALL	l05cc		; page in DOS workspace
		INC	HL
		LD	($FC9F),HL	; update address
		CP	$A5
		JR	C,l1661		; move on unless 48K BASIC token
		SUB	$A5		; get token number (0+)
		CALL	$FCAE		; expand to ASCII text
		JP	l1611		; go to get first char
l1661:		CP	$A3
		JR	C,l1675		; move on unless +3 BASIC token
		JR	NZ,l166c
		LD	HL,l168e	; SPECTRUM token
		JR	l166f
l166c:		LD	HL,l1696	; PLAY token
l166f:		CALL	$FD09		; expand to ASCII text
		JP	l1611		; go to get first char
l1675:		PUSH	AF
		LD	A,$00
		LD	($FC9E),A	; flag "print leading space"
		POP	AF
		CP	$0D
		JR	NZ,l1689	; exit with success unless end of line
		LD	HL,$0000
		LD	($FCA1),HL	; set no ASCII text
		LD	($FC9F),HL	; and no line data
l1689:		SCF
		RET
l168b:		SCF
		CCF			; fail
		RET

l168e:		DB	"SPECTRU", "M"+$80
l1696:		DB	"PLA", "Y"+$80
		DB	"GOT", "O"+$80
		DB	"GOSU", "B"+$80
		DB	"DEFF", "N"+$80
		DB	"OPEN", "#"+$80
		DB	"CLOSE", "#"+$80

l16b3:		DB	$02
		DB	$01
		DB	$05

l16b6:		LD	HL,l16b3
		LD	DE,$FD96
		JP	l2152



; Subroutine to ???

l16bf:		LD	L,B
		LD	H,$00
		ADD	HL,DE
		LD	(HL),A
		INC	B
		RET
l16c6:		CALL	l16e0		; pad line at DE to length 32 with nulls
		LD	A,(HL)
		OR	$18
		LD	(HL),A
		LD	HL,$FD96
		SET	0,(HL)
		SCF
		RET
l16d4:		CALL	l16e0		; pad line at DE to length 32 with nulls
		SET	3,(HL)
		LD	HL,$FD96
		SET	0,(HL)
		SCF
		RET

; Subroutine to pad line at DE (length B) with nulls to length B=32
; On exit, HL=end of line+1

l16e0:		LD	L,B
		LD	H,0
		ADD	HL,DE		; get past last char on line
		LD	A,$20
l16e6:		CP	B
		RET	Z		; exit if 32 chars already
		LD	(HL),0		; pad with zeros
		INC	HL
		INC	B
		JR	l16e6


l16ee:		LD	A,($FD97)
		LD	B,$00

l16f3:		LD	H,$00
		LD	L,B
		ADD	HL,DE
		LD	(HL),$00
		INC	B
		DEC	A
		JR	NZ,l16f3	; (-10)
		RET

l16fe:		PUSH	BC
		PUSH	DE
		PUSH	HL
		PUSH	HL
		LD	HL,$EEF5
		BIT	2,(HL)
		POP	HL
		JR	NZ,l170e	; (4)
		LD	B,C
		CALL	l1d2b

l170e:		POP	HL
		POP	DE
		POP	BC
		RET

l1712:		PUSH	BC
		PUSH	DE
		PUSH	HL
		PUSH	HL
		LD	HL,$EEF5
		BIT	2,(HL)
		POP	HL
		JR	NZ,l1722	; (4)
		LD	E,C
		CALL	l1ccc

l1722:		POP	HL
		POP	DE
		POP	BC
		RET

l1726:		PUSH	BC
		PUSH	DE
		PUSH	HL
		PUSH	HL
		LD	HL,$EEF5
		BIT	2,(HL)
		POP	HL
		JR	NZ,l1736	; (4)
		LD	E,C
		CALL	l1cd3

l1736:		POP	HL
		POP	DE
		POP	BC
		RET

; Subroutine to place cursor at column B, line C (of editing area)

l173a:		PUSH	AF
		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	A,B
		LD	B,C		; B=line of editing area
		LD	C,A		; C=column
		CALL	l1caa
		POP	HL
		POP	DE
		POP	BC
		POP	AF
		RET

; Subroutine to remove cursor from column B, line C (of editing area)

l1749:		PUSH	AF
		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	A,B
		LD	B,C
		LD	C,A
		CALL	l1cbf
		POP	HL
		POP	DE
		POP	BC
		POP	AF
		RET

l1758:		LD	A,$00
		LD	(MODE),A
		LD	A,$02
		LD	(REPPER),A
		CALL	l185a
		RET


		LD	(HL),H
		RST	18H
		SBC	A,$55
		DJNZ	l17bd+1		; (82)
		RET
		SBC	A,(HL)
		SBC	A,(HL)
		CP	L
		LD	H,D
		PUSH	BC
		RET	NZ
		LD	D,L
		DB	$C2,$44,$10
		RLA
		CP	$5F
		SUB	B
		CP	$D1
		DB	$DD
		PUSH	DE
		RLA
		SUB	B
		RST	30H
		RST	18H
		RST	18H
		CALL	NC,$D9C7
		SBC	A,$C3
		CP	L
		DI
		CALL	C,$D659
		LD	D,(HL)
		DJNZ	l17a8		; (23)
		LD	H,H
		LD	B,A
		LD	E,A
		SUB	B
		LD	H,B
		RST	18H
		LD	B,B

l1798:		LD	B,E
		SUB	A
		DJNZ	l1798		; (-4)
l179c:		LD	D,C
		RST	$0
		LD	B,E
		LD	E,A
		SBC	A,$BD
		LD	H,(HL)
		EXX

l17a4:		LD	E,E
		SUB	B
		RLA
		DB	$E2
l17a8:		PUSH	DE
l17a9:		LD	D,H
		DJNZ	l17a4		; (-8)
		LD	D,L
		LD	B,D
		LD	B,D
		LD	E,C
		SBC	A,$57
		RLA
		SUB	B
		LD	A,A
		LD	E,H
		LD	E,H
		EXX
		ADD	A,$55
		JP	NZ,$5190
l17bd:		SBC	A,$BD
		CALL	po,$D5D8
		SUB	B
		LD	(HL),H
		LD	E,A
		JP	NZ,$595B
		LD	E,(HL)
		RST	10H
		SUB	B
		LD	A,L
		LD	E,A
		JP	NC,$64BD
		LD	E,B
		POP	DE

l17d2:		LD	E,(HL)
		IN	A,($C3)
		DJNZ	l179c-1		; (-60)
		LD	E,A
		LD	E,$1E
		CP	L
		LD	H,H
		RET	C
		PUSH	DE
		DJNZ	l17d2		; (-14)
		LD	B,D
		PUSH	DE
		LD	B,A
		PUSH	DE
		LD	B,D
		RET
		SUB	B
		CALL	po,$C0D1
		SUB	B
		DB	$18
		DB	88
		EXX
		OUT	($19),A
		CP	L
		JP	po,$DC5F

l17f4:		LD	D,C
		SBC	A,$54
		INC	E
		SUB	B
		LD	H,D
		EXX
		OUT	($58),A
		LD	D,C
		JP	NZ,$9054
		LD	D,L
		LD	B,H
		SUB	B
		POP	DE
		LD	E,H
		CP	L
		LD	D,C
		LD	E,(HL)
		LD	D,H
		SUB	B
		LD	SP,HL
		ADD	A,$DF
		LD	B,D
		DJNZ	l17a9		; (-104)
		RST	$0
		RET	C
		LD	C,C
		DJNZ	l17f4		; (-34)
		LD	E,A

l1817:		CALL	NZ,$BD19
		LD	H,A
		LD	B,D
		EXX
		LD	B,H
		CALL	NZ,$5ED5
		SUB	B
		RST	18H
		LD	E,(HL)
		SUB	B
		LD	H,B
		DI
		LD	H,A
		SUB	B
		ADC	A,B
		DEC	B
		ADD	A,C
		LD	(BC),A
		LD	B,E
		INC	E
		DJNZ	l1875+1		; (69)
		LD	B,E
		EXX
		LD	E,(HL)
		CP	L
		DB	$FD
		EX	AF,AF'
		NOP
		SUB	B
		LD	D,C

l183a:		SBC	A,$D4
		DJNZ	l183a		; (-4)
		EX	AF,AF'
		NOP
		DJNZ	l1887		; (69)
		LD	E,(HL)
		CALL	NC,$C255
		DJNZ	l183a+1		; (-13)
		LD	H,B
		SBC	A,A
		DB	$FD
		SBC	A,E
		CP	L
		CP	L
		JP	$C490
		RST	18H
		LD	E,$9E
		CP	L
		LD	H,H
		RET	C
		LD	D,L
		DJNZ	l1817		; (-67)

l185a:		LD	HL,FLAGS
		LD	A,(HL)
		OR	$0C
		LD	(HL),A
		LD	HL,ed_flags
		BIT	4,(HL)
		LD	HL,FLAGS3
		JR	NZ,l186e
		res	0,(HL)
		RET
l186e:		SET	0,(HL)
		RET


; Subroutine to get a key - this may include keypad keys which
; send extended mode before a keycode to give a token

l1871:		PUSH	HL		; save HL
l1872:		LD	HL,FLAGS
l1875:		BIT	5,(HL)
		JR	Z,l1875		; loop until a key is available
		res	5,(HL)		; signal no key available
		LD	A,(LAST_K)	; get key
		LD	HL,MODE
		res	0,(HL)		; set "L" mode (?)
		CP	$20
		JR	NC,l1894	; move on if not a control code
l1887:		CP	$10
		JR	NC,l1872	; loop back to ignore control codes>=$10
		CP	$06
		JR	C,l1872		; ignore control codes<$06
		CALL	l1896		; change mode if required
		JR	NC,l1872
l1894:		POP	HL		; restore HL
		RET
l1896:		RST	28H
		DW	o10DB		; call key mode change routine
		RET

; Subroutine to display a menu
; On entry HL=address of menu

l189a:		PUSH	HL
		CALL	l194d		; save menu window area of screen
		LD	HL,TV_FLAG
		res	0,(HL)		; signal "using main screen"
		POP	HL
		LD	E,(HL)		; E=number of menu lines
		INC	HL
		PUSH	HL
		LD	HL,l19fe
		CALL	l1925		; output control codes for top menu line
		POP	HL
		CALL	l1925		; output menu title
		PUSH	HL
		CALL	l1a34		; output rainbow
		LD	HL,l1a0c
		CALL	l1925		; output end of top menu line
		POP	HL
		PUSH	DE
		LD	BC,$0807
		CALL	l191d		; output 'AT 8,7'
l18c3:		PUSH	BC		; save screen position
		LD	B,$0C		; B=menu width 12 (+2 border)
		LD	A," "
		RST	10H		; output border space
l18c9:		LD	A,(HL)		; get next char
		INC	HL
		CP	$80
		JR	NC,l18d2	; move on if last char on menu line
		RST	10H		; else output
		DJNZ	l18c9		; & loop back for more
l18d2:		AND	$7F		; mask off end marker bit
		RST	10H		; output last character
l18d5:		LD	A,$20
		RST	10H		; output spaces for rest of menu width
		DJNZ	l18d5
		POP	BC
		INC	B		; get to next line
		CALL	l191d		; output AT command
		DEC	E
		JR	NZ,l18c3	; loop back for more lines
		LD	HL,$6F38	; HL=pixel coords for top left of menu
		POP	DE		; E=total number of menu lines
		SLA	E
		SLA	E
		SLA	E
		LD	D,E
		DEC	D		; D=menu height in pixel lines-1
		LD	E,$6F		; E=menu width in pixel lines-1
		LD	BC,$FF00
		LD	A,D
		CALL	l190b		; draw line top to bottom
		LD	BC,$0001
		LD	A,E
		CALL	l190b		; draw line left to right
		LD	BC,$0100
		LD	A,D
		INC	A
		CALL	l190b		; draw line bottom to top
		XOR	A
		CALL	l19dc		; put highlight on top line
		RET

; Subroutine to draw a line, used in menu display
; On entry, H=Y coord, L=X coord, A=line length, BC=amount to
; add to HL to get to next pixel

l190b:		PUSH	AF		; save registers
		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	B,H
		LD	C,L
		RST	28H
		DW	$22E9		; plot a pixel
		POP	BC		; restore registers
		POP	DE
		POP	HL
		POP	AF
		ADD	HL,BC		; get coords of next pixel
		DEC	A
		JR	NZ,l190b	; back for rest of line
		RET

; Subroutine to output 'AT b,c'

l191d:		LD	A,$16
		RST	10H
		LD	A,B
		RST	10H
		LD	A,C
		RST	10H
		RET

; Subroutine to output a $FF-terminated message (used in menus)
; If '+3' is encountered and a disk interface is not present, '+2e'
; is substituted

l1925:		LD	A,(HL)		; get next character
		INC	HL
		CP	$FF
		RET	Z		; exit if end-of-message marker
		CP	'+'
		JR	Z,l1931		; move on to check '+'
l192e:		RST	10H		; else output and loop back for more
		JR	l1925
l1931:		LD	A,(HL)
		CP	"3"		; check for '+3' string
		LD	A,$2B		; reload with '+'
		JR	NZ,l192e	; go back to output if not '+3'
		PUSH	HL
		LD	HL,FLAGS3
		BIT	4,(HL)
		POP	HL
		JR	NZ,l192e	; go back to output if disk interface present
		INC	HL
		INC	HL		; skip '+3 '
		LD	A,'+'		; and output '+2A' instead
		RST	10H
		LD	A,"2"
		RST	10H

		IF garry
		LD	A, "e"
		ELSE
		LD	A,"A"
		ENDIF

		JR	l192e		; back

; This routine has a dual purpose: to either copy a "window" area to the
; screen, or to save a "window" area in high memory (of page 7)
; The window area is of fixed size 12 lines x 14 chars, located at
; 7,7
; Enter at l194d to copy the area FROM the screen, or at l1950 to copy
; the area TO the screen
; A total of:  21 (system variables)
;            + 12x14x8 (bitmap)
;            + 12x14 (attributes)
;            = 1533 bytes are saved, at $EEF6 to $F4F2

l194d:		SCF
		JR	l1951
l1950:		AND	A		; reset carry (copying TO screen)
l1951:		LD	DE,$EEF6	; DE contains window save area in page 7
		LD	HL,TV_FLAG
		JR	C,l195a
		EX	DE,HL		; swap source & dest if necessary
l195a:		LDI
		JR	C,l195f
		EX	DE,HL		; swap back
l195f:		LD	HL,COORDS
		JR	C,l1965		; swap source & dest
		EX	DE,HL
l1965:		LD	BC,$0014
		LDIR			; copy COORDS to ATTR_T system variables
		JR	C,l196d
		EX	DE,HL		; swap back
l196d:		EX	AF,AF'		; save carry flag
		LD	BC,$0707	; Top left of window is 7,7
		CALL	l1da1		; get C=33-C and B=24-B-(IX+1)
		LD	A,(IX+$01)
		ADD	A,B
		LD	B,A		; correct B to 24-B
		LD	A,$0C		; for 12 character lines
l197b:		PUSH	BC		; save registers
		PUSH	AF
		PUSH	DE
		RST	28H
		DW	o0E9B		; get HL=address of line B in display file
		LD	BC,$0007
		ADD	HL,BC		; HL=address of left of window line
		POP	DE		; restore save area in page 7
		CALL	l1990		; copy a character line (width 14)
		POP	AF		; restore registers
		POP	BC
		DEC	B		; move to next character line
		DEC	A
		JR	NZ,l197b	; loop back for more lines
		RET

; Subroutine used by menu window transfer routine, to transfer a single
; character line (width 14)
; On entry, HL=screen address of top pixel line
;           DE=address of save area
;           Carry'=save/restore flag

l1990:		LD	BC,$080E	; B=8 pixel lines,C=14 bytes window width
l1993:		PUSH	BC		; save counters
		LD	B,$00		; BC=bytes to copy
		PUSH	HL		; save screen address
		EX	AF,AF'		; get carry flag
		JR	C,l199b
		EX	DE,HL		; swap source & dest if necessary
l199b:		LDIR
		JR	C,l19a0
		EX	DE,HL		; swap back
l19a0:		EX	AF,AF'		; save carry flag
		POP	HL		; restore screen address
		INC	H		; get to next pixel line
		POP	BC		; restore counts
		DJNZ	l1993		; back for more pixel lines
		PUSH	BC
		PUSH	DE
		RST	28H
		DW	o0E88
		EX	DE,HL		; HL=attributes address
		POP	DE
		POP	BC
		EX	AF,AF'		; get carry flag
		JR	C,l19b2
		EX	DE,HL		; swap source & dest if necessary
l19b2:		LDIR
		JR	C,l19b7
		EX	DE,HL		; swap back
l19b7:		EX	AF,AF'		; save carry flag
		RET

; Move menu highlight up a line
; On entry, A=current highlight line number, HL=address of menu size

l19b9:		CALL	l19dc		; remove current highlight
		DEC	A		; decrement line number
		JP	p,l19c3		; move on if still positive
		LD	A,(HL)		; else set to bottom line
		DEC	A
		DEC	A
l19c3:		CALL	l19dc		; replace highlight on new line
		SCF			; set carry, so calling routine doesn't
		RET			;  immediately call "move highlight down"

; Move menu highlight down a line
; On entry, A=current highlight line number, HL=address of menu size

l19c8:		PUSH	DE		; save DE
		CALL	l19dc		; remove current highlight
		INC	A		; increment line number
		LD	D,A
		LD	A,(HL)
		DEC	A
		DEC	A
		CP	D		; check against max line number
		LD	A,D
		JP	p,l19d7
		XOR	A		; set to 0 if too large
l19d7:		CALL	l19dc		; replace highlight on new line
		POP	DE		; restore DE
		RET

; Subroutine to switch menu line A (0=top) between highlighted and
; non-highlighted

l19dc:		PUSH	AF		; save registers
		PUSH	HL
		PUSH	DE
		LD	HL,$5907	; attribute address of top menu line
		LD	DE,$0020
		AND	A
		JR	Z,l19ec
l19e8:		ADD	HL,DE		; get to attribute address of required line
		DEC	A
		JR	NZ,l19e8
l19ec:		LD	A,$78
		CP	(HL)		; is it BRIGHT 1, PAPER 7, INK 0?
		JR	NZ,l19f3	; if not, change to this
		LD	A,$68		; if so, change to BRIGHT 1, PAPER 5, INK 0
l19f3:		LD	D,$0E		; 14 characters to do
l19f5:		LD	(HL),A		; change attributes
		INC	HL
		DEC	D
		JR	NZ,l19f5
		POP	DE		; restore registers
		POP	HL
		POP	AF
		RET

; Control codes for top line of menus

l19fe:		DB	$16,$07,$07	; AT 7,7
		DB	$15,$00,$14,$00	; OVER 0,INVERSE 0
		DB	$10,$07,$11,$00	; INK 7,PAPER 0
		DB	$13,$01		; BRIGHT 1
		DB	$FF

; Control codes for end of top line of menus

l1a0c:		DB	$11,$00		; PAPER 0
		DB	" "
		DB	$11,$07		; PAPER 7
		DB	$10,$00		; INK 0
		DB	$FF

; A two-character "character set" used for displaying the
; rainbow on menus and bars

l1a14:		DB	$01,$03,$07,$0F
		DB	$1F,$3F,$7F,$FF
		DB	$FE,$FC,$F8,$F0
		DB	$E0,$C0,$80,$00

; The rainbow string

l1a24:		DB	$10,$02," "
		DB	$11,$06,'!'
		DB	$10,$04," "
		DB	$11,$05,'!'
		DB	$10,$00," "
		DB	$FF

; Subroutine to output the "rainbow" on menus and bars

l1a34:		PUSH	BC		; save registers
		PUSH	DE
		PUSH	HL
		LD	HL,l1a14
		LD	DE,STRIP1
		LD	BC,$0010
		LDIR			; copy rainbow charset into RAM
		LD	HL,(CHARS)
		PUSH	HL		; save CHARS
		LD	HL,STRIP1-$0100
		LD	(CHARS),HL	; set to rainbow set
		LD	HL,l1a24
		CALL	l1925		; output rainbow
		POP	HL
		LD	(CHARS),HL	; restore CHARS
		POP	HL		; restore registers
		POP	DE
		POP	BC
		RET

; Subroutines to display the bars for various functions

l1a5a:		LD	HL,l080f	; +3 BASIC
		JR	l1a67

l1a5f:		LD	HL,l0817	; Calculator
		JR	l1a67

l1a64:		LD	HL,l0809	; Loader


; Subroutine to clear the bottom 3 lines to editor colours, and display
; a bar with a rainbow and the text at HL (bit 7-terminated) on line 21

l1a67:		PUSH	HL
		CALL	l1a8e		; clear bottom 3 lines to editor colours
		LD	HL,$5AA0	; attribute address of line 21
		LD	B,$20
		LD	A,$40
l1a72:		LD	(HL),A		; fill line 21 to BRIGHT 1, PAPER 0, INK 0
		INC	HL
		DJNZ	l1a72
		LD	HL,l19fe
		CALL	l1925		; output control codes for top menu lines
		LD	BC,$1500
		CALL	l191d		; output AT 21,0
		POP	DE
		CALL	l029e		; ouput the bar text
		LD	C,$1A		; output AT 21,26
		CALL	l191d
		JP	l1a34		; output the rainbow

; Subroutine to clear bottom 3 lines to editor colours

l1a8e:		LD	B,$15
		LD	D,$17
l1a92:		JP	l1d6b		; clear bottom 3 lines to editor colours


; The renumber routine

l1a95:		CALL	l05a7		; page in normal memory
		CALL	l1c12		; get number of lines in BASIC program
		LD	A,D
		OR	E
		JP	Z,l1bcd		; if none, signal "command failed" & exit
		LD	HL,(RC_STEP)
		RST	28H
		DW	o30A9		; HL=STEP*number of lines
		EX	DE,HL		; DE=STEP*number of lines
		LD	HL,(RC_START)
		ADD	HL,DE		; HL=projected last line number
		LD	DE,$2710
		OR	A
		SBC	HL,DE
		JP	NC,l1bcd	; if >9999, signal "command failed" & exit
		LD	HL,(PROG)	; get start of program
l1ab7:		RST	28H
		DW	o19B8		; get address of next line
		INC	HL
		INC	HL
		LD	(RC_LINE),HL	; store address of current line (after number)
		INC	HL
		INC	HL		; point after line length
		LD	(STRIP2+$11),DE	; store address of next line
l1ac5:		LD	A,(HL)		; get next character
		RST	28H
		DW	o18B6		; skip past embedded number if necessary
		CP	$0D
		JR	Z,l1ad2		; move on if end of line
		CALL	l1b1b		; replace any line number in this command
		JR	l1ac5		; loop back
l1ad2:		LD	DE,(STRIP2+$11)	; get address of next line
		LD	HL,(VARS)
		AND	A
		SBC	HL,DE
		EX	DE,HL
		JR	NZ,l1ab7	; loop back if not end of program
		CALL	l1c12
		LD	B,D
		LD	C,E		; BC=number of lines in program
		LD	DE,$0000
		LD	HL,(PROG)	; HL=address of first line
l1aea:		PUSH	BC		; save registers
		PUSH	DE
		PUSH	HL
		LD	HL,(RC_STEP)
		RST	28H
		DW	o30A9		; HL=(line-1)*STEP
		LD	DE,(RC_START)
		ADD	HL,DE
		EX	DE,HL		; DE=new line number
		POP	HL
		LD	(HL),D
		INC	HL
		LD	(HL),E		; store new number at start of line
		INC	HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		ADD	HL,BC		; get to start of next line
		POP	DE
		INC	DE		; increment line count
		POP	BC
		DEC	BC		; decrement lines to do
		LD	A,B
		OR	C
		JR	NZ,l1aea	; loop back for more
		CALL	l05cc		; page back DOS workspace
		LD	(RC_LINE),BC	; reset "current line being renumbered"
		SCF			; signal "command succeeded"
		RET

; Table of commands containing line numbers

l1b14:		DB	$CA		; LINE
		DB	$F0		; LIST
		DB	$E1		; LLIST
		DB	$EC		; GOTO
		DB	$ED		; GOSUB
		DB	$E5		; RESTORE
		DB	$F7		; RUN

; Subroutine to replace any line number in the current statement.
; On entry, HL=address of code, A=code
; On exit HL=address of next code to check

l1b1b:		INC	HL
		LD	(STRIP2+$0F),HL	; save pointer after command
		EX	DE,HL
		LD	BC,$0007
		LD	HL,l1b14
l1b26:		CPIR
		EX	DE,HL
		RET	NZ		; exit if not
		LD	C,$00		; set BC=0
l1b2c:		LD	A,(HL)		; get next character
		CP	$20
		JR	Z,l1b4c		; go to skip spaces
		RST	28H
		DW	o2D1B		; is it a digit?
		JR	NC,l1b4c	; go to skip if so
		CP	'.'
		JR	Z,l1b4c		; go to skip decimal point
		CP	$0E
		JR	Z,l1b50		; move on if found embedded number
		OR	$20
		CP	$65
		JR	NZ,l1b48	; if it's not an "e", exit
		LD	A,B
		OR	C
		JR	NZ,l1b4c	; found any characters suggesting a number?
l1b48:		LD	HL,(STRIP2+$0F)	; if not, exit with pointer after command
		RET
l1b4c:		INC	BC		; increment characters found
		INC	HL
		JR	l1b2c		; loop back for more
l1b50:		LD	(STRIP2+$07),BC	; save no of characters before embedded #
		PUSH	HL		; save pointer to embedded number
		RST	28H
		DW	o18B6		; skip past embedded number
		CALL	l1c43		; skip past spaces
		LD	A,(HL)		; get following character
		POP	HL		; restore pointer to embedded number
		CP	":"
		JR	Z,l1b64
		CP	$0D
		RET	NZ		; exit if following character not : or ENTER
l1b64:		INC	HL		; HL points to next statement/line
		RST	28H
		DW	o33B4		; stack the embedded number
		RST	28H
		DW	o2DA2		; get embedded number to BC
		LD	H,B
		LD	L,C		; HL=embedded line number
		RST	28H
		DW	o196E		; get HL=address of target line
		JR	Z,l1b7c		; move on if the actual line was found
		LD	A,(HL)

		IF garry
		CP	$28
		JR	C,l1b7c		; or if there is a line afterwards (not end)
		ELSE
		CP	$80
		JR	NZ,l1b7c	; or if there is a line afterwards (not end)
		ENDIF

		LD	HL,$270F	; use 9999 and move on
		JR	l1b8d

l1b7c:		LD	(STRIP2+$0D),HL	; save target line address
		CALL	l1c18		; get DE=number of lines before it
		LD	HL,(RC_STEP)
		RST	28H
		DW	o30A9
		LD	DE,(RC_START)
		ADD	HL,DE		; HL=target line's new number
l1b8d:		LD	DE,STRIP2+$09
		PUSH	HL		; save number
		CALL	l1c49		; form ASCII representation of it
		LD	E,B
		INC	E
		LD	D,$00		; DE=length of ASCII string
		PUSH	DE		; save length
		PUSH	HL		; and address of string
		LD	L,E
		LD	H,$00
		LD	BC,(STRIP2+$07)	; get number of characters available
		OR	A
		SBC	HL,BC
		LD	(STRIP2+$07),HL	; save number of extra chars required
		JR	Z,l1bdc		; move on if right size
		JR	C,l1bd2		; move on if more chars available than needed
		LD	B,H
		LD	C,L		; BC=chars to insert
		LD	HL,(STRIP2+$0F)	; HL=address to insert at
		PUSH	HL		; save registers
		PUSH	DE
		LD	HL,(STKEND)
		ADD	HL,BC
		JR	C,l1bcb		; move on to signal error if no room
		EX	DE,HL
		LD	HL,$0082
		ADD	HL,DE
		JR	C,l1bcb		; error if can't leave $82 bytes free
		SBC	HL,SP
		CCF
		JR	C,l1bcb		; or if would encroach on stack
		POP	DE		; restore registers
		POP	HL
		RST	28H
		DW	o1655		; make room
		JR	l1bdc		; move on
l1bcb:		POP	DE
		POP	HL
l1bcd:		CALL	l05cc		; page in DOS workspace
		AND	A		; signal "command failed"
		RET			; exit
l1bd2:		DEC	BC
		DEC	E
		JR	NZ,l1bd2	; BC=number of bytes to reclaim
		LD	HL,(STRIP2+$0F)
		RST	28H
		DW	o19E8		; reclaim room
l1bdc:		LD	DE,(STRIP2+$0F)
		POP	HL
		POP	BC
		LDIR			; copy ASCII text of line number
		EX	DE,HL
		LD	(HL),$0E	; store embedded number marker
		POP	BC		; BC=new line number
		INC	HL
		PUSH	HL		; save address to place FP number
		RST	28H
		DW	o2D2B		; stack BC on FP stack (HL=address)
		POP	DE
		LD	BC,$0005
		LDIR			; copy FP representation
		EX	DE,HL
		PUSH	HL		; save address of next byte to check
		LD	HL,(RC_LINE)
		PUSH	HL		; save address of current line
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; DE=length of current line
		LD	HL,(STRIP2+$07)
		ADD	HL,DE
		EX	DE,HL		; DE=new length of current line
		POP	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store new length
		LD	HL,(STRIP2+$11)
		LD	DE,(STRIP2+$07)
		ADD	HL,DE
		LD	(STRIP2+$11),HL	; store new next line address
		POP	HL		; restore address of next byte to check
		RET

; Subroutine to count the number of lines in a BASIC program,
; either to the end (enter at l1c12), or to a certain address (enter
; at l1c18 with address in STRIP2+$0D).
; Number of lines is returned in DE

l1c12:		LD	HL,(VARS)
		LD	(STRIP2+$0D),HL	; save VARS
l1c18:		LD	HL,(PROG)
		LD	DE,(STRIP2+$0D)
		OR	A
		SBC	HL,DE
		JR	Z,l1c3e		; move on if no BASIC program in memory
		LD	HL,(PROG)	; start at PROG
		LD	BC,$0000	; with 0 lines
l1c2a:		PUSH	BC
		RST	28H
		DW	o19B8		; find DE=address of next line
		LD	HL,(STRIP2+$0D)
		AND	A
		SBC	HL,DE
		JR	Z,l1c3b		; move on if no more lines
		EX	DE,HL		; else swap to HL
		POP	BC
		INC	BC		; increment line count
		JR	l1c2a		; loop back
l1c3b:		POP	DE		; restore number of lines
		INC	DE		; increment for last line
		RET
l1c3e:		LD	DE,$0000	; BASIC program length=0
		RET

; Subroutine to skip spaces

l1c42:		INC	HL
l1c43:		LD	A,(HL)		; get next char
		CP	" "
		JR	Z,l1c42		; skip if space
		RET			; exit with HL pointing to non-space

; Subroutine to form a text representation of a binary number up to 9999
; On entry, DE=address to form number, HL=number
; On exit, HL=start address of text number, DE=end address+1, B=#digits-1

l1c49:		PUSH	DE		; save start address of text number
		LD	BC,-1000
		CALL	l1c6d		; form 1000s digit
		LD	BC,-100
		CALL	l1c6d		; form 100s digit
		LD	C,-10
		CALL	l1c6d		; form 10s digit
		LD	A,L
		ADD	A,"0"
		LD	(DE),A		; form units digit
		INC	DE
		LD	B,$03		; check first 3 digits
		POP	HL		; restore start address of text number
l1c63:		LD	A,(HL)
		CP	"0"
		RET	NZ		; exit if non-zero digit
		LD	(HL)," "	; replace leading 0s with spaces
		INC	HL
		DJNZ	l1c63		; loop back
		RET

; Subroutine to form a decimal digit from a binary number
; On entry, HL=number, DE=address to store digit, BC=-unit size
; On exit, HL is reduced and DE is incremented

l1c6d:		XOR	A		; zero counter
l1c6e:		ADD	HL,BC		; subtract unit size
		INC	A		; and increment digit counter
		JR	C,l1c6e		; loop back for more until failed
		SBC	HL,BC		; add back last unit
		DEC	A		; and decrement counter
		ADD	A,"0"
		LD	(DE),A		; place ASCII digit
		INC	DE		; increment address
		RET

l1c7a:		DB	$08
		DB	0
		DB	0
		DB	$14
		DB	0
		DB	0
		DB	0
		DB	$0F
		DB	0

l1c83:		DB	$08
		DB	0
		DB	$16
		DB	$01
		DB	0
		DB	0
		DB	0
		DB	$0F
		DB	0

; Subroutine to ???

l1c8c:		LD	IX,$FD98
		LD	HL,l1c7a
		JR	l1c98		; (3)

l1c95:		LD	HL,l1c83
l1c98:		LD	DE,$FD98
		JP	l2152


l1c9e:		RST	10H
		LD	A,D
		RST	10H
		SCF
		RET

; Subroutine to set cursor colours to A

l1ca3:		AND	$3F		; mask off FLASH/BRIGHT bits
		LD	(IX+$06),A	; set colours
		SCF
		RET

; Subroutine to place cursor at column C, line B (of editing area)

l1caa:		LD	A,(IX+$01)	; get line for top of editing area
		ADD	A,B
		LD	B,A		; B=line
		CALL	l1dad		; get attribute address
		LD	A,(HL)
		LD	(IX+$07),A	; save attribute
		CPL
		AND	$C0		; get inverse of current FLASH/BRIGHT bits
		OR	(IX+$06)	; combine with cursor colour
		LD	(HL),A		; set new attribute
		SCF
		RET

; Subroutine to remove cursor from column C, line B (of editing area)

l1cbf:		LD	A,(IX+$01)	; get line for top of editing area
		ADD	A,B
		LD	B,A		; B=line
		CALL	l1dad		; get attribute address
		LD	A,(IX+$07)
		LD	(HL),A		; restore attribute
		RET


l1ccc:		PUSH	HL
		LD	H,$00
		LD	A,E
		SUB	B
		JR	l1cda		; (7)

l1cd3:		PUSH	HL
		LD	A,E
		LD	E,B
		LD	B,A
		SUB	E
		LD	H,$FF

l1cda:		LD	C,A
		LD	A,B
		CP	E
		JR	Z,l1d2a		; (75)
		PUSH	DE
		CALL	l1da5

l1ce3:		PUSH	BC
		LD	C,H
		RST	28H
		DW	o0E9B
		DB	$EB
		XOR	A
		OR	C
		JR	Z,l1cf0		; (3)
		INC	B
		JR	l1cf1		; (1)

l1cf0:		DEC	B

l1cf1:		PUSH	DE
		RST	28H
		DW	o0E9B
		DB	$D1
		LD	A,C
		LD	C,$20
		LD	B,$08

l1cfb:		PUSH	BC
		PUSH	HL
		PUSH	DE
		LD	B,$00
		LDIR
		POP	DE
		POP	HL
		POP	BC
		INC	H
		INC	D
		DJNZ	l1cfb		; (-14)
		PUSH	AF
		PUSH	DE
		RST	28H
		DW	o0E88
		DB	$EB
		EX	(SP),HL
		RST	28H
		DW	o0E88
		DB	$EB
		EX	(SP),HL
		POP	DE
		LD	BC,$0020
		LDIR
		POP	AF
		POP	BC
		AND	A
		JR	Z,l1d23		; (3)
		INC	B
		JR	l1d24		; (1)

l1d23:		DEC	B

l1d24:		DEC	C
		LD	H,A
		JR	NZ,l1ce3	; (-69)
		POP	DE
		LD	B,E

l1d2a:		POP	HL

l1d2b:		CALL	l1dc5		; swap editor/BASIC colours & P_FLAG
		EX	DE,HL
		LD	A,(TV_FLAG)
		PUSH	AF
		LD	HL,ed_flags
		BIT	6,(HL)
		res	0,A
		JR	Z,l1d3e		; (2)
		SET	0,A

l1d3e:		LD	(TV_FLAG),A
		LD	C,$00
		CALL	l191d
		EX	DE,HL
		LD	B,$20

l1d49:		LD	A,(HL)
		AND	A
		JR	NZ,l1d4f	; (2)
		LD	A,$20

l1d4f:		CP	$90
		JR	NC,l1d62	; (15)
		RST	28H
		DW	$0010

l1d56:		INC	HL
		DJNZ	l1d49		; (-16)
		POP	AF
		LD	(TV_FLAG),A
		CALL	l1dc5		; swap editor/BASIC colours & P_FLAG
		SCF
		RET

l1d62:		CALL	l05a7
		RST	10H
		CALL	l05cc
		JR	l1d56		; (-21)

; Subroutine to clear an area of screen to the editor's colours
; On entry, B=first line number and D=last line number (0...23)

l1d6b:		CALL	l1dc5		; swap editor/BASIC colours & P_FLAG
		LD	A,D
		SUB	B
		INC	A
		LD	C,A		; C=number of lines to clear
		CALL	l1da5		; convert line number as required by ROM 3
l1d75:		PUSH	BC
		RST	28H
		DW	o0E9B		; get HL=address of line B in display file
		LD	C,$08		; 8 pixel lines per character
l1d7b:		PUSH	HL
		LD	B,$20		; 32 characters per line
		XOR	A
l1d7f:		LD	(HL),A		; clear a pixel line of a character
		INC	HL
		DJNZ	l1d7f		; back for rest of line
		POP	HL
		INC	H
		DEC	C
		JR	NZ,l1d7b	; back for rest of pixel lines
		LD	B,$20
		PUSH	BC
		RST	28H
		DW	o0E88		; get attribute address
		EX	DE,HL
		POP	BC
		LD	A,(ATTR_P)
l1d93:		LD	(HL),A		; clear attributes to editor's ATTR_P
		INC	HL
		DJNZ	l1d93		; for rest of line
		POP	BC
		DEC	B		; next line
		DEC	C		; decrement counter
		JR	NZ,l1d75	; back for more
		CALL	l1dc5		; swap editor/BASIC colours & P_FLAG
		SCF
		RET

; Subroutine to convert line numbers and column numbers as required
; by certain ROM 3 routines

l1da1:		LD	A,$21
		SUB	C
		LD	C,A		; C=33-oldC
l1da5:		LD	A,$18
		SUB	B
		SUB	(IX+$01)
		LD	B,A		; B=24-oldB-??
		RET

; Subroutine to get attribute address for line B, column C into HL

l1dad:		PUSH	BC
		XOR	A
		LD	D,B
		LD	E,A
		RR	D
		RR	E
		RR	D
		RR	E
		RR	D
		RR	E		; DE=B*32
		LD	HL,$5800	; start of attribs
		LD	B,A
		ADD	HL,BC
		ADD	HL,DE		; form address
		POP	BC
		RET

; Subroutine to swap some system variables with copies in page 7, allowing
; BASIC & the editor to use different values for colours etc

l1dc5:		PUSH	AF		; save registers
		PUSH	HL
		PUSH	DE
		LD	HL,(ATTR_P)	; swap permanent & temporary colours with
		LD	DE,(ATTR_T)	; editor ones
		EXX
		LD	HL,(ed_ATTR_P)
		LD	DE,(ed_ATTR_T)
		LD	(ATTR_P),HL
		LD	(ATTR_T),DE
		EXX
		LD	(ed_ATTR_P),HL
		LD	(ed_ATTR_T),DE
		LD	HL,ed_P_FLAG	; swap P_FLAG with editor one
		LD	A,(P_FLAG)
		LD	D,(HL)
		LD	(HL),A
		LD	A,D
		LD	(P_FLAG),A
		POP	DE		; restore registers
		POP	HL
		POP	AF
		RET

l1df6:		LD	A,$01
		JR	l1dfc		; (2)

l1dfa:		LD	A,$00

l1dfc:		LD	($FDB6),A
		LD	HL,$0000
		LD	($FDB1),HL
		LD	($FDB3),HL
		ADD	HL,SP
		LD	($FDB7),HL
		CALL	l15e4
		LD	A,$00
		LD	($FDB0),A
		LD	HL,$FDA0
		LD	($FDA9),HL
		CALL	l05a7
		RST	28H
		DW	o16B0
		CALL	l05cc
		LD	A,$00
		LD	($FDAD),A
		LD	HL,(E_LINE)
		LD	($FDAE),HL
		LD	HL,$0000
		LD	($FDAB),HL

l1e34:		LD	HL,($FDB1)
		INC	HL
		LD	($FDB1),HL
		CALL	l1f30
		LD	C,A
		LD	A,($FDAD)
		CP	$00
		JR	NZ,l1e87	; (65)

l1e46:		LD	A,C
		AND	$04
		JR	Z,l1e80		; (53)

l1e4b:		CALL	l1f7c
		JR	NC,l1e57	; (7)
		LD	A,$01
		LD	($FDAD),A
		JR	l1e34		; (-35)

l1e57:		LD	HL,($FDAB)
		LD	A,L
		OR	H
		JP	NZ,l1eb1

l1e5f:		PUSH	BC
		CALL	l1f60
		POP	BC
		LD	A,$00
		LD	($FDAD),A

l1e69:		LD	A,C
		AND	$01
		JR	NZ,l1e46	; (-40)
		LD	A,B
		CALL	l1fa9
		RET	NC
		LD	HL,($FDB1)
		INC	HL
		LD	($FDB1),HL
		CALL	l1f30
		LD	C,A
		JR	l1e69		; (-23)

l1e80:		LD	A,B
		CALL	l1fa9
		RET	NC
		JR	l1e34		; (-83)

l1e87:		CP	$01
		JR	NZ,l1e80	; (-11)
		LD	A,C
		AND	$01
		JR	Z,l1e4b		; (-69)
		PUSH	BC

l1e91:		CALL	l2113
		POP	BC
		JR	C,l1f10		; (121)
		LD	HL,($FDAB)
		LD	A,H
		OR	L
		JR	NZ,l1eb1	; (19)
		LD	A,C
		AND	$02
		JR	Z,l1e5f		; (-68)
		CALL	l1f7c
		JR	NC,l1e57	; (-81)
		LD	HL,($FDA9)
		DEC	HL
		LD	($FDAB),HL
		JR	l1e34		; (-125)

l1eb1:		PUSH	BC
		LD	HL,$FDA0
		LD	DE,($FDAB)
		LD	A,D
		CP	H
		JR	NZ,l1ec2	; (5)
		LD	A,E
		CP	L
		JR	NZ,l1ec2	; (1)
		INC	DE

l1ec2:		DEC	DE
		JR	l1ec6		; (1)

l1ec5:		INC	HL

l1ec6:		LD	A,(HL)
		AND	$7F
		PUSH	HL
		PUSH	DE
		CALL	l1fa9
		POP	DE
		POP	HL
		LD	A,H
		CP	D
		JR	NZ,l1ec5	; (-15)
		LD	A,L
		CP	E
		JR	NZ,l1ec5	; (-19)
		LD	DE,($FDAB)
		LD	HL,$FDA0
		LD	($FDAB),HL
		LD	BC,($FDA9)
		DEC	BC
		LD	A,D
		CP	H
		JR	NZ,l1f03	; (24)
		LD	A,E
		CP	L
		JR	NZ,l1f03	; (20)
		INC	DE
		PUSH	HL
		LD	HL,$0000
		LD	($FDAB),HL
		POP	HL
		LD	A,B
		CP	H
		JR	NZ,l1f03	; (7)
		LD	A,C
		CP	L
		JR	NZ,l1f03	; (3)
		POP	BC
		JR	l1f22		; (31)

l1f03:		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		AND	$80
		JR	Z,l1f03		; (-8)
		LD	($FDA9),HL
		JR	l1e91		; (-127)

l1f10:		PUSH	BC
		CALL	l1fa9
		POP	BC
		LD	HL,$0000
		LD	($FDAB),HL
		LD	A,($FDAD)
		CP	$04
		JR	Z,l1f27		; (5)

l1f22:		LD	A,$00
		LD	($FDAD),A

l1f27:		LD	HL,$FDA0
		LD	($FDA9),HL
		JP	l1e46

l1f30:		CALL	l0e22
		LD	B,A
		CP	$3F
		JR	C,l1f42		; (10)
		OR	$20
		CALL	l1f59
		JR	C,l1f56		; (23)

l1f3f:		LD	A,$01
		RET

l1f42:		CP	$20
		JR	Z,l1f53		; (13)
		CP	$23
		JR	Z,l1f50		; (6)
		JR	C,l1f3f		; (-13)
		CP	$24
		JR	NZ,l1f3f	; (-17)

l1f50:		LD	A,$02
		RET

l1f53:		LD	A,$03
		RET

l1f56:		LD	A,$06
		RET

l1f59:		CP	$7B
		RET	NC
		CP	$61
		CCF
		RET

l1f60:		LD	HL,$FDA0
		LD	($FDA9),HL
		SUB	A
		LD	($FDAB),A
		LD	($FDAC),A

l1f6d:		LD	A,(HL)
		AND	$7F
		PUSH	HL

		IF garry
		CALL	l1fa9
		ELSE
		CALL	l202f
		ENDIF

		POP	HL
		LD	A,(HL)
		AND	$80
		RET	NZ
		INC	HL
		JR	l1f6d		; (-15)

l1f7c:		LD	HL,($FDA9)
		LD	DE,$FDA9
		LD	A,D
		CP	H
		JR	NZ,l1f8b	; (5)
		LD	A,E
		CP	L
		JP	Z,l1fa6

l1f8b:		LD	DE,$FDA0
		LD	A,D
		CP	H
		JR	NZ,l1f96	; (4)
		LD	A,E
		CP	L
		JR	Z,l1f9c		; (6)

l1f96:		DEC	HL
		LD	A,(HL)
		AND	$7F
		LD	(HL),A
		INC	HL

l1f9c:		LD	A,B
		OR	$80
		LD	(HL),A
		INC	HL
		LD	($FDA9),HL
		SCF
		RET

l1fa6:		SCF
		CCF
		RET

l1fa9:		PUSH	AF
		LD	A,($FDB5)
		OR	A
		JR	NZ,l1fc2	; (18)
		POP	AF
		CP	$3E
		JR	Z,l1fbd		; (8)
		CP	$3C
		JR	Z,l1fbd		; (4)

l1fb9:		CALL	l1ff7
		RET

l1fbd:		LD	($FDB5),A
		SCF
		RET

l1fc2:		CP	$3C
		LD	A,$00
		LD	($FDB5),A
		JR	NZ,l1fe5	; (26)
		POP	AF
		CP	$3E
		JR	NZ,l1fd4	; (4)
		LD	A,$C9
		JR	l1fb9		; (-27)

l1fd4:		CP	$3D
		JR	NZ,l1fdc	; (4)
		LD	A,$C7
		JR	l1fb9		; (-35)

l1fdc:		PUSH	AF
		LD	A,$3C
		CALL	l1ff7
		POP	AF
		JR	l1fb9		; (-44)

l1fe5:		POP	AF
		CP	$3D
		JR	NZ,l1fee	; (4)
		LD	A,$C8
		JR	l1fb9		; (-53)

l1fee:		PUSH	AF
		LD	A,$3E
		CALL	l1ff7
		POP	AF
		JR	l1fb9		; (-62)

l1ff7:		CP	$0D
		JR	Z,l201b		; (32)
		CP	$EA
		LD	B,A
		JR	NZ,l2007	; (7)
		LD	A,$04
		LD	($FDAD),A
		JR	l2015		; (14)

l2007:		CP	$22
		JR	NZ,l2015	; (10)
		LD	A,($FDAD)
		AND	$FE
		XOR	$02
		LD	($FDAD),A

l2015:		LD	A,B
		CALL	l202f
		SCF
		RET

l201b:		LD	A,($FDB6)
		CP	$00
		JR	Z,l202c		; (10)
		LD	BC,($FDB1)
		LD	HL,($FDB7)
		LD	SP,HL
		SCF
		RET

l202c:		SCF
		CCF
		RET

l202f:		LD	E,A
		LD	A,($FDB0)
		LD	D,A
		LD	A,E
		CP	$20
		JR	NZ,l2059	; (32)
		LD	A,D
		AND	$01
		JR	NZ,l2052	; (20)
		LD	A,D
		AND	$02
		JR	NZ,l204a	; (7)
		LD	A,D
		OR	$02
		LD	($FDB0),A
		RET

l204a:		LD	A,E
		CALL	l208e
		LD	A,($FDB0)
		RET

l2052:		LD	A,D
		AND	$FE
		LD	($FDB0),A
		RET

l2059:		CP	$A3
		JR	NC,l2081	; (36)
		LD	A,D
		AND	$02
		JR	NZ,l206d	; (11)
		LD	A,D
		AND	$FE
		LD	($FDB0),A
		LD	A,E
		CALL	l208e
		RET

l206d:		PUSH	DE
		LD	A,$20
		CALL	l208e
		POP	DE
		LD	A,D
		AND	$FE
		AND	$FD
		LD	($FDB0),A
		LD	A,E
		CALL	l208e
		RET

l2081:		LD	A,D
		AND	$FD
		OR	$01
		LD	($FDB0),A
		LD	A,E
		CALL	l208e
		RET

l208e:		LD	HL,($FDB3)
		INC	HL
		LD	($FDB3),HL
		LD	HL,($FDAE)
		LD	B,A
		LD	A,($FDB6)
		CP	$00
		LD	A,B
		JR	Z,l20c6		; (37)
		LD	DE,(X_PTR)
		LD	A,H
		CP	D
		JR	NZ,l20c3	; (26)
		LD	A,L
		CP	E
		JR	NZ,l20c3	; (22)
		LD	BC,($FDB1)
		LD	HL,($FDB3)
		AND	A

		IF garry
		DB	0,0
		ELSE
		SBC	HL,BC
		ENDIF

		JR	NC,l20bd	; (4)
		LD	BC,($FDB3)

l20bd:		LD	HL,($FDB7)
		LD	SP,HL
		SCF
		RET

l20c3:		SCF
		JR	l20c8		; (2)

l20c6:		SCF
		CCF

l20c8:		CALL	l05a7
		JR	NC,l20da	; (13)
		LD	A,(HL)
		EX	DE,HL
		CP	$0E
		JR	NZ,l20f0	; (29)
		INC	DE
		INC	DE
		INC	DE
		INC	DE
		INC	DE
		JR	l20f0		; (22)

l20da:		PUSH	AF
		LD	BC,$0001
		PUSH	HL
		PUSH	DE
		CALL	l20f9
		POP	DE
		POP	HL
		RST	28H
		DW	o1664
		DB	$2A
		LD	H,L
		LD	E,H
		EX	DE,HL
		LDDR
		POP	AF
		LD	(DE),A

l20f0:		INC	DE
		CALL	l05cc
		LD	($FDAE),DE
		RET

l20f9:		LD	HL,(STKEND)
		ADD	HL,BC
		JR	C,l2109		; (10)
		EX	DE,HL
		LD	HL,$0082
		ADD	HL,DE
		JR	C,l2109		; (3)
		SBC	HL,SP
		RET	C

l2109:		LD	A,$03
		LD	(ERR_NR),A
		CALL	l3e80
		DW	m25cb
l2113:		CALL	l142d
		CALL	$FD43
		RET	C
		LD	B,$F9
		LD	DE,$FDA0
		LD	HL,l168e
		CALL	$FD5C
		RET	NC
		CP	$FF
		JR	NZ,l212e	; (4)
		LD	A,$D4
		JR	l2150		; (34)

l212e:		CP	$FE
		JR	NZ,l2136	; (4)
		LD	A,$D3
		JR	l2150		; (26)

l2136:		CP	$FD
		JR	NZ,l213e	; (4)
		LD	A,$CE
		JR	l2150		; (18)

l213e:		CP	$FC
		JR	NZ,l2146	; (4)
		LD	A,$ED
		JR	l2150		; (10)

l2146:		CP	$FB
		JR	NZ,l214e	; (4)
		LD	A,$EC
		JR	l2150		; (2)

l214e:		SUB	$56

l2150:		SCF
		RET

; Subroutine to transfer a counted string (minus count) from (HL) to (DE)

l2152:		LD	B,(HL)		; get count
		INC	HL
l2154:		LD	A,(HL)
		LD	(DE),A		; transfer a byte
		INC	DE
		INC	HL
		DJNZ	l2154		; back for more
		RET

; Subroutine to check if char in A is a digit. If so, carry set & A=value

l215b:		CP	"0"
		CCF
		RET	NC		; exit if less than "0"
		CP	":"
		RET	NC		; or if > "9"
		SUB	"0"		; convert to value
		SCF			; success
		RET

; Subroutine to perform a routine found with a table lookup
; The value to look for is in A, and the table address is in HL
; The table consists of:
;   a number of entries byte
;   for each entry: a value byte followed by a routine address
; When a match is found, the routine is called. A is tested for
; zero before returning, carry status is preserved and should be
; set by the routine if action succeeded.
; If no match is found, carry is reset.

l2166:		PUSH	BC		; save registers
		PUSH	DE
		LD	B,(HL)		; B=# entries in table
		INC	HL
l216a:		CP	(HL)		; check next entry
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; DE=address associated with current entry
		JR	Z,l2179		; if match, move on
		INC	HL
		DJNZ	l216a		; loop back for more
		SCF
		CCF			; clear carry to signal "no match"
		POP	DE		; restore registers
		POP	BC
		RET
l2179:		EX	DE,HL		; HL=address
		POP	DE		; restore registers
		POP	BC
		CALL	l2186		; call routine address in HL
		JR	C,l2183		; if carry set, go to test A & set carry
		CP	A		; test A (& clear carry)
		RET
l2183:		CP	A		; test A
		SCF			; set carry
		RET
l2186:		JP	(HL)

		IF garry
l2187:		PUSH	AF
		PUSH	HL
		LD	A,(ATTR_P)
		PUSH	AF
		LD	A,(BORDCR)
		LD	(ATTR_P),A
		RST	28H
		DW	o0D6E
		LD	A,$FD
		RST	28H
		DW	o1601
		POP	AF
		LD	(ATTR_P),A
		POP	HL
l21a0:		LD	A,(HL)
		INC	HL
		CP	$FF
		JR	Z,l21a9
		RST	10H
		JR	l21a0
l21a9:		POP	AF
		JR	Z,l21bb
		CALL	l1871
l21af:		PUSH	DE
		RST	28H
		DW	o0D6E
		LD	A,$FE
		RST	28H
		DW	o1601
		POP	DE
		LD	A,E
		RET
l21bb:		CALL	l1871
		AND	$DF
		LD	E,0
		CP	$43
		JR	Z,l21af
		INC	E
		CP	$52
		JR	Z,l21af
		INC	E
		CP	$49
		JR	Z,l21af
		JR	l21bb
		ELSE
		IF spanish
l2187:		PUSH	HL
		PUSH	DE
		PUSH	AF
		LD	A,(ATTR_P)
		PUSH	AF
		LD	A,(BORDCR)
		LD	(ATTR_P),A
		RST	28H
		DW	o0D6E
		LD	A,$FD
		RST	28H
		DW	o1601
		POP	AF
		LD	(ATTR_P),A
		POP	AF
		POP	DE
		POP	BC
		JR	Z,l21eb
		LD	HL,msg1
		PUSH	AF
		CALL	x21df
		POP	AF
		RST	10H
		LD	HL,msg2
		CALL	x21df
		CALL	l1871
x21d3:		PUSH	DE
		RST	28H
		DW	o0D6E
		LD	A,$FE
		RST	28H
		DW	o1601
		POP	DE
		LD	A,E
		RET
x21df:		LD	A,(HL)
		BIT	7,A
		PUSH	AF
		AND	$7F
		RST	10H
		POP	AF
		RET	NZ
		INC	HL
		JR	x21df
l21eb:		LD	HL,msg3
		CALL	x21df
		LD	A,C
		RST	10H
		LD	A,":"
		RST	10H
		LD	C,B
		LD	B,$00
		PUSH	BC
		POP	HL
		ADD	HL,HL
		LD	BC,msg4
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		LD	A,(HL)
		CP	$03
		JR	NC,l2213
		CALL	x224f
		CP	$02
		JR	NZ,l2213
		CALL	l2261
l2213:		LD	A,(ATTR_T)
		PUSH	AF
		AND	$38
		CP	$20
		JR	NC,l2221
		LD	E,$05
		JR	l2223
l2221:		LD	E,$02
l2223:		POP	AF
		PUSH	AF
		AND	$F8
		OR	E
		LD	(ATTR_T),A
		CALL	x21df
		POP	AF
		LD	(ATTR_T),A
		LD	HL,msg5
		CALL	x21df
l2238:		CALL	l1871
		AND	$DF
		LD	E,$00
		CP	"C"
		JR	Z,x21d3
		INC	E
		CP	"R"
		JR	Z,x21d3
		INC	E
		CP	"I"
		JR	Z,x21d3
		JR	l2238
x224f:		PUSH	AF
		PUSH	DE
		PUSH	HL
		LD	HL,msg6
		CALL	x21df
		LD	A,D
		CALL	l2275
		POP	HL
		POP	DE
		POP	AF
		INC	HL
		RET
l2261:		PUSH	AF
		PUSH	DE
		PUSH	HL
		LD	HL,msg7
		CALL	x21df
		LD	A,E
		CALL	l2275
		LD	A,$0D
		RST	10H
		POP	HL
		POP	DE
		POP	AF
		RET
l2275:		LD	E,$00
l2277:		SUB	$64
		JR	C,l227e
		INC	E
		JR	l2277
l227e:		ADD	A,$64
		CALL	l2295
		LD	E,$00
l2285:		SUB	$0A
		JR	C,l228c
		INC	E
		JR	l2285
l228c:		ADD	A,$0A
		CALL	l2295
		ADD	A,$30
		RST	10H
		RET
l2295:		PUSH	AF
		LD	A,E
		ADD	A,$30
		RST	10H
		POP	AF
		RET
msg4:		DW	msg8
		DW	msg9
		DW	msg10
		DW	msg11
		DW	msg12
		DW	msg13
		DW	msg14
		DW	msg15
		DW	msg16
		DW	msg17
msg3:		DB	$16,$00,$00
		DB	"UNIDAD", " "+$80
msg6:		DB	" PISTA", " "+$80
msg7:		DB	" SECTOR", " "+$80
msg5:		DB	$5D, "REINTENTAR, IGNORAR O CANCELAR", "?"+$80
msg8:		DB	$0D, "NO PREPARADA", $0D+$80
msg9:		DB	$0D, "DISCO PROTEGIDO CONTRA ESCRITUR", "A"+$80
msg10:		DB	$01,$0D, "FALLO DE BUSQUEDA", $0D+$80
msg11:		DB	$02, "ERROR DE DATOS", $0D+$80
msg12:		DB	$02, "SIN DATOS", $0D+$80
msg13:		DB	$02, "FALTA MARCA DE DIRECCIONES", $0D+$80
msg14:		DB	$0D, "FORMATO INCORRECTO", $0D+$80
msg15:		DB	$02, "ERROR DESCONOCIDO", $0D+$80
msg16:		DB	$0D, "DISCO CAMBIADO	; SUSTITUYALO", $0D+$80
msg17:		DB	$0D, "DISCO NO ADECUADO", $0D+$80
msg1:		DB	"Introduzca en la unidad el discopara", " "+$80
msg2:		DB	" y luego pulse una tecl", "a"+$80
		ELSE

; Routine called from ROM 2 to display an error message in HL
; On entry Z is set if DE contains a response key list to use

l2187:		JR	Z,l218c
		LD	DE,$0000	; no response list
l218c:		PUSH	DE
		PUSH	HL
		LD	A,$FD
		RST	28H
		DW	o1601		; open channel to stream -3
		POP	HL
		PUSH	HL
l2195:		LD	B,$20		; 32 chars per line
l2197:		LD	A,(HL)		; get next char
		CP	" "
		JR	NZ,l219e
		LD	D,H		; if space, set DE to current position
		LD	E,L
l219e:		CP	$FF
		JR	Z,l21ab		; move on if end of message
		INC	HL
		DJNZ	l2197		; loop back for more
		EX	DE,HL
		LD	A,$0D
		LD	(HL),A		; insert a CR at last space
		JR	l2195		; back for more lines
l21ab:		LD	A,$16		; start at 0,0 in stream -3
		RST	10H
		LD	A,$00
		RST	10H
		LD	A,$00
		RST	10H
		POP	HL		; restore message start
l21b5:		LD	A,(HL)		; get next char
		CP	$FF
		JR	Z,l21be		; move on if end
		RST	10H		; output char
		INC	HL
		JR	l21b5		; back for more
l21be:		CALL	l1871		; get a key
		LD	B,A		; B=key
		POP	HL		; restore response key list
		LD	A,H
		OR	L
		PUSH	HL
		JR	Z,l21d3		; move on if none required
l21c8:		LD	A,(HL)
		CP	B
		JR	Z,l21d3		; move on if response key found
		INC	HL
		CP	$FF
		JR	NZ,l21c8	; loop back if more possibilities to check
		JR	l21be		; else get another key
l21d3:		PUSH	AF
		RST	28H
		DW	o0D6E		; clear lower screen
		LD	A,$FE
		RST	28H
		DW	o1601		; open channel to stream -2
		POP	AF
		POP	HL
		RET
		ENDIF
		ENDIF

		IF garry
		DW	$2562
		DW	$23F1
		DW	$24BD
		DW	$24E8
		DW	$254B
		DW	$2563
		DW	$2692
		DW	o2A49
		DW	$2569
		DW	$2584
		DW	o25BE
		DW	o25DB
		DW	$25EC
		DW	$2A16
		DW	$269F
		DW	$2739
		DW	$273F
		DW	o274C
		DW	$275C
		DW	$2767
		DW	$2772
		DW	$277D
		DW	$261C
		DW	$267C
		DW	$2AB9
		DW	o2788
		DW	$26A3
		DW	$2793
		DW	$27AC
		DW	$27B7
		DW	$2286
		DW	$2296
		DW	$2624
		DW	$2449
		DW	$2562
		DW	$24BD
		DW	$24E8
		DW	$254B
		DW	$2563
		DW	$2562
		DW	o2A49
		DW	$2562
		DW	$2562
		DW	o25BE
		DW	o25DB
		DW	$2562
		DW	$2423
		DW	$269F
		DW	$2739
		DW	$22DE
		DW	$22DE
		DW	$22DE
		DW	$22DE
		DW	$22DE
		DW	$22DE
		DW	$2562
		DW	$2562
		DW	$22DE
		DW	$2562
		DW	$22DE
		DW	$22DE
		DW	$2562
		DW	$22DE
		DW	$2254
		DW	$2562
l2254:		SET	7, (IX+$1A)	; set freeze flag
		RET
l2259:		LD	HL,(CURCHL)
		LD	DE,77
		ADD	HL,DE
		RET
l2261:		CALL	l2259
		LD	BC,$00FF
		BIT	7, (IX+$1C)
		RET	NZ
l226c:		LD	A,(IX+$18)
		BIT	4, (IX+$1A)
		JR	Z,l2277
		SRL	A
l2277:		LD	C,A
		INC	C
		RET
l227a:		PUSH	AF
		PUSH	BC
		PUSH	DE
		PUSH	HL
		CALL	l27d0
		POP	HL
		POP	DE
		POP	BC
		POP	AF
		RET
l2286:		AND	$03
		CP	$03
		RET	Z
		LD	B,A
		LD	A,(IX+$1C)
		AND	$FC
		OR	B
		LD	(IX+$1C),A
		RET
l2296:		res	7, (IX+$1C)
		RRA
		RET	NC
		SET	7, (IX+$1C)
		RET
l22a1:		LD	IX,(CURCHL)
		LD	(IX+$23),A
		BIT	2, (IX+$1A)
		JR	NZ,l22c0
		CP	$A5
		JR	C,l22c0
		SUB	$A5
l22b4:		LD	HL,(RETADDR)
		PUSH	HL
		RST	28H
		DW	o0C10
		POP	HL
		LD	(RETADDR),HL
		RET
l22c0:		BIT	6, (IX+$1A)
		res	6, (IX+$1A)
		JR	NZ,l22f5
		BIT	7, (IX+$1A)
		res	7, (IX+$1A)
		JR	NZ,l2286
		CP	$20
		JR	NC,l22e9
		LD	HL,$2214
		JP	l27e4
		BIT	7, (IX+$1C)
		RET	Z
		SET	6, (IX+$1A)
		JR	l22f5
l22e9:		CP	$20
		JR	NZ,l22f5
		CALL	l2468
		LD	A,(IX+$1E)
		AND	A
		RET	Z
l22f5:		CALL	l2259
		LD	E,(IX+$1D)
		ADD	HL,DE
		LD	A,(IX+$23)
		LD	(HL),A
		INC	(IX+$1D)
		CALL	l2468
		LD	A,(IX+$1D)
		CP	$FF
		JR	Z,l2314
		CALL	l226c
		CP	(IX+$1E)
		RET	NC
l2314:		CALL	l2468
l2317:		CALL	l226c
		CP	(IX+$1E)
		JR	NC,l2359
		CP	(IX+$20)
		JR	C,l2338
		LD	A,(IX+$20)
		CP	$01
		LD	A,(IX+$1F)
		JR	Z,l2331
		AND	A
		JR	Z,l2338
l2331:		LD	C,A
		INC	C
		LD	B,(IX+$20)
		JR	l235f
l2338:		CALL	l2259
		LD	BC,0
l233e:		LD	A,(HL)
		CALL	l227a
		INC	HL
		INC	C
		LD	A,(IX+$17)
		BIT	4, (IX+$1A)
		JR	Z,l2354
		INC	A
		CP	(IX+$18)
		IF garry
		JR	Z,l23d2
		ELSE
		JR	l23d2
		ENDIF
		DEC	A
l2354:		AND	A
		JR	NZ,l233e
		JR	l23d2
l2359:		LD	C,(IX+$1D)
		LD	B,(IX+$1E)
l235f:		LD	A,C
		AND	A
		RET	Z
		CALL	l2259
		LD	A,(IX+$1C)
		AND	$03
		JR	Z,l239a
		PUSH	BC
		CALL	l226c
		POP	BC
		SUB	B
		LD	E,A
		LD	A,(IX+$1C)
		AND	$02
		JR	NZ,l238a
		LD	A,E
		SRL	A
l237d:		AND	A
		JR	Z,l239a
		PUSH	AF
		LD	A,$20
		CALL	l227a
		POP	AF
		DEC	A
		JR	l237d
l238a:		PUSH	BC
		PUSH	HL
		LD	D,0
l238e:		LD	A,(HL)
		INC	HL
		CP	$20
		JR	NZ,l2395
		INC	D
l2395:		DEC	C
		JR	NZ,l238e
l2398:		POP	HL
		POP	BC
l239a:		LD	B,0
		PUSH	BC
		PUSH	HL
l239e:		LD	A,(HL)
		CALL	l227a
		CP	$20
		JR	NZ,l23c5
		LD	A,(IX+$1C)
		AND	$02
		JR	Z,l23c5
		LD	A,E
		LD	B,0
l23b0:		SUB	D
		JR	C,l23b6
		INC	B
		JR	l23b0
l23b6:		DEC	D
		LD	A,B
		AND	A
		JR	Z,l23c5
		LD	A,E
		SUB	B
		LD	E,A
l23be:		LD	A,$20
		CALL	l227a
		DJNZ	l23be
l23c5:		INC	HL
		DEC	C
		JR	NZ,l239e
		LD	A,(IX+$17)
		AND	A
		CALL	NZ,l2a16
		POP	HL
		POP	BC
l23d2:		PUSH	BC
		CALL	l2261
		EX	DE,HL
		POP	HL
		PUSH	HL
		ADD	HL,DE
		LDIR
		POP	BC
		LD	A,(IX+$1D)
		SUB	C
		LD	(IX+$1D),A
		JR	Z,l2415
		CALL	l2259
		LD	C,1
		LD	A,(HL)
		CP	$20
		JR	Z,l23d2
		RET
l23f1:		CALL	l2261
		PUSH	BC
		CALL	l3fb0
		POP	BC
		LD	L,(IX+$0B)
		LD	H,(IX+$0C)
		ADD	HL,BC
		LD	(IX+$0B),L
		LD	(IX+$0C),H
		res	6, (IX+$1A)
		res	7, (IX+$1A)
		LD	A,(IX+$17)
		AND	A
		CALL	NZ,l2a16
l2415:		LD	(IX+$1D),0
		LD	HL,l22a1
l241c:		LD	(IX+$05),L
		LD	(IX+$06),H
		RET
l2423:		CALL	l2468
		LD	A,(IX+$1D)
		PUSH	AF
l242a:		LD	A,(IX+$1C)
		PUSH	AF
		res	1, (IX+$1C)
		CALL	l2317
		POP	AF
		LD	(IX+$1C),A
		LD	A,(IX+$1D)
		AND	A
		JR	NZ,l242a
		POP	AF
		AND	A
		RET	NZ
		CALL	l25be
		RET	C
		JP	l2a49
l2449:		CALL	l2423
		CALL	l2261
		PUSH	BC
		CALL	l3f78
		POP	BC
		LD	L,(IX+$0B)
		LD	H,(IX+$0C)
		AND	A
		SBC	HL,BC
		LD	(IX+$0B),L
		LD	(IX+$0C),H
		LD	HL,l27d0
		JR	l241c
l2468:		CALL	l2259
		LD	D,0
		LD	E,D
		LD	(IX+$1F),D
		LD	C,D
		LD	B,(IX+$1D)
		LD	A,B
		AND	A
		JR	Z,l24af
l2479:		LD	A,(HL)
		CP	$20
		JR	C,l24b6
		JR	NZ,l2486
		DEC	D
		LD	(IX+$1F),D
		INC	D
		LD	C,E
l2486:		INC	E
		CP	$21
		JR	Z,l249f
		CP	$2E
		JR	Z,l249f
		CP	$2C
		JR	Z,l249f
		CP	$3F
		JR	Z,l249f
		CP	$3A
		JR	Z,l249f
		CP	$3B
		JR	NZ,l24ab
l249f:		PUSH	BC
		CALL	l226c
		POP	BC
		CP	E
		JR	C,l24ab
		LD	(IX+$1F),D
		LD	C,E
l24ab:		INC	D
		INC	HL
		DJNZ	l2479
l24af:		LD	(IX+$1E),E
		LD	(IX+$20),C
		RET
l24b6:		INC	D
		INC	HL
		DEC	B
		JR	Z,l24af
		JR	l24ab
l24bd:		BIT	3, (IX+$1A)
		JR	NZ,l24e2
		CALL	l252a
		LD	HL,(CURCHL)
		LD	E,(IX+$0B)
		LD	D,(IX+$0C)
		ADD	HL,DE
		PUSH	BC
		PUSH	DE
		CALL	l3fb0
		POP	HL
		POP	BC
		ADD	HL,BC
		LD	(IX+$0B),L
		LD	(IX+$0C),H
		SET	3, (IX+$1A)
l24e2:		LD	(IX+$24),2
		JR	l24f1
l24e8:		BIT	3, (IX+$1A)
		RET	Z
		LD	(IX+$24),1
l24f1:		EXX
		PUSH	HL
		PUSH	DE
		LD	HL,(CURCHL)
		LD	E,(IX+$0B)
		LD	D,(IX+$0C)
		ADD	HL,DE
		EXX
		CALL	l26aa
		EXX
		POP	DE
		EX	(SP),HL
		EXX
		POP	HL
		LD	A,(IX+$24)
		DEC	A
		RET	NZ
		PUSH	HL
		CALL	l252a
		POP	HL
		PUSH	BC
		CALL	l3f78
		LD	L,(IX+$0B)
		LD	H,(IX+$0C)
		POP	BC
		AND	A
		SBC	HL,BC
		LD	(IX+$0B),L
		LD	(IX+$0C),H
		res	3, (IX+$1A)
		RET
l252a:		LD	A,(IX+$0F)
		SUB	(IX+$0D)
		INC	A
		LD	E,A
		LD	D,0
		LD	A,(IX+$10)
		SUB	(IX+$0E)
		INC	A
		LD	HL,0
l253e:		ADD	HL,DE
		DEC	A
		JR	NZ,l253e
		LD	E,L
		LD	D,H
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,DE
		PUSH	HL
		POP	BC
		RET
l254b:		LD	A,(IX+$0E)
l254e:		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	(IX+$12),A
		LD	A,(IX+$0D)
		LD	(IX+$11),A
		LD	(IX+$16),0
		LD	(IX+$17),0
		RET
l2563:		LD	A,(IX+$10)
		INC	A
		JR	l254e
l2569:		LD	A,(IX+$17)
		AND	A
		RET	Z
		DEC	(IX+$17)
		LD	A,(IX+$16)
		SUB	(IX+$13)
		LD	(IX+$16),A
		RET	NC
		ADD	A,$08
		LD	(IX+$16),A
		DEC	(IX+$11)
		RET
l2584:		LD	A,(IX+$17)
		INC	A
		CP	(IX+$18)
		RET	NC
		LD	(IX+$17),A
		LD	A,(IX+$16)
		ADD	A,(IX+$13)
		LD	(IX+$16),A
		CP	$08
		RET	C
		SUB	$08
		LD	(IX+$16),A
		INC	(IX+$11)
		RET
l25a4:		LD	A,(IX+$12)
		ADD	A,$08
		BIT	6, (IX+$1C)
		RET	Z
		DEC	A
		DEC	A
		RET
l25b1:		LD	A,(IX+$12)
		SUB	$08
		BIT	6, (IX+$1C)
		RET	Z
		INC	A
		INC	A
		RET
l25be:		LD	A,(IX+$10)
		ADD	A,A
		ADD	A,A
		ADD	A,A
		INC	A
		LD	B,A
		LD	A,(IX+$12)
		ADD	A,$08
		BIT	6, (IX+$1C)
		JR	Z,l25d5
		DEC	A
		DEC	A
		INC	B
		INC	B
l25d5:		CP	B
		RET	NC
		LD	(IX+$12),A
		RET
l25db:		LD	A,(IX+$0E)
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	B,A
		CALL	l25b1
		RET	C
		CP	B
		RET	C
		LD	(IX+$12),A
		RET
l25ec:		LD	A,(IX+$0E)
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	B,A
		LD	A,(IX+$12)
		SUB	B
		LD	(IX+$21),A
		LD	A,(IX+$17)
		AND	A
		JR	NZ,l260b
		CALL	l25b1
		SUB	B
		RET	C
		LD	(IX+$21),A
		LD	A,(IX+$18)
l260b:		DEC	A
		PUSH	AF
		LD	(IX+$23),A
		CALL	l2624
		CALL	l2666
		POP	AF
		LD	(IX+$23),A
		JR	l2624
l261c:		LD	(IX+$21),A
		LD	(IX+$1B),$20
		RET
l2624:		LD	A,(IX+$10)
		SUB	(IX+$0E)
		INC	A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		DEC	A
		CP	(IX+$21)
		RET	C
		LD	A,(IX+$23)
		CP	(IX+$18)
		RET	NC
		LD	(IX+$17),A
		LD	B,A
		INC	B
		LD	A,(IX+$0E)
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,(IX+$21)
		LD	(IX+$12),A
		LD	C,(IX+$0D)
		LD	A,0
l2650:		DEC	B
		JR	Z,l265f
		ADD	A,(IX+$13)
		CP	$08
		JR	C,l2650
		SUB	$08
		INC	C
		JR	l2650
l265f:		LD	(IX+$11),C
		LD	(IX+$16),A
		RET
l2666:		PUSH	AF
		LD	(IX+$23),$20
		LD	A,(IX+$1A)
		PUSH	AF
		res	4, (IX+$1A)
		CALL	l2800
		POP	AF
		LD	(IX+$1A),A
		POP	AF
		RET
l267c:		LD	B,(IX+$18)
l267f:		SUB	B
		JR	NC,l267f
		ADD	A,B
		LD	C,(IX+$17)
		SUB	C
		JR	NC,l268a
		ADD	A,B
l268a:		AND	A
		RET	Z
		CALL	l2666
		DEC	A
		JR	l268a
l2692:		LD	A,(IX+$18)
		SRL	A
		CP	(IX+$17)
		JR	NC,l267c
		XOR	A
		JR	l267c
l269f:		CALL	l254b
		XOR	A
		LD	(l26e0+1),A
		LD	(IX+$24),0
l26aa:		LD	C,(IX+$0D)
		LD	B,(IX+$0E)
		CALL	l2abd
		EX	DE,HL
		LD	A,(IX+$10)
		SUB	(IX+$0E)
		INC	A
		LD	C,A
l26bc:		PUSH	HL
		LD	A,(IX+$0F)
		SUB	(IX+$0D)
		INC	A
		LD	B,A
l26c5:		PUSH	BC
		PUSH	HL
		LD	B,8
l26c9:		LD	A,(IX+$24)
		AND	A
		JR	Z,l26e0
		EXX
		DEC	HL
		DEC	A
		JR	Z,l26db
		EXX
		LD	A,(HL)
		EXX
		LD	(HL),A
		EXX
		JR	l26e2
l26db:		LD	A,(HL)
		EXX
		LD	(HL),A
		JR	l26e2
l26e0:		LD	(HL),0
l26e2:		INC	H
		DJNZ	l26c9
		POP	HL
		POP	BC
		INC	HL
		DJNZ	l26c5
		POP	HL
		LD	A,L
		ADD	A,$20
		LD	L,A
		JR	NC,l26f5
		LD	A,H
		ADD	A,8
		LD	H,A
l26f5:		DEC	C
		JR	NZ,l26bc
l26f8:		LD	C,(IX+$0D)
		LD	B,(IX+$0E)
		CALL	l2abd
		LD	DE,32
		LD	A,(IX+$10)
		SUB	(IX+$0E)
		INC	A
		LD	C,A
l270c:		PUSH	HL
		LD	A,(IX+$0F)
		SUB	(IX+$0D)
		INC	A
		LD	B,A
l2715:		LD	A,(IX+$24)
		AND	A
		JR	Z,l272c
		EXX
		DEC	HL
		DEC	A
		JR	Z,l2727
		EXX
		LD	A,(HL)
		EXX
		LD	(HL),A
		EXX
		JR	l2730
l2727:		LD	A,(HL)
		EXX
		LD	(HL),A
		JR	l2730
l272c:		LD	A,(IX+$19)
		LD	(HL),A
l2730:		INC	HL
		DJNZ	l2715
		POP	HL
		ADD	HL,DE
		DEC	C
		JR	NZ,l270c
		RET
l2739:		LD	(IX+$24),0
		JR	l26f8
l273f:		AND	$07
		LD	B,A
		LD	A,(IX+$19)
		AND	$F8
		OR	B
		LD	(IX+$19),A
		RET
l274c:		AND	$07
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	B,A
		LD	A,(IX+$19)
		AND	$C7
		OR	B
		LD	(IX+$19),A
		RET
l275c:		res	7, (IX+$19)
		RRA
		RET	NC
		SET	7, (IX+$19)
		RET
l2767:		res	6, (IX+$19)
		RRA
		RET	NC
		SET	6, (IX+$19)
		RET
l2772:		res	0,(IX+$1A)
		RRA
		RET	NC
		SET	0,(IX+$1A)
		RET
l277d:		res	1, (IX+$1A)
		RRA
		RET	NC
		SET	1, (IX+$1A)
		RET
l2788:		res	2, (IX+$1A)
		RRA
		RET	NC
		SET	2, (IX+$1A)
		RET
l2793:		CALL	l254b
		LD	A,(IX+$10)
		SUB	(IX+$0E)
		INC	A
		LD	C,A
l279e:		LD	B,(IX+$18)
l27a1:		PUSH	BC
		CALL	l2800
		POP	BC
		DJNZ	l27a1
		DEC	C
		JR	NZ,l279e
		RET
l27ac:		res	4, (IX+$1A)
		RRA
		RET	NC
		SET	4, (IX+$1A)
		RET
l27b7:		res	5, (IX+$1A)
		RRA
		JR	NC,l27c2
		SET	5, (IX+$1A)
l27c2:		res	6, (IX+$1C)
		RRA
		RET	NC
		SET	6, (IX+$1C)
		RET
		DB	0,0,0
l27d0:		LD	IX,(CURCHL)
		LD	(IX+$23),A
		LD	A,(IX+$1B)
		AND	A
		JR	Z,l27f1
		LD	(IX+$1B),0
l27e1:		LD	HL,$21D2
l27e4:		LD	E,A
		LD	D,0
		ADD	HL,DE
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		LD	A,(IX+$23)
		JP	(HL)
l27f1:		LD	A,(IX+$23)
		CP	$20
		JR	NC,l2800
		CP	$10
		JR	C,l27e1
		LD	(IX+$1B),A
		RET
l2800:		BIT	2, (IX+$1A)
		JR	NZ,l280e
		LD	A,(IX+$23)
		SUB	$A5
		JP	NC,l22b4
l280e:		LD	A,(IX+$23)
		LD	H,0
		LD	L,A
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		CP	$80
		JR	C,l284a
		CP	$90
		JR	NC,l283d
		LD	E,A
		PUSH	IX
		POP	HL
		LD	BC,69
		ADD	HL,BC
		PUSH	HL
		LD	A,(IX+$13)
		LD	C,A
		SRL	C
		SUB	C
		LD	B,A
		DEC	B
		DEC	C
		PUSH	BC
		CALL	l2aeb
		POP	BC
		CALL	l2aeb
		POP	HL
		JR	l285c
l283d:		LD	DE,(UDG)
		ADD	HL,DE
		LD	DE,$0480
		AND	A
		SBC	HL,DE
		JR	l285c
l284a:		LD	C,(IX+$14)
		LD	B,(IX+$15)
		LD	A,B
		AND	$C0
		JR	NZ,l285b
		LD	DE,l285c
		PUSH	DE
		PUSH	BC
		RET
l285b:		ADD	HL,BC
l285c:		LD	(IX+$22),1
		BIT	4, (IX+$1A)
		JR	Z,l28e4
		EX	DE,HL
		PUSH	IX
		POP	HL
		LD	BC,37
		ADD	HL,BC
		LD	B,8
l2870:		PUSH	BC
		LD	B,(IX+$13)
		SRL	B
		JR	C,l2893
		PUSH	BC
		LD	(HL),1
		LD	A,(DE)
l287c:		RLA
		PUSH	AF
		RL	(HL)
		POP	AF
		RL	(HL)
		DJNZ	l287c
		JR	C,l288b
l2887:		RL	(HL)
		JR	NC,l2887
l288b:		LD	BC,8
		ADD	HL,BC
		LD	(HL),1
		JR	l28af
l2893:		PUSH	BC
		LD	(HL),1
		LD	A,(DE)
l2897:		RLA
		PUSH	AF
		RL	(HL)
		POP	AF
		RL	(HL)
		DJNZ	l2897
		RLA
		PUSH	AF
l28a2:		RL	(HL)
		JR	NC,l28a2
		LD	BC,8
		ADD	HL,BC
		LD	(HL),1
		POP	AF
		RL	(HL)
l28af:		POP	BC
l28b0:		RLA
		PUSH	AF
		RL	(HL)
		POP	AF
		RL	(HL)
		DJNZ	l28b0
		JR	C,l28bf
l28bb:		RL	(HL)
		JR	NC,l28bb
l28bf:		LD	BC,7
		AND	A
		SBC	HL,BC
		INC	DE
		POP	BC
		DJNZ	l2870
		LD	A,(IX+$17)
		INC	A
		CP	(IX+$18)
		CALL	Z,l2a16
		PUSH	IX
		POP	HL
		LD	BC,37
		ADD	HL,BC
		CALL	l28e4
		PUSH	IX
		POP	HL
		LD	BC,45
		ADD	HL,BC
l28e4:		BIT	5, (IX+$1A)
		JR	Z,l2935
		EX	DE,HL
		PUSH	IX
		POP	HL
		LD	BC,53
		ADD	HL,BC
		LD	B,8
l28f4:		LD	A,(DE)
		INC	DE
		LD	(HL),A
		INC	HL
		LD	(HL),A
		INC	HL
		DJNZ	l28f4
		PUSH	IX
		POP	HL
		LD	BC,53
		ADD	HL,BC
		BIT	6, (IX+$1C)
		JR	Z,l290a
		INC	HL
l290a:		LD	(IX+$22),0
		CALL	l2935
		PUSH	IX
		POP	HL
		LD	BC,61
		ADD	HL,BC
		LD	A,(IX+$12)
		ADD	A,8
		BIT	6, (IX+$1C)
		JR	Z,l2926
		DEC	A
		DEC	A
		DEC	HL
l2926:		LD	(IX+$12),A
		CALL	l2935
		CALL	l25b1
		LD	(IX+$12),A
		JP	l29fa
l2935:		PUSH	HL
		LD	A,(IX+$10)
		INC	A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		INC	A
		LD	B,A
		CALL	l25a4
		CP	B
		JR	C,l2950
		LD	A,(IX+$12)
		SUB	8
		LD	(IX+$12),A
		CALL	l2a49
l2950:		LD	C,(IX+$11)
		LD	B,(IX+$12)
		CALL	l2ac2
		LD	B,(IX+$19)
		LD	(HL),B
		LD	A,(IX+$16)
		ADD	A,(IX+$13)
		CP	9
		JR	C,l296a
		INC	HL
		LD	(HL),B
		DEC	HL
l296a:		LD	A,(IX+$12)
		AND	7
		JR	Z,l298e
		BIT	6, (IX+$1C)
		JR	Z,l297b
		CP	3
		JR	C,l298e
l297b:		PUSH	DE
		LD	DE,32
		ADD	HL,DE
		POP	DE
		LD	(HL),B
		LD	A,(IX+$16)
		ADD	A,(IX+$13)
		CP	9
		JR	C,l298e
		INC	HL
		LD	(HL),B
l298e:		POP	HL
		EX	DE,HL
		LD	B,(IX+$13)
		XOR	A
l2994:		SCF
		RRA
		DJNZ	l2994
		LD	B,A
		LD	C,8
		BIT	6, (IX+$1C)
		JR	Z,l29a4
		DEC	C
		DEC	C
		INC	DE
l29a4:		PUSH	BC
		LD	A,(DE)
		INC	DE
		PUSH	DE
		BIT	0,(IX+$1A)
		JR	Z,l29af
		CPL
l29af:		AND	B
		LD	D,A
		LD	C,0
		LD	E,C
		LD	A,(IX+$16)
		AND	A
		JR	Z,l29c5
l29ba:		RR	B
		RR	C
		RR	D
		RR	E
		DEC	A
		JR	NZ,l29ba
l29c5:		BIT	1, (IX+$1A)
		JR	Z,l29d4
		LD	A,(HL)
		XOR	D
		LD	(HL),A
		INC	HL
		LD	A,(HL)
		XOR	E
		LD	(HL),A
		JR	l29df
l29d4:		LD	A,B
		CPL
		AND	(HL)
		OR	D
		LD	(HL),A
		INC	HL
		LD	A,C
		CPL
		AND	(HL)
		OR	E
		LD	(HL),A
l29df:		DEC	HL
		INC	H
		LD	A,H
		AND	7
		JR	NZ,l29f0
		LD	A,L
		ADD	A,$20
		LD	L,A
		JR	C,l29f0
		LD	A,H
		SUB	8
		LD	H,A
l29f0:		POP	DE
		POP	BC
		DEC	C
		JR	NZ,l29a4
		LD	A,(IX+$22)
		AND	A
		RET	Z
l29fa:		INC	(IX+$17)
		LD	A,(IX+$16)
		ADD	A,(IX+$13)
		CP	8
		JR	C,l2a0c
		SUB	8
		INC	(IX+$11)
l2a0c:		LD	(IX+$16),A
		LD	A,(IX+$17)
		CP	(IX+$18)
		RET	C
l2a16:		CALL	l2a3b
		BIT	5, (IX+$1A)
		CALL	NZ,l2a20
l2a20:		CALL	l25a4
		LD	(IX+$12),A
		LD	A,(IX+$10)
		INC	A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		CP	(IX+$12)
		RET	NC
		LD	A,(IX+$12)
		SUB	8
		LD	(IX+$12),A
		JR	l2a49
l2a3b:		XOR	A
		LD	(IX+$16),A
		LD	(IX+$17),A
		LD	A,(IX+$0D)
		LD	(IX+$11),A
		RET
l2a49:		LD	C,(IX+$0D)
		LD	B,(IX+$0E)
		CALL	l2abd
		LD	A,(IX+$10)
		SUB	(IX+$0E)
		LD	B,A
		JR	Z,l2a9c
l2a5b:		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	A,(IX+$0F)
		SUB	(IX+$0D)
		LD	B,A
		INC	B
l2a66:		PUSH	BC
		LD	BC,32
		ADD	HL,BC
		LD	A,(HL)
		SBC	HL,BC
		LD	(HL),A
		PUSH	DE
		LD	B,8
l2a72:		PUSH	DE
		LD	A,E
		ADD	A,$20
		LD	E,A
		JR	NC,l2a7d
		LD	A,D
		ADD	A,8
		LD	D,A
l2a7d:		LD	A,(DE)
		POP	DE
		LD	(DE),A
		INC	D
		DJNZ	l2a72
		POP	DE
		INC	DE
		INC	HL
		POP	BC
		DJNZ	l2a66
		POP	HL
		POP	DE
		LD	BC,32
		ADD	HL,BC
		LD	A,E
		ADD	A,$20
		LD	E,A
		JR	NC,l2a99
		LD	A,D
		ADD	A,8
		LD	D,A
l2a99:		POP	BC
		DJNZ	l2a5b
l2a9c:		EX	DE,HL
		LD	A,(IX+$0F)
		SUB	(IX+$0D)
		LD	B,A
		INC	B
l2aa5:		PUSH	BC
		LD	A,(IX+$19)
		LD	(DE),A
		LD	B,8
		PUSH	HL
l2aad:		LD	(HL),0
		INC	H
		DJNZ	l2aad
		POP	HL
		INC	DE
		INC	HL
		POP	BC
		DJNZ	l2aa5
		RET
l2ab9:		LD	(IX+$19),A
		RET
l2abd:		LD	A,B
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	B,A
l2ac2:		LD	A,B
		RRCA
		RRCA
		RRCA
		LD	L,A
		AND	$18
		OR	$40
		LD	D,A
		LD	A,B
		AND	7
		OR	D
		LD	D,A
		LD	A,B
		RLCA
		RLCA
		AND	$E0
		OR	C
		LD	E,A
		LD	A,L
		AND	$1F
		LD	L,A
		LD	H,0
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		LD	B,0
		ADD	HL,BC
		LD	BC,$5800
		ADD	HL,BC
		RET
l2aeb:		XOR	A
		SRL	E
		RRA
l2aef:		SRA	A
		DEC	C
		JR	NZ,l2aef
		SRL	E
		RRA
l2af7:		SRA	A
		DJNZ	l2af7
		LD	B,4
l2afd:		LD	(HL),A
		INC	HL
		DJNZ	l2afd
		RET

FREE_ROM0_3:	EQU	$

		;...
		;...

R0_FREE_3:	EQU	29-($-FREE_ROM0_3)
ROM0_SPARE3:	DS	R0_FREE_3

l2b1f:		LD	BC,$3C00
		ADD	HL,BC
		EX	DE,HL
		LD	HL,(RETADDR)
		PUSH	HL
		LD	HL,69
		PUSH	IX
		POP	BC
		ADD	HL,BC
		EX	DE,HL
		PUSH	DE
		LD	BC,8
		RST	28H
		DW	$33C3
		POP	HL
		POP	DE
		LD	(RETADDR),DE
		RET
l2b3e:		CALL	l2b1f
		SLA	(IX+$45)
		SLA	(IX+$46)
		SLA	(IX+$47)
		SLA	(IX+$48)
		SLA	(IX+$49)
		SLA	(IX+$4A)
		SLA	(IX+$4B)
		SLA	(IX+$4C)
		RET
l2b62:		LD	BC,l2b95-$100
		ADD	HL,BC
		PUSH	IX
		POP	DE
		LD	BC,69
		EX	DE,HL
		ADD	HL,BC
		PUSH	HL
		LD	B,8
l2b71:		LD	A,(DE)
		INC	DE
		AND	$E0
		LD	(HL),A
		INC	HL
		DJNZ	l2b71
		POP	HL
		RET
l2b7b:		LD	BC,l2b95-$100
		ADD	HL,BC
		PUSH	IX
		POP	DE
		LD	BC,69
		EX	DE,HL
		ADD	HL,BC
		PUSH	HL
		LD	B,8
l2b8a:		LD	A,(DE)
		INC	DE
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	(HL),A
		INC	HL
		DJNZ	l2b8a
		POP	HL
		RET
l2b95:		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000

		DB	%00000000
		DB	%01001000
		DB	%01001000
		DB	%01001000
		DB	%01001000
		DB	%00000000
		DB	%01001000
		DB	%00000000

		DB	%00000000
		DB	%10101010
		DB	%10101010
		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000
		DB	%00000000

		DB	%00000000
		DB	%10101010
		DB	%11111111
		DB	%10101010
		DB	%10101010
		DB	%11111111
		DB	%10101010
		DB	%00000000

		DB	%00000000
		DB	%01000100
		DB	%11111110
		DB	%10010100
		DB	%11111110
		DB	%00100110
		DB	%11111110
		DB	%01000100

		DB	%00000000
		DB	%10010010
		DB	%10000010
		DB	%00100100
		DB	%01001000
		DB	%10010000
		DB	%00110010
		DB	%00100000

		DB	$00,$48,$B4,$48,$DD,$B2,$FD,$00
		DB	$00,$44,$88,$00,$00,$00,$00,$00
		DB	$00,$42,$84,$84,$84,$84,$42,$00
		DB	$00,$88,$44,$44,$44,$44,$88,$00
		DB	$00,$00,$AA,$44,$EE,$44,$AA,$00
		DB	$00,$00,$44,$44,$EE,$44,$44,$00
		DB	$00,$00,$00,$00,$00,$44,$44,$88
		DB	$00,$00,$00,$00,$EE,$00,$00,$00
		DB	$00,$00,$00,$00,$00,$CC,$CC,$00
		DB	$00,$20,$22,$42,$44,$88,$88,$00
		DB	$00,$EC,$B2,$B2,$B2,$B2,$EC,$00
		DB	$00,$4C,$C4,$44,$44,$44,$EE,$00
		DB	$00,$4C,$B2,$22,$4C,$90,$FE,$00
		DB	$00,$CC,$32,$C4,$22,$32,$CC,$00
		DB	$00,$24,$64,$AC,$B4,$FE,$24,$00
		DB	$00,$FE,$90,$DC,$22,$32,$CC,$00
		DB	$00,$6C,$90,$DC,$B2,$B2,$4C,$00
		DB	$00,$FE,$22,$44,$48,$88,$88,$00
		DB	$00,$EC,$B2,$4C,$B2,$B2,$EC,$00
		DB	$00,$4C,$B2,$B2,$6E,$22,$CC,$00
		DB	$00,$00,$00,$48,$00,$00,$48,$00
		DB	$00,$00,$44,$00,$00,$44,$44,$88
		DB	$00,$00,$22,$44,$88,$44,$22,$00
		DB	$00,$00,$00,$EE,$00,$EE,$00,$00
		DB	$00,$00,$88,$44,$22,$44,$88,$00
		DB	$00,$4C,$B2,$22,$44,$00,$44,$00
		DB	$00,$5E,$FE,$FA,$FE,$90,$EE,$00
		DB	$00,$4C,$B2,$B2,$FE,$B2,$B2,$00
		DB	$00,$DC,$B2,$DC,$B2,$B2,$DC,$00
		DB	$00,$4C,$B2,$90,$90,$B2,$4C,$00
		DB	$00,$DC,$B2,$B2,$B2,$B2,$DC,$00
		DB	$00,$FE,$90,$DC,$90,$90,$FE,$00
		DB	$00,$FE,$90,$FC,$90,$90,$90,$00
		DB	$00,$4C,$B2,$90,$F6,$B2,$4E,$00
		DB	$00,$B2,$B2,$FE,$B2,$B2,$B2,$00
		DB	$00,$EE,$44,$44,$44,$44,$EE,$00
		DB	$00,$22,$22,$22,$A2,$B2,$4C,$00
		DB	$00,$B2,$B4,$D8,$D4,$B2,$B2,$00
		DB	$00,$90,$90,$90,$90,$90,$FE,$00
		DB	$00,$B2,$FE,$FE,$F2,$B2,$B2,$00
		DB	$00,$F2,$BA,$BA,$B6,$B6,$B2,$00
		DB	$00,$4C,$B2,$B2,$B2,$B2,$4C,$00
		DB	$00,$DC,$B2,$B2,$DC,$90,$90,$00
		DB	$00,$EC,$B2,$B2,$B6,$F6,$EE,$20
		DB	$00,$FC,$B2,$B2,$DC,$D2,$B2,$00
		DB	$00,$6E,$90,$4C,$22,$32,$CC,$00
		DB	$00,$EE,$44,$44,$44,$44,$44,$00
		DB	$00,$B2,$B2,$B2,$B2,$B2,$EC,$00
		DB	$00,$AA,$AA,$AA,$AA,$AA,$44,$00
		DB	$00,$B2,$F2,$F2,$FE,$FE,$52,$00
		DB	$00,$AA,$AA,$44,$44,$AA,$AA,$00
		DB	$00,$AA,$AA,$AA,$44,$44,$44,$00
		DB	$00,$FE,$22,$44,$48,$90,$FE,$00
		DB	$00,$66,$44,$44,$44,$44,$66,$00
		DB	$00,$00,$88,$C8,$44,$62,$22,$00
		DB	$00,$CC,$44,$44,$44,$44,$CC,$00
		DB	$00,$44,$EE,$44,$44,$44,$44,$00
		DB	$00,$00,$00,$00,$00,$00,$00,$FF
		DB	$00,$4E,$B0,$9C,$F0,$90,$FE,$00
		DB	$00,$00,$CC,$22,$EE,$B2,$EE,$00
		DB	$00,$90,$90,$DC,$B2,$B2,$DC,$00
		DB	$00,$00,$6E,$90,$90,$90,$6E,$00
		DB	$00,$22,$22,$6E,$B2,$B2,$6E,$00
		DB	$00,$00,$4C,$B2,$DC,$90,$6E,$00
		DB	$00,$6C,$90,$D8,$90,$90,$90,$00
		DB	$00,$00,$6E,$B2,$B2,$6E,$22,$CC
		DB	$00,$90,$90,$DC,$B2,$B2,$B2,$00
		DB	$00,$44,$00,$4C,$44,$44,$4E,$00
		DB	$00,$22,$00,$22,$22,$22,$B2,$4C
		DB	$00,$90,$B4,$D8,$D8,$B4,$B2,$00
		DB	$00,$48,$48,$48,$48,$48,$46,$00
		DB	$00,$00,$BA,$F5,$F5,$F5,$B5,$00
		DB	$00,$00,$DC,$B2,$B2,$B2,$B2,$00
		DB	$00,$00,$4C,$B2,$B2,$B2,$4C,$00
		DB	$00,$00,$DC,$B2,$B2,$DC,$90,$90
		DB	$00,$00,$6E,$B2,$B2,$6E,$22,$22
		DB	$00,$00,$6E,$90,$90,$90,$90,$00
		DB	$00,$00,$6E,$90,$4E,$22,$DC,$00
		DB	$00,$48,$FC,$48,$48,$48,$46,$00
		DB	$00,$00,$B2,$B2,$B2,$B2,$EC,$00
		DB	$00,$00,$AA,$AA,$AA,$AA,$44,$00
		DB	$00,$00,$B1,$F5,$F5,$F5,$4A,$00
		DB	$00,$00,$AA,$AA,$44,$AA,$AA,$00
		DB	$00,$00,$B2,$B2,$B2,$6E,$22,$CC
		DB	$00,$00,$FE,$22,$44,$88,$FE,$00
		DB	$00,$66,$44,$88,$44,$44,$66,$00
		DB	$00,$44,$44,$44,$44,$44,$44,$00
		DB	$00,$CC,$44,$22,$44,$44,$CC,$00
		DB	$00,$4A,$B4,$00,$00,$00,$00,$00
		DB	$EE,$B1,$F7,$B5,$F5,$B7,$B1,$EE
		block	$36BA-$
		ELSE
; *************** START OF SELF-TEST PROGRAM SECTION ****************
; The self-test program, entered by pressing "QAZPLM" at the test screen
l21df:		DI
		LD	IX,$FFFF	; IX=top of RAM
		LD	A,$07
		OUT	($FE),A		; white border
		LD	SP,$7FFF	; set stack to top of page 5
		CALL	l28f7		; initialise & show title page
		DI			; disable interrupts
		LD	A,$07
		OUT	($FE),A		; white border
		CALL	l271a		; clear screen
l21f6:		LD	BC,$0700
		CALL	l270b		; set INK 7, PAPER 0
		LD	HL,l3261
		CALL	l2703		; display RAM test message
		CALL	l269a		; pause for 0.8s
		LD	A,$04
l2207:		OUT	($FE),A		; green border
		LD	DE,$0002	; D=test pattern,E=pass counter
l220c:		LD	A,$00		; start with page 0
		LD	BC,PBANKM
l2211:		OUT	(C),A		; page in next page
		EX	AF,AF'		; save page number
		LD	HL,$C000
l2217:		LD	(HL),D		; fill page with D
		INC	HL
		LD	A,L
		OR	H
		JR	NZ,l2217	; until page filled
		EX	AF,AF'
		INC	A
		CP	$08
		JR	NZ,l2211	; back for more pages
		LD	A,$00		; start at page 0 again
l2225:		OUT	(C),A		; page in next page
		EX	AF,AF'		; save page number
		LD	HL,$C000
l222b:		LD	A,(HL)
		CP	D		; check pattern
		JR	NZ,l2262	; move on if error
		CPL
		LD	(HL),A		; store inverse pattern
		INC	HL
		LD	A,L
		OR	H
		JR	NZ,l222b	; until page done
		EX	AF,AF'
		INC	A
		CP	$08
		JR	NZ,l2225	; back for more pages
		DEC	A		; back to page 7
l223d:		EX	AF,AF'
		LD	HL,$0000	; start at end of memory
l2241:		DEC	HL
		LD	A,H
		CP	$BF
		JR	Z,l224f		; move on if at start of page
		LD	A,D
		CPL
		CP	(HL)		; check inverse pattern
		JR	NZ,l2262	; move on if error
		LD	(HL),D		; store normal pattern
		JR	l2241		; loop back
l224f:		EX	AF,AF'
		CP	$00
		JR	Z,l2259
		DEC	A
		OUT	(C),A
		JR	l223d		; back for more pages
l2259:		DEC	E		; decrement pass counter
		JP	Z,l2329		; move on if successful
		LD	D,$FF
		JP	l220c		; back for second pass with new pattern
l2262:		EX	AF,AF'
		PUSH	AF		; save page and address of failure
		PUSH	HL
		CALL	l266f		; re-initialise
		XOR	A
		LD	BC,PBANKM
		LD	(BANKM),A
		OUT	(C),A		; page in page 0
		LD	BC,$0700
		CALL	l270b		; set INK 7, PAPER 0
		CALL	l271a		; clear screen
		LD	HL,l22ba
		CALL	l2703		; display RAM test fail message
		POP	HL
		LD	A,H
		CALL	l2299		; output high byte of address
		LD	A,L
		CALL	l2299		; output low byte of address
		EXX
		LD	HL,l22fe
		CALL	l2703		; output page message
		POP	AF
		AND	$07
		EXX
		CALL	l2299		; output page number
		DI
		HALT			; halt!

; Subroutine to output A as two hexadecimal digits

l2299:		PUSH	HL		; save registers
		PUSH	AF
		PUSH	AF
		SRL	A
		SRL	A
		SRL	A
		SRL	A		; A=A/16
		LD	B,$02		; two digits
l22a6:		EXX
		LD	D,$00
		LD	E,A		; DE=0...F
		LD	HL,l2306
		ADD	HL,DE
		LD	A,(HL)		; A=ASCII hex digit
		CALL	l2716		; output it
		POP	AF		; restore value
		AND	$0F		; get low nibble
		EXX
		DJNZ	l22a6		; loop back for second digit
		POP	HL
		RET

l22ba:		DB	$16,$0A,$0
		DB	"RAM fail: address ", $FF

; The "EUA" routine from the testcard, which changes attributes based
; on input at EAR

l22d0:		LD	A,$00
		OUT	($FE),A		; set black border
		LD	HL,$4000
		LD	DE,$4001
		LD	BC,$1800
		LD	(HL),$00
		LDIR			; clear screen to black
l22e1:		LD	HL,$5800	; start of attributes
l22e4:		LD	BC,$0300	; B=loop counter,C=black paper
l22e7:		IN	A,($FE)
		AND	$40		; get EAR bit
		OR	C
		LD	C,A
		RR	C		; combine into C
		INC	IX		; delay
		DEC	IX
		DJNZ	l22e7		; loop 3 times to form a paper colour
		LD	(HL),C		; store in attributes
		INC	HL		; move to next
		LD	A,H
		CP	$5B
		JR	NZ,l22e4	; loop back for whole screen
		JR	l22e1		; start again


l22fe:		DB	", page ", $FF
l2306:		DB	"0123456789ABCDEF"
l2316:		DB	$16,$0A,$0
		DB	"RAM test passed", $FF

; Continuation of self-test program

l2329:		LD	A,$00		; start with page 0
		LD	HL,$CAFE	; location to use
		LD	BC,PBANKM
l2331:		OUT	(C),A		; page in page
		LD	(HL),A		; and store page number
		INC	A
		CP	$08
		JR	NZ,l2331	; back for more
l2339:		DEC	A
		OUT	(C),A		; page in page
		CP	(HL)		; check correct number
		JP	NZ,l2457	; if not, go to signal ULA error
		AND	A
		JR	NZ,l2339	; back for more
		DI
		LD	BC,PBANKM
		LD	A,$00		; start at page 0
l2349:		OUT	(C),A		; page in page
		LD	($E000),A	; store number at midpoint of page
		INC	A
		CP	$08
		JR	NZ,l2349	; back for more
		DEC	A
		CALL	l235f		; copy routine to page 7
		LD	A,$03
		CALL	l235f		; and to page 3
		JP	$D000		; jump into it in page 3

; Subroutine to copy the following routine into page A at $D000

l235f:		LD	BC,PBANKM
		OUT	(C),A		; page in required page
		LD	HL,l2370
		LD	DE,$D000
		LD	BC,$00B9
		LDIR			; copy routine
		RET

; Routine to be executed in pages 3 & 7 ($2370-$2328 at $D000-$D0B8)
; to test the RAM configurations (all of which have one of these pages
; at the top)

l2370:		LD	A,$01		; start with RAM configuration 0,1,2,3
		LD	DE,$D0A9	; start of table of configs
		LD	HL,$2000	; location of page number within lowest page
l2378:		LD	BC,PBANK678
		OUT	(C),A		; page in next configuration
		EX	AF,AF'
		LD	BC,$4000
l2381:		LD	A,(DE)
		INC	DE
		CP	(HL)		; check correct page in place
		JR	NZ,l2394	; if not, move on
		ADD	HL,BC		; add offset to next segment
		JR	NC,l2381	; loop back for more
		EX	AF,AF'
		ADD	A,$02		; increment configuration
		BIT	3,A
		JR	Z,l2378		; loop back if more to test
		LD	D,$01		; flag "RAM configurations passed"
l2392:		JR	l2397
l2394:		EX	AF,AF'
		LD	D,$00		; flag "RAM configurations failed"
l2397:		EXX
		LD	BC,PBANKM
		LD	A,$03
		OUT	(C),A		; make sure page 3 is selected
		LD	B,$1F
		XOR	A
		OUT	(C),A		; and ROM 0
		CALL	$D09B		; get checksum of ROM 0
		JR	NZ,l23f2	; if not zero, go to error
		LD	BC,PBANKM
		LD	A,$13
		OUT	(C),A		; get ROM 1
		CALL	$D09B		; and its checksum
		JR	NZ,l23f2	; if not zero, go to error
		SCF
		CALL	$D08E		; rotate ROM0/1 success into flags
l23b9:		LD	BC,PBANK678
		LD	A,$04
		OUT	(C),A
		LD	BC,PBANKM
		LD	A,$03
		OUT	(C),A		; get ROM 2
		CALL	$D09B		; and its checksum
		JR	NZ,l23f8	; if not zero, go to error
		LD	A,$0B
		LD	BC,PBANKM
		OUT	(C),A		; get ROM 3
		CALL	$D09B		; and its checksum
		JR	NZ,l23f8	; if not zero, go to error
		SCF
		CALL	$D08E		; rotate ROM2/3 success into flags
l23dc:		LD	A,$03
		LD	BC,PBANKM
		OUT	(C),A		; make sure page 3 is paged in
		LD	A,$00
		LD	B,$1F
		OUT	(C),A		; with ROM 0
		LD	(BANKM),A
		LD	(BANK678),A
		JP	l2436		; jump back into routine in ROM
l23f2:		XOR	A
		CALL	$D08E		; rotate ROM0/1 fail into flags
		JR	l23b9
l23f8:		XOR	A
		CALL	$D08E		; rotate ROM2/3 fail into flags
		JR	l23dc

; Subroutine to rotate a test flag bit (1=success) into IX

		PUSH	DE
		PUSH	IX
		POP	DE
		RL	E
		RL	D
		PUSH	DE
		POP	IX
		POP	DE
		RET

; Subroutine to form an 8-bit addition checksum of the current ROM
; All ROMs should checksum to zero

		XOR	A		; zero checksum
		LD	H,A
		LD	L,A		; and address
l240e:		ADD	A,(HL)		; add next byte
		INC	HL
		LD	D,A
		LD	A,H
		CP	$40
		LD	A,D
		JR	NZ,l240e	; back for rest of page
		AND	A		; check if zero
		RET

; Table of +3 special RAM configurations to test

ld0a9:		DB	$00,$01,$02,$03
		DB	$04,$05,$06,$07
		DB	$04,$05,$06,$03
		DB	$04,$07,$06,$03

; Subroutine to rotate carry flag (1=pass,0=fail) into IX

l2429:		PUSH	DE		; save DE
		PUSH	IX
		POP	DE		; DE=test results
		RL	E		; rotate carry into test results
		RL	D
		PUSH	DE
		POP	IX		; IX=test results
		POP	DE		; restore DE
		RET

; Re-entry into self-test routine from RAM here

l2436:		LD	BC,PBANKM
		LD	A,$00
		OUT	(C),A		; page in page 0
		LD	SP,$7FFF	; set stack in page 5
		EXX
		XOR	A
		CP	D		; set carry if RAM configs test passed
		CALL	l2429		; rotate RAM configs test result into flags
		CALL	l266f		; re-initialise
		CALL	l271a		; clear screen
		LD	HL,l2316
		CALL	l2703		; display RAM test passed message
		CALL	l269a		; pause for 0.8s
		JR	l2461		; move on

; routine comes here if ULA error with normal RAM paging

l2457:		CALL	l266f		; reinitialise
		EX	AF,AF'		; get page
		LD	HL,$0000	; specify address 0000 for page error
		JP	l2262		; display error and halt

; More self-test program

l2461:		CALL	l2c99		; GI sound test
		CALL	l28b6		; Symshift-A test
		CALL	l295d		; ULA test
		CALL	l274d		; keyboard test
		CALL	l2c1f		; ULA sound test part 1
		CALL	l2c6e		; ULA sound test part 2
		CALL	l29d0		; joystick test
		CALL	l2b24		; cassette output test
		CALL	l2a4b		; screen switching test
		CALL	l2488		; printer BUSY test & data test
		CALL	l3558		; integral disk test
		CALL	l35a3		; tape test
		JP	l2b91		; move on to display results

; Printer BUSY test & data test

l2488:		CALL	l271a		; cls
		LD	HL,l25d5
		CALL	l2703		; display test message
		LD	HL,l25f3	; ask for printer OFFLINE then any key
		CALL	l2703
		LD	HL,l2613
		CALL	l2703
		LD	HL,l261e
		CALL	l2703
		CALL	l253d		; wait for a key
		LD	BC,$0FFD
		IN	A,(C)
		BIT	0,A		; check BUSY signal
		JR	Z,l24c4		; move on if not set
		LD	HL,l2608
		CALL	l2703		; ask for ONLINE then any key
		CALL	l253d		; wait for a key
		LD	BC,$0FFD
		IN	A,(C)
		BIT	0,A		; check BUSY signal
		JR	NZ,l24c4	; move on if set
		SCF			; signal success
		JR	l24c6
l24c4:		SCF
		CCF			; signal fail
l24c6:		CALL	l2429		; set success/fail flag
		CALL	l271a		; cls
		LD	HL,BANK678
		SET	4,(HL)
		LD	HL,l2549
		CALL	l2703		; display test messages
		LD	HL,l255e
		CALL	l2703
		LD	E,$00		; chars per line count
l24df:		LD	A," "		; start with space
l24e1:		PUSH	AF
		CALL	l24f4		; output character
		POP	AF
		INC	A		; get next ASCII code
		CP	$80
		JR	Z,l24f0		; go to skip $80-$9F
		OR	A
		JR	Z,l24df		; restart after $FF at space
		JR	l24e1
l24f0:		LD	A,$A0
		JR	l24e1

; Subroutine to output a character to the printer

l24f4:		CALL	l2507		; output it
		PUSH	AF
		INC	E		; increment chars printed this line
		LD	A,E
		CP	$48
		JR	NZ,l2505
		LD	E,$00		; if $48, reset and start new line
		LD	A,$0D
		CALL	l2507
l2505:		POP	AF
		RET

; Subroutine to output a character to the printer,
; checking for Quit from user, exiting to higher calling
; routine

l2507:		PUSH	AF
l2508:		LD	A,$FB
		IN	A,($FE)
		RRA			; get Q status in carry
		JR	NC,l252c	; move on if pressed
		LD	BC,$0FFD
		IN	A,(C)
		BIT	0,A		; check BUSY status
		JR	NZ,l2508	; loop back if busy
		POP	AF
		OUT	(C),A		; place character at port
		DI
		LD	A,(BANK678)
l251f:		LD	BC,PBANK678
		XOR	$10
		OUT	(C),A		; STROBE parallel port
		BIT	4,A
		JR	Z,l251f		; loop back to change state if necessary
		EI
		RET
l252c:		POP	AF		; discard AF
		POP	AF		; discard return address
		POP	AF		; and two stacked values from calling routine
		POP	AF
l2530:		LD	HL,l2597
		CALL	l2703		; ask if printed OK
		CALL	l26af		; get SPACE/ENTER
		CALL	l2429		; set success/fail
		RET			; exit to earlier routine

; Subroutine to wait until a new key is available

l253d:		LD	HL,FLAGS
		res	5,(HL)
l2542:		BIT	5,(HL)
		JR	Z,l2542
		res	5,(HL)
		RET

; Printer test messages

l2549:		DB	$16,$4,$4
		DB	"Printer data test", $FF
l255e:		DB	$16,$8,$1
		DB	"Make sure printer is ready"
		DB	$16,$0A,$1
		DB	"Press Q to quit printing", $FF
l2597:		DB	$16,$0C,$01
		DB	"If characters printed OK,", $0D
		DB	"Press [ENTER], otherwise [SPACE]", $FF
l25d5:		DB	$15,$0,$16,$4,$4
		DB	"Printer BUSY signal test", $FF
l25f3:		DB	$16,$8,$8
		DB	"Turn the printer ", $FF
l2608:		DB	$16,$0A,$0C
		DB	"ONLINE ", $FF
l2613:		DB	$16,$0A,$0C
		DB	"OFFLINE", $FF
l261e:		DB	$16,$0C,$4
		DB	"Press any key to continue", $FF
		DB	$16,$10,$5
		DB	"Passed - press [ENTER]", $FF
		DB	$16,$10,$5
		DB	"Failed - press [SPACE]", $FF

; Subroutine to do some initialisation

l266f:		LD	A,$52		; signal "in test program"
		EX	AF,AF'
		JP	l016c		; do some initialisation & return here
l2675:		LD	A,$02
		RST	28H
		DW	o1601		; open channel to stream 2
l267a:		LD	HL,l2681
		CALL	l2703		; output normal colour control codes
		RET

; "Normal" colour control codes

l2681:		DB	$10,$00,$11,$07
		DB	$13,$00,$14,$00
		DB	$15,$00,$12,$00
		DB	$FF

; Apparently unused routine, to pause

l268e:		PUSH	BC
		PUSH	HL
		LD	B,$19
		EI
l2693:		HALT
		DJNZ	l2693
l2696:		POP	HL
l2697:		POP	BC
		DI
		RET


; Subroutine to pause for approx 0.8s

l269a:		LD	B,$28
		EI
l269d:		HALT
		DJNZ	l269d
		DI
		RET

; Pause subroutine

l26a2:		LD	HL,$3000
l26a5:		DEC	HL
		PUSH	IX
		POP	IX
		LD	A,L
		OR	H
		JR	NZ,l26a5
		RET

; Subroutine to wait for ENTER or SPACE to be pressed.
; On exit, carry is set if ENTER was pressed, reset if SPACE.

l26af:		PUSH	HL		; save registers
		PUSH	DE
		PUSH	BC
		LD	BC,$00FE
l26b5:		IN	A,(C)		; scan all keyrows
		AND	$1F
		CP	$1F
		JR	NZ,l26b5	; loop back if any are pressed
l26bd:		CALL	l26d2		; get a scancode
		CP	$21		; check for ENTER
		SCF
l26c3:		JR	Z,l26c9		; if so, move on with carry set
		CP	$20		; check for SPACE
		JR	NZ,l26bd	; loop back if not
l26c9:		PUSH	AF		; save carry
		CALL	l2720		; make a beep
		POP	AF		; restore registers
		POP	BC
		POP	DE
		POP	HL
		RET
l26d2:		CALL	l26d6
		RET

; Subroutine to read the keyboard, returning a scancode
; in DE. This routine is a virtual copy of the routine
; at 028e in ROM 3, but doesn't return until a scancode
; has been detected.

l26d6:		LD	L,$2F
		LD	DE,$FFFF
		LD	BC,$FEFE
l26de:		IN	A,(C)
		CPL
		AND	$1F
		JR	Z,l26f4
		LD	H,A
		LD	A,L
l26e7:		INC	D
		JR	NZ,l26d6
l26ea:		SUB	$08
		SRL	H
		JR	NC,l26ea
		LD	D,E
		LD	E,A
		JR	NZ,l26e7
l26f4:		DEC	L
		RLC	B
		JR	C,l26de
		LD	A,E
		CP	$FF
		RET	NZ
		LD	A,D
		CP	$FF
		JR	Z,l26d6
		RET

; Subroutine to output an $FF-terminated string

l2703:		LD	A,(HL)		; get byte
		CP	$FF
		RET	Z		; exit if $FF
		RST	10H		; else output
		INC	HL
		JR	l2703		; back for more


; Subroutine to set colours to INK b, PAPER c

l270b:		LD	A,$10
		RST	10H
		LD	A,B
		RST	10H
		LD	A,$11
		RST	10H
		LD	A,C
		RST	10H
		RET

; Subroutine to output a character

l2716:		RST	28H
		DW	$0010
l2719:		RET

; Subroutine to clear screen

l271a:		RST	28H
		DW	o0DAF		; call ROM 3 CLS
		JP	l267a		; go to set "normal" colours & exit

; Subroutine to make a beep

l2720:		LD	HL,$0100	; parameters for the beep
		LD	DE,$00A0
l2726:		CALL	l34e9		; call BEEPER
		DI			; re-disable interrupts
		RET

; Copy of next routine, apparently unused

		LD	A,$7F
		IN	A,($FE)
		RRA
		RET

; Subroutine to check for SPACE, exiting with carry reset if pressed

l2731:		LD	A,$7F
		IN	A,($FE)
		RR	A
		RET

; Apparently unused routine, to check for SPACE+Symbol shift

		LD	A,$7F
		IN	A,($FE)
		OR	$E0
		CP	$FC
		RET

; Apparently unused routine, to output D spaces and set A'="8"

		LD	A,"8"
		EX	AF,AF'
l2744:		LD	A," "
		CALL	l2716
		DEC	D
		JR	NZ,l2744
		RET

; Keyboard test

l274d:		CALL	l271a		; cls
		LD	A,$38
		LD	(BORDCR),A
		LD	A,$07
		OUT	($FE),A		; white border
		LD	HL,l2f17
		CALL	l2703		; display test message
		CALL	l26a2		; pause
		CALL	l2766		; execute test
		RET

l2766:		LD	HL,l2841	; keyboard test table
		PUSH	HL
l276a:		POP	HL
		LD	A,(HL)		; get next test value
		INC	A
		RET	Z		; exit if end of table
		PUSH	HL
		CALL	l26d2		; get keyscan code
		LD	BC,$1000
l2775:		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,l2775	; timing loop
		LD	A,D
		CP	$FF
		JR	Z,l27e0		; move on if no first key
		CP	$27
		JR	Z,l2790		; move on if capsshift held
		CP	$18
		JR	Z,l27db		; move on if symshift held
		LD	A,E		; swap scancodes
		LD	E,D
		LD	D,A
		CP	$18
		JR	Z,l27db		; move on if symshift held
		JR	l276a		; loop back if no shifts
l2790:		LD	A,E		; here we substitute various codes for
		CP	$23		;  keys with caps shift
		LD	E,$28
		JR	Z,l27f9
		CP	$24
		LD	E,$29
		JR	Z,l27f9
		CP	$1C
		LD	E,$2A
		JR	Z,l27f9
		CP	$14
		LD	E,$2B
		JR	Z,l27f9
		CP	$0C
		LD	E,$2C
		JR	Z,l27f9
		CP	$04
		LD	E,$2D
		JR	Z,l27f9
		CP	$03
		LD	E,$2E
		JR	Z,l27f9
		CP	$0B
		LD	E,$2F
		JR	Z,l27f9
		CP	$13
		LD	E,$30
		JR	Z,l27f9
		CP	$1B
		LD	E,$31
		JR	Z,l27f9
		CP	$20
		LD	E,$32
		JR	Z,l27f9
		CP	$18
		LD	E,$37
		JR	Z,l27f9
		JR	l276a		; loop back for untested caps shift codes
l27db:		LD	A,E		; here we substitute various codes for keys
		CP	$10		;  with symbol shift
		LD	E,$33
l27e0:		JR	Z,l27f9
		CP	$08
		LD	E,$34
		JR	Z,l27f9
		CP	$1A
		LD	E,$35
		JR	Z,l27f9
		CP	$22
		LD	E,$36
		JR	Z,l27f9
		LD	E,$37
		JP	NZ,l276a
l27f9:		LD	A,E		; A=final scancode
		POP	HL
		PUSH	HL
		CP	(HL)		; test against table entry
		JP	NZ,l276a	; loop back if not equal
		POP	HL
		INC	HL		; get to next entry
		PUSH	HL
		LD	HL,$0080
		LD	DE,$0080
		PUSH	AF
		PUSH	BC
		PUSH	IX
		CALL	l2726		; make a beep
		POP	IX
		POP	BC
		POP	AF
		POP	HL
		PUSH	HL
		LD	A,$11		; set PAPER 2
		RST	10H
		LD	A,$02
		RST	10H
		DEC	HL
		LD	DE,$003B
		ADD	HL,DE
		LD	A,(HL)		; get position code from table
		AND	$F0
		RRA
		RRA
		RRA
		LD	B,$06
		ADD	A,B
		LD	B,A		; B=line+1
		LD	A,(HL)
		AND	$0F
		RLA
		LD	C,$01
		ADD	A,C		; C=column
		LD	C,A
		LD	A,$16		; output space to blank key at correct pos
		RST	10H
		LD	A,B
		DEC	A
		RST	10H
		LD	A,C
		RST	10H
		LD	A," "
		RST	10H
		JP	l276a		; back for more

; Keyboard test table of scancodes

l2841:		DB	$2B,$2C,$24,$1C
		DB	$14,$0C,$04,$03
		DB	$0B,$13,$1B,$23
		DB	$32,$28,$31,$25
		DB	$1D,$15,$0D,$05
		DB	$02,$0A,$12,$1A
		DB	$22,$37,$29,$26
		DB	$1E,$16,$0E,$06
		DB	$01,$09,$11,$19
		DB	$21,$27,$2A,$1F
		DB	$17,$0F,$07,$00
		DB	$08,$10,$33,$27
		DB	$18,$35,$36,$2D
		DB	$30,$20,$2F,$2E
		DB	$34,$18,$FF

; Keyboard test table of screen positions

		DB	$00,$01,$02,$03
		DB	$04,$05,$06,$07
		DB	$08,$09,$0A,$0B
		DB	$0D,$10,$11,$13
		DB	$14,$15,$16,$17
		DB	$18,$19,$1A,$1B
		DB	$1C,$20,$21,$23
		DB	$24,$25,$26,$27
		DB	$28,$29,$2A,$2B
		DB	$2D,$30,$31,$33
		DB	$34,$35,$36,$37
		DB	$38,$39,$3A,$3D
		DB	$40,$41,$42,$43
		DB	$44,$47,$4A,$4B
		DB	$4C,$4D

; Symbolshift-A test

l28b6:		CALL	l271a		; cls
		LD	A,$05
		OUT	($FE),A		; cyan border
		LD	BC,$0600
		CALL	l270b		; INK 6, PAPER 0
		LD	HL,l30d9
		CALL	l2703		; display test message
l28c9:		CALL	l28e5
		JR	NZ,l28c9	; loop back until symshift-A pressed
		LD	BC,$0200	; timing counter
l28d1:		PUSH	BC
		CALL	l28e5		; check for symshift-A
		POP	BC
		JR	NZ,l28e1	; exit with error if not pressed
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,l28d1	; loop back to test again
		SCF
		JP	l2429		; exit, setting "success" flag
l28e1:		OR	A
		JP	l2429		; exit, setting "fail" flag

; Subroutine to check if symbol-shift & A are being pressed
; On exit, Z is set if both are being pressed

l28e5:		CALL	l26d2		; get key scan code
		LD	A,D
		CP	$18
		JR	Z,xl28f3	; move on if first key is sym/shft
		LD	A,E		; swap keys
		LD	E,D
		LD	D,A
		CP	$18
		RET	NZ		; exit with Z reset if 2nd key not sym/shft
xl28f3:		LD	A,E
		CP	$26		; compare other key with A and exit
		RET

; Subroutine to initialise test environment, display title & get
; colour test results

l28f7:		CALL	l266f		; do some initialisation
		CALL	l291d		; fill attributes, do sound registers
		LD	BC,$0000
		LD	HL,l311e
		CALL	l2703		; output test program title
		CALL	l26af		; wait for ENTER or SPACE
		CALL	l2429		; update results flags
		LD	C,$FD		; some sound stuff
		LD	D,$FF
		LD	E,$BF
		LD	H,$FF
		LD	B,D
		LD	A,$07
		OUT	(C),A
		LD	B,E
		OUT	(C),H
		RET

; Subroutine to fill attributes & set up some sound registers

l291d:		XOR	A
		LD	HL,$5800	; start of attributes
		LD	B,$10		; 16 x 2chars = 1 line
l2923:		LD	(HL),A
		INC	HL
		LD	(HL),A		; colour next two chars
		ADD	A,$08		; increment paper & higher attributes
		INC	HL
		DJNZ	l2923		; back for rest of line
		LD	DE,$5820
		LD	BC,$02DF
		LDIR			; fill rest of attribs with what's on line 1
		LD	C,$FD
		LD	D,$FF
		LD	E,$BF
		LD	HL,l2d0c	; sound data
l293c:		LD	A,(HL)		; loop to set up two sound registers
		INC	HL
		BIT	7,A
		JR	NZ,l294c
		LD	B,D
		OUT	(C),A
		LD	A,(HL)
		INC	HL
		LD	B,E
		OUT	(C),A
		JR	l293c
l294c:		LD	C,$FD		; set up more sound registers
		LD	D,$FF
		LD	E,$BF
		LD	H,$FB
		LD	B,D
		LD	A,$07
		OUT	(C),A
		LD	B,E
		OUT	(C),H
		RET


; ULA test

l295d:		CALL	l271a		; cls
		LD	HL,l324e
		CALL	l2703		; display test message
		LD	DE,$6000
		CALL	xl298c		; copy test routine to RAM at $6000
		CALL	$6000		; execute test
		LD	A,$AA		; test RAM integrity
		LD	($8000),A
		LD	A,($8000)
		CP	$AA
		JR	NZ,l2984	; move on if error
		LD	DE,$8000
		CALL	xl298c		; copy test routine to RAM at $8000
		CALL	$8000		; execute test
l2984:		LD	A,$06
		OUT	($FE),A		; yellow border
		SCF
		JP	l2429		; exit, setting "success" flag

; Subroutine to copy following routine to memory at DE

xl298c:		LD	HL,l2995
		LD	BC,$003B
		LDIR
		RET

; Routine to execute in RAM to test ULA

l2995:		LD	BC,$2000	; $2000 times
l2998:		LD	A,$00		; alternately output $00/$FF to port $FE
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		OUT	($FE),A
		CPL
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,l2998	; loop back for more
		RET

; Joystick test

l29d0:		CALL	l271a		; cls
		LD	HL,l32e0
		CALL	l2703		; display test message
		LD	DE,$1F1F	; initially, no bits reset in D or E ports
l29dc:		PUSH	DE
		LD	BC,$EFFE
		IN	A,(C)
		CPL
		AND	D
		XOR	D
		LD	D,A		; mask in reset bits from port 1 to D
		LD	BC,$F7FE
		IN	A,(C)
		CPL
		AND	E
		XOR	E
		LD	E,A		; mask in reset bits from port 2 to D
		POP	BC
		LD	A,D
		CP	B
		JR	NZ,l2a06	; move on if change in port 1
		LD	A,E
		CP	C
		JR	NZ,l2a06	; move on if change in port 2
		CALL	l2731		; check for SPACE
		JR	C,l29dc
		JR	Z,l29dc
		AND	A
		JR	l2a03		; fail if SPACE pressed
l2a02:		SCF
l2a03:		JP	l2429		; exit, setting success/fail flag
l2a06:		PUSH	DE
		LD	HL,l3530	; table of screen positions
		LD	B,$05		; 5 bits for first joystick port in D
l2a0c:		RRC	D
		CALL	NC,l2a28	; blank screen chars if reset
		INC	HL
		INC	HL
		DJNZ	l2a0c
		LD	B,$05		; 5 bits for second joystick port in E
l2a17:		RRC	E
		CALL	NC,l2a28	; blank screen chars if reset
		INC	HL
		INC	HL
		DJNZ	l2a17
		POP	DE
		LD	A,D
		OR	E
		JR	Z,l2a02		; exit with success once all reset
		JP	l29dc		; back for more

; Joystick subroutine to output two red spaces at position
; in table referenced at HL

l2a28:		PUSH	BC
		LD	B,(HL)
		INC	HL
		LD	C,(HL)		; get position from table
		LD	A,$16		; output spaces at position
		RST	10H
		LD	A,B
		DEC	A
		DEC	A
		DEC	A
		RST	10H
		LD	A,C
		RST	10H
		LD	A,$11
		RST	10H
		LD	A,$02
		RST	10H
		LD	A," "
		RST	10H
		LD	A," "
		RST	10H
		LD	A,$11
		RST	10H
		LD	A,$07
		RST	10H
		DEC	HL
		POP	BC
		RET

; Screen switching test

l2a4b:		CALL	l271a		; cls
		DI
		LD	HL,$5800
		LD	DE,$5801
		LD	BC,$02FF
		LD	(HL),$00
		LDIR			; set page 5 screen attributes to black
		LD	HL,l2acb
		CALL	l2703		; display test message
		LD	HL,l2ae8
		CALL	l2703		; and success - press ENTER message
		LD	A,(BANKM)
		PUSH	AF
		OR	$07
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; switch in page 7
		LD	HL,$4000
		LD	DE,$C000
		LD	BC,$1800
		LDIR			; copy screen into page 7
		LD	HL,l2acb	; display test message
		CALL	l2703
		LD	HL,l2b06	; and fail - press SPACE message
		CALL	l2703
		LD	A,(BANKM)
		SET	3,A
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; switch in alternate screen
		LD	HL,$5800
		LD	DE,$5801
		LD	BC,$02FF
		LD	(HL),$38
		LDIR			; change attribs on normal screen to visible
		LD	HL,$D800
		LD	DE,$D801
		LD	BC,$02FF
		LD	(HL),$38
		LDIR			; and on alternate screen
		CALL	l26af		; get ENTER or SPACE as appropriate
		CALL	l2429		; set success/failure flag
		CALL	l271a		; cls
		POP	AF
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; switch back original memory & screen
		CALL	l271a		; cls
		EI
		RET

l2acb:		DB	$16,$8,$3,$11,$0,$10,$0
		DB	"Screen switching test", $FF
l2ae8:		DB	$16,$0C,$3,$11,$0,$10,$0
		DB	"Passed - press [ENTER]", $FF
l2b06:		DB	$16,$0C,$3,$11,$0,$10,$0
		DB	"Failed - press [SPACE]", $FF

; Cassette output test

l2b24:		CALL	l271a		; cls
		LD	A,$02
		OUT	($FE),A		; red border
		LD	A,$08
		LD	(BORDCR),A
		LD	HL,l32c1
		CALL	l2703		; display test message
		LD	HL,$0100	; ouput a tone to MIC
		LD	DE,$0A00
		DI
		PUSH	IX
		LD	A,L
		SRL	L
l2b42:		SRL	L
		CPL
		AND	$03
		LD	C,A
		LD	B,$00
		LD	IX,l2b5a
		ADD	IX,BC
		LD	A,(BORDCR)
		AND	$38
		RRCA
		RRCA
		RRCA
l2b58:		OR	$10
l2b5a:		NOP
		NOP
		NOP
		INC	B
		INC	C
l2b5f:		DEC	C
		JR	NZ,l2b5f
		LD	C,$3F
		DEC	B
		JP	NZ,l2b5f
		XOR	$08
		OUT	($FE),A
		LD	B,H
		LD	C,A
		BIT	3,A
		JR	NZ,l2b7b
		LD	A,D
		OR	E
		JR	Z,l2b7f
		LD	A,C
		LD	C,L
l2b78:		DEC	DE
		JP	(IX)
l2b7b:		LD	C,L
		INC	C
		JP	(IX)
l2b7f:		POP	IX
		LD	BC,$1200
		LD	HL,l2d21
l2b87:		CALL	l2703		; ask if heard sound
		CALL	l26af		; get ENTER/SPACE
		CALL	l2429		; signal success/failure
		RET


; End of test program - display results

l2b91:		CALL	l271a		; cls
		PUSH	IX
		POP	DE		; DE=results
		LD	A,E
		AND	D
		CP	$FF
		JR	NZ,l2ba9	; move on if any test failed
		LD	A,$04
		OUT	($FE),A		; green border
		LD	HL,x346f
		CALL	l2703		; display success message
		JR	l2bee		; move on
l2ba9:		LD	A,$02
		OUT	($FE),A		; red border
		LD	HL,l3486
		CALL	l2703		; display fail message
		LD	BC,$0807
		PUSH	IX
		POP	DE
		PUSH	DE		; E=low byte of results
		LD	HL,l2ec7	; HL=table of message addresses
		LD	D,$08		; 8 bits
l2bbf:		RR	E		; get next result
		JR	C,l2bce		; move on if passed
		PUSH	HL
		PUSH	DE
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		CALL	l2703		; display appropriate message
		POP	DE
		POP	HL
l2bce:		INC	HL		; get to next table entry
		INC	HL
		DEC	D
		JR	NZ,l2bbf	; back for more bits
		POP	DE
		LD	E,D		; E=high byte of results
		LD	D,$08		; 8 bits
l2bd7:		RR	E		; get next result
		JR	C,l2be9		; move on if passed
		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		CALL	l2703		; display appropriate message
		POP	BC
		POP	DE
		POP	HL
		INC	B
l2be9:		INC	HL		; get to next table entry
		INC	HL
		DEC	D
		JR	NZ,l2bd7	; loop back for more bits
l2bee:		LD	HL,l2c00
		LD	BC,$1000
		CALL	l2703		; display end message
l2bf7:		CALL	l26af		; get SPACE or ENTER
		JR	C,l2bf7		; loop until SPACE
		DI
		JP	l0000		; reset


l2c00:		DB	$0D,$0D
		DB	"Hold [BREAK] to repeat tests", $FF

; ULA sound test

l2c1f:		LD	C,$FD		; some sound stuff
		LD	D,$FF		; to clear AY registers ?
		LD	E,$BF
		LD	B,D
		LD	A,$0E
		OUT	(C),A
		LD	A,$FF
		LD	B,E
		OUT	(C),A
		LD	B,D
		IN	A,(C)
		CP	$FF
		JR	NZ,xl2c6a
		LD	A,$FE
		LD	B,E
		OUT	(C),A
		LD	B,D
		IN	A,(C)
		CP	$7E
		JR	NZ,xl2c6a
		LD	A,$FD
		LD	B,E
		OUT	(C),A
		LD	B,D
		IN	A,(C)
		CP	$BD
		JR	NZ,xl2c6a
		LD	A,$FB
		LD	B,E
		OUT	(C),A
		LD	B,D
		IN	A,(C)
		CP	$DB
		JR	NZ,xl2c6a
		LD	A,$F7
		LD	B,E
		OUT	(C),A
		LD	B,D
		IN	A,(C)
		CP	$E7
		JR	NZ,xl2c6a
		SCF
		JP	l2429		; exit with success
xl2c6a:		OR	A
		JP	l2429		; exit with failure

; ULA sound test part 2

l2c6e:		CALL	l271a		; clear screen
		LD	A,$02
		OUT	($FE),A		; red border
		LD	A,$08
		LD	(BORDCR),A
		LD	HL,l32a6
		CALL	l2703		; display test message
		LD	HL,$0100
		LD	DE,$0A00
		CALL	l2726		; make a beep
		LD	BC,$1200
		LD	HL,l2d21
		CALL	l2703		; ask if heard sound
		CALL	l26af		; get ENTER/SPACE
		CALL	l2429		; set success/fail flag
		RET

; GI Sound Test routine

l2c99:		CALL	l271a		; clear screen
		LD	A,$05
		OUT	($FE),A		; cyan border
		LD	HL,l328d
		CALL	l2703		; display GI message
		LD	C,$FD		; make some sounds
		LD	D,$FF
		LD	E,$BF
		LD	HL,l2d0c
l2caf:		LD	A,(HL)
		INC	HL
		BIT	7,A
		JR	NZ,l2cbf
		LD	B,D
		OUT	(C),A
		LD	A,(HL)
		INC	HL
		LD	B,E
		OUT	(C),A
		JR	l2caf
l2cbf:		LD	C,$FD
		LD	D,$FF
		LD	E,$BF
		LD	L,$03
		LD	H,$FE
l2cc9:		LD	B,D
		LD	A,$07
		OUT	(C),A
		LD	B,E
		OUT	(C),H
		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l269a		; pause
		POP	BC
		POP	DE
		POP	HL
		SCF
		RL	H
		DEC	L
		JR	NZ,l2cc9	; loop back for more
		LD	H,$F8
		LD	B,D
		LD	A,$07
		OUT	(C),A
		LD	B,E
		OUT	(C),H
		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l269a		; pause
		POP	BC
		POP	DE
		POP	HL
		LD	H,$FF
		LD	B,D
		LD	A,$07
		OUT	(C),A
		LD	B,E
		OUT	(C),H
		LD	BC,$0A00
		LD	HL,l34a8
		CALL	l2703		; ask if heard sounds
		CALL	l26af		; get ENTER/SPACE
		JP	l2429		; rotate into flags & exit

l2d0c:		DB	$00,$40,$01,$00
		DB	$02,$80
		DB	$03,$00,$04,$00
		DB	$05,$01,$06,$1F
		DB	$08,$0F,$09,$0F
		DB	$0A,$0F,$80

l2d21:		DB	"Press [ENTER] if you heard the  tone, else press [SPACE]", $FF
l2d5a:		DB	"colour test failed", $0D, $FF
l2d6e:		DB	"ULA sound test failed", $0D, $FF
l2d85:		DB	"Symshft/A key test failed", $0D, $FF
x2da0:		DB	"ULA test failed", $0D, $FF
l2db1:		DB	"RS232 test failed", $0D, $FF
l2dc4:		DB	"GI sound test failed", $0D, $FF
l2dda:		DB	"All-RAM page test failed", $0D, $FF
l2df4:		DB	"Joystick test failed", $0D, $FF
l2e0a:		DB	"IC 7 checksum failed", $0D, $FF
l2e20:		DB	"IC 8 checksum failed", $0D, $FF
l2e36:		DB	"Disk tests failed", $0D, $FF
l2e49:		DB	"Second Screen test failed", $0D, $FF
l2e64:		DB	"Cassette Output failed", $0D, $FF
l2e7c:		DB	"Cassette Input failed", $0D, $FF
l2e93:		DB	"Printer BUSY test failed", $0D, $FF
l2ead:		DB	"Printer DATA test failed", $0D, $FF

; Table of test failure message addresses

l2ec7:		DW	l2e7c
		DW	l2e36
		DW	l2ead
		DW	l2e93
		DW	l2e49
		DW	l2e64
		DW	l2df4
		DW	l2d6e
		DW	l2db1
		DW	x2da0
		DW	l2d85
		DW	l2dc4
		DW	l2dda
		DW	l2e20
		DW	l2e0a
		DW	l2d5a

; Test program messages

l2ee7:		DB	$16,$5,$5,$12,$1
		DB	"ROM TEST FAILED", $12, $0, $0D, $FF
		DB	$16,$5,$5,$12,$1
		DB	"RAM TEST FAILED", $12, $0, $0D, $FF
l2f17:		DB	$16,$2,$0
		DB	" >>> SPECTRUM KEYBOARD TEST <<< ", $0D, $0D
		DB	$14,$1
		DB	"                                 "
		DB	"t i 1 2 3 4 5 6 7 8 9 0   b"
		DB	"                                     "
		DB	"d g   Q W E R T Y U I O P  "
		DB	"                                     "
		DB	"e e   A S D F G H J K L   e"
		DB	"                                     "
		DB	"c c   Z X C V B N M .     c"
		DB	"                                     "
		DB	"s		; ", $22, " < >     S     ^ v , s"
		DB	"                                     "
		DB	"                               "
		DB	$14,$0,$16,$14,$5,$12,$1
		DB	"TEST ALL THE KEYS", $12, $0, $FF
l30d9:		DB	$16,$5,$5,$14,$1
		DB	" SYM SHFT/A  TEST "
		DB	$16,$0A,$5
		DB	"   Press "
		DB	$16,$0C,$5,$12,$1
		DB	"SYM SHFT/A"
		DB	$12,$0,$16,$0E,$5
		DB	" for 1 sec ", $14, $0, $FF
l311e:		DB	$16,$9,$0,$14,$1
		DB	" SPECTRUM +3 test program V 4.1  AMSTRAD 1987..."
		DB	"  by RG/CL/VO ", $0D
		DB	"        Check TV tuning         "
		DB	"connect the loopback connector! ", $0D
		DB	"Press [ENTER] if colour is OK,  "
		DB	"press [SPACE] if it is not      ", $0D
		DB	$12,$1,$0D
		DB	"TAKE CARE - THESE TESTS CORRUPT DISKS, AND REQUIRE"
		DB	" FACTORY TEST EQUIPMENT! "
		DB	$14,$0
		DB	"YOU HAVE BEEN WARNED!"
		DB	$14,$0,$12,$0,$FF
l324e:		DB	$16,$5,$5,$14,$1
		DB	" ULA TEST "
		DB	$14,$0,$0D,$FF
l3261:		DB	$16,$5,$7,$14,$1
		DB	"RAM DATA TESTS", $16, $0A, $5, ".. STARTING NOW .."
		DB	$14,$0,$0D,$FF
l328d:		DB	$16,$5,$5,$14,$1
		DB	" GI SOUND TEST "
		DB	$0D,$0D,$14,$0,$FF
l32a6:		DB	$16,$5,$5,$14,$1
		DB	" ULA SOUND TEST "
		DB	$0D,$0D,$14,$0,$FF,$0
l32c1:		DB	$16,$5,$5,$14,$1
		DB	"CASSETTE OUTPUT TEST"
		DB	$14,$0,$0D,$0D,$FF,$0
l32e0:		DB	$16,$0,$0A,$14,$1
		DB	"JOYSTICK TEST"
		DB	$14,$0,$0D,$0D
		DB	"Move both joysticks and press", $0D
		DB	"the FIRE buttons until the", $0D
		DB	"letters below are wiped out", $0D, $0D
		DB	"Press [SPACE] to give up.", $0D, $0D, $0D, $0D
		DB	"+-----J1-----------J2------+", $0D
		DB	"!                          !", $0D
		DB	"!     UP           UP      !", $0D
		DB	"!                          !", $0D
		DB	"!  LF FI RI     LF FI RI   !", $0D
		DB	"!                          !", $0D
		DB	"!     DN           DN      !", $0D
		DB	"!                          !", $0D
		DB	"+--------------------------+", $0D, $FF
x346f:		DB	$16,$5,$5
		DB	" ALL TESTS PASSED ", $0D, $FF
l3486:		DB	$16,$0,$0,$12,$1
		DB	" TEST FAILED, because:- ", $0D, $12, $0, $0D, $FF
l34a8:		DB	"press [ENTER] if you heard four sounds, "
		DB	"else press [SPACE].     ", $FF

; Subroutine to make a beep. This is a copy of the BEEPER
; subroutine at 03B5 in ROM3

l34e9:		DI
		PUSH	IX
		LD	A,L
		SRL	L
		SRL	L
		CPL
		AND	$03
		LD	C,A
		LD	B,$00
		LD	IX,l3507
		ADD	IX,BC
		LD	A,(BORDCR)
		AND	$38
		RRCA
		RRCA
		RRCA
		OR	$08
l3507:		NOP
l3508:		NOP
		NOP
l350a:		INC	B
		INC	C
l350c:		DEC	C
		JR	NZ,l350c
		LD	C,$3F
		DEC	B
		JP	NZ,l350c
		XOR	$10
		OUT	($FE),A
		LD	B,H
		LD	C,A
		BIT	4,A
		JR	NZ,l3528
		LD	A,D
l3520:		OR	E
		JR	Z,l352c
		LD	A,C
		LD	C,L
		DEC	DE
l3526:		JP	(IX)
l3528:		LD	C,L
		INC	C
l352a:		JP	(IX)
l352c:		EI
		POP	IX
		RET

; Table of screen positions for joystick test

l3530:		DB	$11,$06,$0F,$06
		DB	$13,$06,$11,$09
		DB	$11,$03,$11,$10
		DB	$11,$16,$13,$13
		DB	$0F,$13,$11,$13

; These bits don't seem to be used

l3544:		DB	"FIUPDNRILFLFRIDNUPFI"

; Integral disk test

l3558:		LD	HL,l3566	; address of routine to execute in RAM
		LD	DE,$5F00
		LD	BC,$003D
		LDIR			; copy it
		JP	$5F00		; jump into it

; Integral disk test routine to execute in RAM

l3566:		LD	A,$04
		LD	BC,PBANK678
		OUT	(C),A		; switch in ROM 2
		LD	HL,$6000
		LD	DE,$6000
		LD	BC,$0C00
		LDIR
		LD	A,$00
		LD	BC,PBANK678
		OUT	(C),A		; switch in ROM 0
		LD	(BANK678),A
		LD	A,$10
		LD	B,$7F
		OUT	(C),A		; switch in ROM 1
		LD	(BANKM),A
		EI
		PUSH	IX
		CALL	$6000		; call ROM 2 routine in RAM
		POP	IX
		PUSH	AF		; save success/fail flag
		LD	A,$00
		LD	BC,PBANKM
		OUT	(C),A		; switch in ROM 0
		LD	(BANKM),A
		POP	AF
		CALL	l2429		; set success/fail in flags
		RET			; done

; Tape test

l35a3:		CALL	l271a		; cls
		LD	HL,l362b
		CALL	l2703		; display test message
		CALL	l3612		; short pause
		DI
		LD	HL,$58E1	; set up attribs
		LD	DE,$0006
		LD	B,E
		LD	A,D
l35b8:		LD	(HL),A
		ADD	HL,DE
		DJNZ	l35b8
l35bc:		LD	HL,$0000	; tape testing
		LD	DE,$1000
		LD	C,$FE
		LD	B,$7F
		IN	A,(C)
		BIT	0,A
		JP	Z,l361f		; move on if SPACE pressed
		LD	BC,$BFFE
		IN	A,(C)
		BIT	0,A
		JP	Z,l3625		; move on if ENTER pressed
l35d7:		DEC	DE
		LD	A,D
		OR	E
		JR	Z,l35e7
		IN	A,($FE)
		AND	$40
		CP	C
		JR	Z,l35d7
		INC	HL
		LD	C,A
		JR	l35d7
l35e7:		RL	L
		RL	H
		RL	L
		RL	H
		RL	L
		RL	H
		LD	L,H
		LD	A,$20
		CP	H
		JR	NC,l35fb
		LD	L,$20
l35fb:		XOR	A
		LD	H,A
		LD	DE,$591F
		LD	B,$20
		LD	A,$48
		EI
		HALT
		DI
l3607:		LD	(DE),A
		DEC	DE
		DJNZ	l3607
		INC	DE
		ADD	HL,DE
		LD	A,$68
		LD	(HL),A
		JR	l35bc

; Subroutine to pause for a short while

l3612:		EI
		LD	B,$19
l3615:		HALT
		DJNZ	l3615		; pause
		LD	HL,FLAGS
		res	5,(HL)		; clear "new key" flag
		SCF
		RET

; Set "tape test fail" flag

l361f:		AND	A
		CALL	l2429
		JR	l3612

; Set "tape test succeed" flag

l3625:		SCF
		CALL	l2429
		JR	l3612

l362b:		DB	$16,$0,$0
		DB	"Insert test tape, press PLAY,", $0D
		DB	"and adjust azimuth screw for", $0D
		DB	"maximum reading on screen.", $0D
		DB	"Press [ENTER] if successful,", $0D
		DB	"press [SPACE] if failed", $0D, $FF


; *********** END OF SELF-TEST PROGRAM SECTION ***********
		ENDIF


l36ba:		LD	(HL),H
		RST	18H
		SBC	A,$55
		DJNZ	l3719-7		; (82)
		RET
		SBC	A,(HL)
		SBC	A,(HL)
		CP	L
		LD	H,D
		PUSH	BC
		RET	NZ
		LD	D,L
		JP	NZ,$1044
		RLA

l36cc:		CP	$5F
		SUB	B
		CP	$D1
		DB	$DD
		PUSH	DE

l36d3:		RLA
		SUB	B
		RST	30H
		RST	18H
		RST	18H
		CALL	NC,$D9C7
		SBC	A,$C3
		CP	L
		DI
		CALL	C,$D659
		LD	D,(HL)
		DJNZ	l36f8+4		; (23)
		LD	H,H

l36e6:		LD	B,A
		LD	E,A
		SUB	B

l36e9:		LD	H,B
		RST	18H
		LD	B,B

l36ec:		LD	B,E
		SUB	A
		DJNZ	l36ec		; (-4)

l36f0:		LD	D,C
		RST	$0
		LD	B,E
		LD	E,A
		SBC	A,$BD
		LD	H,(HL)
		EXX

l36f8:		LD	E,E
		SUB	B
		RLA
		JP	po,$54D5
		DJNZ	l36f8		; (-8)
		LD	D,L
		LD	B,D
		LD	B,D

l3703:		LD	E,C
		SBC	A,$57
		RLA
		SUB	B
		LD	A,A

l3709:		LD	E,H
		LD	E,H
		EXX
		ADD	A,$55
		JP	NZ,$5190
		SBC	A,$BD
		CALL	po,$D5D8
		SUB	B
		LD	(HL),H
		LD	E,A

l3719:		JP	NZ,$595B
		LD	E,(HL)
		RST	10H
		SUB	B
		LD	A,L
		LD	E,A
		JP	NC,$64BD
		LD	E,B
		POP	DE

l3726:		LD	E,(HL)
		IN	A,($C3)
		DJNZ	l36f0-1		; (-60)
		LD	E,A
		LD	E,$1E
		CP	L
		LD	H,H
		RET	C
		PUSH	DE
		DJNZ	l3726		; (-14)
		LD	B,D
		PUSH	DE
		LD	B,A
		PUSH	DE
		LD	B,D
		RET
		SUB	B
		CALL	po,$C0D1
		SUB	B
		DB	$18
		DB	88
		EXX
		OUT	($19),A
		CP	L
		JP	po,$DC5F

l3748:		LD	D,C
		SBC	A,$54
		INC	E
		SUB	B
		LD	H,D
		EXX
		OUT	($58),A
		LD	D,C
		JP	NZ,$9054
		LD	D,L
		LD	B,H
		SUB	B
		POP	DE
		LD	E,H
		CP	L
		LD	D,C
		LD	E,(HL)
		LD	D,H
		SUB	B
		LD	SP,HL
		ADD	A,$DF
		LD	B,D
		DJNZ	l36f8+5		; (-104)
		RST	$0
		RET	C
		LD	C,C
		DJNZ	l3748		; (-34)
		LD	E,A

l376b:		CALL	NZ,$BD19
		LD	H,A
		LD	B,D
		EXX
		LD	B,H
		CALL	NZ,$5ED5
		SUB	B
		RST	18H
		LD	E,(HL)
		SUB	B
		LD	H,B
		DI
		LD	H,A
		SUB	B
		ADC	A,B
		DEC	B
		ADD	A,C
		LD	(BC),A
		LD	B,E
		INC	E
		DJNZ	l37ca		; (69)
		LD	B,E
		EXX
		LD	E,(HL)
		CP	L
		DB	$FD
		EX	AF,AF'
		NOP
		SUB	B
		LD	D,C
l378e:		SBC	A,$D4
		DJNZ	l378e		; (-4)
		EX	AF,AF'
		NOP
		DJNZ	l37db		; (69)
		LD	E,(HL)
		CALL	NC,$C255
		DJNZ	l378e+1		; (-13)
		LD	H,B
		SBC	A,A
		DB	$FD
		SBC	A,E
		CP	L
		CP	L
		JP	$C490
		RST	18H
		LD	E,$9E
		CP	L
		LD	H,H
		RET	C
		LD	D,L
		DJNZ	l376b		; (-67)

		DS	$1C

l37ca:		DS	$11

l37db:

	IF garry

		DS	$10
		DB	$16,$00,$00
m14e5:		DB	$10,$00,$11,$07,$13,$00
		DB	"Insert tape and press PLAY", 13
		DB	"To cancel - press BREAK twic", "e"+$80
		DB	0,0,0,0,0,0,0,0

		IF alternative
l3834:			DB	$7F, "1982, 1986, 1987 Amstrad Plc.", 13 + $80
			DB	"                                "
		ELSE
l3834:			DB	$7F, "1982, 1986, 1987 Amstrad Plc.", 13
			DB	$7F, "2000-2009 Garry Lancaster v"
			DB	$30+VMAYOR,"."
			DB	$30+VMINOR,$30+VPATCH+$80
		ENDIF

l3873:		RET	NC
		CALL	l15da
		RET	C
		LD	A,B
		OR	C
		SCF
		RET	Z
		JP	l1604
l387f:		EX	DE,HL
		AND	A
		SBC	HL,DE
		LD	C,L
		LD	B,A
		INC	DE
		INC	DE
		INC	DE
		INC	DE
		LD	A,(DE)
		CP	$50
		JR	Z,l38a2
		INC	DE
		DEC	C
		JR	Z,l389b
		INC	DE
		INC	DE
		EX	DE,HL
		LD	E,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,E
		JR	l38ab
l389b:		EX	DE,HL
		LD	E,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,E
		JR	l3907
l38a2:		LD	HL,l3921
		DEC	C
		JR	Z,l3907
		LD	HL,l3927
l38ab:		res	3, (IY+$02)
		PUSH	HL
		LD	HL,(ERR_SP)
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		AND	A
		LD	HL,o107F
		SBC	HL,DE
		POP	HL
		JR	NZ,l38f8
		LD	SP,(ERR_SP)
		POP	DE
		POP	DE
		LD	(ERR_SP),DE
		PUSH	IX
l38cb:		PUSH	HL
		LD	DE,l38d1
		PUSH	DE
		JP	(HL)
l38d1:		JR	C,l38dc
		JR	Z,l38d9
l38d5:		LD	A,7
		JR	l3917
l38d9:		POP	HL
		JR	l38cb
l38dc:		CP	13
		JR	Z,l38ee
		LD	HL,(RETADDR)
		PUSH	HL
		RST	28H
		DW	$0F85
		POP	HL
		LD	(RETADDR),HL
		POP	HL
		JR	l38cb
l38ee:		POP	HL
		POP	IX
		LD	HL,l15fe
		PUSH	HL
		JP	$5B00
l38f8:		LD	DE,l15fe
		PUSH	DE
		CALL	l3916
		JP	C,$5B00
		JP	Z,$5B00
		JR	l38d5
l3907:		LD	DE,l15fe
		PUSH	DE
		LD	A,B
		PUSH	IX
		CALL	l3916
		POP	IX
		JP	$5B00
l3916:		JP	(HL)
l3917:		LD	HL,(CH_ADD)
		LD	(X_PTR),HL
		LD	L,A
		RST	28H
		DW	o0055
l3921:		CALL	l3e80
		LD	L,(HL)
		RRA
		RET
l3927:		CALL	l3e80
		LD	(HL),B
		LD	E,$C9
l392d:		LD	HL,(CURCHL)
		LD	A,(HL)
		CP	0
		JR	NZ,l3967
		INC	HL
		LD	A,(HL)
		DEC	HL
		CP	$5B
		JR	NZ,l3967
		INC	HL
		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		INC	HL
		CP	$50
		JR	NZ,l3955
		LD	A,E
		CP	4
		JR	Z,l399c
		CP	2
		JP	Z,l3927
		EXX
		LD	A,C
		JP	l3921
l3955:		LD	A,E
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		CP	2
		JR	Z,l3962
		EXX
		LD	A,C
		RET
l3962:		LD	HL,l3996
		EX	(SP),HL
		JP	(HL)
l3967:		LD	A,E
		CP	4
		JR	Z,l399c
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		LD	HL,(RETADDR)
		PUSH	HL
		CP	2
		JR	Z,l3989
		LD	HL,l3985
		LD	(RETADDR),HL
		EX	DE,HL
		EXX
		LD	A,C
		EXX
		JP	l00ae
l3985:		LD	(RETADDR),HL
		RET
l3989:		LD	HL,l3993
		LD	(RETADDR),HL
		EX	DE,HL
		JP	l00ae
l3993:		LD	(RETADDR),HL
l3996:		RET	C
		LD	DE,2
		JR	l392d
l399c:		LD	A,$12
		JP	l3917
l39a1:		LD	HL,(CURCHL)
		LD	DE,13
		ADD	HL,DE
		LD	A,(HL)
		PUSH	HL
		LD	HL,(VARS)
		LD	C,A
l39ae:		LD	A,(HL)
		CP	$80
		JR	Z,l399c
		CP	C
		JR	Z,l39be
		PUSH	BC
		RST	28H
		DW	o19B8
		POP	BC
		EX	DE,HL
		JR	l39ae
l39be:		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		INC	HL
		DEC	A
		JR	NZ,l399c
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		EX	DE,HL
		POP	HL
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		PUSH	HL
		AND	A
		SBC	HL,BC
		POP	HL
		RET
l39d7:		CALL	l39a1
		JP	NC,l38d5
		ADD	HL,DE
		LD	A,(HL)
l39df:		LD	HL,(CURCHL)
		LD	DE,14
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	DE
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		SCF
		RET
l39ef:		PUSH	AF
		CALL	l39a1
		JP	NC,l38d5
		ADD	HL,DE
		POP	AF
		LD	(HL),A
		JR	l39df
l39fb:		PUSH	HL
		PUSH	DE
		LD	A,B
		PUSH	AF
		CALL	l39a1
		POP	AF
		AND	A
		JR	NZ,l3a0c
		POP	BC
		POP	BC
		LD	DE,0
		RET
l3a0c:		DEC	A
		JR	NZ,l3a29
		POP	HL
		LD	A,H
		OR	L
		JP	NZ,l38d5
		POP	HL
		AND	A
		SBC	HL,BC
		JP	NC,l38d5
		ADD	HL,BC
		EX	DE,HL
		LD	HL,(CURCHL)
		LD	BC,14
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D
		RET
l3a29:		POP	HL
		POP	HL
		LD	H,B
		LD	L,C
		LD	DE,0
		RET

FREE_ROM0_4:	EQU	$

		;...
		;...

R0_FREE_4:	EQU	$03CF-($-FREE_ROM0_4)
ROM0_SPARE4:	DS	R0_FREE_4

	ELSE

		IF spanish		; acá hay una culada de espacio libre pero
			DS	$040A	; no es para +3e, de modo que en principio
		ELSE			; no me interesa
			DS	$0625
		ENDIF

	ENDIF

		LD	(OLDHL),HL
		PUSH	AF
		POP	HL

l3e05:		LD	(OLDAF),HL
		EX	(SP),HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		EX	(SP),HL
		PUSH	BC
		POP	HL
		LD	A,(BANK678)
		LD	BC,PBANK678
		res	2,A
		DI
		LD	(BANK678),A
		OUT	(C),A
		EI
		LD	BC,$3E2D
		PUSH	BC
		PUSH	HL
		LD	HL,(OLDAF)
		PUSH	HL
		POP	AF
		LD	HL,(OLDHL)
		RET
		PUSH	BC
		PUSH	AF
		LD	A,(BANK678)
		LD	BC,PBANK678
		SET	2,A
		DI
		LD	(BANK678),A
		OUT	(C),A
		EI
		POP	AF
		POP	BC
		RET
		IF garry
l3e41:		LD	HL,(CURCHL)	; get address of current channel information
		LD	DE,13
		ADD	HL,DE
		LD	B,(HL)
		POP	HL
		CALL	l05cc
		JP	(HL)
l3e4e:		EXX
		CALL	l3e41
		PUSH	BC
		EXX
		LD	A,B
		POP	BC
		AND	A
		JR	Z,l3e63
		DEC	A
		JR	Z,l3e6c
		CALL	l3ec9
		ADD	HL,SP
		LD	BC,$0518
l3e63:		CALL	l3ec9
		INC	SP
		LD	BC,22
		JR	l3ee9
l3e6c:		CALL	l3ec9
		LD	(HL),1
		JR	l3ee9
		CALL	l3e41
		CALL	l3ec9
		JR	l3e7c
l3e7b:		LD	A,C
l3e7c:		JR	l3ee9
		DS	2
		ELSE
		DS	$3F
		ENDIF
; Subroutine to call a subroutine in ROM 1
; The address to call is stored inline after the call to this routine

l3e80:		LD	(OLDHL),HL	; save HL in OLDHL
		LD	(OLDBC),BC	; save BC in OLDBC
		PUSH	AF
		POP	HL
		LD	(OLDAF),HL	; save AF in OLDAF
		EX	(SP),HL		; HL=return address
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=inline address for ROM 1
		INC	HL
		EX	(SP),HL		; restore proper return address
		PUSH	BC
		POP	HL		; HL=address in ROM 1 to call
		LD	A,(BANKM)
		OR	$10
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; page in ROM 1

; The rest of the routine continues at $3EA2 in ROM 1
; The following is a continuation of a mirrored routine in ROM 1 for
; returning to this ROM

l3ea2:		EI
		LD	BC,$3EB5
		PUSH	BC		; stack return add to swap back ROMS
		PUSH	HL		; stack address of routine to call
		LD	HL,(OLDAF)
		PUSH	HL
		POP	AF		; restore AF
		LD	BC,(OLDBC)	; restore BC
		LD	HL,(OLDHL)	; restore HL
		RET			; exit to routine in this ROM

; This is the routine which returns control to the calling routine in ROM 1

l3eb5:		PUSH	AF		; save AF & BC
		PUSH	BC
		LD	A,(BANKM)
		OR	$10
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; page back ROM 1

; The rest of the routine continues at $3EC5 in ROM 1
; The following is a continuation of a mirrored routine in ROM 1 for
; returning to this ROM

l3ec5:		EI
		POP	BC
		POP	AF
		RET

		IF garry
l3ec9:		LD	(OLDHL),HL	; save HL in OLDHL
		LD	(OLDBC),BC	; save BC in OLDBC
		PUSH	AF
		POP	HL
		LD	(OLDAF),HL	; save AF in OLDAF
		EX	(SP),HL		; HL=return address
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=inline address for ROM 1
		INC	HL
		EX	(SP),HL		; restore proper return address
		LD	HL,l3f63
		JR	l3f15
l3ee0:		LD	C,A
		CALL	l3e41
		CALL	l3ec9
		DEC	DE
		DB	1		; ld bc, xxxx
l3ee9:		CALL	l05a7
		RET	C
		LD	A,$12
		JP	l3917
		DS	14
		ELSE
		DS	$37
		ENDIF

; Subroutine to call a subroutine in ROM 2, with inline address
; This routine is not used in this ROM, but is a duplicate of a
; routine in ROM 1, which takes over during ROM switching to ROM 2
; via this ROM, and back again at the end.

l3f00:		LD	(OLDHL),HL	; save HL, BC and AF
		LD	(OLDBC),BC
		PUSH	AF
		POP	HL
		LD	(OLDAF),HL
		EX	(SP),HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=inline address
		INC	HL
		EX	(SP),HL		; restack updated return address
		IF garry
		LD	HL,l3f42
l3f15:		PUSH	HL
		ENDIF
		PUSH	BC
		POP	HL
		LD	A,(BANKM)
		AND	$EF
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; switch in ROM 0
		LD	A,(BANK678)
		OR	$04
		LD	(BANK678),A
		LD	BC,PBANK678
		OUT	(C),A		; switch in ROM 2
		EI
		IF garry=0
		LD	BC,l3f42
		PUSH	BC		; stack routine address to return to ROM 1
		ENDIF
		PUSH	HL		; stack routine address to call in ROM 2
		LD	HL,(OLDAF)	; restore registers
		PUSH	HL
		POP	AF
		LD	BC,(OLDBC)
		LD	HL,(OLDHL)
		RET			; exit to routine

; This part of the routine then returns control to ROM 1

l3f42:		PUSH	BC		; save registers
		PUSH	AF
		LD	A,(BANK678)
		AND	$FB
		DI
		LD	(BANK678),A
		LD	BC,PBANK678
		OUT	(C),A		; page in ROM 0
		LD	A,(BANKM)
		OR	$10
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; page in ROM 1
		EI
		POP	AF		; restore registers
		POP	BC
		RET			; done!

		IF garry
l3f63:		PUSH	BC
		PUSH	AF
		LD	A,(BANK678)
		AND	$FB
		DI
		LD	(BANK678),A
		LD	BC,$1FFD
		OUT	(C),A
		EI
		POP	AF
		POP	BC
		RET
l3f77:		EXX
l3f78:		PUSH	HL
		PUSH	BC
		PUSH	HL
		LD	HL,(RETADDR)
		EX	(SP),HL
		RST	28H
		DW	o19E8
		POP	HL
		LD	(RETADDR),HL
		POP	BC
		POP	HL
		LD	DE,(CHANS)
		AND	A
		SBC	HL,DE
		PUSH	HL
		LD	A,$13
		LD	HL,STRMS
l3f95:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	(SP),HL
		AND	A
		SBC	HL,DE
		ADD	HL,DE
		JR	NC,l3fa4
		EX	DE,HL
		AND	A
		SBC	HL,BC
		EX	DE,HL
l3fa4:		EX	(SP),HL
		DEC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		DEC	A
		JR	NZ,l3f95
		POP	HL
		RET
l3faf:		EXX
l3fb0:		PUSH	HL
		PUSH	BC
		PUSH	HL
		LD	HL,(RETADDR)
		EX	(SP),HL
		RST	28H
		DW	o1655
		POP	HL
		LD	(RETADDR),HL
		LD	HL,(DATADD)
		LD	DE,(PROG)
		DEC	DE
		AND	A
		SBC	HL,DE
		JR	NC,l3fcf
		LD	(DATADD),DE
l3fcf:		POP	BC
		POP	HL
		LD	DE,(CHANS)
		AND	A
		SBC	HL,DE
		PUSH	HL
		LD	A,$13
		LD	HL,STRMS
l3fde:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	(SP),HL
		AND	A
		SBC	HL,DE
		ADD	HL,DE
		JR	NC,l3fec
		EX	DE,HL
		AND	A
		ADD	HL,BC
		EX	DE,HL
l3fec:		EX	(SP),HL
		DEC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		DEC	A
		JR	NZ,l3fde
		POP	HL
		RET
		ELSE
		DS	$8D
		ENDIF

; This routine is called from ROM 2 to display error messages, and
; optionally get a response

l3ff0:		JP	l2187		; go to the routine

		IF garry
		DB	0,0,0,0,0,0
		ELSE
		DB	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
		DB	$FF,$FF,$FF,$FF

		IF spanish
		DB	$B6
		ELSE
		DB	$1F
		ENDIF

		ENDIF
