		OUTPUT	"p3t_rom3.rom"

		ORG	$0000

/*
Esta ROM contiene mi extensión para cargar archivos TAPs, y no necesariamente para las ROMS +3e,
se podría usar para un +3 normal con las modificaciones necesarias. (se hizo uso casi del 100%
del espacio no usado en esta ROM)

Se requieren ligeras modificaciones en ROM1 (sintaxis) para aceptar el comando:
SPECTRUM "archivo.tap"

LA ROM 3 DEL PROY +3E EN INGLES ES BINARIAMENTE IGUAL AL PRODUCTO DE ESTE FUENTE
buscar **** disponible **** para analizar los cambios
*/

;************************************************************************
;** An Assembly File Listing to generate a 16K ROM for the ZX Spectrum **
;************************************************************************

; -------------------------
; Last updated: 05-FEB-2012
; -------------------------

;   It is always a good idea to anchor, using ORGs, important sections such as
;   the character bitmaps so that they don't move as code is added and removed.

;   Generally most approaches try to maintain main entry points as they are
;   often used by third-party software.

; System variable definitions for 48K Spectrum

		ORG	$0000

;*****************************************
;** Part 1. RESTART ROUTINES AND TABLES **
;*****************************************

; -----------
; THE 'START'
; -----------
;   At switch on, the Z80 chip is in Interrupt Mode 0.
;   The Spectrum uses Interrupt Mode 1.
;   This location can also be 'called' to reset the machine.
;   Typically with PRINT USR 0.

;; START
o0000:		DI
		XOR	A		; Signal coming from START.
		LD	DE,$FFFF	; Set pointer to top of possible physical RAM.
		JP	o11CB		; Jump forward to common code at START-NEW.

; -------------------
; THE 'ERROR' RESTART
; -------------------
;   The error pointer is made to point to the position of the error to enable
;   the editor to highlight the error position if it occurred during syntax
;   checking.  It is used at 37 places in the program.  An instruction fetch
;   on address $0008 may page in a peripheral ROM such as the Sinclair
;   Interface 1 or Disciple Disk Interface.  This was not an original design
;   concept and not all errors pass through here.

;; ERROR-1
o0008:		LD	HL,(CH_ADD)	; Fetch the character address from CH_ADD.
		LD	(X_PTR),HL	; Copy it to the error pointer X_PTR.
		JR	o0053		; Forward to continue at ERROR-2.

; -----------------------------
; THE 'PRINT CHARACTER' RESTART
; -----------------------------
;   The A register holds the code of the character that is to be sent to
;   the output stream of the current channel.  The alternate register set is
;   used to output a character in the A register so there is no need to
;   preserve any of the current main registers (HL, DE, BC).
;   This restart is used 21 times.

;; PRINT-A
o0010:		JP	o15F2		; Jump forward to continue at PRINT-A-2.
		DB	$A7,$FF		; Five unused locations.
		DB	$FF,$FF,$FF	;

; -------------------------------
; THE 'COLLECT CHARACTER' RESTART
; -------------------------------
;   The contents of the location currently addressed by CH_ADD are fetched.
;   A return is made if the value represents a character that has
;   relevance to the BASIC parser. Otherwise CH_ADD is incremented and the
;   tests repeated. CH_ADD will be addressing somewhere -
;   1) in the BASIC program area during line execution.
;   2) in workspace if evaluating, for example, a string expression.
;   3) in the edit buffer if parsing a direct command or a new BASIC line.
;   4) in workspace if accepting input but not that from INPUT LINE.

;; GET-CHAR
o0018:		LD	HL,(CH_ADD)	; fetch the address from CH_ADD.
		LD	A,(HL)		; use it to pick up current character.

;; TEST-CHAR
o001C:		CALL	o007D		; routine SKIP-OVER tests if the character is
					; relevant.
		RET	NC		; Return if it is significant.

; ------------------------------------
; THE 'COLLECT NEXT CHARACTER' RESTART
; ------------------------------------
;   As the BASIC commands and expressions are interpreted, this routine is
;   called repeatedly to step along the line.  It is used 83 times.

;; NEXT-CHAR
o0020:		CALL	o0074		; routine CH-ADD+1 fetches the next immediate
					; character.
		JR	o001C		; jump back to TEST-CHAR until a valid
					; character is found.

; ---

		DB	$FF,$FF,$FF	; unused

; -----------------------
; THE 'CALCULATE' RESTART
; -----------------------
;   This restart enters the Spectrum's internal, floating-point, stack-based,
;   FORTH-like language.
;   It is further used recursively from within the calculator.
;   It is used on 77 occasions.

;; FP-CALC
o0028:		JP	o335B		; jump forward to the CALCULATE routine.

; ---

		DB	$FF,$FF,$FF	; spare - note that on the ZX81, space being a
		DB	$FF,$FF		; little cramped, these same locations were
					; used for the five-byte end-calc literal.

; ------------------------------
; THE 'CREATE BC SPACES' RESTART
; ------------------------------
;   This restart is used on only 12 occasions to create BC spaces
;   between workspace and the calculator stack.

;; BC-SPACES
o0030:		PUSH	BC		; Save number of spaces.
		LD	HL,(WORKSP)	; Fetch WORKSP.
		PUSH	HL		; Save address of workspace.
		JP	o169E		; Jump forward to continuation code RESERVE.

; --------------------------------
; THE 'MASKABLE INTERRUPT' ROUTINE
; --------------------------------
;   This routine increments the Spectrum's three-byte FRAMES counter fifty
;   times a second (sixty times a second in the USA ).
;   Both this routine and the called KEYBOARD subroutine use the IY register
;   to access system variables and flags so a user-written program must
;   disable interrupts to make use of the IY register.

;; MASK-INT
o0038:		PUSH	AF		; Save the registers that will be used but not
		PUSH	HL		; the IY register unfortunately.
		LD	HL,(FRAMES)	; Fetch the first two bytes at FRAMES1.
		INC	HL		; Increment lowest two bytes of counter.
		LD	(FRAMES),HL	; Place back in FRAMES1.
		LD	A,H		; Test if the result was zero.
		OR	L		;
		JR	NZ,o0048	; Forward, if not, to KEY-INT

		INC	(IY+$40)	; otherwise increment FRAMES3 the third byte.

;   Now save the rest of the main registers and read and decode the keyboard.

;; KEY-INT
o0048:		PUSH	BC		; Save the other main registers.
		PUSH	DE		;

		CALL	o386E		; Routine KEYBOARD executes a stage in the
					; process of reading a key-press.
		POP	DE		;
		POP	BC		; Restore registers.

		POP	HL		;
		POP	AF		;

		EI			; Enable Interrupts.
		RET			; Return.

; ---------------------
; THE 'ERROR-2' ROUTINE
; ---------------------
;   A continuation of the code at 0008.
;   The error code is stored and after clearing down stacks, an indirect jump
;   is made to MAIN-4, etc. to handle the error.

;; ERROR-2
o0053:		POP	HL		; drop the return address - the location
					; after the RST 08H instruction.
		LD	L,(HL)		; fetch the error code that follows.
					; (nice to see this instruction used.)

;   Note. this entry point is used when out of memory at REPORT-4.
;   The L register has been loaded with the report code but X-PTR is not
;   updated.

;; ERROR-3
o0055:		LD	(IY+$00),L	; Store it in the system variable ERR_NR.
		LD	SP,(ERR_SP)	; ERR_SP points to an error handler on the
					; machine stack. There may be a hierarchy
					; of routines.
					; To MAIN-4 initially at base.
					; or REPORT-G on line entry.
					; or ED-ERROR when editing.
					; or ED-FULL during ed-enter.
					; or IN-VAR-1 during runtime input etc.

		JP	o16C5		; Jump to SET-STK to clear the calculator stack
					; and reset MEM to usual place in the systems
					; variables area and then indirectly to MAIN-4,
					; etc.

; ------------------------------------
; THE 'NON-MASKABLE INTERRUPT' ROUTINE
; ------------------------------------
;
;   There is no NMI switch on the standard Spectrum or its peripherals.
;   When the NMI line is held low, then no matter what the Z80 was doing at
;   the time, it will now execute the code at 66 Hex.
;   This Interrupt Service Routine will jump to location zero if the contents
;   of the system variable NMIADD are zero or return if the location holds a
;   non-zero address.   So attaching a simple switch to the NMI as in the book
;   "Spectrum Hardware Manual" causes a reset.  The logic was obviously
;   intended to work the other way.  Sinclair Research said that, since they
;   had never advertised the NMI, they had no plans to fix the error "until
;   the opportunity arose".
;
;   Note. The location NMIADD was, in fact, later used by Sinclair Research
;   to enhance the text channel on the ZX Interface 1.
;   On later Amstrad-made Spectrums, and the Brazilian Spectrum, the logic of
;   this routine was indeed reversed but not as at first intended.
;
;   It can be deduced by looking elsewhere in this ROM that the NMIADD system
;   variable pointed to o121C and that this enabled a Warm Restart to be
;   performed at any time, even while playing machine code games, or while
;   another Spectrum has been allowed to gain control of this one.
;
;   Software houses would have been able to protect their games from attack by
;   placing two zeros in the NMIADD system variable.

;; RESET
o005F:		DB	$FF,$FF,$FF	; Unused locations
		DB	$FF,$FF,$FF	; before the fixed-position
		DB	$FF		; NMI routine.
o0066:		PUSH	AF		; save the
		PUSH	HL		; registers.
		LD	HL,(NMIADD)	; fetch the system variable NMIADD.
		LD	A,H		; test address
		OR	L		; for zero.

		JR	Z,o0070		; skip to NO-RESET if ZERO
		JP	(HL)		; jump to routine ( i.e. o0000 )

;; NO-RESET
o0070:		POP	HL		; restore the
		POP	AF		; registers.
		RETN			; return to previous interrupt state.

; ---------------------------
; THE 'CH ADD + 1' SUBROUTINE
; ---------------------------
;   This subroutine is called from RST 20, and three times from elsewhere
;   to fetch the next immediate character following the current valid character
;   address and update the associated system variable.
;   The entry point TEMP-PTR1 is used from the SCANNING routine.
;   Both TEMP-PTR1 and TEMP-PTR2 are used by the READ command routine.

;; CH-ADD+1
o0074:		LD	HL,(CH_ADD)	; fetch address from CH_ADD.

;; TEMP-PTR1
o0077:		INC	HL		; increase the character address by one.

;; TEMP-PTR2
o0078:		LD	(CH_ADD),HL	; update CH_ADD with character address.
		LD	A,(HL)		; load character to A from HL.
		RET			; and return.

; --------------------------
; THE 'SKIP OVER' SUBROUTINE
; --------------------------
;   This subroutine is called once from RST 18 to skip over white-space and
;   other characters irrelevant to the parsing of a BASIC line etc. .
;   Initially the A register holds the character to be considered
;   and HL holds its address which will not be within quoted text
;   when a BASIC line is parsed.
;   Although the 'tab' and 'at' characters will not appear in a BASIC line,
;   they could be present in a string expression, and in other situations.
;   Note. although white-space is usually poaced in a program to indent loops
;   and make it more readable, it can also be used for the opposite effect and
;   spaces may appear in variable names although the parser never sees them.
;   It is this routine that helps make the variables 'Anum bEr5 3BUS' and
;   'a number 53 bus' appear the same to the parser.

;; SKIP-OVER
o007D:		CP	$21		; test if higher than space.
		RET	NC		; return with carry clear if so.

		CP	$0D		; carriage return ?
		RET	Z		; return also with carry clear if so.

; all other characters have no relevance
; to the parser and must be returned with
; carry set.

		CP	$10		; test if 0-15d
		RET	C		; return, if so, with carry set.

		CP	$18		; test if 24-32d
		CCF			; complement carry flag.
		RET	C		; return with carry set if so.

; now leaves 16d-23d

		INC	HL		; all above have at least one extra character
					; to be stepped over.

		CP	$16		; controls 22d ('at') and 23d ('tab') have two.
		JR	C,o0090		; forward to SKIPS with ink, paper, flash,
					; bright, inverse or over controls.
					; Note. the high byte of tab is for RS232 only.
					; it has no relevance on this machine.

		INC	HL		; step over the second character of 'at"/"tab'.

;; SKIPS
o0090:		SCF
		LD	(CH_ADD),HL	; update the CH_ADD system variable.
		RET			; return with carry set.


; ------------------
; THE 'TOKEN' TABLES
; ------------------
;   The tokenized characters 134d (RND) to 255d (COPY) are expanded using
;   this table. The last byte of a token is inverted to denote the end of
;   the word. The first is an inverted step-over byte.

;; TKN-TABLE
o0095:		DB	"?"+$80
		DM	"RN"
		DB	"D"+$80
		DM	"INKEY"
		DB	"$"+$80
		DB	"P","I"+$80
		DB	"F","N"+$80
		DM	"POIN"
		DB	"T"+$80
		DM	"SCREEN"
		DB	"$"+$80
		DM	"ATT"
		DB	"R"+$80
		DB	"A","T"+$80
		DM	"TA"
		DB	"B"+$80
		DM	"VAL"
		DB	"$"+$80
		DM	"COD"
		DB	"E"+$80
		DM	"VA"
		DB	"L"+$80
		DM	"LE"
		DB	"N"+$80
		DM	"SI"
		DB	"N"+$80
		DM	"CO"
		DB	"S"+$80
		DM	"TA"
		DB	"N"+$80
		DM	"AS"
		DB	"N"+$80
		DM	"AC"
		DB	"S"+$80
		DM	"AT"
		DB	"N"+$80
		DB	"L","N"+$80
		DM	"EX"
		DB	"P"+$80
		DM	"IN"
		DB	"T"+$80
		DM	"SQ"
		DB	"R"+$80
		DM	"SG"
		DB	"N"+$80
		DM	"AB"
		DB	"S"+$80
		DM	"PEE"
		DB	"K"+$80
		DB	"I","N"+$80
		DM	"US"
		DB	"R"+$80
		DM	"STR"
		DB	"$"+$80
		DM	"CHR"
		DB	"$"+$80
		DM	"NO"
		DB	"T"+$80
		DM	"BI"
		DB	"N"+$80

;   The previous 32 function-type words are printed without a leading space
;   The following have a leading space if they begin with a letter

		DB	"O","R"+$80
		DM	"AN"
		DB	"D"+$80
		DB	$3C,"="+$80	; <=
		DB	$3E,"="+$80	; >=
		DB	$3C,$3E+$80	; <>
		DM	"LIN"
		DB	"E"+$80
		DM	"THE"
		DB	"N"+$80
		DB	"T","O"+$80
		DM	"STE"
		DB	"P"+$80
		DM	"DEF F"
		DB	"N"+$80
		DM	"CA"
		DB	"T"+$80
		DM	"FORMA"
		DB	"T"+$80
		DM	"MOV"
		DB	"E"+$80
		DM	"ERAS"
		DB	"E"+$80
		DM	"OPEN "
		DB	"#"+$80
		DM	"CLOSE "
		DB	"#"+$80
		DM	"MERG"
		DB	"E"+$80
		DM	"VERIF"
		DB	"Y"+$80
		DM	"BEE"
		DB	"P"+$80
		DM	"CIRCL"
		DB	"E"+$80
		DM	"IN"
		DB	"K"+$80
		DM	"PAPE"
		DB	"R"+$80
		DM	"FLAS"
		DB	"H"+$80
		DM	"BRIGH"
		DB	"T"+$80
		DM	"INVERS"
		DB	"E"+$80
		DM	"OVE"
		DB	"R"+$80
		DM	"OU"
		DB	"T"+$80
		DM	"LPRIN"
		DB	"T"+$80
		DM	"LLIS"
		DB	"T"+$80
		DM	"STO"
		DB	"P"+$80
		DM	"REA"
		DB	"D"+$80
		DM	"DAT"
		DB	"A"+$80
		DM	"RESTOR"
		DB	"E"+$80
		DM	"NE"
		DB	"W"+$80
		DM	"BORDE"
		DB	"R"+$80
		DM	"CONTINU"
		DB	"E"+$80
		DM	"DI"
		DB	"M"+$80
		DM	"RE"
		DB	"M"+$80
		DM	"FO"
		DB	"R"+$80
		DM	"GO T"
		DB	"O"+$80
		DM	"GO SU"
		DB	"B"+$80
		DM	"INPU"
		DB	"T"+$80
		DM	"LOA"
		DB	"D"+$80
		DM	"LIS"
		DB	"T"+$80
		DM	"LE"
		DB	"T"+$80
		DM	"PAUS"
		DB	"E"+$80
		DM	"NEX"
		DB	"T"+$80
		DM	"POK"
		DB	"E"+$80
		DM	"PRIN"
		DB	"T"+$80
		DM	"PLO"
		DB	"T"+$80
		DM	"RU"
		DB	"N"+$80
		DM	"SAV"
		DB	"E"+$80
		DM	"RANDOMIZ"
		DB	"E"+$80
		DB	"I","F"+$80
		DM	"CL"
		DB	"S"+$80
		DM	"DRA"
		DB	"W"+$80
		DM	"CLEA"
		DB	"R"+$80
		DM	"RETUR"
		DB	"N"+$80
		DM	"COP"
		DB	"Y"+$80

; ----------------
; THE 'KEY' TABLES
; ----------------
;   These six look-up tables are used by the keyboard reading routine
;   to decode the key values.
;
;   The first table contains the maps for the 39 keys of the standard
;   40-key Spectrum keyboard. The remaining key [SHIFT $27] is read directly.
;   The keys consist of the 26 upper-case alphabetic characters, the 10 digit
;   keys and the space, ENTER and symbol shift key.
;   Unshifted alphabetic keys have $20 added to the value.
;   The keywords for the main alphabetic keys are obtained by adding $A5 to
;   the values obtained from this table.

;; MAIN-KEYS
o0205:		DB	$42		; B
		DB	$48		; H
		DB	$59		; Y
		DB	$36		; 6
		DB	$35		; 5
		DB	$54		; T
		DB	$47		; G
		DB	$56		; V
		DB	$4E		; N
		DB	$4A		; J
		DB	$55		; U
		DB	$37		; 7
		DB	$34		; 4
		DB	$52		; R
		DB	$46		; F
		DB	$43		; C
		DB	$4D		; M
		DB	$4B		; K
		DB	$49		; I
		DB	$38		; 8
		DB	$33		; 3
		DB	$45		; E
		DB	$44		; D
		DB	$58		; X
		DB	$0E		; SYMBOL SHIFT
		DB	$4C		; L
		DB	$4F		; O
		DB	$39		; 9
		DB	$32		; 2
		DB	$57		; W
		DB	$53		; S
		DB	$5A		; Z
		DB	$20		; SPACE
		DB	$0D		; ENTER
		DB	$50		; P
		DB	$30		; 0
		DB	$31		; 1
		DB	$51		; Q
		DB	$41		; A


;; E-UNSHIFT
;  The 26 unshifted extended mode keys for the alphabetic characters.
;  The green keywords on the original keyboard.
o022C:		DB	$E3		; READ
		DB	$C4		; BIN
		DB	$E0		; LPRINT
		DB	$E4		; DATA
		DB	$B4		; TAN
		DB	$BC		; SGN
		DB	$BD		; ABS
		DB	$BB		; SQR
		DB	$AF		; CODE
		DB	$B0		; VAL
		DB	$B1		; LEN
		DB	$C0		; USR
		DB	$A7		; PI
		DB	$A6		; INKEY$
		DB	$BE		; PEEK
		DB	$AD		; TAB
		DB	$B2		; SIN
		DB	$BA		; INT
		DB	$E5		; RESTORE
		DB	$A5		; RND
		DB	$C2		; CHR$
		DB	$E1		; LLIST
		DB	$B3		; COS
		DB	$B9		; EXP
		DB	$C1		; STR$
		DB	$B8		; LN


;; EXT-SHIFT
;  The 26 shifted extended mode keys for the alphabetic characters.
;  The red keywords below keys on the original keyboard.
o0246:		DB	$7E		; ~
		DB	$DC		; BRIGHT
		DB	$DA		; PAPER
		DB	$5C		; \
		DB	$B7		; ATN
		DB	$7B		; {
		DB	$7D		; }
		DB	$D8		; CIRCLE
		DB	$BF		; IN
		DB	$AE		; VAL$
		DB	$AA		; SCREEN$
		DB	$AB		; ATTR
		DB	$DD		; INVERSE
		DB	$DE		; OVER
		DB	$DF		; OUT
		DB	$7F		; (Copyright character)
		DB	$B5		; ASN
		DB	$D6		; VERIFY
		DB	$7C		; |
		DB	$D5		; MERGE
		DB	$5D		; ]
		DB	$DB		; FLASH
		DB	$B6		; ACS
		DB	$D9		; INK
		DB	$5B		; [
		DB	$D7		; BEEP


;; CTL-CODES
;  The ten control codes assigned to the top line of digits when the shift
;  key is pressed.
o0260:		DB	$0C		; DELETE
		DB	$07		; EDIT
		DB	$06		; CAPS LOCK
		DB	$04		; TRUE VIDEO
		DB	$05		; INVERSE VIDEO
		DB	$08		; CURSOR LEFT
		DB	$0A		; CURSOR DOWN
		DB	$0B		; CURSOR UP
		DB	$09		; CURSOR RIGHT
		DB	$0F		; GRAPHICS


;; SYM-CODES
;  The 26 red symbols assigned to the alphabetic characters of the keyboard.
;  The ten single-character digit symbols are converted without the aid of
;  a table using subtraction and minor manipulation.
o026A:		DB	$E2		; STOP
		DB	$2A		; *
		DB	$3F		; ?
		DB	$CD		; STEP
		DB	$C8		; >=
		DB	$CC		; TO
		DB	$CB		; THEN
		DB	$5E		; ^
		DB	$AC		; AT
		DB	$2D		; -
		DB	$2B		; +
		DB	$3D		; =
		DB	$2E		; .
		DB	$2C		; ,
		DB	$3B		; ;
		DB	$22		; "
		DB	$C7		; <=
		DB	$3C		; <
		DB	$C3		; NOT
		DB	$3E		; >
		DB	$C5		; OR
		DB	$2F		; /
		DB	$C9		; <>
		DB	$60		; pound
		DB	$C6		; AND
		DB	$3A		; :

;; E-DIGITS
;  The ten keywords assigned to the digits in extended mode.
;  The remaining red keywords below the keys.
o0284:		DB	$D0		; FORMAT
		DB	$CE		; DEF FN
		DB	$A8		; FN
		DB	$CA		; LINE
		DB	$D3		; OPEN #
		DB	$D4		; CLOSE #
		DB	$D1		; MOVE
		DB	$D2		; ERASE
		DB	$A9		; POINT
		DB	$CF		; CAT


;*******************************
;** Part 2. KEYBOARD ROUTINES **
;*******************************

;   Using shift keys and a combination of modes the Spectrum 40-key keyboard
;   can be mapped to 256 input characters

; ---------------------------------------------------------------------------
;
;         0     1     2     3     4 -Bits-  4     3     2     1     0
; PORT                                                                    PORT
;
; F7FE  [ 1 ] [ 2 ] [ 3 ] [ 4 ] [ 5 ]  |  [ 6 ] [ 7 ] [ 8 ] [ 9 ] [ 0 ]   EFFE
;  ^                                   |                                   v
; FBFE  [ Q ] [ W ] [ E ] [ R ] [ T ]  |  [ Y ] [ U ] [ I ] [ O ] [ P ]   DFFE
;  ^                                   |                                   v
; FDFE  [ A ] [ S ] [ D ] [ F ] [ G ]  |  [ H ] [ J ] [ K ] [ L ] [ ENT ] BFFE
;  ^                                   |                                   v
; FEFE  [SHI] [ Z ] [ X ] [ C ] [ V ]  |  [ B ] [ N ] [ M ] [sym] [ SPC ] 7FFE
;  ^     $27                                                 $18           v
; Start                                                                   End
;        00100111                                            00011000
;
; ---------------------------------------------------------------------------
;   The above map may help in reading.
;   The neat arrangement of ports means that the B register need only be
;   rotated left to work up the left hand side and then down the right
;   hand side of the keyboard. When the reset bit drops into the carry
;   then all 8 half-rows have been read. Shift is the first key to be
;   read. The lower six bits of the shifts are unambiguous.

; -------------------------------
; THE 'KEYBOARD SCANNING' ROUTINE
; -------------------------------
;   From keyboard and s-inkey$
;   Returns 1 or 2 keys in DE, most significant shift first if any
;   key values 0-39 else 255

;; KEY-SCAN
o028E:		LD	L,$2F		; initial key value
					; valid values are obtained by subtracting
					; eight five times.
		LD	DE,$FFFF	; a buffer to receive 2 keys.

		LD	BC,$FEFE	; the commencing port address
					; B holds 11111110 initially and is also
					; used to count the 8 half-rows
					;; KEY-LINE
o0296:		IN	A,(C)		; read the port to A - bits will be reset
					; if a key is pressed else set.
		CPL			; complement - pressed key-bits are now set
		AND	$1F		; apply 00011111 mask to pick up the
					; relevant set bits.

		JR	Z,o02AB		; forward to KEY-DONE if zero and therefore
					; no keys pressed in row at all.

		LD	H,A		; transfer row bits to H
		LD	A,L		; load the initial key value to A

;; KEY-3KEYS
o029F:		INC	D		; now test the key buffer
		RET	NZ		; if we have collected 2 keys already
					; then too many so quit.

;; KEY-BITS
o02A1:		SUB	$08		; subtract 8 from the key value
					; cycling through key values (top = $27)
					; e.g. 2F>   27>1F>17>0F>07
					;      2E>   26>1E>16>0E>06
		SRL	H		; shift key bits right into carry.
		JR	NC,o02A1	; back to KEY-BITS if not pressed
					; but if pressed we have a value (0-39d)

		LD	D,E		; transfer a possible previous key to D
		LD	E,A		; transfer the new key to E
		JR	NZ,o029F	; back to KEY-3KEYS if there were more
					; set bits - H was not yet zero.

;; KEY-DONE
o02AB:		DEC	L		; cycles 2F>2E>2D>2C>2B>2A>29>28 for
					; each half-row.
		RLC	B		; form next port address e.g. FEFE > FDFE
		JR	C,o0296		; back to KEY-LINE if still more rows to do.

		LD	A,D		; now test if D is still FF ?
		INC	A		; if it is zero we have at most 1 key
					; range now $01-$28  (1-40d)
		RET	Z		; return if one key or no key.

		CP	$28		; is it capsshift (was $27) ?
		RET	Z		; return if so.

		CP	$19		; is it symbol shift (was $18) ?
		RET	Z		; return also

		LD	A,E		; now test E
		LD	E,D		; but first switch
		LD	D,A		; the two keys.
		CP	$18		; is it symbol shift ?
		RET			; return (with zero set if it was).
					; but with symbol shift now in D

; ----------------------
; THE 'KEYBOARD' ROUTINE
; ----------------------
;   Called from the interrupt 50 times a second.
;

;; KEYBOARD
o02BF:		CALL	o028E		; routine KEY-SCAN
		RET	NZ		; return if invalid combinations

;   then decrease the counters within the two key-state maps
;   as this could cause one to become free.
;   if the keyboard has not been pressed during the last five interrupts
;   then both sets will be free.


		LD	HL,KSTATE	; point to KSTATE-0

;; K-ST-LOOP
o02C6:		BIT	7,(HL)		; is it free ?  (i.e. $FF)
		JR	NZ,o02D1	; forward to K-CH-SET if so

		INC	HL		; address the 5-counter
		DEC	(HL)		; decrease the counter
		DEC	HL		; step back

		JR	NZ,o02D1	; forward to K-CH-SET if not at end of count

		LD	(HL),$FF	; else mark this particular map free.

;; K-CH-SET
o02D1:		LD	A,L		; make a copy of the low address byte.
		LD	HL,$5C04	; point to KSTATE-4
					; (ld l,$04 would do)
		CP	L		; have both sets been considered ?
		JR	NZ,o02C6	; back to K-ST-LOOP to consider this 2nd set

;   now the raw key (0-38d) is converted to a main key (uppercase).

		CALL	o031E		; routine K-TEST to get main key in A

		RET	NC		; return if just a single shift

		LD	HL,KSTATE	; point to KSTATE-0
		CP	(HL)		; does the main key code match ?
		JR	Z,o0310		; forward to K-REPEAT if so

;   if not consider the second key map.

		EX	DE,HL		; save kstate-0 in de
		LD	HL,$5C04	; point to KSTATE-4
		CP	(HL)		; does the main key code match ?
		JR	Z,o0310		; forward to K-REPEAT if so

;   having excluded a repeating key we can now consider a new key.
;   the second set is always examined before the first.

		BIT	7,(HL)		; is the key map free ?
		JR	NZ,o02F1	; forward to K-NEW if so.

		EX	DE,HL		; bring back KSTATE-0
		BIT	7,(HL)		; is it free ?
		RET	Z		; return if not.
					; as we have a key but nowhere to put it yet.

;   continue or jump to here if one of the buffers was free.

;; K-NEW
o02F1:		LD	E,A		; store key in E
		LD	(HL),A		; place in free location
		INC	HL		; advance to the interrupt counter
		LD	(HL),$05	; and initialize counter to 5
		INC	HL		; advance to the delay
		LD	A,(REPDEL)	; pick up the system variable REPDEL
		LD	(HL),A		; and insert that for first repeat delay.
		INC	HL		; advance to last location of state map.

		LD	C,(IY+$07)	; pick up MODE  (3 bytes)
		LD	D,(IY+$01)	; pick up FLAGS (3 bytes)
		PUSH	HL		; save state map location
					; Note. could now have used, to avoid IY,
					; ld l,$41; ld c,(hl); ld l,$3B; ld d,(hl).
					; six and two threes of course.

		CALL	o0333		; routine K-DECODE

		POP	HL		; restore map pointer
		LD	(HL),A		; put the decoded key in last location of map.

;; K-END
o0308:		LD	(LAST_K),A	; update LASTK system variable.
		SET	5,(IY+$01)	; update FLAGS  - signal a new key.
		RET			; return to interrupt routine.

; -----------------------
; THE 'REPEAT KEY' BRANCH
; -----------------------
;   A possible repeat has been identified. HL addresses the raw key.
;   The last location of the key map holds the decoded key from the first
;   context.  This could be a keyword and, with the exception of NOT a repeat
;   is syntactically incorrect and not really desirable.

;; K-REPEAT
o0310:		INC	HL		; increment the map pointer to second location.
		LD	(HL),$05	; maintain interrupt counter at 5.
		INC	HL		; now point to third location.
		DEC	(HL)		; decrease the REPDEL value which is used to
					; time the delay of a repeat key.

		RET	NZ		; return if not yet zero.

		LD	A,(REPPER)	; fetch the system variable value REPPER.
		LD	(HL),A		; for subsequent repeats REPPER will be used.

		INC	HL		; advance
					;
		LD	A,(HL)		; pick up the key decoded possibly in another
					; context.
					; Note. should compare with $A5 (RND) and make
					; a simple return if this is a keyword.
					; e.g. cp $A5; ret nc; (3 extra bytes)
		JR	o0308		; back to K-END

; ----------------------
; THE 'KEY-TEST' ROUTINE
; ----------------------
;   also called from s-inkey$
;   begin by testing for a shift with no other.

;; K-TEST
o031E:		LD	B,D		; load most significant key to B
					; will be $FF if not shift.
		LD	D,$00		; and reset D to index into main table
		LD	A,E		; load least significant key from E
		CP	$27		; is it higher than 39d   i.e. FF
		RET	NC		; return with just a shift (in B now)

		CP	$18		; is it symbol shift ?
		JR	NZ,o032C	; forward to K-MAIN if not

;   but we could have just symbol shift and no other

		BIT	7,B		; is other key $FF (ie not shift)
		RET	NZ		; return with solitary symbol shift


;; K-MAIN
o032C:		LD	HL,o0205	; address: MAIN-KEYS
		ADD	HL,DE		; add offset 0-38
		LD	A,(HL)		; pick up main key value
		SCF			; set carry flag
		RET			; return    (B has other key still)

; ----------------------------------
; THE 'KEYBOARD DECODING' SUBROUTINE
; ----------------------------------
;   also called from s-inkey$

;; K-DECODE
o0333:		LD	A,E		; pick up the stored main key
		CP	$3A		; an arbitrary point between digits and letters
		JR	C,o0367		; forward to K-DIGIT with digits, space, enter.

		DEC	C		; decrease MODE ( 0='KLC', 1='E', 2='G')

		JP	M,o034F		; to K-KLC-LET if was zero

		JR	Z,o0341		; to K-E-LET if was 1 for extended letters.

;   proceed with graphic codes.
;   Note. should selectively drop return address if code > 'U' ($55).
;   i.e. abort the KEYBOARD call.
;   e.g. cp 'V'; jr c,addit; pop af ;pop af ;;addit etc. (6 extra bytes).
;   (s-inkey$ never gets into graphics mode.)

;; addit
		ADD	A,$4F		; add offset to augment 'A' to graphics A say.
		RET			; return.
					; Note. ( but [GRAPH] V gives RND, etc ).

; ---

;   the jump was to here with extended mode with uppercase A-Z.

;; K-E-LET
o0341:		LD	HL,o022C-$41	; base address of E-UNSHIFT o022c.
					; ( $01EB in standard ROM ).
		INC	B		; test B is it empty i.e. not a shift.
		JR	Z,o034A		; forward to K-LOOK-UP if neither shift.

		LD	HL,o0246-$41	; Address: $0205 o0246-$41 EXT-SHIFT base

;; K-LOOK-UP
o034A:		LD	D,$00		; prepare to index.
		ADD	HL,DE		; add the main key value.
		LD	A,(HL)		; pick up other mode value.
		RET			; return.

; ---

;   the jump was here with mode = 0

;; K-KLC-LET
o034F:		LD	HL,o026A-$41	; prepare base of sym-codes
		BIT	0,B		; shift=$27 sym-shift=$18
		JR	Z,o034A		; back to K-LOOK-UP with symbol-shift

		BIT	3,D		; test FLAGS is it 'K' mode (from OUT-CURS)
		JR	Z,o0364		; skip to K-TOKENS if so

		BIT	3,(IY+$30)	; test FLAGS2 - consider CAPS LOCK ?
		RET	NZ		; return if so with main code.

		INC	B		; is shift being pressed ?
					; result zero if not
		RET	NZ		; return if shift pressed.

		ADD	A,$20		; else convert the code to lower case.
		RET			; return.

; ---

;   the jump was here for tokens

;; K-TOKENS
o0364:		ADD	A,$A5		; add offset to main code so that 'A'
					; becomes 'NEW' etc.

		RET			; return.

; ---

;   the jump was here with digits, space, enter and symbol shift (< $xx)

;; K-DIGIT
o0367:		CP	$30		; is it "0" or higher ?
		RET	C		; return with space, enter and symbol-shift

		DEC	C		; test MODE (was 0='KLC', 1='E', 2='G')
		JP	M,o039D		; jump to K-KLC-DGT if was 0.

		JR	NZ,o0389	; forward to K-GRA-DGT if mode was 2.

;   continue with extended digits 0-9.

		LD	HL,o0284-$30	; $0254 - base of E-DIGITS
		BIT	5,B		; test - shift=$27 sym-shift=$18
		JR	Z,o034A		; to K-LOOK-UP if sym-shift

		CP	$38		; is character "8" ?
		JR	NC,o0382	; to K-8-&-9 if greater than "7"

		SUB	$20		; reduce to ink range $10-$17
		INC	B		; shift ?
		RET	Z		; return if not.

		ADD	A,$08		; add 8 to give paper range $18 - $1F
		RET			; return

; ---

;   89

;; K-8-&-9
o0382:		SUB	$36		; reduce to 02 and 03  bright codes
		INC	B		; test if shift pressed.
		RET	Z		; return if not.

		ADD	A,$FE		; subtract 2 setting carry
		RET			; to give 0 and 1    flash codes.

; ---

;   graphics mode with digits

;; K-GRA-DGT
o0389:		LD	HL,o0260-$30	; $0230 base address of CTL-CODES

		CP	$39		; is key "9" ?
		JR	Z,o034A		; back to K-LOOK-UP - changed to $0F, GRAPHICS.

		CP	$30		; is key "0" ?
		JR	Z,o034A		; back to K-LOOK-UP - changed to $0C, delete.

;   for keys "0" - "7" we assign a mosaic character depending on shift.

		AND	$07		; convert character to number. 0 - 7.
		ADD	A,$80		; add offset - they start at $80

		INC	B		; destructively test for shift
		RET	Z		; and return if not pressed.

		XOR	$0F		; toggle bits becomes range $88-$8F
		RET			; return.

; ---

;   now digits in 'KLC' mode

;; K-KLC-DGT
o039D:		INC	B		; return with digit codes if neither
		RET	Z		; shift key pressed.

		BIT	5,B		; test for caps shift.

		LD	HL,o0260-$30	; prepare base of table CTL-CODES.
		JR	NZ,o034A	; back to K-LOOK-UP if shift pressed.

;   must have been symbol shift

		SUB	$10		; for ASCII most will now be correct
					; on a standard typewriter.

		CP	$22		; but '@' is not - see below.
		JR	Z,o03B2		; forward to K-@-CHAR if so

		CP	$20		; '_' is the other one that fails
		RET	NZ		; return if not.

		LD	A,$5F		; substitute ASCII '_'
		RET			; return.

; ---

;; K-@-CHAR
o03B2:		LD	A,$40		; substitute ASCII '@'
		RET			; return.


; ------------------------------------------------------------------------
;   The Spectrum Input character keys. One or two are abbreviated.
;   From $00 Flash 0 to $FF COPY. The routine above has decoded all these.

;  | 00 Fl0| 01 Fl1| 02 Br0| 03 Br1| 04 In0| 05 In1| 06 CAP| 07 EDT|
;  | 08 LFT| 09 RIG| 0A DWN| 0B UP | 0C DEL| 0D ENT| 0E SYM| 0F GRA|
;  | 10 Ik0| 11 Ik1| 12 Ik2| 13 Ik3| 14 Ik4| 15 Ik5| 16 Ik6| 17 Ik7|
;  | 18 Pa0| 19 Pa1| 1A Pa2| 1B Pa3| 1C Pa4| 1D Pa5| 1E Pa6| 1F Pa7|
;  | 20 SP | 21  ! | 22  " | 23  # | 24  $ | 25  % | 26  & | 27  ' |
;  | 28  ( | 29  ) | 2A  * | 2B  + | 2C  , | 2D  - | 2E  . | 2F  / |
;  | 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7 |
;  | 38  8 | 39  9 | 3A  : | 3B  ; | 3C  < | 3D  = | 3E  > | 3F  ? |
;  | 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G |
;  | 48  H | 49  I | 4A  J | 4B  K | 4C  L | 4D  M | 4E  N | 4F  O |
;  | 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W |
;  | 58  X | 59  Y | 5A  Z | 5B  [ | 5C  \ | 5D  ] | 5E  ^ | 5F  _ |
;  | 60  £ | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g |
;  | 68  h | 69  i | 6A  j | 6B  k | 6C  l | 6D  m | 6E  n | 6F  o |
;  | 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w |
;  | 78  x | 79  y | 7A  z | 7B  { | 7C  | | 7D  } | 7E  ~ | 7F  © |
;  | 80 128| 81 129| 82 130| 83 131| 84 132| 85 133| 86 134| 87 135|
;  | 88 136| 89 137| 8A 138| 8B 139| 8C 140| 8D 141| 8E 142| 8F 143|
;  | 90 [A]| 91 [B]| 92 [C]| 93 [D]| 94 [E]| 95 [F]| 96 [G]| 97 [H]|
;  | 98 [I]| 99 [J]| 9A [K]| 9B [L]| 9C [M]| 9D [N]| 9E [O]| 9F [P]|
;  | A0 [Q]| A1 [R]| A2 [S]| A3 [T]| A4 [U]| A5 RND| A6 IK$| A7 PI |
;  | A8 FN | A9 PNT| AA SC$| AB ATT| AC AT | AD TAB| AE VL$| AF COD|
;  | B0 VAL| B1 LEN| B2 SIN| B3 COS| B4 TAN| B5 ASN| B6 ACS| B7 ATN|
;  | B8 LN | B9 EXP| BA INT| BB SQR| BC SGN| BD ABS| BE PEK| BF IN |
;  | C0 USR| C1 ST$| C2 CH$| C3 NOT| C4 BIN| C5 OR | C6 AND| C7 <= |
;  | C8 >= | C9 <> | CA LIN| CB THN| CC TO | CD STP| CE DEF| CF CAT|
;  | D0 FMT| D1 MOV| D2 ERS| D3 OPN| D4 CLO| D5 MRG| D6 VFY| D7 BEP|
;  | D8 CIR| D9 INK| DA PAP| DB FLA| DC BRI| DD INV| DE OVR| DF OUT|
;  | E0 LPR| E1 LLI| E2 STP| E3 REA| E4 DAT| E5 RES| E6 NEW| E7 BDR|
;  | E8 CON| E9 DIM| EA REM| EB FOR| EC GTO| ED GSB| EE INP| EF LOA|
;  | F0 LIS| F1 LET| F2 PAU| F3 NXT| F4 POK| F5 PRI| F6 PLO| F7 RUN|
;  | F8 SAV| F9 RAN| FA IF | FB CLS| FC DRW| FD CLR| FE RET| FF CPY|

;   Note that for simplicity, Sinclair have located all the control codes
;   below the space character.
;   ASCII DEL, $7F, has been made a copyright symbol.
;   Also $60, '`', not used in BASIC but used in other languages, has been
;   allocated the local currency symbol for the relevant country -
;    £  in most Spectrums.

; ------------------------------------------------------------------------


;**********************************
;** Part 3. LOUDSPEAKER ROUTINES **
;**********************************

; Documented by Alvin Albrecht.

; ------------------------------
; Routine to control loudspeaker
; ------------------------------
; Outputs a square wave of given duration and frequency
; to the loudspeaker.
;   Enter with: DE = #cycles - 1
;               HL = tone period as described next
;
; The tone period is measured in T states and consists of
; three parts: a coarse part (H register), a medium part
; (bits 7..2 of L) and a fine part (bits 1..0 of L) which
; contribute to the waveform timing as follows:
;
;                          coarse    medium       fine
; duration of low  = 118 + 1024*H + 16*(L>>2) + 4*(L&0x3)
; duration of hi   = 118 + 1024*H + 16*(L>>2) + 4*(L&0x3)
; Tp = tone period = 236 + 2048*H + 32*(L>>2) + 8*(L&0x3)
;                  = 236 + 2048*H + 8*L = 236 + 8*HL
;
; As an example, to output five seconds of middle C (261.624 Hz):
;   (a) Tone period = 1/261.624 = 3.822ms
;   (b) Tone period in T-States = 3.822ms*fCPU = 13378
;         where fCPU = clock frequency of the CPU = 3.5MHz
;    ©  Find H and L for desired tone period:
;         HL = (Tp - 236) / 8 = (13378 - 236) / 8 = 1643 = 0x066B
;   (d) Tone duration in cycles = 5s/3.822ms = 1308 cycles
;         DE = 1308 - 1 = 0x051B
;
; The resulting waveform has a duty ratio of exactly 50%.
;
;
;; BEEPER
o03B5:		DI
		LD	A,L		;
		SRL	L		;
		SRL	L		; L = medium part of tone period
		CPL			;
		AND	$03		; A = 3 - fine part of tone period
		LD	C,A		;
		LD	B,$00		;
		LD	IX,o03D1	; Address: BE-IX+3
		ADD	IX,BC		;   IX holds address of entry into the loop
					;   the loop will contain 0-3 NOPs, implementing
					;   the fine part of the tone period.
		LD	A,(BORDCR)	; BORDCR
		AND	$38		; bits 5..3 contain border colour
		RRCA			; border colour bits moved to 2..0
		RRCA			;   to match border bits on port #FE
		RRCA			;
		OR	$08		; bit 3 set (tape output bit on port #FE)
					;   for loud sound output
					;; BE-IX+3
o03D1:		NOP
					;   adjustments to tone period
					;; BE-IX+2
o03D2:		NOP

;; BE-IX+1
o03D3:		NOP

;; BE-IX+0
o03D4:		INC	B		; (4)   ;
		INC	C		; (4)   ;

;; BE-H&L-LP
o03D6:		DEC	C		; (4)   ; timing loop for duration of
		JR	NZ,o03D6	; (12/7);   high or low pulse of waveform

		LD	C,$3F		; (7)   ;
		DEC	B		; (4)   ;
		JP	NZ,o03D6	; (10)  ; to BE-H&L-LP

		XOR	$10		; (7)   ; toggle output beep bit
		OUT	($FE),A		; (11)  ; output pulse
		LD	B,H		; (4)   ; B = coarse part of tone period
		LD	C,A		; (4)   ; save port #FE output byte
		BIT	4,A		; (8)   ; if new output bit is high, go
		JR	NZ,o03F2	; (12/7);   to BE-AGAIN

		LD	A,D		; (4)   ; one cycle of waveform has completed
		OR	E		; (4)   ;   (low->low). if cycle countdown = 0
		JR	Z,o03F6		; (12/7);   go to BE-END

		LD	A,C		; (4)   ; restore output byte for port #FE
		LD	C,L		; (4)   ; C = medium part of tone period
		DEC	DE		; (6)   ; decrement cycle count
		JP	(IX)		; (8)   ; do another cycle

;; BE-AGAIN                     ; halfway through cycle
o03F2:		LD	C,L		; (4)   ; C = medium part of tone period
		INC	C		; (4)   ; adds 16 cycles to make duration of high = duration of low
		JP	(IX)		; (8)   ; do high pulse of tone

;; BE-END
o03F6:		EI
		RET			;


; ------------------
; THE 'BEEP' COMMAND
; ------------------
; BASIC interface to BEEPER subroutine.
; Invoked in BASIC with:
;   BEEP dur, pitch
;   where dur   = duration in seconds
;         pitch = # of semitones above/below middle C
;
; Enter with: pitch on top of calculator stack
;             duration next on calculator stack
;
;; beep
o03F8:		RST	28H		; FP-CALC
		DB	$31		; duplicate                  ; duplicate pitch
		DB	$27		; int                        ; convert to integer
		DB	$C0		; st-mem-0                   ; store integer pitch to memory 0
		DB	$03		; subtract                   ; calculate fractional part of pitch = fp_pitch - int_pitch
		DB	$34		; stk-data                   ; push constant
		DB	$EC		; Exponent: $7C, Bytes: 4    ; constant = 0.05762265
		DB	$6C,$98,$1F,$F5	; ($6C,$98,$1F,$F5)
		DB	$04		; multiply                   ; compute:
		DB	$A1		; stk-one                    ; 1 + 0.05762265 * fraction_part(pitch)
		DB	$0F		; addition
		DB	$38		; end-calc                   ; leave on calc stack

		LD	HL,MEMBOT	; MEM-0: number stored here is in 16 bit integer format (pitch)
					;   0, 0/FF (pos/neg), LSB, MSB, 0
					;   LSB/MSB is stored in two's complement
					; In the following, the pitch is checked if it is in the range -128<=p<=127
		LD	A,(HL)		; First byte must be zero, otherwise
		AND	A		;   error in integer conversion
		JR	NZ,o046C	; to REPORT-B

		INC	HL		;
		LD	C,(HL)		; C = pos/neg flag = 0/FF
		INC	HL		;
		LD	B,(HL)		; B = LSB, two's complement
		LD	A,B		;
		RLA			;
		SBC	A,A		; A = 0/FF if B is pos/neg
		CP	C		; must be the same as C if the pitch is -128<=p<=127
		JR	NZ,o046C	; if no, error REPORT-B

		INC	HL		; if -128<=p<=127, MSB will be 0/FF if B is pos/neg
		CP	(HL)		; verify this
		JR	NZ,o046C	; if no, error REPORT-B
					; now we know -128<=p<=127
		LD	A,B		; A = pitch + 60
		ADD	A,$3C		; if -60<=pitch<=67,
		JP	P,o0425		;   goto BE-i-OK

		JP	PO,o046C	; if pitch <= 67 goto REPORT-B
					;   lower bound of pitch set at -60

;; BE-I-OK                      ; here, -60<=pitch<=127
; and A=pitch+60 -> 0<=A<=187

o0425:		LD	B,$FA		; 6 octaves below middle C

;; BE-OCTAVE                    ; A=# semitones above 5 octaves below middle C
o0427:		INC	B		; increment octave
		SUB	$0C		; 12 semitones = one octave
		JR	NC,o0427	; to BE-OCTAVE

		ADD	A,$0C		; A = # semitones above C (0-11)
		PUSH	BC		; B = octave displacement from middle C, 2's complement: -5<=B<=10
		LD	HL,o046E	; Address: semi-tone
		CALL	o3406		; routine LOC-MEM
					;   HL = 5*A + $046E
		CALL	o33B4		; routine STACK-NUM
					;   read FP value (freq) from semitone table (HL) and push onto calc stack

		RST	28H		; FP-CALC
		DB	$04		; multiply   mult freq by 1 + 0.0576 * fraction_part(pitch) stacked earlier
					;;             thus taking into account fractional part of pitch.
					;;           the number 0.0576*frequency is the distance in Hz to the next
					;;             note (verify with the frequencies recorded in the semitone
					;;             table below) so that the fraction_part of the pitch does
					;;             indeed represent a fractional distance to the next note.
		DB	$38		; end-calc   HL points to first byte of fp num on stack = middle frequency to generate

		POP	AF		; A = octave displacement from middle C, 2's complement: -5<=A<=10
		ADD	A,(HL)		; increase exponent by A (equivalent to multiplying by 2^A)
		LD	(HL),A		;

		RST	28H		; FP-CALC
		DB	$C0		; st-mem-0          ; store frequency in memory 0
		DB	$02		; delete            ; remove from calc stack
		DB	$31		; duplicate         ; duplicate duration (seconds)
		DB	$38		; end-calc

		CALL	o1E94		; routine FIND-INT1 ; FP duration to A
		CP	$0B		; if dur > 10 seconds,
		JR	NC,o046C	;   goto REPORT-B

;;; The following calculation finds the tone period for HL and the cycle count
;;; for DE expected in the BEEPER subroutine.  From the example in the BEEPER comments,
;;;
;;; HL = ((fCPU / f) - 236) / 8 = fCPU/8/f - 236/8 = 437500/f -29.5
;;; DE = duration * frequency - 1
;;;
;;; Note the different constant (30.125) used in the calculation of HL
;;; below.  This is probably an error.

		RST	28H		; FP-CALC
		DB	$E0		; get-mem-0                 ; push frequency
		DB	$04		; multiply                  ; result1: #cycles = duration * frequency
		DB	$E0		; get-mem-0                 ; push frequency
		DB	$34		; stk-data                  ; push constant
		DB	$80		; Exponent $93, Bytes: 3    ; constant = 437500
		DB	$43,$55,$9F,$80	; ($55,$9F,$80,$00)
		DB	$01		; exchange                  ; frequency on top
		DB	$05		; division                  ; 437500 / frequency
		DB	$34		; stk-data                  ; push constant
		DB	$35		; Exponent: $85, Bytes: 1   ; constant = 30.125
		DB	$71		; ($71,$00,$00,$00)
		DB	$03		; subtract                  ; result2: tone_period(HL) = 437500 / freq - 30.125
		DB	$38		; end-calc

		CALL	o1E99		; routine FIND-INT2
		PUSH	BC		;   BC = tone_period(HL)
		CALL	o1E99		; routine FIND-INT2, BC = #cycles to generate
		POP	HL		; HL = tone period
		LD	D,B		;
		LD	E,C		; DE = #cycles
		LD	A,D		;
		OR	E		;
		RET	Z		; if duration = 0, skip BEEP and avoid 65536 cycle
					;   boondoggle that would occur next
		DEC	DE		; DE = #cycles - 1
		JP	o03B5		; to BEEPER

; ---


;; REPORT-B
o046C:		RST	08H		; ERROR-1
		DB	$0A		; Error Report: Integer out of range



; ---------------------
; THE 'SEMI-TONE' TABLE
; ---------------------
;
;   Holds frequencies corresponding to semitones in middle octave.
;   To move n octaves higher or lower, frequencies are multiplied by 2^n.

;; semi-tone         five byte fp         decimal freq     note (middle)
o046E:		DB	$89,$02,$D0,$12,$86	;  261.625565290         C
		DB	$89,$0A,$97,$60,$75	;  277.182631135         C#
		DB	$89,$12,$D5,$17,$1F	;  293.664768100         D
		DB	$89,$1B,$90,$41,$02	;  311.126983881         D#
		DB	$89,$24,$D0,$53,$CA	;  329.627557039         E
		DB	$89,$2E,$9D,$36,$B1	;  349.228231549         F
		DB	$89,$38,$FF,$49,$3E	;  369.994422674         F#
		DB	$89,$43,$FF,$6A,$73	;  391.995436072         G
		DB	$89,$4F,$A7,$00,$54	;  415.304697513         G#
		DB	$89,$5C,$00,$00,$00	;  440.000000000         A
		DB	$89,$69,$14,$F6,$24	;  466.163761616         A#
		DB	$89,$76,$F1,$10,$05	;  493.883301378         B


;   "Music is the hidden mathematical endeavour of a soul unconscious it
;    is calculating" - Gottfried Wilhelm Liebnitz 1646 - 1716


;****************************************
;** Part 4. CASSETTE HANDLING ROUTINES **
;****************************************

;   These routines begin with the service routines followed by a single
;   command entry point.
;   The first of these service routines is a curiosity.

; -----------------------
; THE 'ZX81 NAME' ROUTINE
; -----------------------
;   This routine fetches a filename in ZX81 format and is not used by the
;   cassette handling routines in this ROM.

;; zx81-name

o04AA:		CALL	o24FB		; routine SCANNING to evaluate expression.
		LD	A,(FLAGS)	; fetch system variable FLAGS.
		ADD	A,A		; test bit 7 - syntax, bit 6 - result type.
		JP	M,o1C8A		; to REPORT-C if not string result
					; 'Nonsense in BASIC'.

		POP	HL		; drop return address.
		RET	NC		; return early if checking syntax.

		PUSH	HL		; re-save return address.
		CALL	o2BF1		; routine STK-FETCH fetches string parameters.
		LD	H,D		; transfer start of filename
		LD	L,E		; to the HL register.
		DEC	C		; adjust to point to last character and
		RET	M		; return if the null string.
					; or multiple of 256!

		ADD	HL,BC		; find last character of the filename.
					; and also clear carry.
		SET	7,(HL)		; invert it.
		RET			; return.

; =========================================
;
; PORT 254 ($FE)
;
;                      spk mic { border  }
;          ___ ___ ___ ___ ___ ___ ___ ___
; PORT    |   |   |   |   |   |   |   |   |
; 254     |   |   |   |   |   |   |   |   |
; $FE     |___|___|___|___|___|___|___|___|
;           7   6   5   4   3   2   1   0
;

; ----------------------------------
; Save header and program/data bytes
; ----------------------------------
;   This routine saves a section of data. It is called from SA-CTRL to save the
;   seventeen bytes of header data. It is also the exit route from that routine
;   when it is set up to save the actual data.
;   On entry -
;   HL points to start of data.
;   IX points to descriptor.
;   The accumulator is set to  $00 for a header, $FF for data.

;; SA-BYTES
o04C2:		LD	HL,o053F	; address: SA/LD-RET
		PUSH	HL		; is pushed as common exit route.
					; however there is only one non-terminal exit
					; point.

		LD	HL,$1F80	; a timing constant H=$1F, L=$80
					; inner and outer loop counters
					; a five second lead-in is used for a header.

		BIT	7,A		; test one bit of accumulator.
					; (AND A ?)
		JR	Z,o04D0		; skip to SA-FLAG if a header is being saved.

;   else is data bytes and a shorter lead-in is used.

		LD	HL,$0C98	; another timing value H=$0C, L=$98.
					; a two second lead-in is used for the data.

;; SA-FLAG
o04D0:		EX	AF,AF'		; save flag
		INC	DE		; increase length by one.
		DEC	IX		; decrease start.

		DI			; disable interrupts

		LD	A,$02		; select red for border, microphone bit on.
		LD	B,A		; also does as an initial slight counter value.

;; SA-oEADER
o04D8:		DJNZ	o04D8		; self loop to SA-oEADER for delay.
					; after initial loop, count is $A4 (or $A3)

		OUT	($FE),A		; output byte $02/$0D to tape port.

		XOR	$0F		; switch from RED (mic on) to CYAN (mic off).

		LD	B,$A4		; hold count. also timed instruction.

		DEC	L		; originally $80 or $98.
					; but subsequently cycles 256 times.
		JR	NZ,o04D8	; back to SA-oEADER until L is zero.

;   the outer loop is counted by H

		DEC	B		; decrement count
		DEC	H		; originally  twelve or thirty-one.
		JP	P,o04D8		; back to SA-oEADER until H becomes $FF

;   now send a sync pulse. At this stage mic is off and A holds value
;   for mic on.
;   A sync pulse is much shorter than the steady pulses of the lead-in.

		LD	B,$2F		; another short timed delay.

;; SA-SYNC-1
o04EA:		DJNZ	o04EA		; self loop to SA-SYNC-1

		OUT	($FE),A		; switch to mic on and red.
		LD	A,$0D		; prepare mic off - cyan
		LD	B,$37		; another short timed delay.

;; SA-SYNC-2
o04F2:		DJNZ	o04F2		; self loop to SA-SYNC-2

		OUT	($FE),A		; output mic off, cyan border.
		LD	BC,$3B0E	; B=$3B time(*), C=$0E, YELLOW, MIC OFF.

;

		EX	AF,AF'		; restore saved flag
					; which is 1st byte to be saved.

		LD	L,A		; and transfer to L.
					; the initial parity is A, $FF or $00.
		JP	o0507		; JUMP forward to SA-START     ->
					; the mid entry point of loop.

; -------------------------
;   During the save loop a parity byte is maintained in H.
;   the save loop begins by testing if reduced length is zero and if so
;   the final parity byte is saved reducing count to $FFFF.

;; SA-LOOP
o04FE:		LD	A,D		; fetch high byte
		OR	E		; test against low byte.
		JR	Z,o050E		; forward to SA-PARITY if zero.

		LD	L,(IX+$00)	; load currently addressed byte to L.

;; SA-LOOP-P
o0505:		LD	A,H		; fetch parity byte.
		XOR	L		; exclusive or with new byte.

; -> the mid entry point of loop.

;; SA-START
o0507:		LD	H,A		; put parity byte in H.
		LD	A,$01		; prepare blue, mic=on.
		SCF			; set carry flag ready to rotate in.
		JP	o0525		; JUMP forward to SA-8-BITS            -8->

; ---

;; SA-PARITY
o050E:		LD	L,H		; transfer the running parity byte to L and
		JR	o0505		; back to SA-LOOP-P
					; to output that byte before quitting normally.

; ---

;   The entry point to save yellow part of bit.
;   A bit consists of a period with mic on and blue border followed by
;   a period of mic off with yellow border.
;   Note. since the DJNZ instruction does not affect flags, the zero flag is
;   used to indicate which of the two passes is in effect and the carry
;   maintains the state of the bit to be saved.

;; SA-BIT-2
o0511:		LD	A,C		; fetch 'mic on and yellow' which is
					; held permanently in C.
		BIT	7,B		; set the zero flag. B holds $3E.

;   The entry point to save 1 entire bit. For first bit B holds $3B(*).
;   Carry is set if saved bit is 1. zero is reset NZ on entry.

;; SA-BIT-1
o0514:		DJNZ	o0514		; self loop for delay to SA-BIT-1

		JR	NC,o051C	; forward to SA-OUT if bit is 0.

;   but if bit is 1 then the mic state is held for longer.

		LD	B,$42		; set timed delay. (66 decimal)

;; SA-SET
o051A:		DJNZ	o051A		; self loop to SA-SET
					; (roughly an extra 66*13 clock cycles)

;; SA-OUT
o051C:		OUT	($FE),A		; blue and mic on OR  yellow and mic off.

		LD	B,$3E		; set up delay
		JR	NZ,o0511	; back to SA-BIT-2 if zero reset NZ (first pass)

;   proceed when the blue and yellow bands have been output.

		DEC	B		; change value $3E to $3D.
		XOR	A		; clear carry flag (ready to rotate in).
		INC	A		; reset zero flag i.e. NZ.

; -8->

;; SA-8-BITS
o0525:		RL	L		; rotate left through carry
					; C<76543210<C
		JP	NZ,o0514	; JUMP back to SA-BIT-1
					; until all 8 bits done.

;   when the initial set carry is passed out again then a byte is complete.

		DEC	DE		; decrease length
		INC	IX		; increase byte pointer
		LD	B,$31		; set up timing.

		LD	A,$7F		; test the space key and
		IN	A,($FE)		; return to common exit (to restore border)
		RRA			; if a space is pressed
		RET	NC		; return to SA/LD-RET.   - - >

;   now test if byte counter has reached $FFFF.

		LD	A,D		; fetch high byte
		INC	A		; increment.
		JP	NZ,o04FE	; JUMP to SA-LOOP if more bytes.

		LD	B,$3B		; a final delay.

;; SA-DELAY
o053C:		DJNZ	o053C		; self loop to SA-DELAY

		RET			; return - - >

; ------------------------------
; THE 'SAVE/LOAD RETURN' ROUTINE
; ------------------------------
;   The address of this routine is pushed on the stack prior to any load/save
;   operation and it handles normal completion with the restoration of the
;   border and also abnormal termination when the break key, or to be more
;   precise the space key is pressed during a tape operation.
;
; - - >

;; SA/LD-RET
o053F:		PUSH	AF		; preserve accumulator throughout.
		LD	A,(BORDCR)	; fetch border colour from BORDCR.
		AND	$38		; mask off paper bits.
		RRCA			; rotate
		RRCA			; to the
		RRCA			; range 0-7.

		OUT	($FE),A		; change the border colour.

		LD	A,$7F		; read from port address $7FFE the
		IN	A,($FE)		; row with the space key at outside.

		RRA			; test for space key pressed.
		EI			; enable interrupts
		JR	C,o0554		; forward to SA/LD-END if not


;; REPORT-Da
o0552:		RST	08H		; ERROR-1
		DB	$0C		; Error Report: BREAK - CONT repeats

; ---

;; SA/LD-END
o0554:		POP	AF		; restore the accumulator.
		RET			; return.

; ------------------------------------
; Load header or block of information
; ------------------------------------
;   This routine is used to load bytes and on entry A is set to $00 for a
;   header or to $FF for data.  IX points to the start of receiving location
;   and DE holds the length of bytes to be loaded. If, on entry the carry flag
;   is set then data is loaded, if reset then it is verified.

;; LD-BYTES
o0556:		INC	D		; reset the zero flag without disturbing carry.
		EX	AF,AF'		; preserve entry flags.
		DEC	D		; restore high byte of length.

		DI			; disable interrupts

		LD	A,$0F		; make the border white and mic off.
		OUT	($FE),A		; output to port.

		LD	HL,o053F	; Address: SA/LD-RET
		PUSH	HL		; is saved on stack as terminating routine.

;   the reading of the EAR bit (D6) will always be preceded by a test of the
;   space key (D0), so store the initial post-test state.

;IN      A,($FE)         ; read the ear state - bit 6.
;RRA                     ; rotate to bit 5.
		CALL	NEW_LOAD

CONT_LOAD:
		AND	$20		; isolate this bit.
		OR	$02		; combine with red border colour.
		LD	C,A		; and store initial state long-term in C.
		CP	A		; set the zero flag.

;

;; LD-BREAK
o056B:		RET	NZ		; return if at any time space is pressed.

;; LD-START
o056C:		CALL	o05E7		; routine LD-EDGE-1
		JR	NC,o056B	; back to LD-BREAK with time out and no
					; edge present on tape.

;   but continue when a transition is found on tape.

		LD	HL,$0415	; set up 16-bit outer loop counter for
					; approx 1 second delay.

;; LD-WAIT
o0574:		DJNZ	o0574		; self loop to LD-WAIT (for 256 times)

		DEC	HL		; decrease outer loop counter.
		LD	A,H		; test for
		OR	L		; zero.
		JR	NZ,o0574	; back to LD-WAIT, if not zero, with zero in B.

;   continue after delay with H holding zero and B also.
;   sample 256 edges to check that we are in the middle of a lead-in section.

		CALL	o05E3		; routine LD-EDGE-2
		JR	NC,o056B	; back to LD-BREAK
					; if no edges at all.

;; LD-oEADER
o0580:		LD	B,$9C		; two edges must be spaced apart.
		CALL	o05E3		; routine LD-EDGE-2
		JR	NC,o056B	; back to LD-BREAK if time-out

		LD	A,$C6		; two edges must be spaced apart.
		CP	B		; compare
		JR	NC,o056C	; back to LD-START if too close together for a
					; lead-in.

		INC	H		; proceed to test 256 edged sample.
		JR	NZ,o0580	; back to LD-oEADER while more to do.

;   sample indicates we are in the middle of a two or five second lead-in.
;   Now test every edge looking for the terminal sync signal.

;; LD-SYNC
o058F:		LD	B,$C9		; two edges must be spaced apart.
		CALL	o05E7		; routine LD-EDGE-1
		JR	NC,o056B	; back to LD-BREAK with time-out.

		LD	A,B		; fetch augmented timing value from B.
		CP	$D4		; compare
		JR	NC,o058F	; back to LD-SYNC if gap too big, that is,
					; a normal lead-in edge gap.

;   but a short gap will be the sync pulse.
;   in which case another edge should appear before B rises to $FF

		CALL	o05E7		; routine LD-EDGE-1
		RET	NC		; return with time-out.

; proceed when the sync at the end of the lead-in is found.
; We are about to load data so change the border colours.

		LD	A,C		; fetch long-term mask from C
		XOR	$03		; and make blue/yellow.

		LD	C,A		; store the new long-term byte.

		LD	H,$00		; set up parity byte as zero.
		LD	B,$B0		; two edges must be spaced apart.
		JR	o05C8		; forward to LD-MARKER
					; the loop mid entry point with the alternate
					; zero flag reset to indicate first byte
					; is discarded.

; --------------
;   the loading loop loads each byte and is entered at the mid point.

;; LD-LOOP
o05A9:		EX	AF,AF'		; restore entry flags and type in A.
		JR	NZ,o05B3	; forward to LD-FLAG if awaiting initial flag
					; which is to be discarded.

		JR	NC,o05BD	; forward to LD-VERIFY if not to be loaded.

		LD	(IX+$00),L	; place loaded byte at memory location.
		JR	o05C2		; forward to LD-NEXT

; ---

;; LD-FLAG
o05B3:		RL	C		; preserve carry (verify) flag in long-term
					; state byte. Bit 7 can be lost.

		XOR	L		; compare type in A with first byte in L.
		RET	NZ		; return if no match e.g. CODE vs. DATA.

;   continue when data type matches.

		LD	A,C		; fetch byte with stored carry
		RRA			; rotate it to carry flag again
		LD	C,A		; restore long-term port state.

		INC	DE		; increment length ??
		JR	o05C4		; forward to LD-DEC.
					; but why not to location after ?

; ---
;   for verification the byte read from tape is compared with that in memory.

;; LD-VERIFY
o05BD:		LD	A,(IX+$00)	; fetch byte from memory.
		XOR	L		; compare with that on tape
		RET	NZ		; return if not zero.

;; LD-NEXT
o05C2:		INC	IX		; increment byte pointer.

;; LD-DEC
o05C4:		DEC	DE		; decrement length.
		EX	AF,AF'		; store the flags.
		LD	B,$B2		; timing.

;   when starting to read 8 bits the receiving byte is marked with bit at right.
;   when this is rotated out again then 8 bits have been read.

;; LD-MARKER
o05C8:		LD	L,$01		; initialize as %00000001

;; LD-8-BITS
o05CA:		CALL	o05E3		; routine LD-EDGE-2 increments B relative to
					; gap between 2 edges.
		RET	NC		; return with time-out.

		LD	A,$CB		; the comparison byte.
		CP	B		; compare to incremented value of B.
					; if B is higher then bit on tape was set.
					; if <= then bit on tape is reset.

		RL	L		; rotate the carry bit into L.

		LD	B,$B0		; reset the B timer byte.
		JP	NC,o05CA	; JUMP back to LD-8-BITS

;   when carry set then marker bit has been passed out and byte is complete.

		LD	A,H		; fetch the running parity byte.
		XOR	L		; include the new byte.
		LD	H,A		; and store back in parity register.

		LD	A,D		; check length of
		OR	E		; expected bytes.
		JR	NZ,o05A9	; back to LD-LOOP
					; while there are more.

;   when all bytes loaded then parity byte should be zero.

		LD	A,H		; fetch parity byte.
		CP	$01		; set carry if zero.
		RET			; return
					; in no carry then error as checksum disagrees.

; -------------------------
; Check signal being loaded
; -------------------------
;   An edge is a transition from one mic state to another.
;   More specifically a change in bit 6 of value input from port $FE.
;   Graphically it is a change of border colour, say, blue to yellow.
;   The first entry point looks for two adjacent edges. The second entry point
;   is used to find a single edge.
;   The B register holds a count, up to 256, within which the edge (or edges)
;   must be found. The gap between two edges will be more for a "1" than a "0"
;   so the value of B denotes the state of the bit (two edges) read from tape.

; ->

;; LD-EDGE-2
o05E3:		CALL	o05E7		; call routine LD-EDGE-1 below.
		RET	NC		; return if space pressed or time-out.
					; else continue and look for another adjacent
					; edge which together represent a bit on the
					; tape.

; ->
;   this entry point is used to find a single edge from above but also
;   when detecting a read-in signal on the tape.

;; LD-EDGE-1
o05E7:		LD	A,$16		; a delay value of twenty two.

;; LD-DELAY
o05E9:		DEC	A		; decrement counter
		JR	NZ,o05E9	; loop back to LD-DELAY 22 times.

		AND	A		; clear carry.

;; LD-SAMPLE
o05ED:		INC	B		; increment the time-out counter.
		RET	Z		; return with failure when $FF passed.

		LD	A,$7F		; prepare to read keyboard and EAR port
		IN	A,($FE)		; row $7FFE. bit 6 is EAR, bit 0 is SPACE key.
		RRA			; test outer key the space. (bit 6 moves to 5)
		RET	NC		; return if space pressed.  >>>

		XOR	C		; compare with initial long-term state.
		AND	$20		; isolate bit 5
		JR	Z,o05ED		; back to LD-SAMPLE if no edge.

;   but an edge, a transition of the EAR bit, has been found so switch the
;   long-term comparison byte containing both border colour and EAR bit.

		LD	A,C		; fetch comparison value.
		CPL			; switch the bits
		LD	C,A		; and put back in C for long-term.

		AND	$07		; isolate new colour bits.
		OR	$08		; set bit 3 - MIC off.
		OUT	($FE),A		; send to port to effect the change of colour.

		SCF			; set carry flag signaling edge found within
					; time allowed.
		RET			; return.

; ---------------------------------
; Entry point for all tape commands
; ---------------------------------
;   This is the single entry point for the four tape commands.
;   The routine first determines in what context it has been called by examining
;   the low byte of the Syntax table entry which was stored in T_ADDR.
;   Subtracting $EO (the present arrangement) gives a value of
;   $00 - SAVE
;   $01 - LOAD
;   $02 - VERIFY
;   $03 - MERGE
;   As with all commands the address STMT-RET is on the stack.

;; SAVE-ETC
o0605:		POP	AF		; discard address STMT-RET.
		LD	A,(T_ADDR)	; fetch T_ADDR

;   Now reduce the low byte of the Syntax table entry to give command.
;   Note. For ZASM use SUB $E0 as next instruction.

o0609:		SUB	(o1ADF+1) % 256	; subtract the known offset.
					; ( is SUB $E0 in standard ROM )

		LD	(T_ADDR),A	; and put back in T_ADDR as 0,1,2, or 3
					; for future reference.

		CALL	o1C8C		; routine EXPT-EXP checks that a string
					; expression follows and stacks the
					; parameters in run-time.

		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o0652		; forward to SA-DATA if checking syntax.

		LD	BC,$0011	; presume seventeen bytes for a header.
		LD	A,(T_ADDR)	; fetch command from T_ADDR.
		AND	A		; test for zero - SAVE.
		JR	Z,o0621		; forward to SA-SPACE if so.

		LD	C,$22		; else double length to thirty four.

;; SA-SPACE
o0621:		RST	30H		; BC-SPACES creates 17/34 bytes in workspace.

		PUSH	DE		; transfer the start of new space to
		POP	IX		; the available index register.

;   ten spaces are required for the default filename but it is simpler to
;   overwrite the first file-type indicator byte as well.

		LD	B,$0B		; set counter to eleven.
		LD	A,$20		; prepare a space.

;; SA-BLANK
o0629:		LD	(DE),A		; set workspace location to space.
		INC	DE		; next location.
		DJNZ	o0629		; loop back to SA-BLANK till all eleven done.

		LD	(IX+$01),$FF	; set first byte of ten character filename
					; to $FF as a default to signal null string.

		CALL	o2BF1		; routine STK-FETCH fetches the filename
					; parameters from the calculator stack.
					; length of string in BC.
					; start of string in DE.

		LD	HL,$FFF6	; prepare the value minus ten.
		DEC	BC		; decrement length.
					; ten becomes nine, zero becomes $FFFF.
		ADD	HL,BC		; trial addition.
		INC	BC		; restore true length.
		JR	NC,o064B	; forward to SA-NAME if length is one to ten.

;   the filename is more than ten characters in length or the null string.

		LD	A,(T_ADDR)	; fetch command from T_ADDR.
		AND	A		; test for zero - SAVE.
		JR	NZ,o0644	; forward to SA-NULL if not the SAVE command.

;   but no more than ten characters are allowed for SAVE.
;   The first ten characters of any other command parameter are acceptable.
;   Weird, but necessary, if saving to sectors.
;   Note. the golden rule that there are no restriction on anything is broken.

;; REPORT-Fa
o0642:		RST	08H		; ERROR-1
		DB	$0E		; Error Report: Invalid file name

;   continue with LOAD, MERGE, VERIFY and also SAVE within ten character limit.

;; SA-NULL
o0644:		LD	A,B		; test length of filename
		OR	C		; for zero.
		JR	Z,o0652		; forward to SA-DATA if so using the 255
					; indicator followed by spaces.

		LD	BC,$000A	; else trim length to ten.

;   other paths rejoin here with BC holding length in range 1 - 10.

;; SA-NAME
o064B:		PUSH	IX		; push start of file descriptor.
		POP	HL		; and pop into HL.

		INC	HL		; HL now addresses first byte of filename.
		EX	DE,HL		; transfer destination address to DE, start
					; of string in command to HL.
		LDIR			; copy up to ten bytes
					; if less than ten then trailing spaces follow.

;   the case for the null string rejoins here.

;; SA-DATA
o0652:		RST	18H		; GET-CHAR
		CP	$E4		; is character after filename the token 'DATA' ?
		JR	NZ,o06A0	; forward to SA-SCR$ to consider SCREEN$ if
					; not.

;   continue to consider DATA.

		LD	A,(T_ADDR)	; fetch command from T_ADDR
		CP	$03		; is it 'VERIFY' ?
		JP	Z,o1C8A		; jump forward to REPORT-C if so.
					; 'Nonsense in BASIC'
					; VERIFY "d" DATA is not allowed.

;   continue with SAVE, LOAD, MERGE of DATA.

		RST	20H		; NEXT-CHAR
		CALL	o28B2		; routine LOOK-VARS searches variables area
					; returning with carry reset if found or
					; checking syntax.
		SET	7,C		; this converts a simple string to a
					; string array. The test for an array or string
					; comes later.
		JR	NC,o0672	; forward to SA-V-OLD if variable found.

		LD	HL,$0000	; set destination to zero as not fixed.
		LD	A,(T_ADDR)	; fetch command from T_ADDR
		DEC	A		; test for 1 - LOAD
		JR	Z,o0685		; forward to SA-V-NEW with LOAD DATA.
					; to load a new array.

;   otherwise the variable was not found in run-time with SAVE/MERGE.

;; REPORT-2a
o0670:		RST	08H		; ERROR-1
		DB	$01		; Error Report: Variable not found

;   continue with SAVE/LOAD  DATA

;; SA-V-OLD
o0672:		JP	NZ,o1C8A	; to REPORT-C if not an array variable.
					; or erroneously a simple string.
					; 'Nonsense in BASIC'


		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o0692		; forward to SA-DATA-1 if checking syntax.

		INC	HL		; step past single character variable name.
		LD	A,(HL)		; fetch low byte of length.
		LD	(IX+$0B),A	; place in descriptor.
		INC	HL		; point to high byte.
		LD	A,(HL)		; and transfer that
		LD	(IX+$0C),A	; to descriptor.
		INC	HL		; increase pointer within variable.

;; SA-V-NEW
o0685:		LD	(IX+$0E),C	; place character array name in  header.
		LD	A,$01		; default to type numeric.
		BIT	6,C		; test result from look-vars.
		JR	Z,o068F		; forward to SA-V-TYPE if numeric.

		INC	A		; set type to 2 - string array.

;; SA-V-TYPE
o068F:		LD	(IX+$00),A	; place type 0, 1 or 2 in descriptor.

;; SA-DATA-1
o0692:		EX	DE,HL		; save var pointer in DE

		RST	20H		; NEXT-CHAR
		CP	$29		; is character ")" ?
		JR	NZ,o0672	; back if not to SA-V-OLD to report
					; 'Nonsense in BASIC'

		RST	20H		; NEXT-CHAR advances character address.
		CALL	o1BEE		; routine CHECK-END errors if not end of
					; the statement.

		EX	DE,HL		; bring back variables data pointer.
		JP	o075A		; jump forward to SA-ALL

; ---
;   the branch was here to consider a 'SCREEN$', the display file.

;; SA-SCR$
o06A0:		CP	$AA		; is character the token 'SCREEN$' ?
		JR	NZ,o06C3	; forward to SA-CODE if not.

		LD	A,(T_ADDR)	; fetch command from T_ADDR
		CP	$03		; is it MERGE ?
		JP	Z,o1C8A		; jump to REPORT-C if so.
					; 'Nonsense in BASIC'

;   continue with SAVE/LOAD/VERIFY SCREEN$.

		RST	20H		; NEXT-CHAR
		CALL	o1BEE		; routine CHECK-END errors if not at end of
					; statement.

;   continue in runtime.

		LD	(IX+$0B),$00	; set descriptor length
		LD	(IX+$0C),$1B	; to $1B00 to include bitmaps and attributes.

		LD	HL,$4000	; set start to display file start.
		LD	(IX+$0D),L	; place start in
		LD	(IX+$0E),H	; the descriptor.
		JR	o0710		; forward to SA-TYPE-3

; ---
;   the branch was here to consider CODE.

;; SA-CODE
o06C3:		CP	$AF		; is character the token 'CODE' ?
		JR	NZ,o0716	; forward if not to SA-LINE to consider an
					; auto-started BASIC program.

		LD	A,(T_ADDR)	; fetch command from T_ADDR
		CP	$03		; is it MERGE ?
		JP	Z,o1C8A		; jump forward to REPORT-C if so.
					; 'Nonsense in BASIC'


		RST	20H		; NEXT-CHAR advances character address.
		CALL	o2048		; routine PR-ST-END checks if a carriage
					; return or ":" follows.
		JR	NZ,o06E1	; forward to SA-CODE-1 if there are parameters.

		LD	A,(T_ADDR)	; else fetch the command from T_ADDR.
		AND	A		; test for zero - SAVE without a specification.
		JP	Z,o1C8A		; jump to REPORT-C if so.
					; 'Nonsense in BASIC'

;   for LOAD/VERIFY put zero on stack to signify handle at location saved from.

		CALL	o1CE6		; routine USE-ZERO
		JR	o06F0		; forward to SA-CODE-2

; ---

;   if there are more characters after CODE expect start and possibly length.

;; SA-CODE-1
o06E1:		CALL	o1C82		; routine EXPT-1NUM checks for numeric
					; expression and stacks it in run-time.

		RST	18H		; GET-CHAR
		CP	$2C		; does a comma follow ?
		JR	Z,o06F5		; forward if so to SA-CODE-3

;   else allow saved code to be loaded to a specified address.

		LD	A,(T_ADDR)	; fetch command from T_ADDR.
		AND	A		; is the command SAVE which requires length ?
		JP	Z,o1C8A		; jump to REPORT-C if so.
					; 'Nonsense in BASIC'

;   the command LOAD code may rejoin here with zero stacked as start.

;; SA-CODE-2
o06F0:		CALL	o1CE6		; routine USE-ZERO stacks zero for length.
		JR	o06F9		; forward to SA-CODE-4

; ---
;   the branch was here with SAVE CODE start,

;; SA-CODE-3
o06F5:		RST	20H		; NEXT-CHAR advances character address.
		CALL	o1C82		; routine EXPT-1NUM checks for expression
					; and stacks in run-time.

;   paths converge here and nothing must follow.

;; SA-CODE-4
o06F9:		CALL	o1BEE		; routine CHECK-END errors with extraneous
					; characters and quits if checking syntax.

;   in run-time there are two 16-bit parameters on the calculator stack.

		CALL	o1E99		; routine FIND-INT2 gets length.
		LD	(IX+$0B),C	; place length
		LD	(IX+$0C),B	; in descriptor.
		CALL	o1E99		; routine FIND-INT2 gets start.
		LD	(IX+$0D),C	; place start
		LD	(IX+$0E),B	; in descriptor.
		LD	H,B		; transfer the
		LD	L,C		; start to HL also.

;; SA-TYPE-3
o0710:		LD	(IX+$00),$03	; place type 3 - code in descriptor.
		JR	o075A		; forward to SA-ALL.

; ---
;   the branch was here with BASIC to consider an optional auto-start line
;   number.

;; SA-LINE
o0716:		CP	$CA		; is character the token 'LINE' ?
		JR	Z,o0723		; forward to SA-LINE-1 if so.

;   else all possibilities have been considered and nothing must follow.

		CALL	o1BEE		; routine CHECK-END

;   continue in run-time to save BASIC without auto-start.

		LD	(IX+$0E),$80	; place high line number in descriptor to
					; disable auto-start.
		JR	o073A		; forward to SA-TYPE-0 to save program.

; ---
;   the branch was here to consider auto-start.

;; SA-LINE-1
o0723:		LD	A,(T_ADDR)	; fetch command from T_ADDR
		AND	A		; test for SAVE.
		JP	NZ,o1C8A	; jump forward to REPORT-C with anything else.
					; 'Nonsense in BASIC'

;

		RST	20H		; NEXT-CHAR
		CALL	o1C82		; routine EXPT-1NUM checks for numeric
					; expression and stacks in run-time.
		CALL	o1BEE		; routine CHECK-END quits if syntax path.
		CALL	o1E99		; routine FIND-INT2 fetches the numeric
					; expression.
		LD	(IX+$0D),C	; place the auto-start
		LD	(IX+$0E),B	; line number in the descriptor.

;   Note. this isn't checked, but is subsequently handled by the system.
;   If the user typed 40000 instead of 4000 then it won't auto-start
;   at line 4000, or indeed, at all.

;   continue to save program and any variables.

;; SA-TYPE-0
o073A:		LD	(IX+$00),$00	; place type zero - program in descriptor.
		LD	HL,(E_LINE)	; fetch E_LINE to HL.
		LD	DE,(PROG)	; fetch PROG to DE.
		SCF			; set carry flag to calculate from end of
					; variables E_LINE -1.
		SBC	HL,DE		; subtract to give total length.

		LD	(IX+$0B),L	; place total length
		LD	(IX+$0C),H	; in descriptor.
		LD	HL,(VARS)	; load HL from system variable VARS
		SBC	HL,DE		; subtract to give program length.
		LD	(IX+$0F),L	; place length of program
		LD	(IX+$10),H	; in the descriptor.
		EX	DE,HL		; start to HL, length to DE.

;; SA-ALL
o075A:		LD	A,(T_ADDR)	; fetch command from T_ADDR
		AND	A		; test for zero - SAVE.
		JP	Z,o0970		; jump forward to SA-CONTRL with SAVE  ->

; ---
;   continue with LOAD, MERGE and VERIFY.

		PUSH	HL		; save start.
		LD	BC,$0011	; prepare to add seventeen
		ADD	IX,BC		; to point IX at second descriptor.

;; LD-LOOK-H
o0767:		PUSH	IX		; save IX
		LD	DE,$0011	; seventeen bytes
		XOR	A		; reset zero flag
		SCF			; set carry flag
		CALL	o0556		; routine LD-BYTES loads a header from tape
					; to second descriptor.
		POP	IX		; restore IX.
		JR	NC,o0767	; loop back to LD-LOOK-H until header found.

		LD	A,$FE		; select system channel 'S'
		CALL	o1601		; routine CHAN-OPEN opens it.

		LD	(IY+$52),$03	; set SCR_CT to 3 lines.

		LD	C,$80		; C has bit 7 set to indicate type mismatch as
					; a default startpoint.

		LD	A,(IX+$00)	; fetch loaded header type to A
		CP	(IX-$11)	; compare with expected type.
		JR	NZ,o078A	; forward to LD-TYPE with mis-match.

		LD	C,$F6		; set C to minus ten - will count characters
					; up to zero.

;; LD-TYPE
o078A:		CP	$04		; check if type in acceptable range 0 - 3.
		JR	NC,o0767	; back to LD-LOOK-H with 4 and over.

;   else A indicates type 0-3.

		LD	DE,o09C0	; address base of last 4 tape messages
		PUSH	BC		; save BC
		CALL	o0C0A		; routine PO-MSG outputs relevant message.
					; Note. all messages have a leading newline.
		POP	BC		; restore BC

		PUSH	IX		; transfer IX,
		POP	DE		; the 2nd descriptor, to DE.
		LD	HL,$FFF0	; prepare minus seventeen.
		ADD	HL,DE		; add to point HL to 1st descriptor.
		LD	B,$0A		; the count will be ten characters for the
					; filename.

		LD	A,(HL)		; fetch first character and test for
		INC	A		; value 255.
		JR	NZ,o07A6	; forward to LD-NAME if not the wildcard.

;   but if it is the wildcard, then add ten to C which is minus ten for a type
;   match or -128 for a type mismatch. Although characters have to be counted
;   bit 7 of C will not alter from state set here.

		LD	A,C		; transfer $F6 or $80 to A
		ADD	A,B		; add $0A
		LD	C,A		; place result, zero or -118, in C.

;   At this point we have either a type mismatch, a wildcard match or ten
;   characters to be counted. The characters must be shown on the screen.

;; LD-NAME
o07A6:		INC	DE		; address next input character
		LD	A,(DE)		; fetch character
		CP	(HL)		; compare to expected
		INC	HL		; address next expected character
		JR	NZ,o07AD	; forward to LD-CH-PR with mismatch

		INC	C		; increment matched character count

;; LD-CH-PR
o07AD:		RST	10H		; PRINT-A prints character
		DJNZ	o07A6		; loop back to LD-NAME for ten characters.

;   if ten characters matched and the types previously matched then C will
;   now hold zero.

		BIT	7,C		; test if all matched
		JR	NZ,o0767	; back to LD-LOOK-H if not

;   else print a terminal carriage return.

		LD	A,$0D		; prepare carriage return.
		RST	10H		; PRINT-A outputs it.

;   The various control routines for LOAD, VERIFY and MERGE are executed
;   during the one-second gap following the header on tape.

		POP	HL		; restore xx
		LD	A,(IX+$00)	; fetch incoming type
		CP	$03		; compare with CODE
		JR	Z,o07CB		; forward to VR-CONTRL if it is CODE.

;  type is a program or an array.

		LD	A,(T_ADDR)	; fetch command from T_ADDR
		DEC	A		; was it LOAD ?
		JP	Z,o0808		; JUMP forward to LD-CONTRL if so to
					; load BASIC or variables.

		CP	$02		; was command MERGE ?
		JP	Z,o08B6		; jump forward to ME-CONTRL if so.

;   else continue into VERIFY control routine to verify.

; ----------------------------
; THE 'VERIFY CONTROL' ROUTINE
; ----------------------------
;   There are two branches to this routine.
;   1) From above to verify a program or array
;   2) from earlier with no carry to load or verify code.

;; VR-CONTRL
o07CB:		PUSH	HL		; save pointer to data.
		LD	L,(IX-$06)	; fetch length of old data
		LD	H,(IX-$05)	; to HL.
		LD	E,(IX+$0B)	; fetch length of new data
		LD	D,(IX+$0C)	; to DE.
		LD	A,H		; check length of old
		OR	L		; for zero.
		JR	Z,o07E9		; forward to VR-CONT-1 if length unspecified
					; e.g. LOAD "x" CODE

;   as opposed to, say, LOAD 'x' CODE 32768,300.

		SBC	HL,DE		; subtract the two lengths.
		JR	C,o0806		; forward to REPORT-R if the length on tape is
					; larger than that specified in command.
					; 'Tape loading error'

		JR	Z,o07E9		; forward to VR-CONT-1 if lengths match.

;   a length on tape shorter than expected is not allowed for CODE

		LD	A,(IX+$00)	; else fetch type from tape.
		CP	$03		; is it CODE ?
		JR	NZ,o0806	; forward to REPORT-R if so
					; 'Tape loading error'

;; VR-CONT-1
o07E9:		POP	HL		; pop pointer to data
		LD	A,H		; test for zero
		OR	L		; e.g. LOAD 'x' CODE
		JR	NZ,o07F4	; forward to VR-CONT-2 if destination specified.

		LD	L,(IX+$0D)	; else use the destination in the header
		LD	H,(IX+$0E)	; and load code at address saved from.

;; VR-CONT-2
o07F4:		PUSH	HL		; push pointer to start of data block.
		POP	IX		; transfer to IX.
		LD	A,(T_ADDR)	; fetch reduced command from T_ADDR
		CP	$02		; is it VERIFY ?
		SCF			; prepare a set carry flag
		JR	NZ,o0800	; skip to VR-CONT-3 if not

		AND	A		; clear carry flag for VERIFY so that
					; data is not loaded.

;; VR-CONT-3
o0800:		LD	A,$FF		; signal data block to be loaded

; -----------------
; Load a data block
; -----------------
;   This routine is called from 3 places other than above to load a data block.
;   In all cases the accumulator is first set to $FF so the routine could be
;   called at the previous instruction.

;; LD-BLOCK
o0802:		CALL	o0556		; routine LD-BYTES
		RET	C		; return if successful.


;; REPORT-R
o0806:		RST	08H		; ERROR-1
		DB	$1A		; Error Report: Tape loading error

; --------------------------
; THE 'LOAD CONTROL' ROUTINE
; --------------------------
;   This branch is taken when the command is LOAD with type 0, 1 or 2.

;; LD-CONTRL
o0808:		LD	E,(IX+$0B)	; fetch length of found data block
		LD	D,(IX+$0C)	; from 2nd descriptor.
		PUSH	HL		; save destination
		LD	A,H		; test for zero
		OR	L		;
		JR	NZ,o0819	; forward if not to LD-CONT-1

		INC	DE		; increase length
		INC	DE		; for letter name
		INC	DE		; and 16-bit length
		EX	DE,HL		; length to HL,
		JR	o0825		; forward to LD-CONT-2

; ---

;; LD-CONT-1
o0819:		LD	L,(IX-$06)	; fetch length from
		LD	H,(IX-$05)	; the first header.
		EX	DE,HL		;
		SCF			; set carry flag
		SBC	HL,DE		;
		JR	C,o082E		; to LD-DATA

;; LD-CONT-2
o0825:		LD	DE,$0005	; allow overhead of five bytes.
		ADD	HL,DE		; add in the difference in data lengths.
		LD	B,H		; transfer to
		LD	C,L		; the BC register pair
		CALL	o1F05		; routine TEST-ROOM fails if not enough room.

;; LD-DATA
o082E:		POP	HL		; pop destination
		LD	A,(IX+$00)	; fetch type 0, 1 or 2.
		AND	A		; test for program and variables.
		JR	Z,o0873		; forward if so to LD-PROG

;   the type is a numeric or string array.

		LD	A,H		; test the destination for zero
		OR	L		; indicating variable does not already exist.
		JR	Z,o084C		; forward if so to LD-DATA-1

;   else the destination is the first dimension within the array structure

		DEC	HL		; address high byte of total length
		LD	B,(HL)		; transfer to B.
		DEC	HL		; address low byte of total length.
		LD	C,(HL)		; transfer to C.
		DEC	HL		; point to letter of variable.
		INC	BC		; adjust length to
		INC	BC		; include these
		INC	BC		; three bytes also.
		LD	(X_PTR),IX	; save header pointer in X_PTR.
		CALL	o19E8		; routine RECLAIM-2 reclaims the old variable
					; sliding workspace including the two headers
					; downwards.
		LD	IX,(X_PTR)	; reload IX from X_PTR which will have been
					; adjusted down by POINTERS routine.

;; LD-DATA-1
o084C:		LD	HL,(E_LINE)	; address E_LINE
		DEC	HL		; now point to the $80 variables end-marker.
		LD	C,(IX+$0B)	; fetch new data length
		LD	B,(IX+$0C)	; from 2nd header.
		PUSH	BC		; * save it.
		INC	BC		; adjust the
		INC	BC		; length to include
		INC	BC		; letter name and total length.
		LD	A,(IX-$03)	; fetch letter name from old header.
		PUSH	AF		; preserve accumulator though not corrupted.

		CALL	o1655		; routine MAKE-ROOM creates space for variable
					; sliding workspace up. IX no longer addresses
					; anywhere meaningful.
		INC	HL		; point to first new location.

		POP	AF		; fetch back the letter name.
		LD	(HL),A		; place in first new location.
		POP	DE		; * pop the data length.
		INC	HL		; address 2nd location
		LD	(HL),E		; store low byte of length.
		INC	HL		; address next.
		LD	(HL),D		; store high byte.
		INC	HL		; address start of data.
		PUSH	HL		; transfer address
		POP	IX		; to IX register pair.
		SCF			; set carry flag indicating load not verify.
		LD	A,$FF		; signal data not header.
		JP	o0802		; JUMP back to LD-BLOCK

; -----------------
;   the branch is here when a program as opposed to an array is to be loaded.

;; LD-PROG
o0873:		EX	DE,HL		; transfer dest to DE.
		LD	HL,(E_LINE)	; address E_LINE
		DEC	HL		; now variables end-marker.
		LD	(X_PTR),IX	; place the IX header pointer in X_PTR
		LD	C,(IX+$0B)	; get new length
		LD	B,(IX+$0C)	; from 2nd header
		PUSH	BC		; and save it.

		CALL	o19E5		; routine RECLAIM-1 reclaims program and vars.
					; adjusting X-PTR.

		POP	BC		; restore new length.
		PUSH	HL		; * save start
		PUSH	BC		; ** and length.

		CALL	o1655		; routine MAKE-ROOM creates the space.

		LD	IX,(X_PTR)	; reload IX from adjusted X_PTR
		INC	HL		; point to start of new area.
		LD	C,(IX+$0F)	; fetch length of BASIC on tape
		LD	B,(IX+$10)	; from 2nd descriptor
		ADD	HL,BC		; add to address the start of variables.
		LD	(VARS),HL	; set system variable VARS

		LD	H,(IX+$0E)	; fetch high byte of autostart line number.
		LD	A,H		; transfer to A
		AND	$C0		; test if greater than $3F.
		JR	NZ,o08AD	; forward to LD-PROG-1 if so with no autostart.

		LD	L,(IX+$0D)	; else fetch the low byte.
		LD	(NEWPPC),HL	; set system variable to line number NEWPPC
		LD	(IY+$0A),$00	; set statement NSPPC to zero.

;; LD-PROG-1
o08AD:		POP	DE		; ** pop the length
		POP	IX		; * and start.
		SCF			; set carry flag
		LD	A,$FF		; signal data as opposed to a header.
		JP	o0802		; jump back to LD-BLOCK

; ---------------------------
; THE 'MERGE CONTROL' ROUTINE
; ---------------------------
;   the branch was here to merge a program and its variables or an array.
;

;; ME-CONTRL
o08B6:		LD	C,(IX+$0B)	; fetch length
		LD	B,(IX+$0C)	; of data block on tape.
		PUSH	BC		; save it.
		INC	BC		; one for the pot.

		RST	30H		; BC-SPACES creates room in workspace.
					; HL addresses last new location.
		LD	(HL),$80	; place end-marker at end.
		EX	DE,HL		; transfer first location to HL.
		POP	DE		; restore length to DE.
		PUSH	HL		; save start.

		PUSH	HL		; and transfer it
		POP	IX		; to IX register.
		SCF			; set carry flag to load data on tape.
		LD	A,$FF		; signal data not a header.
		CALL	o0802		; routine LD-BLOCK loads to workspace.
		POP	HL		; restore first location in workspace to HL.
		LD	DE,(PROG)	; set DE from system variable PROG.

;   now enter a loop to merge the data block in workspace with the program and
;   variables.

;; ME-NEW-LP
o08D2:		LD	A,(HL)		; fetch next byte from workspace.
		AND	$C0		; compare with $3F.
		JR	NZ,o08F0	; forward to ME-VAR-LP if a variable or
					; end-marker.

;   continue when HL addresses a BASIC line number.

;; ME-OLD-LP
o08D7:		LD	A,(DE)		; fetch high byte from program area.
		INC	DE		; bump prog address.
		CP	(HL)		; compare with that in workspace.
		INC	HL		; bump workspace address.
		JR	NZ,o08DF	; forward to ME-OLD-L1 if high bytes don't match

		LD	A,(DE)		; fetch the low byte of program line number.
		CP	(HL)		; compare with that in workspace.

;; ME-OLD-L1
o08DF:		DEC	DE		; point to start of
		DEC	HL		; respective lines again.
		JR	NC,o08EB	; forward to ME-NEW-L2 if line number in
					; workspace is less than or equal to current
					; program line as has to be added to program.

		PUSH	HL		; else save workspace pointer.
		EX	DE,HL		; transfer prog pointer to HL
		CALL	o19B8		; routine NEXT-ONE finds next line in DE.
		POP	HL		; restore workspace pointer
		JR	o08D7		; back to ME-OLD-LP until destination position
					; in program area found.

; ---
;   the branch was here with an insertion or replacement point.

;; ME-NEW-L2
o08EB:		CALL	o092C		; routine ME-ENTER enters the line
		JR	o08D2		; loop back to ME-NEW-LP.

; ---
;   the branch was here when the location in workspace held a variable.

;; ME-VAR-LP
o08F0:		LD	A,(HL)		; fetch first byte of workspace variable.
		LD	C,A		; copy to C also.
		CP	$80		; is it the end-marker ?
		RET	Z		; return if so as complete.  >>>>>

		PUSH	HL		; save workspace area pointer.
		LD	HL,(VARS)	; load HL with VARS - start of variables area.

;; ME-OLD-VP
o08F9:		LD	A,(HL)		; fetch first byte.
		CP	$80		; is it the end-marker ?
		JR	Z,o0923		; forward if so to ME-VAR-L2 to add
					; variable at end of variables area.

		CP	C		; compare with variable in workspace area.
		JR	Z,o0909		; forward to ME-OLD-V2 if a match to replace.

;   else entire variables area has to be searched.

;; ME-OLD-V1
o0901:		PUSH	BC		; save character in C.
		CALL	o19B8		; routine NEXT-ONE gets following variable
					; address in DE.
		POP	BC		; restore character in C
		EX	DE,HL		; transfer next address to HL.
		JR	o08F9		; loop back to ME-OLD-VP

; ---
;   the branch was here when first characters of name matched.

;; ME-OLD-V2
o0909:		AND	$E0		; keep bits 11100000
		CP	$A0		; compare   10100000 - a long-named variable.

		JR	NZ,o0921	; forward to ME-VAR-L1 if just one-character.

;   but long-named variables have to be matched character by character.

		POP	DE		; fetch workspace 1st character pointer
		PUSH	DE		; and save it on the stack again.
		PUSH	HL		; save variables area pointer on stack.

;; ME-OLD-V3
o0912:		INC	HL		; address next character in vars area.
		INC	DE		; address next character in workspace area.
		LD	A,(DE)		; fetch workspace character.
		CP	(HL)		; compare to variables character.
		JR	NZ,o091E	; forward to ME-OLD-V4 with a mismatch.

		RLA			; test if the terminal inverted character.
		JR	NC,o0912	; loop back to ME-OLD-V3 if more to test.

;   otherwise the long name matches in its entirety.

		POP	HL		; restore pointer to first character of variable
		JR	o0921		; forward to ME-VAR-L1

; ---
;   the branch is here when two characters don't match

;; ME-OLD-V4
o091E:		POP	HL		; restore the prog/vars pointer.
		JR	o0901		; back to ME-OLD-V1 to resume search.

; ---
;   branch here when variable is to replace an existing one

;; ME-VAR-L1
o0921:		LD	A,$FF		; indicate a replacement.

;   this entry point is when A holds $80 indicating a new variable.

;; ME-VAR-L2
o0923:		POP	DE		; pop workspace pointer.
		EX	DE,HL		; now make HL workspace pointer, DE vars pointer
		INC	A		; zero flag set if replacement.
		SCF			; set carry flag indicating a variable not a
					; program line.
		CALL	o092C		; routine ME-ENTER copies variable in.
		JR	o08F0		; loop back to ME-VAR-LP

; ------------------------
; Merge a Line or Variable
; ------------------------
;   A BASIC line or variable is inserted at the current point. If the line
;   number or variable names match (zero flag set) then a replacement takes
;   place.

;; ME-ENTER
o092C:		JR	NZ,o093E	; forward to ME-ENT-1 for insertion only.

;   but the program line or variable matches so old one is reclaimed.

		EX	AF,AF'		; save flag??
		LD	(X_PTR),HL	; preserve workspace pointer in dynamic X_PTR
		EX	DE,HL		; transfer program dest pointer to HL.
		CALL	o19B8		; routine NEXT-ONE finds following location
					; in program or variables area.
		CALL	o19E8		; routine RECLAIM-2 reclaims the space between.
		EX	DE,HL		; transfer program dest pointer back to DE.
		LD	HL,(X_PTR)	; fetch adjusted workspace pointer from X_PTR
		EX	AF,AF'		; restore flags.

;   now the new line or variable is entered.

;; ME-ENT-1
o093E:		EX	AF,AF'		; save or re-save flags.
		PUSH	DE		; save dest pointer in prog/vars area.
		CALL	o19B8		; routine NEXT-ONE finds next in workspace.
					; gets next in DE, difference in BC.
					; prev addr in HL
		LD	(X_PTR),HL	; store pointer in X_PTR
		LD	HL,(PROG)	; load HL from system variable PROG
		EX	(SP),HL		; swap with prog/vars pointer on stack.
		PUSH	BC		; ** save length of new program line/variable.
		EX	AF,AF'		; fetch flags back.
		JR	C,o0955		; skip to ME-ENT-2 if variable

		DEC	HL		; address location before pointer
		CALL	o1655		; routine MAKE-ROOM creates room for BASIC line
		INC	HL		; address next.
		JR	o0958		; forward to ME-ENT-3

; ---

;; ME-ENT-2
o0955:		CALL	o1655		; routine MAKE-ROOM creates room for variable.

;; ME-ENT-3
o0958:		INC	HL		; address next?

		POP	BC		; ** pop length
		POP	DE		; * pop value for PROG which may have been
					; altered by POINTERS if first line.
		LD	(PROG),DE	; set PROG to original value.
		LD	DE,(X_PTR)	; fetch adjusted workspace pointer from X_PTR
		PUSH	BC		; save length
		PUSH	DE		; and workspace pointer
		EX	DE,HL		; make workspace pointer source, prog/vars
					; pointer the destination
		LDIR			; copy bytes of line or variable into new area.
		POP	HL		; restore workspace pointer.
		POP	BC		; restore length.
		PUSH	DE		; save new prog/vars pointer.
		CALL	o19E8		; routine RECLAIM-2 reclaims the space used
					; by the line or variable in workspace block
					; as no longer required and space could be
					; useful for adding more lines.
		POP	DE		; restore the prog/vars pointer
		RET			; return.

; --------------------------
; THE 'SAVE CONTROL' ROUTINE
; --------------------------
;   A branch from the main SAVE-ETC routine at SAVE-ALL.
;   First the header data is saved. Then after a wait of 1 second
;   the data itself is saved.
;   HL points to start of data.
;   IX points to start of descriptor.

;; SA-CONTRL
o0970:		PUSH	HL		; save start of data

		LD	A,$FD		; select system channel 'S'
		CALL	o1601		; routine CHAN-OPEN

		XOR	A		; clear to address table directly
		LD	DE,o09A1	; address: tape-msgs
		CALL	o0C0A		; routine PO-MSG -
					; 'Start tape then press any key.'

		SET	5,(IY+$02)	; TV_FLAG  - Signal lower screen requires
					; clearing
		CALL	o15D4		; routine WAIT-KEY

		PUSH	IX		; save pointer to descriptor.
		LD	DE,$0011	; there are seventeen bytes.
		XOR	A		; signal a header.
		CALL	o04C2		; routine SA-BYTES

		POP	IX		; restore descriptor pointer.

		LD	B,$32		; wait for a second - 50 interrupts.

;; SA-1-SEC
o0991:		HALT
		DJNZ	o0991		; back to SA-1-SEC until pause complete.

		LD	E,(IX+$0B)	; fetch length of bytes from the
		LD	D,(IX+$0C)	; descriptor.

		LD	A,$FF		; signal data bytes.

		POP	IX		; retrieve pointer to start
		JP	o04C2		; jump back to SA-BYTES


;   Arrangement of two headers in workspace.
;   Originally IX addresses first location and only one header is required
;   when saving.
;
;   OLD     NEW         PROG   DATA  DATA  CODE
;   HEADER  HEADER             num   chr          NOTES.
;   ------  ------      ----   ----  ----  ----   -----------------------------
;   IX-$11  IX+$00      0      1     2     3      Type.
;   IX-$10  IX+$01      x      x     x     x      F  ($FF if filename is null).
;   IX-$0F  IX+$02      x      x     x     x      i
;   IX-$0E  IX+$03      x      x     x     x      l
;   IX-$0D  IX+$04      x      x     x     x      e
;   IX-$0C  IX+$05      x      x     x     x      n
;   IX-$0B  IX+$06      x      x     x     x      a
;   IX-$0A  IX+$07      x      x     x     x      m
;   IX-$09  IX+$08      x      x     x     x      e
;   IX-$08  IX+$09      x      x     x     x      .
;   IX-$07  IX+$0A      x      x     x     x      (terminal spaces).
;   IX-$06  IX+$0B      lo     lo    lo    lo     Total
;   IX-$05  IX+$0C      hi     hi    hi    hi     Length of datablock.
;   IX-$04  IX+$0D      Auto   -     -     Start  Various
;   IX-$03  IX+$0E      Start  a-z   a-z   addr   ($80 if no autostart).
;   IX-$02  IX+$0F      lo     -     -     -      Length of Program
;   IX-$01  IX+$10      hi     -     -     -      only i.e. without variables.
;


; ------------------------
; Canned cassette messages
; ------------------------
;   The last-character-inverted Cassette messages.
;   Starts with normal initial step-over byte.

;; tape-msgs
o09A1:		DB	$80
		DM	"Press REC & PLAY, then any key"
o09C0:		DB	'.'+$80
		DB	$0D
		DM	"Program:"
		DB	" "+$80
		DB	$0D
		DM	"Number array:"
		DB	" "+$80
		DB	$0D
		DM	"Character array:"
		DB	" "+$80
		DB	$0D
		DM	"Bytes:"
		DB	" "+$80

;**************************************************
;** Part 5. SCREEN AND PRINTER HANDLING ROUTINES **
;**************************************************

; --------------------------
; THE 'PRINT OUTPUT' ROUTINE
; --------------------------
;   This is the routine most often used by the RST 10 restart although the
;   subroutine is on two occasions called directly when it is known that
;   output will definitely be to the lower screen.

;; PRINT-OUT
o09F4:		CALL	o0B03		; routine PO-FETCH fetches print position
					; to HL register pair.
		CP	$20		; is character a space or higher ?
		JP	NC,o0AD9	; jump forward to PO-ABLE if so.

		CP	$06		; is character in range 00-05 ?
		JR	C,o0A69		; to PO-QUEST to print "?" if so.

		CP	$18		; is character in range 24d - 31d ?
		JR	NC,o0A69	; to PO-QUEST to also print "?" if so.

		LD	HL,o0A11-6	; address 0A0B - the base address of control
					; character table - where zero would be.
		LD	E,A		; control character 06 - 23d
		LD	D,$00		; is transferred to DE.

		ADD	HL,DE		; index into table.

		LD	E,(HL)		; fetch the offset to routine.
		ADD	HL,DE		; add to make HL the address.
		PUSH	HL		; push the address.

		JP	o0B03		; Jump forward to PO-FETCH,
					; as the screen/printer position has been
					; disturbed, and then indirectly to the PO-STORE
					; routine on stack.

; -----------------------------
; THE 'CONTROL CHARACTER' TABLE
; -----------------------------
;   For control characters in the range 6 - 23d the following table
;   is indexed to provide an offset to the handling routine that
;   follows the table.

;; ctlchrtab
o0A11:		DB	o0A5F-$		; 06d offset $4E to Address: PO-COMMA
		DB	o0A69-$		; 07d offset $57 to Address: PO-QUEST
		DB	o0A23-$		; 08d offset $10 to Address: PO-BACK-1
		DB	o0A3D-$		; 09d offset $29 to Address: PO-RIGHT
		DB	o0A69-$		; 10d offset $54 to Address: PO-QUEST
		DB	o0A69-$		; 11d offset $53 to Address: PO-QUEST
		DB	o0A69-$		; 12d offset $52 to Address: PO-QUEST
		DB	o0A4F-$		; 13d offset $37 to Address: PO-ENTER
		DB	o0A69-$		; 14d offset $50 to Address: PO-QUEST
		DB	o0A69-$		; 15d offset $4F to Address: PO-QUEST
		DB	o0A7A-$		; 16d offset $5F to Address: PO-1-OPER
		DB	o0A7A-$		; 17d offset $5E to Address: PO-1-OPER
		DB	o0A7A-$		; 18d offset $5D to Address: PO-1-OPER
		DB	o0A7A-$		; 19d offset $5C to Address: PO-1-OPER
		DB	o0A7A-$		; 20d offset $5B to Address: PO-1-OPER
		DB	o0A7A-$		; 21d offset $5A to Address: PO-1-OPER
		DB	o0A75-$		; 22d offset $54 to Address: PO-2-OPER
		DB	o0A75-$		; 23d offset $53 to Address: PO-2-OPER


; -------------------------
; THE 'CURSOR LEFT' ROUTINE
; -------------------------
;   Backspace and up a line if that action is from the left of screen.
;   For ZX printer backspace up to first column but not beyond.

;; PO-BACK-1
o0A23:		INC	C		; move left one column.
		LD	A,$22		; value $21 is leftmost column.
		CP	C		; have we passed ?
		JR	NZ,o0A3A	; to PO-BACK-3 if not and store new position.

		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		JR	NZ,o0A38	; to PO-BACK-2 if so, as we are unable to
					; backspace from the leftmost position.


		INC	B		; move up one screen line
		LD	C,$02		; the rightmost column position.
		LD	A,$18		; Note. This should be $19
					; credit. Dr. Frank O'Hara, 1982

		CP	B		; has position moved past top of screen ?
		JR	NZ,o0A3A	; to PO-BACK-3 if not and store new position.

		DEC	B		; else back to $18.

;; PO-BACK-2
o0A38:		LD	C,$21		; the leftmost column position.

;; PO-BACK-3
o0A3A:		JP	o0DD9		; to CL-SET and PO-STORE to save new
					; position in system variables.

; --------------------------
; THE 'CURSOR RIGHT' ROUTINE
; --------------------------
;   This moves the print position to the right leaving a trail in the
;   current background colour.
;   "However the programmer has failed to store the new print position
;   so CHR$ 9 will only work if the next print position is at a newly
;   defined place.
;   e.g. PRINT PAPER 2; CHR$ 9; AT 4,0;
;   does work but is not very helpful"
;   - Dr. Ian Logan, Understanding Your Spectrum, 1982.

;; PO-RIGHT
o0A3D:		LD	A,(P_FLAG)	; fetch P_FLAG value
		PUSH	AF		; and save it on stack.

		LD	(IY+$57),$01	; temporarily set P_FLAG 'OVER 1'.
		LD	A,$20		; prepare a space.
		CALL	o0B65		; routine PO-CHAR to print it.
					; Note. could be PO-ABLE which would update
					; the column position.

		POP	AF		; restore the permanent flag.
		LD	(P_FLAG),A	; and restore system variable P_FLAG

		RET			; return without updating column position

; -----------------------
; Perform carriage return
; -----------------------
; A carriage return is 'printed' to screen or printer buffer.

;; PO-ENTER
o0A4F:		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		JP	NZ,o0ECD	; to COPY-BUFF if so, to flush buffer and reset
					; the print position.

		LD	C,$21		; the leftmost column position.
		CALL	o0C55		; routine PO-SCR handles any scrolling required.
		DEC	B		; to next screen line.
		JP	o0DD9		; jump forward to CL-SET to store new position.

; -----------
; Print comma
; -----------
; The comma control character. The 32 column screen has two 16 character
; tabstops.  The routine is only reached via the control character table.

;; PO-COMMA
o0A5F:		CALL	o0B03		; routine PO-FETCH - seems unnecessary.

		LD	A,C		; the column position. $21-$01
		DEC	A		; move right. $20-$00
		DEC	A		; and again   $1F-$00 or $FF if trailing
		AND	$10		; will be $00 or $10.
		JR	o0AC3		; forward to PO-FILL

; -------------------
; Print question mark
; -------------------
; This routine prints a question mark which is commonly
; used to print an unassigned control character in range 0-31d.
; there are a surprising number yet to be assigned.

;; PO-QUEST
o0A69:		LD	A,$3F		; prepare the character "?".
		JR	o0AD9		; forward to PO-ABLE.

; --------------------------------
; Control characters with operands
; --------------------------------
; Certain control characters are followed by 1 or 2 operands.
; The entry points from control character table are PO-2-OPER and PO-1-OPER.
; The routines alter the output address of the current channel so that
; subsequent RST $10 instructions take the appropriate action
; before finally resetting the output address back to PRINT-OUT.

;; PO-TV-2
o0A6D:		LD	DE,o0A87	; address: PO-CONT will be next output routine
		LD	($5C0F),A	; store first operand in TVDATA-hi
		JR	o0A80		; forward to PO-CHANGE >>

; ---

; -> This initial entry point deals with two operands - AT or TAB.

;; PO-2-OPER
o0A75:		LD	DE,o0A6D	; address: PO-TV-2 will be next output routine
		JR	o0A7D		; forward to PO-TV-1

; ---

; -> This initial entry point deals with one operand INK to OVER.

;; PO-1-OPER
o0A7A:		LD	DE,o0A87	; address: PO-CONT will be next output routine

;; PO-TV-1
o0A7D:		LD	(TVDATA),A	; store control code in TVDATA-lo

;; PO-CHANGE
o0A80:		LD	HL,(CURCHL)	; use CURCHL to find current output channel.
		LD	(HL),E		; make it
		INC	HL		; the supplied
		LD	(HL),D		; address from DE.
		RET			; return.

; ---

;; PO-CONT
o0A87:		LD	DE,o09F4	; Address: PRINT-OUT
		CALL	o0A80		; routine PO-CHANGE to restore normal channel.
		LD	HL,(TVDATA)	; TVDATA gives control code and possible
					; subsequent character
		LD	D,A		; save current character
		LD	A,L		; the stored control code
		CP	$16		; was it INK to OVER (1 operand) ?
		JP	C,o2211		; to CO-TEMP-5

		JR	NZ,o0AC2	; to PO-TAB if not 22d i.e. 23d TAB.

; else must have been 22d AT.
		LD	B,H		; line to H   (0-23d)
		LD	C,D		; column to C (0-31d)
		LD	A,$1F		; the value 31d
		SUB	C		; reverse the column number.
		JR	C,o0AAC		; to PO-AT-ERR if C was greater than 31d.

		ADD	A,$02		; transform to system range $02-$21
		LD	C,A		; and place in column register.

		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		JR	NZ,o0ABF	; to PO-AT-SET as line can be ignored.

		LD	A,$16		; 22 decimal
		SUB	B		; subtract line number to reverse
					; 0 - 22 becomes 22 - 0.

;; PO-AT-ERR
o0AAC:		JP	C,o1E9F		; to REPORT-B if higher than 22 decimal
					; Integer out of range.

		INC	A		; adjust for system range $01-$17
		LD	B,A		; place in line register
		INC	B		; adjust to system range  $02-$18
		BIT	0,(IY+$02)	; TV_FLAG  - Lower screen in use ?
		JP	NZ,o0C55	; exit to PO-SCR to test for scrolling

		CP	(IY+$31)	; Compare against DF_SZ
		JP	C,o0C86		; to REPORT-5 if too low
					; Out of screen.

;; PO-AT-SET
o0ABF:		JP	o0DD9		; print position is valid so exit via CL-SET

; ---

; Continue here when dealing with TAB.
; Note. In BASIC, TAB is followed by a 16-bit number and was initially
; designed to work with any output device.

;; PO-TAB
o0AC2:		LD	A,H		; transfer parameter to A
					; Losing current character -
					; High byte of TAB parameter.


;; PO-FILL
o0AC3:		CALL	o0B03		; routine PO-FETCH, HL-addr, BC=line/column.
					; column 1 (right), $21 (left)
		ADD	A,C		; add operand to current column
		DEC	A		; range 0 - 31+
		AND	$1F		; make range 0 - 31d
		RET	Z		; return if result zero

		LD	D,A		; Counter to D
		SET	0,(IY+$01)	; update FLAGS  - signal suppress leading space.

;; PO-SPACE
o0AD0:		LD	A,$20		; space character.

		CALL	o0C3B		; routine PO-SAVE prints the character
					; using alternate set (normal output routine)

		DEC	D		; decrement counter.
		JR	NZ,o0AD0	; to PO-SPACE until done

		RET			; return

; ----------------------
; Printable character(s)
; ----------------------
; This routine prints printable characters and continues into
; the position store routine

;; PO-ABLE
o0AD9:		CALL	o0B24		; routine PO-ANY
					; and continue into position store routine.

; ----------------------------
; THE 'POSITION STORE' ROUTINE
; ----------------------------
;   This routine updates the system variables associated with the main screen,
;   the lower screen/input buffer or the ZX printer.

;; PO-STORE
o0ADC:		BIT	1,(IY+$01)	; Test FLAGS - is printer in use ?
		JR	NZ,o0AFC	; Forward, if so, to PO-ST-PR

		BIT	0,(IY+$02)	; Test TV_FLAG - is lower screen in use ?
		JR	NZ,o0AF0	; Forward, if so, to PO-ST-E

;   This section deals with the upper screen.

		LD	(S_POSN),BC	; Update S_POSN - line/column upper screen
		LD	(DF_CC),HL	; Update DF_CC - upper display file address

		RET			; Return.

; ---

;   This section deals with the lower screen.

;; PO-ST-E
o0AF0:		LD	(SPOSNL),BC	; Update SPOSNL line/column lower screen
		LD	(ECHO_E),BC	; Update ECHO_E line/column input buffer
		LD	(DF_CCL),HL	; Update DFCCL  lower screen memory address
		RET			; Return.

; ---

;   This section deals with the ZX Printer.

;; PO-ST-PR
o0AFC:		LD	(IY+$45),C	; Update P_POSN column position printer
		LD	(PR_CC),HL	; Update PR_CC - full printer buffer memory
					; address
		RET			; Return.

;   Note. that any values stored in location 23681 will be overwritten with
;   the value 91 decimal.
;   Credit April 1983, Dilwyn Jones. "Delving Deeper into your ZX Spectrum".

; ----------------------------
; THE 'POSITION FETCH' ROUTINE
; ----------------------------
;   This routine fetches the line/column and display file address of the upper
;   and lower screen or, if the printer is in use, the column position and
;   absolute memory address.
;   Note. that PR-CC-hi (23681) is used by this routine and if, in accordance
;   with the manual (that says this is unused), the location has been used for
;   other purposes, then subsequent output to the printer buffer could corrupt
;   a 256-byte section of memory.

;; PO-FETCH
o0B03:		BIT	1,(IY+$01)	; Test FLAGS - is printer in use ?
		JR	NZ,o0B1D	; Forward, if so, to PO-F-PR

;   assume upper screen in use and thus optimize for path that requires speed.

		LD	BC,(S_POSN)	; Fetch line/column from S_POSN
		LD	HL,(DF_CC)	; Fetch DF_CC display file address

		BIT	0,(IY+$02)	; Test TV_FLAG - lower screen in use ?
		RET	Z		; Return if upper screen in use.

;   Overwrite registers with values for lower screen.

		LD	BC,(SPOSNL)	; Fetch line/column from SPOSNL
		LD	HL,(DF_CCL)	; Fetch display file address from DFCCL
		RET			; Return.

; ---

;   This section deals with the ZX Printer.

;; PO-F-PR
o0B1D:		LD	C,(IY+$45)	; Fetch column from P_POSN.
		LD	HL,(PR_CC)	; Fetch printer buffer address from PR_CC.
		RET			; Return.

; ---------------------------------
; THE 'PRINT ANY CHARACTER' ROUTINE
; ---------------------------------
;   This routine is used to print any character in range 32d - 255d
;   It is only called from PO-ABLE which continues into PO-STORE

;; PO-ANY
o0B24:		CP	$80		; ASCII ?
		JR	C,o0B65		; to PO-CHAR is so.

		CP	$90		; test if a block graphic character.
		JR	NC,o0B52	; to PO-T&UDG to print tokens and UDGs

; The 16 2*2 mosaic characters 128-143 decimal are formed from
; bits 0-3 of the character.

		LD	B,A		; save character
		CALL	o0B38		; routine PO-GR-1 to construct top half
					; then bottom half.
		CALL	o0B03		; routine PO-FETCH fetches print position.
		LD	DE,MEMBOT	; MEM-0 is location of 8 bytes of character
		JR	o0B7F		; to PR-ALL to print to screen or printer

; ---

;; PO-GR-1
o0B38:		LD	HL,MEMBOT	; address MEM-0 - a temporary buffer in
					; systems variables which is normally used
					; by the calculator.
		CALL	o0B3E		; routine PO-GR-2 to construct top half
					; and continue into routine to construct
					; bottom half.

;; PO-GR-2
o0B3E:		RR	B		; rotate bit 0/2 to carry
		SBC	A,A		; result $00 or $FF
		AND	$0F		; mask off right hand side
		LD	C,A		; store part in C
		RR	B		; rotate bit 1/3 of original chr to carry
		SBC	A,A		; result $00 or $FF
		AND	$F0		; mask off left hand side
		OR	C		; combine with stored pattern
		LD	C,$04		; four bytes for top/bottom half

;; PO-GR-3
o0B4C:		LD	(HL),A		; store bit patterns in temporary buffer
		INC	HL		; next address
		DEC	C		; jump back to
		JR	NZ,o0B4C	; to PO-GR-3 until byte is stored 4 times

		RET			; return

; ---

; Tokens and User defined graphics are now separated.

;; PO-T&UDG
o0B52:		JP	o3A7E
		NOP

o0B56:		ADD	A,$15		; add 21d to restore to 0 - 20
		PUSH	BC		; save current print position
		LD	BC,(UDG)	; fetch UDG to address bit patterns
		JR	o0B6A		; to PO-CHAR-2 - common code to lay down
					; a bit patterned character

; ---

;; PO-T
o0B5F:		CALL	o0C10		; routine PO-TOKENS prints tokens
		JP	o0B03		; exit via a JUMP to PO-FETCH as this routine
					; must continue into PO-STORE.
					; A JR instruction could be used.

; This point is used to print ASCII characters  32d - 127d.

;; PO-CHAR
o0B65:		PUSH	BC		; save print position
		LD	BC,(CHARS)	; address CHARS

; This common code is used to transfer the character bytes to memory.

;; PO-CHAR-2
o0B6A:		EX	DE,HL		; transfer destination address to DE
		LD	HL,FLAGS	; point to FLAGS
		RES	0,(HL)		; allow for leading space
		CP	$20		; is it a space ?
		JR	NZ,o0B76	; to PO-CHAR-3 if not

		SET	0,(HL)		; signal no leading space to FLAGS

;; PO-CHAR-3
o0B76:		LD	H,$00		; set high byte to 0
		LD	L,A		; character to A
					; 0-21 UDG or 32-127 ASCII.
		ADD	HL,HL		; multiply
		ADD	HL,HL		; by
		ADD	HL,HL		; eight
		ADD	HL,BC		; HL now points to first byte of character
		POP	BC		; the source address CHARS or UDG
		EX	DE,HL		; character address to DE

; ----------------------------------
; THE 'PRINT ALL CHARACTERS' ROUTINE
; ----------------------------------
;   This entry point entered from above to print ASCII and UDGs but also from
;   earlier to print mosaic characters.
;   HL=destination
;   DE=character source
;   BC=line/column

;; PR-ALL
o0B7F:		LD	A,C		; column to A
		DEC	A		; move right
		LD	A,$21		; pre-load with leftmost position
		JR	NZ,o0B93	; but if not zero to PR-ALL-1

		DEC	B		; down one line
		LD	C,A		; load C with $21
		BIT	1,(IY+$01)	; test FLAGS  - Is printer in use
		JR	Z,o0B93		; to PR-ALL-1 if not

		PUSH	DE		; save source address
		CALL	o0ECD		; routine COPY-BUFF outputs line to printer
		POP	DE		; restore character source address
		LD	A,C		; the new column number ($21) to C

;; PR-ALL-1
o0B93:		CP	C		; this test is really for screen - new line ?
		PUSH	DE		; save source

		CALL	Z,o0C55		; routine PO-SCR considers scrolling

		POP	DE		; restore source
		PUSH	BC		; save line/column
		PUSH	HL		; and destination
		LD	A,(P_FLAG)	; fetch P_FLAG to accumulator
		LD	B,$FF		; prepare OVER mask in B.
		RRA			; bit 0 set if OVER 1
		JR	C,o0BA4		; to PR-ALL-2

		INC	B		; set OVER mask to 0

;; PR-ALL-2
o0BA4:		RRA
		RRA			; bit 2 is INVERSE
		SBC	A,A		; will be FF for INVERSE 1 else zero
		LD	C,A		; transfer INVERSE mask to C
		LD	A,$08		; prepare to count 8 bytes
		AND	A		; clear carry to signal screen
		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		JR	Z,o0BB6		; to PR-ALL-3 if screen

		SET	1,(IY+$30)	; update FLAGS2  - signal printer buffer has
					; been used.
		SCF			; set carry flag to signal printer.

;; PR-ALL-3
o0BB6:		EX	DE,HL		; now HL=source, DE=destination

;; PR-ALL-4
o0BB7:		EX	AF,AF'		; save printer/screen flag
		LD	A,(DE)		; fetch existing destination byte
		AND	B		; consider OVER
		XOR	(HL)		; now XOR with source
		XOR	C		; now with INVERSE MASK
		LD	(DE),A		; update screen/printer
		EX	AF,AF'		; restore flag
		JR	C,o0BD3		; to PR-ALL-6 - printer address update

		INC	D		; gives next pixel line down screen

;; PR-ALL-5
o0BC1:		INC	HL		; address next character byte
		DEC	A		; the byte count is decremented
		JR	NZ,o0BB7	; back to PR-ALL-4 for all 8 bytes

		EX	DE,HL		; destination to HL
		DEC	H		; bring back to last updated screen position
		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		CALL	Z,o0BDB		; if not, call routine PO-ATTR to update
					; corresponding colour attribute.
		POP	HL		; restore original screen/printer position
		POP	BC		; and line column
		DEC	C		; move column to right
		INC	HL		; increase screen/printer position
		RET			; return and continue into PO-STORE
					; within PO-ABLE

; ---

;   This branch is used to update the printer position by 32 places
;   Note. The high byte of the address D remains constant (which it should).

;; PR-ALL-6
o0BD3:		EX	AF,AF'		; save the flag
		LD	A,$20		; load A with 32 decimal
		ADD	A,E		; add this to E
		LD	E,A		; and store result in E
		EX	AF,AF'		; fetch the flag
		JR	o0BC1		; back to PR-ALL-5

; -----------------------------------
; THE 'GET ATTRIBUTE ADDRESS' ROUTINE
; -----------------------------------
;   This routine is entered with the HL register holding the last screen
;   address to be updated by PRINT or PLOT.
;   The Spectrum screen arrangement leads to the L register holding the correct
;   value for the attribute file and it is only necessary to manipulate H to
;   form the correct colour attribute address.

;; PO-ATTR
o0BDB:		LD	A,H		; fetch high byte $40 - $57
		RRCA			; shift
		RRCA			; bits 3 and 4
		RRCA			; to right.
		AND	$03		; range is now 0 - 2
		OR	$58		; form correct high byte for third of screen
		LD	H,A		; HL is now correct
		LD	DE,(ATTR_T)	; make D hold ATTR_T, E hold MASK-T
		LD	A,(HL)		; fetch existing attribute
		XOR	E		; apply masks
		AND	D		;
		XOR	E		;
		BIT	6,(IY+$57)	; test P_FLAG  - is this PAPER 9 ??
		JR	Z,o0BFA		; skip to PO-ATTR-1 if not.

		AND	$C7		; set paper
		BIT	2,A		; to contrast with ink
		JR	NZ,o0BFA	; skip to PO-ATTR-1

		XOR	$38		;

;; PO-ATTR-1
o0BFA:		BIT	4,(IY+$57)	; test P_FLAG  - Is this INK 9 ??
		JR	Z,o0C08		; skip to PO-ATTR-2 if not

		AND	$F8		; make ink
		BIT	5,A		; contrast with paper.
		JR	NZ,o0C08	; to PO-ATTR-2

		XOR	$07		;

;; PO-ATTR-2
o0C08:		LD	(HL),A		; save the new attribute.
		RET			; return.

; ---------------------------------
; THE 'MESSAGE PRINTING' SUBROUTINE
; ---------------------------------
;   This entry point is used to print tape, boot-up, scroll? and error messages.
;   On entry the DE register points to an initial step-over byte or the
;   inverted end-marker of the previous entry in the table.
;   Register A contains the message number, often zero to print first message.
;   (HL has nothing important usually P_FLAG)

;; PO-MSG
o0C0A:		PUSH	HL		; put hi-byte zero on stack to suppress
		LD	H,$00		; trailing spaces
		EX	(SP),HL		; ld h,0; push hl would have done ?.
		JR	o0C14		; forward to PO-TABLE.

; ---

;   This entry point prints the BASIC keywords, '<>' etc. from alt set

;; PO-TOKENS
o0C10:		LD	DE,o0095	; address: TKN-TABLE
		PUSH	AF		; save the token number to control
					; trailing spaces - see later *

; ->

;; PO-TABLE
o0C14:		CALL	o0C41		; routine PO-SEARCH will set carry for
					; all messages and function words.

o0C17:		JR	C,o0C22		; forward to PO-EACH if not a command, '<>' etc.

		LD	A,$20		; prepare leading space
		BIT	0,(IY+$01)	; test FLAGS  - leading space if not set

		CALL	Z,o0C3B		; routine PO-SAVE to print a space without
					; disturbing registers.

;; PO-EACH
o0C22:		LD	A,(DE)		; Fetch character from the table.
		AND	$7F		; Cancel any inverted bit.

		CALL	o0C3B		; Routine PO-SAVE to print using the alternate
					; set of registers.

		LD	A,(DE)		; Re-fetch character from table.
		INC	DE		; Address next character in the table.

		ADD	A,A		; Was character inverted ?
					; (this also doubles character)
		JR	NC,o0C22	; back to PO-EACH if not.

		POP	DE		; * re-fetch trailing space byte to D

		CP	$48		; was the last character "$" ?
		JR	Z,o0C35		; forward to PO-TR-SP to consider trailing
					; space if so.

		CP	$82		; was it < 'A' i.e. "#",">","=" from tokens
					; or " ",'.' (from tape) or "?" from scroll

		RET	C		; Return if so as no trailing space required.

;; PO-TR-SP
o0C35:		LD	A,D		; The trailing space flag (zero if an error msg)

		CP	$03		; Test against RND, INKEY$ and PI which have no
					; parameters and therefore no trailing space.

		RET	C		; Return if no trailing space.

		LD	A,$20		; Prepare the space character and continue to
					; print and make an indirect return.

; -----------------------------------
; THE 'RECURSIVE PRINTING' SUBROUTINE
; -----------------------------------
;   This routine which is part of PRINT-OUT allows RST $10 to be used
;   recursively to print tokens and the spaces associated with them.
;   It is called on three occasions when the value of DE must be preserved.

;; PO-SAVE
o0C3B:		PUSH	DE		; Save DE value.
		EXX			; Switch in main set

		RST	10H		; PRINT-A prints using this alternate set.

		EXX			; Switch back to this alternate set.
		POP	DE		; Restore the initial DE value.

		RET			; Return.

; ------------
; Table search
; ------------
; This subroutine searches a message or the token table for the
; message number held in A. DE holds the address of the table.

;; PO-SEARCH
o0C41:		PUSH	AF		; save the message/token number
		EX	DE,HL		; transfer DE to HL
		INC	A		; adjust for initial step-over byte

;; PO-STEP
o0C44:		BIT	7,(HL)		; is character inverted ?
		INC	HL		; address next
		JR	Z,o0C44		; back to PO-STEP if not inverted.

		DEC	A		; decrease counter
		JR	NZ,o0C44	; back to PO-STEP if not zero

		EX	DE,HL		; transfer address to DE
		POP	AF		; restore message/token number
		CP	$20		; return with carry set
		RET	C		; for all messages and function tokens

		LD	A,(DE)		; test first character of token
		SUB	$41		; and return with carry set
		RET			; if it is less that 'A'
					; i.e. '<>', '<=', '>='

; ---------------
; Test for scroll
; ---------------
; This test routine is called when printing carriage return, when considering
; PRINT AT and from the general PRINT ALL characters routine to test if
; scrolling is required, prompting the user if necessary.
; This is therefore using the alternate set.
; The B register holds the current line.

;; PO-SCR
o0C55:		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		RET	NZ		; return immediately if so.

		LD	DE,o0DD9	; set DE to address: CL-SET
		PUSH	DE		; and push for return address.

		LD	A,B		; transfer the line to A.
		BIT	0,(IY+$02)	; test TV_FLAG - lower screen in use ?
		JP	NZ,o0D02	; jump forward to PO-SCR-4 if so.

		CP	(IY+$31)	; greater than DF_SZ display file size ?
		JR	C,o0C86		; forward to REPORT-5 if less.
					; 'Out of screen'

		RET	NZ		; return (via CL-SET) if greater

		BIT	4,(IY+$02)	; test TV_FLAG  - Automatic listing ?
		JR	Z,o0C88		; forward to PO-SCR-2 if not.

		LD	E,(IY+$2D)	; fetch BREG - the count of scroll lines to E.
		DEC	E		; decrease and jump
		JR	Z,o0CD2		; to PO-SCR-3 if zero and scrolling required.

		LD	A,$00		; explicit - select channel zero.
		CALL	o1601		; routine CHAN-OPEN opens it.

		LD	SP,(LIST_SP)	; set stack pointer to LIST_SP

		RES	4,(IY+$02)	; reset TV_FLAG  - signal auto listing finished.
		RET			; return ignoring pushed value, CL-SET
					; to MAIN or EDITOR without updating
					; print position                         >>

; ---


;; REPORT-5
o0C86:		RST	08H		; ERROR-1
		DB	$04		; Error Report: Out of screen

; continue here if not an automatic listing.

;; PO-SCR-2
o0C88:		DEC	(IY+$52)	; decrease SCR_CT
		JR	NZ,o0CD2	; forward to PO-SCR-3 to scroll display if
					; result not zero.

; now produce prompt.

		LD	A,$18		; reset
		SUB	B		; the
		LD	(SCR_CT),A	; SCR_CT scroll count
		LD	HL,(ATTR_T)	; L=ATTR_T, H=MASK_T
		PUSH	HL		; save on stack
		LD	A,(P_FLAG)	; P_FLAG
		PUSH	AF		; save on stack to prevent lower screen
					; attributes (BORDCR etc.) being applied.
		LD	A,$FD		; select system channel 'K'
		CALL	o1601		; routine CHAN-OPEN opens it
		XOR	A		; clear to address message directly
		LD	DE,o0CF8	; make DE address: scrl-mssg
		CALL	o0C0A		; routine PO-MSG prints to lower screen
		SET	5,(IY+$02)	; set TV_FLAG  - signal lower screen requires
					; clearing
		LD	HL,FLAGS	; make HL address FLAGS
		SET	3,(HL)		; signal 'L' mode.
		RES	5,(HL)		; signal 'no new key'.
		EXX			; switch to main set.
					; as calling chr input from alternativo set.
		CALL	o15D4		; routine WAIT-KEY waits for new key
					; Note. this is the right routine but the
					; stream in use is unsatisfactory. From the
					; choices available, it is however the best.

		EXX			; switch back to alternate set.
		CP	$20		; space is considered as BREAK
		JR	Z,o0D00		; forward to REPORT-D if so
					; 'BREAK - CONT repeats'

		CP	$E2		; is character 'STOP' ?
		JR	Z,o0D00		; forward to REPORT-D if so

		OR	$20		; convert to lower-case
		CP	$6E		; is character 'n' ?
		JR	Z,o0D00		; forward to REPORT-D if so else scroll.

		LD	A,$FE		; select system channel 'S'
		CALL	o1601		; routine CHAN-OPEN
		POP	AF		; restore original P_FLAG
		LD	(P_FLAG),A	; and save in P_FLAG.
		POP	HL		; restore original ATTR_T, MASK_T
		LD	(ATTR_T),HL	; and reset ATTR_T, MASK-T as 'scroll?' has
					; been printed.

;; PO-SCR-3
o0CD2:		CALL	o0DFE		; routine CL-SC-ALL to scroll whole display
		LD	B,(IY+$31)	; fetch DF_SZ to B
		INC	B		; increase to address last line of display
		LD	C,$21		; set C to $21 (was $21 from above routine)
		PUSH	BC		; save the line and column in BC.

		CALL	o0E9B		; routine CL-ADDR finds display address.

		LD	A,H		; now find the corresponding attribute byte
		RRCA			; (this code sequence is used twice
		RRCA			; elsewhere and is a candidate for
		RRCA			; a subroutine.)
		AND	$03		;
		OR	$58		;
		LD	H,A		;

		LD	DE,$5AE0	; start of last 'line' of attribute area
		LD	A,(DE)		; get attribute for last line
		LD	C,(HL)		; transfer to base line of upper part
		LD	B,$20		; there are thirty two bytes
		EX	DE,HL		; swap the pointers.

;; PO-SCR-3A
o0CF0:		LD	(DE),A		; transfer
		LD	(HL),C		; attributes.
		INC	DE		; address next.
		INC	HL		; address next.
		DJNZ	o0CF0		; loop back to PO-SCR-3A for all adjacent
					; attribute lines.

		POP	BC		; restore the line/column.
		RET			; return via CL-SET (was pushed on stack).

; ---

; The message 'scroll?' appears here with last byte inverted.

;; scrl-mssg
o0CF8:		DB	$80		; initial step-over byte.
		DB	"scroll","?"+$80

;; REPORT-D
o0D00:		RST	08H		; ERROR-1
		DB	$0C		; Error Report: BREAK - CONT repeats

; continue here if using lower display - A holds line number.

;; PO-SCR-4
o0D02:		CP	$02		; is line number less than 2 ?
		JR	C,o0C86		; to REPORT-5 if so
					; 'Out of Screen'.

		ADD	A,(IY+$31)	; add DF_SZ
		SUB	$19		;
		RET	NC		; return if scrolling unnecessary

		NEG			; Negate to give number of scrolls required.
		PUSH	BC		; save line/column
		LD	B,A		; count to B
		LD	HL,(ATTR_T)	; fetch current ATTR_T, MASK_T to HL.
		PUSH	HL		; and save
		LD	HL,(P_FLAG)	; fetch P_FLAG
		PUSH	HL		; and save.
					; to prevent corruption by input AT

		CALL	o0D4D		; routine TEMPS sets to BORDCR etc
		LD	A,B		; transfer scroll number to A.

;; PO-SCR-4A
o0D1C:		PUSH	AF		; save scroll number.
		LD	HL,DF_SZ	; address DF_SZ
		LD	B,(HL)		; fetch old value
		LD	A,B		; transfer to A
		INC	A		; and increment
		LD	(HL),A		; then put back.
		LD	HL,$5C89	; address S_POSN_hi - line
		CP	(HL)		; compare
		JR	C,o0D2D		; forward to PO-SCR-4B if scrolling required

		INC	(HL)		; else increment S_POSN_hi
		LD	B,$18		; set count to whole display ??
					; Note. should be $17 and the top line will be
					; scrolled into the ROM which is harmless on
					; the standard set up.
					; credit P.Giblin 1984.

;; PO-SCR-4B
o0D2D:		CALL	o0E00		; routine CL-SCROLL scrolls B lines
		POP	AF		; restore scroll counter.
		DEC	A		; decrease
		JR	NZ,o0D1C	; back to PO-SCR-4A until done

		POP	HL		; restore original P_FLAG.
		LD	(IY+$57),L	; and overwrite system variable P_FLAG.

		POP	HL		; restore original ATTR_T/MASK_T.
		LD	(ATTR_T),HL	; and update system variables.

		LD	BC,(S_POSN)	; fetch S_POSN to BC.
		RES	0,(IY+$02)	; signal to TV_FLAG  - main screen in use.
		CALL	o0DD9		; call routine CL-SET for upper display.

		SET	0,(IY+$02)	; signal to TV_FLAG  - lower screen in use.
		POP	BC		; restore line/column
		RET			; return via CL-SET for lower display.

; ----------------------
; Temporary colour items
; ----------------------
; This subroutine is called 11 times to copy the permanent colour items
; to the temporary ones.

;; TEMPS
o0D4D:		XOR	A		; clear the accumulator
		LD	HL,(ATTR_P)	; fetch L=ATTR_P and H=MASK_P
		BIT	0,(IY+$02)	; test TV_FLAG  - is lower screen in use ?
		JR	Z,o0D5B		; skip to TEMPS-1 if not

		LD	H,A		; set H, MASK P, to 00000000.
		LD	L,(IY+$0E)	; fetch BORDCR to L which is used for lower
					; screen.

;; TEMPS-1
o0D5B:		LD	(ATTR_T),HL	; transfer values to ATTR_T and MASK_T

; for the print flag the permanent values are odd bits, temporary even bits.

		LD	HL,P_FLAG	; address P_FLAG.
		JR	NZ,o0D65	; skip to TEMPS-2 if lower screen using A=0.

		LD	A,(HL)		; else pick up flag bits.
		RRCA			; rotate permanent bits to temporary bits.

;; TEMPS-2
o0D65:		XOR	(HL)		;
		AND	$55		; BIN 01010101
		XOR	(HL)		; permanent now as original
		LD	(HL),A		; apply permanent bits to temporary bits.
		RET			; and return.

; -----------------
; THE 'CLS' COMMAND
; -----------------
;    This command clears the display.
;    The routine is also called during initialization and by the CLEAR command.
;    If it's difficult to write it should be difficult to read.

;; CLS
o0D6B:		CALL	o0DAF		; Routine CL-ALL clears the entire display and
					; sets the attributes to the permanent ones
					; from ATTR-P.

;   Having cleared all 24 lines of the display area, continue into the
;   subroutine that clears the lower display area.  Note that at the moment
;   the attributes for the lower lines are the same as upper ones and have
;   to be changed to match the BORDER colour.

; --------------------------
; THE 'CLS-LOWER' SUBROUTINE
; --------------------------
;   This routine is called from INPUT, and from the MAIN execution loop.
;   This is very much a housekeeping routine which clears between 2 and 23
;   lines of the display, setting attributes and correcting situations where
;   errors have occurred while the normal input and output routines have been
;   temporarily diverted to deal with, say colour control codes.

;; CLS-LOWER
o0D6E:		LD	HL,TV_FLAG	; address System Variable TV_FLAG.
		RES	5,(HL)		; TV_FLAG - signal do not clear lower screen.
		SET	0,(HL)		; TV_FLAG - signal lower screen in use.

		CALL	o0D4D		; routine TEMPS applies permanent attributes,
					; in this case BORDCR to ATTR_T.
					; Note. this seems unnecessary and is repeated
					; within CL-LINE.

		LD	B,(IY+$31)	; fetch lower screen display file size DF_SZ

		CALL	o0E44		; routine CL-LINE clears lines to bottom of the
					; display and sets attributes from BORDCR while
					; preserving the B register.

		LD	HL,$5AC0	; set initial attribute address to the leftmost
					; cell of second line up.

		LD	A,(ATTR_P)	; fetch permanent attribute from ATTR_P.

		DEC	B		; decrement lower screen display file size.

		JR	o0D8E		; forward to enter the backfill loop at CLS-3
					; where B is decremented again.

; ---

;   The backfill loop is entered at midpoint and ensures, if more than 2
;   lines have been cleared, that any other lines take the permanent screen
;   attributes.

;; CLS-1
o0D87:		LD	C,$20		; set counter to 32 character cells per line

;; CLS-2
o0D89:		DEC	HL		; decrease attribute address.
		LD	(HL),A		; and place attributes in next line up.
		DEC	C		; decrease the 32 counter.
		JR	NZ,o0D89	; loop back to CLS-2 until all 32 cells done.

;; CLS-3
o0D8E:		DJNZ	o0D87		; decrease B counter and back to CLS-1
					; if not zero.

		LD	(IY+$31),$02	; now set DF_SZ lower screen to 2

; This entry point is also called from CL-ALL below to
; reset the system channel input and output addresses to normal.

;; CL-CHAN
o0D94:		LD	A,$FD		; select system channel 'K'

		CALL	o1601		; routine CHAN-OPEN opens it.

		LD	HL,(CURCHL)	; fetch CURCHL to HL to address current channel
		LD	DE,o09F4	; set address to PRINT-OUT for first pass.
		AND	A		; clear carry for first pass.

;; CL-CHAN-A
o0DA0:		LD	(HL),E		; Insert the output address on the first pass
		INC	HL		; or the input address on the second pass.
		LD	(HL),D		;
		INC	HL		;

		LD	DE,o10A8	; fetch address KEY-INPUT for second pass
		CCF			; complement carry flag - will set on pass 1.

		JR	C,o0DA0		; back to CL-CHAN-A if first pass else done.

		LD	BC,$1721	; line 23 for lower screen
		JR	o0DD9		; exit via CL-SET to set column
					; for lower display

; ---------------------------
; Clearing whole display area
; ---------------------------
; This subroutine called from CLS, AUTO-LIST and MAIN-3
; clears 24 lines of the display and resets the relevant system variables.
; This routine also recovers from an error situation where, for instance, an
; invalid colour or position control code has left the output routine addressing
; PO-TV-2 or PO-CONT.

;; CL-ALL
o0DAF:		LD	HL,$0000	; Initialize plot coordinates.
		LD	(COORDS),HL	; Set system variable COORDS to 0,0.

		RES	0,(IY+$30)	; update FLAGS2  - signal main screen is clear.

		CALL	o0D94		; routine CL-CHAN makes channel 'K" "normal'.

		LD	A,$FE		; select system channel 'S'
		CALL	o1601		; routine CHAN-OPEN opens it.

		CALL	o0D4D		; routine TEMPS applies permanent attributes,
					; in this case ATTR_P, to ATTR_T.
					; Note. this seems unnecessary.

		LD	B,$18		; There are 24 lines.

		CALL	o0E44		; routine CL-LINE clears 24 text lines and sets
					; attributes from ATTR-P.
					; This routine preserves B and sets C to $21.

		LD	HL,(CURCHL)	; fetch CURCHL make HL address output routine.

		LD	DE,o09F4	; address: PRINT-OUT
		LD	(HL),E		; is made
		INC	HL		; the normal
		LD	(HL),D		; output address.

		LD	(IY+$52),$01	; set SCR_CT - scroll count - to default.

;   Note. BC already contains $1821.

		LD	BC,$1821	; reset column and line to 0,0
					; and continue into CL-SET, below, exiting
					; via PO-STORE (for the upper screen).

; --------------------
; THE 'CL-SET' ROUTINE
; --------------------
; This important subroutine is used to calculate the character output
; address for screens or printer based on the line/column for screens
; or the column for printer.

;; CL-SET
o0DD9:		LD	HL,SWAP		; the base address of printer buffer
		BIT	1,(IY+$01)	; test FLAGS  - is printer in use ?
		JR	NZ,o0DF4	; forward to CL-SET-2 if so.

		LD	A,B		; transfer line to A.
		BIT	0,(IY+$02)	; test TV_FLAG  - lower screen in use ?
		JR	Z,o0DEE		; skip to CL-SET-1 if handling upper part

		ADD	A,(IY+$31)	; add DF_SZ for lower screen
		SUB	$18		; and adjust.

;; CL-SET-1
o0DEE:		PUSH	BC		; save the line/column.
		LD	B,A		; transfer line to B
					; (adjusted if lower screen)

		CALL	o0E9B		; routine CL-ADDR calculates address at left
					; of screen.
		POP	BC		; restore the line/column.

;; CL-SET-2
o0DF4:		LD	A,$21		; the column $01-$21 is reversed
		SUB	C		; to range $00 - $20
		LD	E,A		; now transfer to DE
		LD	D,$00		; prepare for addition
		ADD	HL,DE		; and add to base address

		JP	o0ADC		; exit via PO-STORE to update the relevant
					; system variables.
					; ----------------
					; Handle scrolling
					; ----------------
					; The routine CL-SC-ALL is called once from PO to scroll all the display
					; and from the routine CL-SCROLL, once, to scroll part of the display.

;; CL-SC-ALL
o0DFE:		LD	B,$17		; scroll 23 lines, after 'scroll?'.

;; CL-SCROLL
o0E00:		CALL	o0E9B		; routine CL-ADDR gets screen address in HL.
		LD	C,$08		; there are 8 pixel lines to scroll.

;; CL-SCR-1
o0E05:		PUSH	BC		; save counters.
		PUSH	HL		; and initial address.
		LD	A,B		; get line count.
		AND	$07		; will set zero if all third to be scrolled.
		LD	A,B		; re-fetch the line count.
		JR	NZ,o0E19	; forward to CL-SCR-3 if partial scroll.

; HL points to top line of third and must be copied to bottom of previous 3rd.
; ( so HL = $4800 or $5000 ) ( but also sometimes $4000 )

;; CL-SCR-2
o0E0D:		EX	DE,HL		; copy HL to DE.
		LD	HL,$F8E0	; subtract $08 from H and add $E0 to L -
		ADD	HL,DE		; to make destination bottom line of previous
					; third.
		EX	DE,HL		; restore the source and destination.
		LD	BC,$0020	; thirty-two bytes are to be copied.
		DEC	A		; decrement the line count.
		LDIR			; copy a pixel line to previous third.

;; CL-SCR-3
o0E19:		EX	DE,HL		; save source in DE.
		LD	HL,$FFE0	; load the value -32.
		ADD	HL,DE		; add to form destination in HL.
		EX	DE,HL		; switch source and destination
		LD	B,A		; save the count in B.
		AND	$07		; mask to find count applicable to current
		RRCA			; third and
		RRCA			; multiply by
		RRCA			; thirty two (same as 5 RLCAs)

		LD	C,A		; transfer byte count to C ($E0 at most)
		LD	A,B		; store line count to A
		LD	B,$00		; make B zero
		LDIR			; copy bytes (BC=0, H incremented, L=0)
		LD	B,$07		; set B to 7, C is zero.
		ADD	HL,BC		; add 7 to H to address next third.
		AND	$F8		; has last third been done ?
		JR	NZ,o0E0D	; back to CL-SCR-2 if not.

		POP	HL		; restore topmost address.
		INC	H		; next pixel line down.
		POP	BC		; restore counts.
		DEC	C		; reduce pixel line count.
		JR	NZ,o0E05	; back to CL-SCR-1 if all eight not done.

		CALL	o0E88		; routine CL-ATTR gets address in attributes
					; from current 'ninth line', count in BC.

		LD	HL,$FFE0	; set HL to the 16-bit value -32.
		ADD	HL,DE		; and add to form destination address.
		EX	DE,HL		; swap source and destination addresses.
		LDIR			; copy bytes scrolling the linear attributes.
		LD	B,$01		; continue to clear the bottom line.

; ------------------------------
; THE 'CLEAR TEXT LINES' ROUTINE
; ------------------------------
; This subroutine, called from CL-ALL, CLS-LOWER and AUTO-LIST and above,
; clears text lines at bottom of display.
; The B register holds on entry the number of lines to be cleared 1-24.

;; CL-LINE
o0E44:		PUSH	BC		; save line count
		CALL	o0E9B		; routine CL-ADDR gets top address
		LD	C,$08		; there are eight screen lines to a text line.

;; CL-LINE-1
o0E4A:		PUSH	BC		; save pixel line count
		PUSH	HL		; and save the address
		LD	A,B		; transfer the line to A (1-24).

;; CL-LINE-2
o0E4D:		AND	$07		; mask 0-7 to consider thirds at a time
		RRCA			; multiply
		RRCA			; by 32  (same as five RLCA instructions)
		RRCA			; now 32 - 256(0)
		LD	C,A		; store result in C
		LD	A,B		; save line in A (1-24)
		LD	B,$00		; set high byte to 0, prepare for ldir.
		DEC	C		; decrement count 31-255.
		LD	D,H		; copy HL
		LD	E,L		; to DE.
		LD	(HL),$00	; blank the first byte.
		INC	DE		; make DE point to next byte.
		LDIR			; ldir will clear lines.
		LD	DE,$0701	; now address next third adjusting
		ADD	HL,DE		; register E to address left hand side
		DEC	A		; decrease the line count.
		AND	$F8		; will be 16, 8 or 0  (AND $18 will do).
		LD	B,A		; transfer count to B.
		JR	NZ,o0E4D	; back to CL-LINE-2 if 16 or 8 to do
					; the next third.

		POP	HL		; restore start address.
		INC	H		; address next line down.
		POP	BC		; fetch counts.
		DEC	C		; decrement pixel line count
		JR	NZ,o0E4A	; back to CL-LINE-1 till all done.

		CALL	o0E88		; routine CL-ATTR gets attribute address
					; in DE and B * 32 in BC.

		LD	H,D		; transfer the address
		LD	L,E		; to HL.

		INC	DE		; make DE point to next location.

		LD	A,(ATTR_P)	; fetch ATTR_P - permanent attributes
		BIT	0,(IY+$02)	; test TV_FLAG  - lower screen in use ?
		JR	Z,o0E80		; skip to CL-LINE-3 if not.

		LD	A,(BORDCR)	; else lower screen uses BORDCR as attribute.

;; CL-LINE-3
o0E80:		LD	(HL),A		; put attribute in first byte.
		DEC	BC		; decrement the counter.
		LDIR			; copy bytes to set all attributes.
		POP	BC		; restore the line $01-$24.
		LD	C,$21		; make column $21. (No use is made of this)
		RET			; return to the calling routine.

; ------------------
; Attribute handling
; ------------------
; This subroutine is called from CL-LINE or CL-SCROLL with the HL register
; pointing to the 'ninth' line and H needs to be decremented before or after
; the division. Had it been done first then either present code or that used
; at the start of PO-ATTR could have been used.
; The Spectrum screen arrangement leads to the L register already holding
; the correct value for the attribute file and it is only necessary
; to manipulate H to form the correct colour attribute address.

;; CL-ATTR
o0E88:		LD	A,H		; fetch H to A - $48, $50, or $58.
		RRCA			; divide by
		RRCA			; eight.
		RRCA			; $09, $0A or $0B.
		DEC	A		; $08, $09 or $0A.
		OR	$50		; $58, $59 or $5A.
		LD	H,A		; save high byte of attributes.

		EX	DE,HL		; transfer attribute address to DE
		LD	H,C		; set H to zero - from last LDIR.
		LD	L,B		; load L with the line from B.
		ADD	HL,HL		; multiply
		ADD	HL,HL		; by
		ADD	HL,HL		; thirty two
		ADD	HL,HL		; to give count of attribute
		ADD	HL,HL		; cells to the end of display.

		LD	B,H		; transfer the result
		LD	C,L		; to register BC.

		RET			; return.

; -------------------------------
; Handle display with line number
; -------------------------------
; This subroutine is called from four places to calculate the address
; of the start of a screen character line which is supplied in B.

;; CL-ADDR
o0E9B:		LD	A,$18		; reverse the line number
		SUB	B		; to range $00 - $17.
		LD	D,A		; save line in D for later.
		RRCA			; multiply
		RRCA			; by
		RRCA			; thirty-two.

		AND	$E0		; mask off low bits to make
		LD	L,A		; L a multiple of 32.

		LD	A,D		; bring back the line to A.

		AND	$18		; now $00, $08 or $10.

		OR	$40		; add the base address of screen.

		LD	H,A		; HL now has the correct address.
		RET			; return.

; -------------------
; Handle COPY command
; -------------------
; This command copies the top 176 lines to the ZX Printer
; It is popular to call this from machine code at point
; o0EAF with B holding 192 (and interrupts disabled) for a full-screen
; copy. This particularly applies to 16K Spectrums as time-critical
; machine code routines cannot be written in the first 16K of RAM as
; it is shared with the ULA which has precedence over the Z80 chip.

;; COPY
o0EAC:		DI

		LD	B,$B0		; top 176 lines.
o0EAF:		LD	HL,$4000	; address start of the display file.

; now enter a loop to handle each pixel line.

;; COPY-1
o0EB2:		PUSH	HL		; save the screen address.
		PUSH	BC		; and the line counter.

		CALL	o0EF4		; routine COPY-LINE outputs one line.

		POP	BC		; restore the line counter.
		POP	HL		; and display address.
		INC	H		; next line down screen within 'thirds'.
		LD	A,H		; high byte to A.
		AND	$07		; result will be zero if we have left third.
		JR	NZ,o0EC9	; forward to COPY-2 if not to continue loop.

		LD	A,L		; consider low byte first.
		ADD	A,$20		; increase by 32 - sets carry if back to zero.
		LD	L,A		; will be next group of 8.
		CCF			; complement - carry set if more lines in
					; the previous third.
		SBC	A,A		; will be FF, if more, else 00.
		AND	$F8		; will be F8 (-8) or 00.
		ADD	A,H		; that is subtract 8, if more to do in third.
		LD	H,A		; and reset address.

;; COPY-2
o0EC9:		DJNZ	o0EB2		; back to COPY-1 for all lines.

		JR	o0EDA		; forward to COPY-END to switch off the printer
					; motor and enable interrupts.
					; Note. Nothing else is required.

; ------------------------------
; Pass printer buffer to printer
; ------------------------------
; This routine is used to copy 8 text lines from the printer buffer
; to the ZX Printer. These text lines are mapped linearly so HL does
; not need to be adjusted at the end of each line.

;; COPY-BUFF
o0ECD:		DI
		LD	HL,SWAP		; the base address of the Printer Buffer.
		LD	B,$08		; set count to 8 lines of 32 bytes.

;; COPY-3
o0ED3:		PUSH	BC		; save counter.

		CALL	o0EF4		; routine COPY-LINE outputs 32 bytes

		POP	BC		; restore counter.
		DJNZ	o0ED3		; loop back to COPY-3 for all 8 lines.
					; then stop motor and clear buffer.

; Note. the COPY command rejoins here, essentially to execute the next
; three instructions.

;; COPY-END
o0EDA:		LD	A,$04		; output value 4 to port
		OUT	($FB),A		; to stop the slowed printer motor.
		EI			; enable interrupts.

; --------------------
; Clear Printer Buffer
; --------------------
; This routine clears an arbitrary 256 bytes of memory.
; Note. The routine seems designed to clear a buffer that follows the
; system variables.
; The routine should check a flag or HL address and simply return if COPY
; is in use.
; As a consequence of this omission the buffer will needlessly
; be cleared when COPY is used and the screen/printer position may be set to
; the start of the buffer and the line number to 0 (B)
; giving an 'Out of Screen' error.
; There seems to have been an unsuccessful attempt to circumvent the use
; of PR_CC_hi.

;; CLEAR-PRB
o0EDF:		LD	HL,SWAP		; the location of the buffer.
		LD	(IY+$46),L	; update PR_CC_lo - set to zero - superfluous.
		XOR	A		; clear the accumulator.
		LD	B,A		; set count to 256 bytes.

;; PRB-BYTES
o0EE7:		LD	(HL),A		; set addressed location to zero.
		INC	HL		; address next byte - Note. not INC L.
		DJNZ	o0EE7		; back to PRB-BYTES. repeat for 256 bytes.

		RES	1,(IY+$30)	; set FLAGS2 - signal printer buffer is clear.
		LD	C,$21		; set the column position .
		JP	o0DD9		; exit via CL-SET and then PO-STORE.

; -----------------
; Copy line routine
; -----------------
; This routine is called from COPY and COPY-BUFF to output a line of
; 32 bytes to the ZX Printer.
; Output to port $FB -
; bit 7 set - activate stylus.
; bit 7 low - deactivate stylus.
; bit 2 set - stops printer.
; bit 2 reset - starts printer
; bit 1 set - slows printer.
; bit 1 reset - normal speed.

;; COPY-LINE
o0EF4:		LD	A,B		; fetch the counter 1-8 or 1-176
		CP	$03		; is it 01 or 02 ?.
		SBC	A,A		; result is $FF if so else $00.
		AND	$02		; result is 02 now else 00.
					; bit 1 set slows the printer.
		OUT	($FB),A		; slow the printer for the
					; last two lines.
		LD	D,A		; save the mask to control the printer later.

;; COPY-L-1
o0EFD:		CALL	o1F54		; call BREAK-KEY to read keyboard immediately.
		JR	C,o0F0C		; forward to COPY-L-2 if 'break' not pressed.

		LD	A,$04		; else stop the
		OUT	($FB),A		; printer motor.
		EI			; enable interrupts.
		CALL	o0EDF		; call routine CLEAR-PRB.
					; Note. should not be cleared if COPY in use.

;; REPORT-Dc
o0F0A:		RST	08H		; ERROR-1
		DB	$0C		; Error Report: BREAK - CONT repeats

;; COPY-L-2
o0F0C:		IN	A,($FB)		; test now to see if
		ADD	A,A		; a printer is attached.
		RET	M		; return if not - but continue with parent
					; command.

		JR	NC,o0EFD	; back to COPY-L-1 if stylus of printer not
					; in position.

		LD	C,$20		; set count to 32 bytes.

;; COPY-L-3
o0F14:		LD	E,(HL)		; fetch a byte from line.
		INC	HL		; address next location. Note. not INC L.
		LD	B,$08		; count the bits.

;; COPY-L-4
o0F18:		RL	D		; prepare mask to receive bit.
		RL	E		; rotate leftmost print bit to carry
		RR	D		; and back to bit 7 of D restoring bit 1

;; COPY-L-5
o0F1E:		IN	A,($FB)		; read the port.
		RRA			; bit 0 to carry.
		JR	NC,o0F1E	; back to COPY-L-5 if stylus not in position.

		LD	A,D		; transfer command bits to A.
		OUT	($FB),A		; and output to port.
		DJNZ	o0F18		; loop back to COPY-L-4 for all 8 bits.

		DEC	C		; decrease the byte count.
		JR	NZ,o0F14	; back to COPY-L-3 until 256 bits done.

		RET			; return to calling routine COPY/COPY-BUFF.


; ----------------------------------
; Editor routine for BASIC and INPUT
; ----------------------------------
; The editor is called to prepare or edit a BASIC line.
; It is also called from INPUT to input a numeric or string expression.
; The behaviour and options are quite different in the various modes
; and distinguished by bit 5 of FLAGX.
;
; This is a compact and highly versatile routine.

;; EDITOR
o0F2C:		LD	HL,(ERR_SP)	; fetch ERR_SP
		PUSH	HL		; save on stack

;; ED-AGAIN
o0F30:		LD	HL,o107F	; address: ED-ERROR
		PUSH	HL		; save address on stack and
		LD	(ERR_SP),SP	; make ERR_SP point to it.

; Note. While in editing/input mode should an error occur then RST 08 will
; update X_PTR to the location reached by CH_ADD and jump to ED-ERROR
; where the error will be cancelled and the loop begin again from ED-AGAIN
; above. The position of the error will be apparent when the lower screen is
; reprinted. If no error then the re-iteration is to ED-LOOP below when
; input is arriving from the keyboard.

;; ED-LOOP
o0F38:		CALL	o15D4		; routine WAIT-KEY gets key possibly
					; changing the mode.
		PUSH	AF		; save key.
		LD	D,$00		; and give a short click based
		LD	E,(IY-$01)	; on PIP value for duration.
		LD	HL,$00C8	; and pitch.
		CALL	o03B5		; routine BEEPER gives click - effective
					; with rubber keyboard.
		POP	AF		; get saved key value.
		LD	HL,o0F38	; address: ED-LOOP is loaded to HL.
		PUSH	HL		; and pushed onto stack.

; At this point there is a looping return address on the stack, an error
; handler and an input stream set up to supply characters.
; The character that has been received can now be processed.

		CP	$18		; range 24 to 255 ?
		JR	NC,o0F81	; forward to ADD-CHAR if so.

		CP	$07		; lower than 7 ?
		JR	C,o0F81		; forward to ADD-CHAR also.
					; Note. This is a 'bug' and chr$ 6, the comma
					; control character, should have had an
					; entry in the ED-KEYS table.
					; Steven Vickers, 1984, Pitman.

		CP	$10		; less than 16 ?
		JR	C,o0F92		; forward to ED-KEYS if editing control
					; range 7 to 15 dealt with by a table

		LD	BC,$0002	; prepare for ink/paper etc.
		LD	D,A		; save character in D
		CP	$16		; is it ink/paper/bright etc. ?
		JR	C,o0F6C		; forward to ED-CONTR if so

; leaves 22d AT and 23d TAB
; which can't be entered via KEY-INPUT.
; so this code is never normally executed
; when the keyboard is used for input.

		INC	BC		; if it was AT/TAB - 3 locations required
		BIT	7,(IY+$37)	; test FLAGX  - Is this INPUT LINE ?
		JP	Z,o101E		; jump to ED-IGNORE if not, else

		CALL	o15D4		; routine WAIT-KEY - input address is KEY-NEXT
					; but is reset to KEY-INPUT
		LD	E,A		; save first in E

;; ED-CONTR
o0F6C:		CALL	o15D4		; routine WAIT-KEY for control.
					; input address will be key-next.

		PUSH	DE		; saved code/parameters
		LD	HL,(K_CUR)	; fetch address of keyboard cursor from K_CUR
		RES	0,(IY+$07)	; set MODE to 'L'

		CALL	o1655		; routine MAKE-ROOM makes 2/3 spaces at cursor

		POP	BC		; restore code/parameters
		INC	HL		; address first location
		LD	(HL),B		; place code (ink etc.)
		INC	HL		; address next
		LD	(HL),C		; place possible parameter. If only one
					; then DE points to this location also.
		JR	o0F8B		; forward to ADD-CH-1

; ------------------------
; Add code to current line
; ------------------------
; this is the branch used to add normal non-control characters
; with ED-LOOP as the stacked return address.
; it is also the OUTPUT service routine for system channel 'R'.

;; ADD-CHAR
o0F81:		RES	0,(IY+$07)	; set MODE to 'L'
		LD	HL,(K_CUR)	; fetch address of keyboard cursor from K_CUR
		CALL	o1652		; routine ONE-SPACE creates one space.

; either a continuation of above or from ED-CONTR with ED-LOOP on stack.

;; ADD-CH-1
o0F8B:		LD	(DE),A		; load current character to last new location.
		INC	DE		; address next
		LD	(K_CUR),DE	; and update K_CUR system variable.
		RET			; return - either a simple return
					; from ADD-CHAR or to ED-LOOP on stack.

; ---

; a branch of the editing loop to deal with control characters
; using a look-up table.

;; ED-KEYS
o0F92:		LD	E,A		; character to E.
		LD	D,$00		; prepare to add.
		LD	HL,o0FA0-7	; base address of editing keys table. $0F99
		ADD	HL,DE		; add E
		LD	E,(HL)		; fetch offset to E
		ADD	HL,DE		; add offset for address of handling routine.
		PUSH	HL		; push the address on machine stack.
		LD	HL,(K_CUR)	; load address of cursor from K_CUR.
		RET			; Make an indirect jump forward to routine.

; ------------------
; Editing keys table
; ------------------
; For each code in the range $07 to $0F this table contains a
; single offset byte to the routine that services that code.
; Note. for what was intended there should also have been an
; entry for chr$ 6 with offset to ed-symbol.

;; ed-keys-t
o0FA0:		DB	o0FA9-$		; 07d offset $09 to Address: ED-EDIT
		DB	o1007-$		; 08d offset $66 to Address: ED-LEFT
		DB	o100C-$		; 09d offset $6A to Address: ED-RIGHT
		DB	o0FF3-$		; 10d offset $50 to Address: ED-DOWN
		DB	o1059-$		; 11d offset $B5 to Address: ED-UP
		DB	o1015-$		; 12d offset $70 to Address: ED-DELETE
		DB	o1024-$		; 13d offset $7E to Address: ED-ENTER
		DB	o1076-$		; 14d offset $CF to Address: ED-SYMBOL
		DB	o107C-$		; 15d offset $D4 to Address: ED-GRAPH

; ---------------
; Handle EDIT key
; ---------------
; The user has pressed SHIFT 1 to bring edit line down to bottom of screen.
; alternativoly the user wishes to clear the input buffer and start again.
; alternativoly ...

;; ED-EDIT
o0FA9:		LD	HL,(E_PPC)	; fetch E_PPC the last line number entered.
					; Note. may not exist and may follow program.
		BIT	5,(IY+$37)	; test FLAGX  - input mode ?
		JP	NZ,CLEARSP	; jump forward to CLEAR-SP if not in editor.

		CALL	o196E		; routine LINE-ADDR to find address of line
					; or following line if it doesn't exist.
		CALL	o1695		; routine LINE-NO will get line number from
					; address or previous line if at end-marker.
		LD	A,D		; if there is no program then DE will
		OR	E		; contain zero so test for this.
		JP	Z,CLEARSP		; jump to CLEAR-SP if so.

; Note. at this point we have a validated line number, not just an
; approximation and it would be best to update E_PPC with the true
; cursor line value which would enable the line cursor to be suppressed
; in all situations - see shortly.

		PUSH	HL		; save address of line.
		INC	HL		; address low byte of length.
		LD	C,(HL)		; transfer to C
		INC	HL		; next to high byte
		LD	B,(HL)		; transfer to B.
		LD	HL,$000A	; an overhead of ten bytes
		ADD	HL,BC		; is added to length.
		LD	B,H		; transfer adjusted value
		LD	C,L		; to BC register.
		CALL	o1F05		; routine TEST-ROOM checks free memory.
		CALL	CLEARSP		; routine CLEAR-SP clears editing area.
		LD	HL,(CURCHL)	; address CURCHL
		EX	(SP),HL		; swap with line address on stack
		PUSH	HL		; save line address underneath

		LD	A,$FF		; select system channel 'R'
		CALL	o1601		; routine CHAN-OPEN opens it

		POP	HL		; drop line address
		DEC	HL		; make it point to first byte of line num.
		DEC	(IY+$0F)	; decrease E_PPC_lo to suppress line cursor.
					; Note. ineffective when E_PPC is one
					; greater than last line of program perhaps
					; as a result of a delete.
					; credit. Paul Harrison 1982.

		CALL	o1855		; routine OUT-LINE outputs the BASIC line
					; to the editing area.
		INC	(IY+$0F)	; restore E_PPC_lo to the previous value.
		LD	HL,(E_LINE)	; address E_LINE in editing area.
		INC	HL		; advance
		INC	HL		; past space
		INC	HL		; and digit characters
		INC	HL		; of line number.

		LD	(K_CUR),HL	; update K_CUR to address start of BASIC.
		POP	HL		; restore the address of CURCHL.
		CALL	o1615		; routine CHAN-FLAG sets flags for it.

		RET			; RETURN to ED-LOOP.

; -------------------
; Cursor down editing
; -------------------
;   The BASIC lines are displayed at the top of the screen and the user
;   wishes to move the cursor down one line in edit mode.
;   With INPUT LINE, this key must be used instead of entering STOP.

;; ED-DOWN
o0FF3:		BIT	5,(IY+$37)	; test FLAGX  - Input Mode ?
		JR	NZ,o1001	; skip to ED-STOP if so

		LD	HL,E_PPC	; address E_PPC - 'current line'
		CALL	o190F		; routine LN-FETCH fetches number of next
					; line or same if at end of program.
		JR	o106E		; forward to ED-LIST to produce an
					; automatic listing.

; ---

;; ED-STOP
o1001:		LD	(IY+$00),$10	; set ERR_NR to 'STOP in INPUT' code
		JR	o1024		; forward to ED-ENTER to produce error.

; -------------------
; Cursor left editing
; -------------------
; This acts on the cursor in the lower section of the screen in both
; editing and input mode.

;; ED-LEFT
o1007:		CALL	o1031		; routine ED-EDGE moves left if possible
		JR	o1011		; forward to ED-CUR to update K-CUR
					; and return to ED-LOOP.

; --------------------
; Cursor right editing
; --------------------
; This acts on the cursor in the lower screen in both editing and input
; mode and moves it to the right.

;; ED-RIGHT
o100C:		LD	A,(HL)		; fetch addressed character.
		CP	$0D		; is it carriage return ?
		RET	Z		; return if so to ED-LOOP

		INC	HL		; address next character

;; ED-CUR
o1011:		LD	(K_CUR),HL	; update K_CUR system variable
		RET			; return to ED-LOOP

; --------------
; DELETE editing
; --------------
; This acts on the lower screen and deletes the character to left of
; cursor. If control characters are present these are deleted first
; leaving the naked parameter (0-7) which appears as a "?" except in the
; case of chr$ 6 which is the comma control character. It is not mandatory
; to delete these second characters.

;; ED-DELETE
o1015:		CALL	o1031		; routine ED-EDGE moves cursor to left.
		LD	BC,$0001	; of character to be deleted.
		JP	o19E8		; to RECLAIM-2 reclaim the character.

; ------------------------------------------
; Ignore next 2 codes from key-input routine
; ------------------------------------------
; Since AT and TAB cannot be entered this point is never reached
; from the keyboard. If inputting from a tape device or network then
; the control and two following characters are ignored and processing
; continues as if a carriage return had been received.
; Here, perhaps, another Spectrum has said print #15; AT 0,0; "This is yellow"
; and this one is interpreting input #15; a$.

;; ED-IGNORE
o101E:		CALL	o15D4		; routine WAIT-KEY to ignore keystroke.
		CALL	o15D4		; routine WAIT-KEY to ignore next key.

; -------------
; Enter/newline
; -------------
; The enter key has been pressed to have BASIC line or input accepted.

;; ED-ENTER
o1024:		POP	HL		; discard address ED-LOOP
		POP	HL		; drop address ED-ERROR

;; ED-END
o1026:		POP	HL		; the previous value of ERR_SP
		LD	(ERR_SP),HL	; is restored to ERR_SP system variable
		BIT	7,(IY+$00)	; is ERR_NR $FF (= 'OK') ?
		RET	NZ		; return if so

		LD	SP,HL		; else put error routine on stack
		RET			; and make an indirect jump to it.

; -----------------------------
; Move cursor left when editing
; -----------------------------
; This routine moves the cursor left. The complication is that it must
; not position the cursor between control codes and their parameters.
; It is further complicated in that it deals with TAB and AT characters
; which are never present from the keyboard.
; The method is to advance from the beginning of the line each time,
; jumping one, two, or three characters as necessary saving the original
; position at each jump in DE. Once it arrives at the cursor then the next
; legitimate leftmost position is in DE.

;; ED-EDGE
o1031:		SCF
		CALL	o1195		; subroutine SET-DE.
					; if input   then DE=WORKSP
					; if editing then DE=E_LINE
		SBC	HL,DE		; subtract address from start of line
		ADD	HL,DE		; and add back.
		INC	HL		; adjust for carry.
		POP	BC		; drop return address
		RET	C		; return to ED-LOOP if already at left
					; of line.

		PUSH	BC		; resave return address - ED-LOOP.
		LD	B,H		; transfer HL - cursor address
		LD	C,L		; to BC register pair.
					; at this point DE addresses start of line.

;; ED-EDGE-1
o103E:		LD	H,D		; transfer DE - leftmost pointer
		LD	L,E		; to HL
		INC	HL		; address next leftmost character to
					; advance position each time.
		LD	A,(DE)		; pick up previous in A
		AND	$F0		; lose the low bits
		CP	$10		; is it INK to TAB $10-$1F ?
					; that is, is it followed by a parameter ?
		JR	NZ,o1051	; to ED-EDGE-2 if not
					; HL has been incremented once

		INC	HL		; address next as at least one parameter.

; in fact since 'tab' and 'at' cannot be entered the next section seems
; superfluous.
; The test will always fail and the jump to ED-EDGE-2 will be taken.

		LD	A,(DE)		; reload leftmost character
		SUB	$17		; decimal 23 ('tab')
		ADC	A,$00		; will be 0 for 'tab' and 'at'.
		JR	NZ,o1051	; forward to ED-EDGE-2 if not
					; HL has been incremented twice

		INC	HL		; increment a third time for 'at"/"tab'

;; ED-EDGE-2
o1051:		AND	A		; prepare for true subtraction
		SBC	HL,BC		; subtract cursor address from pointer
		ADD	HL,BC		; and add back
					; Note when HL matches the cursor position BC,
					; there is no carry and the previous
					; position is in DE.
		EX	DE,HL		; transfer result to DE if looping again.
					; transfer DE to HL to be used as K-CUR
					; if exiting loop.
		JR	C,o103E		; back to ED-EDGE-1 if cursor not matched.

		RET			; return.

; -----------------
; Cursor up editing
; -----------------
; The main screen displays part of the BASIC program and the user wishes
; to move up one line scrolling if necessary.
; This has no alternativo use in input mode.

;; ED-UP
o1059:		BIT	5,(IY+$37)	; test FLAGX  - input mode ?
		RET	NZ		; return if not in editor - to ED-LOOP.

		LD	HL,(E_PPC)	; get current line from E_PPC
		CALL	o196E		; routine LINE-ADDR gets address
		EX	DE,HL		; and previous in DE
		CALL	o1695		; routine LINE-NO gets prev line number
		LD	HL,$5C4A	; set HL to E_PPC_hi as next routine stores
					; top first.
		CALL	o191C		; routine LN-STORE loads DE value to HL
					; high byte first - E_PPC_lo takes E

; this branch is also taken from ed-down.

;; ED-LIST
o106E:		CALL	o1795		; routine AUTO-LIST lists to upper screen
					; including adjusted current line.
		LD	A,$00		; select lower screen again
		JP	o1601		; exit via CHAN-OPEN to ED-LOOP

; --------------------------------
; Use of symbol and graphics codes
; --------------------------------
; These will not be encountered with the keyboard but would be handled
; otherwise as follows.
; As noted earlier, Vickers says there should have been an entry in
; the KEYS table for chr$ 6 which also pointed here.
; If, for simplicity, two Spectrums were both using #15 as a bi-directional
; channel connected to each other:-
; then when the other Spectrum has said PRINT #15; x, y
; input #15; i ; j  would treat the comma control as a newline and the
; control would skip to input j.
; You can get round the missing chr$ 6 handler by sending multiple print
; items separated by a newline '.

; chr$14 would have the same functionality.

; This is chr$ 14.
;; ED-SYMBOL
o1076:		BIT	7,(IY+$37)	; test FLAGX - is this INPUT LINE ?
		JR	Z,o1024		; back to ED-ENTER if not to treat as if
					; enter had been pressed.
					; else continue and add code to buffer.

; Next is chr$ 15
; Note that ADD-CHAR precedes the table so we can't offset to it directly.

;; ED-GRAPH
o107C:		JP	o0F81		; jump back to ADD-CHAR

; --------------------
; Editor error routine
; --------------------
; If an error occurs while editing, or inputting, then ERR_SP
; points to the stack location holding address ED_ERROR.

;; ED-ERROR
o107F:		BIT	4,(IY+$30)	; test FLAGS2  - is K channel in use ?
		JR	Z,o1026		; back to ED-END if not.

; but as long as we're editing lines or inputting from the keyboard, then
; we've run out of memory so give a short rasp.

		LD	(IY+$00),$FF	; reset ERR_NR to 'OK'.
		LD	D,$00		; prepare for beeper.
		LD	E,(IY-$02)	; use RASP value.
		LD	HL,$1A90	; set the pitch - or tone period.
		CALL	o03B5		; routine BEEPER emits a warning rasp.
		JP	o0F30		; to ED-AGAIN to re-stack address of
					; this routine and make ERR_SP point to it.

; ---------------------
; Clear edit/work space
; ---------------------
; The editing area or workspace is cleared depending on context.
; This is called from ED-EDIT to clear workspace if edit key is
; used during input, to clear editing area if no program exists
; and to clear editing area prior to copying the edit line to it.
; It is also used by the error routine to clear the respective
; area depending on FLAGX.

;; CLEAR-SP
CLEARSP:	PUSH	HL		; preserve HL
		CALL	o1190		; routine SET-HL
					; if in edit   HL = WORKSP-1, DE = E_LINE
					; if in input  HL = STKBOT,   DE = WORKSP
		DEC	HL		; adjust
		CALL	o19E5		; routine RECLAIM-1 reclaims space
		LD	(K_CUR),HL	; set K_CUR to start of empty area
		LD	(IY+$07),$00	; set MODE to 'KLC'
		POP	HL		; restore HL.
		RET			; return.

; ----------------------------
; THE 'KEYBOARD INPUT' ROUTINE
; ----------------------------
; This is the service routine for the input stream of the keyboard channel 'K'.

;; KEY-INPUT
o10A8:		BIT	3,(IY+$02)	; test TV_FLAG  - has a key been pressed in
					; editor ?

		CALL	NZ,o111D	; routine ED-COPY, if so, to reprint the lower
					; screen at every keystroke/mode change.

		AND	A		; clear carry flag - required exit condition.

		BIT	5,(IY+$01)	; test FLAGS  - has a new key been pressed ?
		RET	Z		; return if not.                        >>

		LD	A,(LAST_K)	; system variable LASTK will hold last key -
					; from the interrupt routine.

		RES	5,(IY+$01)	; update FLAGS  - reset the new key flag.
		PUSH	AF		; save the input character.

		BIT	5,(IY+$02)	; test TV_FLAG  - clear lower screen ?

		CALL	NZ,o0D6E	; routine CLS-LOWER if so.

		POP	AF		; restore the character code.

		CP	$20		; if space or higher then
		JR	NC,o111B	; forward to KEY-DONE2 and return with carry
					; set to signal key-found.

		CP	$10		; with 16d INK and higher skip
		JR	NC,o10FA	; forward to KEY-CONTR.

		CP	$06		; for 6 - 15d
		JR	NC,o10DB	; skip forward to KEY-M-CL to handle Modes
					; and CapsLock.

; that only leaves 0-5, the flash bright inverse switches.

		LD	B,A		; save character in B
		AND	$01		; isolate the embedded parameter (0/1).
		LD	C,A		; and store in C
		LD	A,B		; re-fetch copy (0-5)
		RRA			; halve it 0, 1 or 2.
		ADD	A,$12		; add 18d gives 'flash', 'bright'
					; and 'inverse'.
		JR	o1105		; forward to KEY-DATA with the
					; parameter (0/1) in C.

; ---

; Now separate capslock 06 from modes 7-15.

;; KEY-M-CL
o10DB:		JR	NZ,o10E6	; forward to KEY-MODE if not 06 (capslock)

		LD	HL,FLAGS2	; point to FLAGS2
		LD	A,$08		; value 00001000
		XOR	(HL)		; toggle BIT 3 of FLAGS2 the capslock bit
		LD	(HL),A		; and store result in FLAGS2 again.
		JR	o10F4		; forward to KEY-FLAG to signal no-key.

; ---

;; KEY-MODE
o10E6:		CP	$0E		; compare with chr 14d
		RET	C		; return with carry set "key found" for
					; codes 7 - 13d leaving 14d and 15d
					; which are converted to mode codes.

		SUB	$0D		; subtract 13d leaving 1 and 2
					; 1 is 'E' mode, 2 is 'G' mode.
		LD	HL,MODE		; address the MODE system variable.
		CP	(HL)		; compare with existing value before
		LD	(HL),A		; inserting the new value.
		JR	NZ,o10F4	; forward to KEY-FLAG if it has changed.

		LD	(HL),$00	; else make MODE zero - KLC mode
					; Note. while in Extended/Graphics mode,
					; the Extended Mode/Graphics key is pressed
					; again to get out.

;; KEY-FLAG
o10F4:		SET	3,(IY+$02)	; update TV_FLAG  - show key state has changed
		CP	A		; clear carry and reset zero flags -
					; no actual key returned.
		RET			; make the return.

; ---

; now deal with colour controls - 16-23 ink, 24-31 paper

;; KEY-CONTR
o10FA:		LD	B,A		; make a copy of character.
		AND	$07		; mask to leave bits 0-7
		LD	C,A		; and store in C.
		LD	A,$10		; initialize to 16d - INK.
		BIT	3,B		; was it paper ?
		JR	NZ,o1105	; forward to KEY-DATA with INK 16d and
					; colour in C.

		INC	A		; else change from INK to PAPER (17d) if so.

;; KEY-DATA
o1105:		LD	(IY-$2D),C	; put the colour (0-7)/state(0/1) in KDATA
		LD	DE,o110D	; address: KEY-NEXT will be next input stream
		JR	o1113		; forward to KEY-CHAN to change it ...

; ---

; ... so that INPUT_AD directs control to here at next call to WAIT-KEY

;; KEY-NEXT
o110D:		LD	A,(K_DATA)	; pick up the parameter stored in KDATA.
		LD	DE,o10A8	; address: KEY-INPUT will be next input stream
					; continue to restore default channel and
					; make a return with the control code.

;; KEY-CHAN
o1113:		LD	HL,(CHANS)	; address start of CHANNELS area using CHANS
					; system variable.
					; Note. One might have expected CURCHL to
					; have been used.
		INC	HL		; step over the
		INC	HL		; output address
		LD	(HL),E		; and update the input
		INC	HL		; routine address for
		LD	(HL),D		; the next call to WAIT-KEY.

;; KEY-DONE2
o111B:		SCF
		RET			; and return.

; --------------------
; Lower screen copying
; --------------------
; This subroutine is called whenever the line in the editing area or
; input workspace is required to be printed to the lower screen.
; It is by calling this routine after any change that the cursor, for
; instance, appears to move to the left.
; Remember the edit line will contain characters and tokens
; e.g. "1000 LET a=1" is 8 characters.

;; ED-COPY
o111D:		CALL	o0D4D		; routine TEMPS sets temporary attributes.
		RES	3,(IY+$02)	; update TV_FLAG  - signal no change in mode
		RES	5,(IY+$02)	; update TV_FLAG  - signal don't clear lower
					; screen.
		LD	HL,(SPOSNL)	; fetch SPOSNL
		PUSH	HL		; and save on stack.

		LD	HL,(ERR_SP)	; fetch ERR_SP
		PUSH	HL		; and save also
		LD	HL,o1167	; address: ED-FULL
		PUSH	HL		; is pushed as the error routine
		LD	(ERR_SP),SP	; and ERR_SP made to point to it.

		LD	HL,(ECHO_E)	; fetch ECHO_E
		PUSH	HL		; and push also

		SCF			; set carry flag to control SET-DE
		CALL	o1195		; call routine SET-DE
					; if in input DE = WORKSP
					; if in edit  DE = E_LINE
		EX	DE,HL		; start address to HL

		CALL	o187D		; routine OUT-LINE2 outputs entire line up to
					; carriage return including initial
					; characterized line number when present.
		EX	DE,HL		; transfer new address to DE
		CALL	o18E1		; routine OUT-CURS considers a
					; terminating cursor.

		LD	HL,(SPOSNL)	; fetch updated SPOSNL
		EX	(SP),HL		; exchange with ECHO_E on stack
		EX	DE,HL		; transfer ECHO_E to DE
		CALL	o0D4D		; routine TEMPS to re-set attributes
					; if altered.

; the lower screen was not cleared, at the outset, so if deleting then old
; text from a previous print may follow this line and requires blanking.

;; ED-BLANK
o1150:		LD	A,($5C8B)	; fetch SPOSNL_hi is current line
		SUB	D		; compare with old
		JR	C,o117C		; forward to ED-C-DONE if no blanking

		JR	NZ,o115E	; forward to ED-SPACES if line has changed

		LD	A,E		; old column to A
		SUB	(IY+$50)	; subtract new in SPOSNL_lo
		JR	NC,o117C	; forward to ED-C-DONE if no backfilling.

;; ED-SPACES
o115E:		LD	A,$20		; prepare a space.
		PUSH	DE		; save old line/column.
		CALL	o09F4		; routine PRINT-OUT prints a space over
					; any text from previous print.
					; Note. Since the blanking only occurs when
					; using $09F4 to print to the lower screen,
					; there is no need to vector via a RST 10
					; and we can use this alternate set.
		POP	DE		; restore the old line column.
		JR	o1150		; back to ED-BLANK until all old text blanked.

; -------------------------------
; THE 'EDITOR-FULL' ERROR ROUTINE
; -------------------------------
;   This is the error routine addressed by ERR_SP.  This is not for the out of
;   memory situation as we're just printing.  The pitch and duration are exactly
;   the same as used by ED-ERROR from which this has been augmented.  The
;   situation is that the lower screen is full and a rasp is given to suggest
;   that this is perhaps not the best idea you've had that day.

;; ED-FULL
o1167:		LD	D,$00		; prepare to moan.
		LD	E,(IY-$02)	; fetch RASP value.
		LD	HL,$1A90	; set pitch or tone period.

		CALL	o03B5		; routine BEEPER.

		LD	(IY+$00),$FF	; clear ERR_NR.
		LD	DE,(SPOSNL)	; fetch SPOSNL.
		JR	o117E		; forward to ED-C-END

; -------

; the exit point from line printing continues here.

;; ED-C-DONE
o117C:		POP	DE		; fetch new line/column.
		POP	HL		; fetch the error address.

; the error path rejoins here.

;; ED-C-END
o117E:		POP	HL		; restore the old value of ERR_SP.
		LD	(ERR_SP),HL	; update the system variable ERR_SP

		POP	BC		; old value of SPOSN_L
		PUSH	DE		; save new value

		CALL	o0DD9		; routine CL-SET and PO-STORE
					; update ECHO_E and SPOSN_L from BC

		POP	HL		; restore new value
		LD	(ECHO_E),HL	; and overwrite ECHO_E

		LD	(IY+$26),$00	; make error pointer X_PTR_hi out of bounds

		RET			; return

; -----------------------------------------------
; Point to first and last locations of work space
; -----------------------------------------------
;   These two nested routines ensure that the appropriate pointers are
;   selected for the editing area or workspace. The routines that call
;   these routines are designed to work on either area.

; this routine is called once

;; SET-HL
o1190:		LD	HL,(WORKSP)	; fetch WORKSP to HL.
		DEC	HL		; point to last location of editing area.
		AND	A		; clear carry to limit exit points to first
					; or last.

; this routine is called with carry set and exits at a conditional return.

;; SET-DE
o1195:		LD	DE,(E_LINE)	; fetch E_LINE to DE
		BIT	5,(IY+$37)	; test FLAGX  - Input Mode ?
		RET	Z		; return now if in editing mode

		LD	DE,(WORKSP)	; fetch WORKSP to DE
		RET	C		; return if carry set ( entry = set-de)

		LD	HL,(STKBOT)	; fetch STKBOT to HL as well
		RET			; and return  (entry = set-hl (in input))

; -----------------------------------
; THE 'REMOVE FLOATING POINT' ROUTINE
; -----------------------------------
;   When a BASIC LINE or the INPUT BUFFER is parsed any numbers will have
;   an invisible chr 14d inserted after them and the 5-byte integer or
;   floating point form inserted after that.  Similar invisible value holders
;   are also created after the numeric and string variables in a DEF FN list.
;   This routine removes these 'compiled' numbers from the edit line or
;   input workspace.

;; REMOVE-FP
o11A7:		LD	A,(HL)		; fetch character
		CP	$0E		; is it the CHR$ 14 number marker ?
		LD	BC,$0006	; prepare to strip six bytes

		CALL	Z,o19E8		; routine RECLAIM-2 reclaims bytes if CHR$ 14.

		LD	A,(HL)		; reload next (or same) character
		INC	HL		; and advance address
		CP	$0D		; end of line or input buffer ?
		JR	NZ,o11A7	; back to REMOVE-FP until entire line done.

		RET			; return.


; *********************************
; ** Part 6. EXECUTIVE ROUTINES  **
; *********************************


; The memory.
;
; +---------+-----------+------------+--------------+-------------+--
; | BASIC   |  Display  | Attributes | ZX Printer   |    System   |
; |  ROM    |   File    |    File    |   Buffer     |  Variables  |
; +---------+-----------+------------+--------------+-------------+--
; ^         ^           ^            ^              ^             ^
; $0000   $4000       $5800        $5B00          $5C00         $5CB6 = CHANS
;
;
;  --+----------+---+---------+-----------+---+------------+--+---+--
;    | Channel  |$80|  BASIC  | Variables |$80| Edit Line  |NL|$80|
;    |   Info   |   | Program |   Area    |   | or Command |  |   |
;  --+----------+---+---------+-----------+---+------------+--+---+--
;    ^              ^         ^               ^                   ^
;  CHANS           PROG      VARS           E_LINE              WORKSP
;
;
;                             ---5-->         <---2---  <--3---
;  --+-------+--+------------+-------+-------+---------+-------+-+---+------+
;    | INPUT |NL| Temporary  | Calc. | Spare | Machine | GOSUB |?|$3E| UDGs |
;    | data  |  | Work Space | Stack |       |  Stack  | Stack | |   |      |
;  --+-------+--+------------+-------+-------+---------+-------+-+---+------+
;    ^                       ^       ^       ^                   ^   ^      ^
;  WORKSP                  STKBOT  STKEND   sp               RAMTOP UDG  P_RAMT
;

; -----------------
; THE 'NEW' COMMAND
; -----------------
;   The NEW command is about to set all RAM below RAMTOP to zero and then
;   re-initialize the system.  All RAM above RAMTOP should, and will be,
;   preserved.
;   There is nowhere to store values in RAM or on the stack which becomes
;   inoperable. Similarly PUSH and CALL instructions cannot be used to store
;   values or section common code. The alternate register set is the only place
;   available to store 3 persistent 16-bit system variables.

;; NEW
o11B7:		DI
					; cleared.
		LD	A,$FF		; Flag coming from NEW.
		LD	DE,(RAMTOP)	; Fetch RAMTOP as top value.
		EXX			; Switch in alternate set.
		LD	BC,(P_RAMT)	; Fetch P-RAMT differs on 16K/48K machines.
		LD	DE,(RASP)	; Fetch RASP/PIP.
		LD	HL,(UDG)	; Fetch UDG    differs on 16K/48K machines.
		EXX			; Switch back to main set and continue into...

; ----------------------
; THE 'START-NEW' BRANCH
; ----------------------
;   This branch is taken from above and from RST 00h.
;   The common code tests RAM and sets it to zero re-initializing all the
;   non-zero system variables and channel information.  The A register flags
;   if coming from START or NEW.

;; START-NEW
o11CB:		LD	B,A		; Save the flag to control later branching.
		LD	A,defborder	; Select a white border (***)
		OUT	($FE),A		; and set it now by writing to a port.

		LD	A,$3F		; Load the accumulator with last page in ROM.
		LD	I,A		; Set the I register - this remains constant
					; and can't be in the range $40 - $7F as 'snow'
					; appears on the screen.

		NOP			; These seem unnecessary.
		NOP			;
		NOP			;
		NOP			;
		NOP			;
		NOP			;

; -----------------------
; THE 'RAM CHECK' SECTION
; -----------------------
;   Typically, a Spectrum will have 16K or 48K of RAM and this code will test
;   it all till it finds an unpopulated location or, less likely, a faulty
;   location.  Usually it stops when it reaches the top $FFFF, or in the case
;   of NEW the supplied top value.  The entire screen turns black with
;   sometimes red stripes on black paper just visible.

;; ram-check
o11DA:		LD	H,D		; Transfer the top value to the HL register
		LD	L,E		; pair.

;; RAM-FILL
o11DC:		LD	(HL),$02	; Load memory with $02 - red ink on black paper.
		DEC	HL		; Decrement memory address.
		CP	H		; Have we reached ROM - $3F ?
		JR	NZ,o11DC	; Back to RAM-FILL if not.

;; RAM-READ
o11E2:		AND	A		; Clear carry - prepare to subtract.
		SBC	HL,DE		; subtract and add back setting
		ADD	HL,DE		; carry when back at start.
		INC	HL		; and increment for next iteration.
		JR	NC,o11EF	; forward to RAM-DONE if we've got back to
					; starting point with no errors.

		DEC	(HL)		; decrement to 1.
		JR	Z,o11EF		; forward to RAM-DONE if faulty.

		DEC	(HL)		; decrement to zero.
		JR	Z,o11E2		; back to RAM-READ if zero flag was set.

;; RAM-DONE
o11EF:		DEC	HL		; step back to last valid location.
		EXX			; regardless of state, set up possibly
					; stored system variables in case from NEW.
		LD	(P_RAMT),BC	; insert P-RAMT.
		LD	(RASP),DE	; insert RASP/PIP.
		LD	(UDG),HL	; insert UDG.
		EXX			; switch in main set.
		INC	B		; now test if we arrived here from NEW.
		JR	Z,o1219		; forward to RAM-SET if we did.

;   This section applies to START only.

		LD	(P_RAMT),HL	; set P-RAMT to the highest working RAM
					; address.
		LD	DE,$3EAF	; address of last byte of 'U' bitmap in ROM.
		LD	BC,$00A8	; there are 21 user defined graphics.
		EX	DE,HL		; switch pointers and make the UDGs a
		LDDR			; copy of the standard characters A - U.
		EX	DE,HL		; switch the pointer to HL.
		INC	HL		; update to start of 'A' in RAM.
		LD	(UDG),HL	; make UDG system variable address the first
					; bitmap.
		DEC	HL		; point at RAMTOP again.

		LD	BC,$0040	; set the values of
		LD	(RASP),BC	; the PIP and RASP system variables.

;   The NEW command path rejoins here.

;; RAM-SET
o1219:		LD	(RAMTOP),HL	; set system variable RAMTOP to HL.

;
;   Note. this entry point is a disabled Warm Restart that was almost certainly
;   once pointed to by the System Variable NMIADD.  It would be essential that
;   any NMI Handler would perform the tasks from here to the EI instruction
;   below.

;; NMI_VECT
o121C:
		LD	HL,$3C00	; a strange place to set the pointer to the
		LD	(CHARS),HL	; character set, CHARS - as no printing yet.

		LD	HL,(RAMTOP)	; fetch RAMTOP to HL again as we've lost it.

		LD	(HL),$3E	; top of user ram holds GOSUB end marker
					; an impossible line number - see RETURN.
					; no significance in the number $3E. It has
					; been traditional since the ZX80.

		DEC	HL		; followed by empty byte (not important).
		LD	SP,HL		; set up the machine stack pointer.
		DEC	HL		;
		DEC	HL		;
		LD	(ERR_SP),HL	; ERR_SP is where the error pointer is
					; at moment empty - will take address MAIN-4
					; at the call preceding that address,
					; although interrupts and calls will make use
					; of this location in meantime.

		IM	1		; select interrupt mode 1.

		LD	IY,ERR_NR	; set IY to ERR_NR. IY can reach all standard
					; system variables but shadow ROM system
					; variables will be mostly out of range.

		EI			; enable interrupts now that we have a stack.

;   If, as suggested above, the NMI service routine pointed to this section of
;   code then a decision would have to be made at this point to jump forward,
;   in a Warm Restart scenario, to produce a report code, leaving any program
;   intact.

		LD	HL,$5CB6	; The address of the channels - initially
					; following system variables.
		LD	(CHANS),HL	; Set the CHANS system variable.

		LD	DE,o15AF	; Address: init-chan in ROM.
		LD	BC,$0015	; There are 21 bytes of initial data in ROM.
		EX	DE,HL		; swap the pointers.
		LDIR			; Copy the bytes to RAM.

		EX	DE,HL		; Swap pointers. HL points to program area.
		DEC	HL		; Decrement address.
		LD	(DATADD),HL	; Set DATADD to location before program area.
		INC	HL		; Increment again.

		LD	(PROG),HL	; Set PROG the location where BASIC starts.
		LD	(VARS),HL	; Set VARS to same location with a
		LD	(HL),$80	; variables end-marker.
		INC	HL		; Advance address.
		LD	(E_LINE),HL	; Set E_LINE, where the edit line
					; will be created.
					; Note. it is not strictly necessary to
					; execute the next fifteen bytes of code
					; as this will be done by the call to SET-MIN.
					; --
		LD	(HL),$0D	; initially just has a carriage return
		INC	HL		; followed by
		LD	(HL),$80	; an end-marker.
		INC	HL		; address the next location.
		LD	(WORKSP),HL	; set WORKSP - empty workspace.
		LD	(STKBOT),HL	; set STKBOT - bottom of the empty stack.
		LD	(STKEND),HL	; set STKEND to the end of the empty stack.
					; --
		LD	A,defattr	; the colour system is set to white paper,
					; black ink, no flash or bright.
		LD	(ATTR_P),A	; set ATTR_P permanent colour attributes.
		LD	(ATTR_T),A	; set ATTR_T temporary colour attributes.
		LD	(BORDCR),A	; set BORDCR the border colour/lower screen
					; attributes.

		LD	HL,$0523	; The keyboard repeat and delay values are
		LD	(REPDEL),HL	; loaded to REPDEL and REPPER.

		DEC	(IY-$3A)	; set KSTATE-0 to $FF - keyboard map available.
		DEC	(IY-$36)	; set KSTATE-4 to $FF - keyboard map available.

		LD	HL,o15C6	; set source to ROM Address: init-strm
		LD	DE,STRMS	; set destination to system variable STRMS-FD
		LD	BC,$000E	; copy the 14 bytes of initial 7 streams data
		LDIR			; from ROM to RAM.

		SET	1,(IY+$01)	; update FLAGS  - signal printer in use.
		CALL	o0EDF		; call routine CLEAR-PRB to initialize system
					; variables associated with printer.
					; The buffer is clear.

		LD	(IY+$31),$02	; set DF_SZ the lower screen display size to
					; two lines
		CALL	o0D6B		; call routine CLS to set up system
					; variables associated with screen and clear
					; the screen and set attributes.
		XOR	A		; clear accumulator so that we can address
		LD	DE,o1539-1	; the message table directly.
		CALL	o0C0A		; routine PO-MSG puts
					; ' ©  1982 Sinclair Research Ltd'
					; at bottom of display.
		SET	5,(IY+$02)	; update TV_FLAG  - signal lower screen will
					; require clearing.

		JR	o12A9		; forward to MAIN-1
					; -------------------------
					; THE 'MAIN EXECUTION LOOP'
					; -------------------------
					;
					;

;; MAIN-EXEC
o12A2:		LD	(IY+$31),$02	; set DF_SZ lower screen display file size to
					; two lines.
		CALL	o1795		; routine AUTO-LIST

;; MAIN-1
o12A9:		CALL	o16B0		; routine SET-MIN clears work areas.

;; MAIN-2
o12AC:		LD	A,$00		; select channel 'K' the keyboard

		CALL	o1601		; routine CHAN-OPEN opens it

		CALL	o0F2C		; routine EDITOR is called.
					; Note the above routine is where the Spectrum
					; waits for user-interaction. Perhaps the
					; most common input at this stage
					; is LOAD "".

		CALL	o1B17		; routine LINE-SCAN scans the input.

		BIT	7,(IY+$00)	; test ERR_NR - will be $FF if syntax is OK.
		JR	NZ,o12CF	; forward, if correct, to MAIN-3.

;

		BIT	4,(IY+$30)	; test FLAGS2 - K channel in use ?
		JR	Z,o1303		; forward to MAIN-4 if not.

;

		LD	HL,(E_LINE)	; an editing error so address E_LINE.
		CALL	o11A7		; routine REMOVE-FP removes the hidden
					; floating-point forms.
		LD	(IY+$00),$FF	; system variable ERR_NR is reset to 'OK'.
		JR	o12AC		; back to MAIN-2 to allow user to correct.

; ---

; the branch was here if syntax has passed test.

;; MAIN-3
o12CF:		LD	HL,(E_LINE)	; fetch the edit line address from E_LINE.

		LD	(CH_ADD),HL	; system variable CH_ADD is set to first
					; character of edit line.
					; Note. the above two instructions are a little
					; inadequate.
					; They are repeated with a subtle difference
					; at the start of the next subroutine and are
					; therefore not required above.

		CALL	o19FB		; routine E-LINE-NO will fetch any line
					; number to BC if this is a program line.

		LD	A,B		; test if the number of
		OR	C		; the line is non-zero.
		JP	NZ,o155D	; jump forward to MAIN-ADD if so to add the
					; line to the BASIC program.

; Has the user just pressed the ENTER key ?

		RST	18H		; GET-CHAR gets character addressed by CH_ADD.
		CP	$0D		; is it a carriage return ?
		JR	Z,o12A2		; back to MAIN-EXEC if so for an automatic
					; listing.

; this must be a direct command.

		BIT	0,(IY+$30)	; test FLAGS2 - clear the main screen ?

		CALL	NZ,o0DAF	; routine CL-ALL, if so, e.g. after listing.

		CALL	o0D6E		; routine CLS-LOWER anyway.

		LD	A,$19		; compute scroll count as 25 minus
		SUB	(IY+$4F)	; value of S_POSN_hi.
		LD	(SCR_CT),A	; update SCR_CT system variable.
		SET	7,(IY+$01)	; update FLAGS - signal running program.
		LD	(IY+$00),$FF	; set ERR_NR to 'OK'.
		LD	(IY+$0A),$01	; set NSPPC to one for first statement.
o1300:		CALL	o1B8A		; call routine LINE-RUN to run the line.
					; sysvar ERR_SP therefore addresses MAIN-4

; Examples of direct commands are RUN, CLS, LOAD "", PRINT USR 40000,
; LPRINT "A"; etc..
; If a user written machine-code program disables interrupts then it
; must enable them to pass the next step. We also jumped to here if the
; keyboard was not being used.

;; MAIN-4
o1303:		HALT
					; set bit 5 of FLAGS.

		RES	5,(IY+$01)	; update bit 5 of FLAGS - signal no new key.

		BIT	1,(IY+$30)	; test FLAGS2 - is printer buffer clear ?
		CALL	NZ,o0ECD	; call routine COPY-BUFF if not.
					; Note. the programmer has neglected
					; to set bit 1 of FLAGS first.

		LD	A,(ERR_NR)	; fetch ERR_NR
		INC	A		; increment to give true code.

; Now deal with a runtime error as opposed to an editing error.
; However if the error code is now zero then the OK message will be printed.

;; MAIN-G
o1313:		PUSH	AF		; save the error number.

		LD	HL,$0000	; prepare to clear some system variables.
		LD	(IY+$37),H	; clear all the bits of FLAGX.
		LD	(IY+$26),H	; blank X_PTR_hi to suppress error marker.
		LD	(DEFADD),HL	; blank DEFADD to signal that no defined
					; function is currently being evaluated.

		LD	HL,$0001	; explicit - inc hl would do.
		LD	($5C16),HL	; ensure STRMS-00 is keyboard.

		CALL	o16B0		; routine SET-MIN clears workspace etc.
		RES	5,(IY+$37)	; update FLAGX - signal in EDIT not INPUT mode.
					; Note. all the bits were reset earlier.

		CALL	o0D6E		; call routine CLS-LOWER.

		SET	5,(IY+$02)	; update TV_FLAG - signal lower screen
					; requires clearing.

		POP	AF		; bring back the true error number
		LD	B,A		; and make a copy in B.
		CP	$0A		; is it a print-ready digit ?
		JR	C,o133C		; forward to MAIN-5 if so.

		ADD	A,$07		; add ASCII offset to letters.

;; MAIN-5
o133C:		CALL	o15EF		; call routine OUT-CODE to print the code.

		LD	A,$20		; followed by a space.
		RST	10H		; PRINT-A

		LD	A,B		; fetch stored report code.
		LD	DE,o1391	; address: rpt-mesgs.

		CALL	o0C0A		; call routine PO-MSG to print the message.
		CALL	o3A29
		NOP
		CALL	o0C0A		; routine PO-MSG prints ', ' although it would
					; be more succinct to use RST $10.

		LD	BC,(PPC)	; fetch PPC the current line number.
		CALL	o1A1B		; routine OUT-NUM-1 will print that

		LD	A,$3A		; then a ":" character.
		RST	10H		; PRINT-A

		LD	C,(IY+$0D)	; then SUBPPC for statement
		LD	B,$00		; limited to 127
		CALL	o1A1B		; routine OUT-NUM-1 prints BC.

		CALL	CLEARSP		; routine CLEAR-SP clears editing area which
					; probably contained 'RUN'.

		LD	A,(ERR_NR)	; fetch ERR_NR again
		INC	A		; test for no error originally $FF.
		JR	Z,o1386		; forward to MAIN-9 if no error.

		CP	$09		; is code Report 9 STOP ?
		JR	Z,o1373		; forward to MAIN-6 if so

		CP	$15		; is code Report L Break ?
		JR	NZ,o1376	; forward to MAIN-7 if not

; Stop or Break was encountered so consider CONTINUE.

;; MAIN-6
o1373:		INC	(IY+$0D)	; increment SUBPPC to next statement.

;; MAIN-7
o1376:		LD	BC,$0003	; prepare to copy 3 system variables to
		LD	DE,OSPCC	; address OSPPC - statement for CONTINUE.
					; also updating OLDPPC line number below.

		LD	HL,NSPPC	; set source top to NSPPC next statement.
		BIT	7,(HL)		; did BREAK occur before the jump ?
					; e.g. between GO TO and next statement.
		JR	Z,o1384		; skip forward to MAIN-8, if not, as set-up
					; is correct.

		ADD	HL,BC		; set source to SUBPPC number of current
					; statement/line which will be repeated.

;; MAIN-8
o1384:		LDDR
					; or NSPPC to OLDPPC and NEWPPC to OSPCC

;; MAIN-9
o1386:		LD	(IY+$0A),$FF	; update NSPPC - signal 'no jump'.
		RES	3,(IY+$01)	; update FLAGS - signal use 'K' mode for
					; the first character in the editor and

		JP	o12AC		; jump back to MAIN-2.


; ----------------------
; Canned report messages
; ----------------------
; The Error reports with the last byte inverted. The first entry
; is a dummy entry. The last, which begins with $7F, the Spectrum
; character for copyright symbol, is poaced here for convenience
; as is the preceding comma and space.
; The report line must accommodate a 4-digit line number and a 3-digit
; statement number which limits the length of the message text to twenty
; characters.
; e.g.  "B Integer out of range, 1000:127"

;; rpt-mesgs

o1391:		DB	$80
		DB	"O","K" + $80	; 0 -> OK
		DB	"NEXT without FO","R"+$80		; 1
		DB	"Variable not foun","d"+$80		; 2
		DB	"Subscript wron","g"+$80		; 3
		DB	"Out of memor","y"+$80			; 4
		DB	"Out of scree","n"+$80			; 5
		DB	"Number too bi","g"+$80			; 6
		DB	"RETURN without GOSU","B"+$80		; 7
		DB	"End of fil","e"+$80			; 8
		DB	"STOP statemen","t"+$80			; 9
		DB	"Invalid argumen","t"+$80		; A
		DB	"Integer out of rang","e"+$80		; B
		DB	"Nonsense in BASI","C"+$80		; C
		DB	"BREAK - CONT repeat","s"+$80		; D
		DB	"Out of DAT","A"+$80			; E
		DB	"Invalid file nam","e"+$80		; F
		DB	"No room for lin","e"+$80		; G
		DB	"STOP in INPU","T"+$80			; H
		DB	"FOR without NEX","T"+$80		; I
		DB	"Invalid I/O devic","e"+$80		; J
		DB	"Invalid colou","r"+$80			; K
		DB	"BREAK into progra","m"+$80		; L
		DB	"RAMTOP no goo","d"+$80			; M
		DB	"Statement los","t"+$80			; N
		DB	"Invalid strea","m"+$80			; O
		DB	"FN without DE","F"+$80			; P
		DB	"Parameter erro","r"+$80		; Q
		DB	"Tape loading erro"			; R
o1536:		DB	"r"+$80
					;; comma-sp
o1537:		DB	","," "+$80	; used in report line.
					;; copyright
o1539:
		DB	$7F		; (C) -> chr copyright

		IF alternative
			DM	" 1982 Sinclair Research Lt","d" + $80
		ELSE
			DM	" 1982 Amstrad             "," " + $80
		ENDIF

; -------------
; REPORT-G
; -------------
; Note ERR_SP points here during line entry which allows the
; normal 'Out of Memory' report to be augmented to the more
; precise 'No Room for line' report.

;; REPORT-G
; No Room for line
o1555:		LD	A,$10		; i.e. 'G' -$30 -$07
		LD	BC,$0000	; this seems unnecessary.
		JP	o1313		; jump back to MAIN-G

; -----------------------------
; Handle addition of BASIC line
; -----------------------------
; Note this is not a subroutine but a branch of the main execution loop.
; System variable ERR_SP still points to editing error handler.
; A new line is added to the BASIC program at the appropriate place.
; An existing line with same number is deleted first.
; Entering an existing line number deletes that line.
; Entering a non-existent line allows the subsequent line to be edited next.

;; MAIN-ADD
o155D:		LD	(E_PPC),BC	; set E_PPC to extracted line number.
		LD	HL,(CH_ADD)	; fetch CH_ADD - points to location after the
					; initial digits (set in E_LINE_NO).
		EX	DE,HL		; save start of BASIC in DE.

		LD	HL,o1555	; Address: REPORT-G
		PUSH	HL		; is pushed on stack and addressed by ERR_SP.
					; the only error that can occur is
					; 'Out of memory'.

		LD	HL,(WORKSP)	; fetch WORKSP - end of line.
		SCF			; prepare for true subtraction.
		SBC	HL,DE		; find length of BASIC and
		PUSH	HL		; save it on stack.
		LD	H,B		; transfer line number
		LD	L,C		; to HL register.
		CALL	o196E		; routine LINE-ADDR will see if
					; a line with the same number exists.
		JR	NZ,o157D	; forward if no existing line to MAIN-ADD1.

		CALL	o19B8		; routine NEXT-ONE finds the existing line.
		CALL	o19E8		; routine RECLAIM-2 reclaims it.

;; MAIN-ADD1
o157D:		POP	BC		; retrieve the length of the new line.
		LD	A,C		; and test if carriage return only
		DEC	A		; i.e. one byte long.
		OR	B		; result would be zero.
		JR	Z,o15AB		; forward to MAIN-ADD2 is so.

		PUSH	BC		; save the length again.
		INC	BC		; adjust for inclusion
		INC	BC		; of line number (two bytes)
		INC	BC		; and line length
		INC	BC		; (two bytes).
		DEC	HL		; HL points to location before the destination

		LD	DE,(PROG)	; fetch the address of PROG
		PUSH	DE		; and save it on the stack
		CALL	o1655		; routine MAKE-ROOM creates BC spaces in
					; program area and updates pointers.
		POP	HL		; restore old program pointer.
		LD	(PROG),HL	; and put back in PROG as it may have been
					; altered by the POINTERS routine.

		POP	BC		; retrieve BASIC length
		PUSH	BC		; and save again.

		INC	DE		; points to end of new area.
		LD	HL,(WORKSP)	; set HL to WORKSP - location after edit line.
		DEC	HL		; decrement to address end marker.
		DEC	HL		; decrement to address carriage return.
		LDDR			; copy the BASIC line back to initial command.

		LD	HL,(E_PPC)	; fetch E_PPC - line number.
		EX	DE,HL		; swap it to DE, HL points to last of
					; four locations.
		POP	BC		; retrieve length of line.
		LD	(HL),B		; high byte last.
		DEC	HL		;
		LD	(HL),C		; then low byte of length.
		DEC	HL		;
		LD	(HL),E		; then low byte of line number.
		DEC	HL		;
		LD	(HL),D		; then high byte range $0 - $27 (1-9999).

;; MAIN-ADD2
o15AB:		POP	AF		; drop the address of Report G
		JP	o12A2		; and back to MAIN-EXEC producing a listing
					; and to reset ERR_SP in EDITOR.


; ---------------------------------
; THE 'INITIAL CHANNEL' INFORMATION
; ---------------------------------
;   This initial channel information is copied from ROM to RAM, during
;   initialization.  It's new location is after the system variables and is
;   addressed by the system variable CHANS which means that it can slide up and
;   down in memory.  The table is never searched, by this ROM, and the last
;   character, which could be anything other than a comma, provides a
;   convenient resting place for DATADD.

;; init-chan
o15AF:		DW	o09F4		; PRINT-OUT
		DW	o10A8		; KEY-INPUT
		DB	$4B		; 'K'
		DW	o09F4		; PRINT-OUT
		DW	o15C4		; REPORT-J
		DB	$53		; 'S'
		DW	o0F81		; ADD-CHAR
		DW	o15C4		; REPORT-J
		DB	$52		; 'R'
		DW	o09F4		; PRINT-OUT
		DW	o15C4		; REPORT-J
		DB	$50		; 'P'

		DB	$80		; End Marker

;; REPORT-J
o15C4:		RST	08H		; ERROR-1
		DB	$12		; Error Report: Invalid I/O device


; -------------------------
; THE 'INITIAL STREAM' DATA
; -------------------------
;   This is the initial stream data for the seven streams $FD - $03 that is
;   copied from ROM to the STRMS system variables area during initialization.
;   There are reserved locations there for another 12 streams.  Each location
;   contains an offset to the second byte of a channel.  The first byte of a
;   channel can't be used as that would result in an offset of zero for some
;   and zero is used to denote that a stream is closed.

;; init-strm
o15C6:		DB	$01,$00		; stream $FD offset to channel 'K'
		DB	$06,$00		; stream $FE offset to channel 'S'
		DB	$0B,$00		; stream $FF offset to channel 'R'

		DB	$01,$00		; stream $00 offset to channel 'K'
		DB	$01,$00		; stream $01 offset to channel 'K'
		DB	$06,$00		; stream $02 offset to channel 'S'
		DB	$10,$00		; stream $03 offset to channel 'P'

; ------------------------------
; THE 'INPUT CONTROL' SUBROUTINE
; ------------------------------
;

;; WAIT-KEY
o15D4:		BIT	5,(IY+$02)	; test TV_FLAG - clear lower screen ?
		JR	NZ,o15DE	; forward to WAIT-KEY1 if so.

		SET	3,(IY+$02)	; update TV_FLAG - signal reprint the edit
					; line to the lower screen.

;; WAIT-KEY1
o15DE:		CALL	o15E6		; routine INPUT-AD is called.

		RET	C		; return with acceptable keys.

		JR	Z,o15DE		; back to WAIT-KEY1 if no key is pressed
					; or it has been handled within INPUT-AD.

;   Note. When inputting from the keyboard all characters are returned with
;   above conditions so this path is never taken.

;; REPORT-8
o15E4:		RST	08H		; ERROR-1
		DB	$07		; Error Report: End of file

; ---------------------------
; THE 'INPUT ADDRESS' ROUTINE
; ---------------------------
;   This routine fetches the address of the input stream from the current
;   channel area using the system variable CURCHL.

;; INPUT-AD
o15E6:		EXX
		PUSH	HL		; save HL register
		LD	HL,(CURCHL)	; fetch address of CURCHL - current channel.
		INC	HL		; step over output routine
		INC	HL		; to point to low byte of input routine.
		JR	o15F7		; forward to CALL-SUB.

; -------------------------
; THE 'CODE OUTPUT' ROUTINE
; -------------------------
;   This routine is called on five occasions to print the ASCII equivalent of
;   a value 0-9.

;; OUT-CODE
o15EF:		LD	E,$30		; add 48 decimal to give the ASCII character
		ADD	A,E		; "0" to "9" and continue into the main output
					; routine.

; -------------------------
; THE 'MAIN OUTPUT' ROUTINE
; -------------------------
;   PRINT-A-2 is a continuation of the RST 10 restart that prints any character.
;   The routine prints to the current channel and the printing of control codes
;   may alter that channel to divert subsequent RST 10 instructions to temporary
;   routines. The normal channel is $09F4.

;; PRINT-A-2
o15F2:		EXX
		PUSH	HL		; save HL register
		LD	HL,(CURCHL)	; fetch CURCHL the current channel.

; input-ad rejoins here also.

;; CALL-SUB
o15F7:		LD	E,(HL)		; put the low byte in E.
		INC	HL		; advance address.
		LD	D,(HL)		; put the high byte to D.
		EX	DE,HL		; transfer the stream to HL.
		CALL	o162C		; use routine CALL-JUMP.
					; in effect CALL (HL).

		POP	HL		; restore saved HL register.
		EXX			; switch back to the main set and
		RET			; return.

; --------------------------
; THE 'OPEN CHANNEL' ROUTINE
; --------------------------
;   This subroutine is used by the ROM to open a channel 'K', 'S', 'R' or 'P'.
;   This is either for its own use or in response to a user's request, for
;   example, when "#" is encountered with output - PRINT, LIST etc.
;   or with input - INPUT, INKEY$ etc.
;   It is entered with a system stream $FD - $FF, or a user stream $00 - $0F
;   in the accumulator.

;; CHAN-OPEN
o1601:		ADD	A,A		; double the stream ($FF will become $FE etc.)
		ADD	A,$16		; add the offset to stream 0 from $5C00
		LD	L,A		; result to L
		LD	H,$5C		; now form the address in STRMS area.
		LD	E,(HL)		; fetch low byte of CHANS offset
		INC	HL		; address next
		LD	D,(HL)		; fetch high byte of offset
		LD	A,D		; test that the stream is open.
		OR	E		; zero if closed.
		JR	NZ,o1610	; forward to CHAN-OP-1 if open.

;; REPORT-Oa
o160E:		RST	08H		; ERROR-1
		DB	$17		; Error Report: Invalid stream

; continue here if stream was open. Note that the offset is from CHANS
; to the second byte of the channel.

;; CHAN-OP-1
o1610:		DEC	DE		; reduce offset so it points to the channel.
		LD	HL,(CHANS)	; fetch CHANS the location of the base of
					; the channel information area
		ADD	HL,DE		; and add the offset to address the channel.
					; and continue to set flags.

; -----------------
; Set channel flags
; -----------------
; This subroutine is used from ED-EDIT, str$ and read-in to reset the
; current channel when it has been temporarily altered.

;; CHAN-FLAG
o1615:		LD	(CURCHL),HL	; set CURCHL system variable to the
					; address in HL
		RES	4,(IY+$30)	; update FLAGS2  - signal K channel not in use.
					; Note. provide a default for channel 'R'.
		INC	HL		; advance past
		INC	HL		; output routine.
		INC	HL		; advance past
		INC	HL		; input routine.
		LD	C,(HL)		; pick up the letter.
		LD	HL,o162D	; address: chn-cd-lu
		CALL	o16DC		; routine INDEXER finds offset to a
					; flag-setting routine.

		RET	NC		; but if the letter wasn't found in the
					; table just return now. - channel 'R'.

		LD	D,$00		; prepare to add
		LD	E,(HL)		; offset to E
		ADD	HL,DE		; add offset to location of offset to form
					; address of routine

;; CALL-JUMP
o162C:		JP	(HL)		; jump to the routine

; Footnote. calling any location that holds JP (HL) is the equivalent to
; a pseudo Z80 instruction CALL (HL). The ROM uses the instruction above.

; --------------------------
; Channel code look-up table
; --------------------------
; This table is used by the routine above to find one of the three
; flag setting routines below it.
; A zero end-marker is required as channel 'R' is not present.

;; chn-cd-lu
o162D:		DB	"K", o1634-$-1	; offset $06 to CHAN-K
		DB	"S", o1642-$-1	; offset $12 to CHAN-S
		DB	"P", o164D-$-1	; offset $1B to CHAN-P

		DB	$00		; end marker.

; --------------
; Channel K flag
; --------------
; routine to set flags for lower screen/keyboard channel.

;; CHAN-K
o1634:		SET	0,(IY+$02)	; update TV_FLAG  - signal lower screen in use
		RES	5,(IY+$01)	; update FLAGS    - signal no new key
		SET	4,(IY+$30)	; update FLAGS2   - signal K channel in use
		JR	o1646		; forward to CHAN-S-1 for indirect exit

; --------------
; Channel S flag
; --------------
; routine to set flags for upper screen channel.

;; CHAN-S
o1642:		RES	0,(IY+$02)	; TV_FLAG  - signal main screen in use

;; CHAN-S-1
o1646:		RES	1,(IY+$01)	; update FLAGS  - signal printer not in use
		JP	o0D4D		; jump back to TEMPS and exit via that
					; routine after setting temporary attributes.
					; --------------
					; Channel P flag
					; --------------
					; This routine sets a flag so that subsequent print related commands
					; print to printer or update the relevant system variables.
					; This status remains in force until reset by the routine above.

;; CHAN-P
o164D:		SET	1,(IY+$01)	; update FLAGS  - signal printer in use
		RET			; return

; --------------------------
; THE 'ONE SPACE' SUBROUTINE
; --------------------------
; This routine is called once only to create a single space
; in workspace by ADD-CHAR.

;; ONE-SPACE
o1652:		LD	BC,$0001	; create space for a single character.

; ---------
; Make Room
; ---------
; This entry point is used to create BC spaces in various areas such as
; program area, variables area, workspace etc..
; The entire free RAM is available to each BASIC statement.
; On entry, HL addresses where the first location is to be created.
; Afterwards, HL will point to the location before this.

;; MAKE-ROOM
o1655:		PUSH	HL		; save the address pointer.
		CALL	o1F05		; routine TEST-ROOM checks if room
					; exists and generates an error if not.
		POP	HL		; restore the address pointer.
		CALL	o1664		; routine POINTERS updates the
					; dynamic memory location pointers.
					; DE now holds the old value of STKEND.
		LD	HL,(STKEND)	; fetch new STKEND the top destination.

		EX	DE,HL		; HL now addresses the top of the area to
					; be moved up - old STKEND.
o1661:		LDDR
		RET			; return with new area ready to be populated.
					; HL points to location before new area,
					; and DE to last of new locations.

; -----------------------------------------------
; Adjust pointers before making or reclaiming room
; -----------------------------------------------
; This routine is called by MAKE-ROOM to adjust upwards and by RECLAIM to
; adjust downwards the pointers within dynamic memory.
; The fourteen pointers to dynamic memory, starting with VARS and ending
; with STKEND, are updated adding BC if they are higher than the position
; in HL.
; The system variables are in no particular order except that STKEND, the first
; free location after dynamic memory must be the last encountered.

;; POINTERS
o1664:		PUSH	AF		; preserve accumulator.
		PUSH	HL		; put pos pointer on stack.
		LD	HL,VARS		; address VARS the first of the
		LD	A,$0E		; fourteen variables to consider.

;; PTR-NEXT
o166B:		LD	E,(HL)		; fetch the low byte of the system variable.
		INC	HL		; advance address.
		LD	D,(HL)		; fetch high byte of the system variable.
		EX	(SP),HL		; swap pointer on stack with the variable
					; pointer.
		AND	A		; prepare to subtract.
		SBC	HL,DE		; subtract variable address
		ADD	HL,DE		; and add back
		EX	(SP),HL		; swap pos with system variable pointer
		JR	NC,o167F	; forward to PTR-DONE if var before pos

		PUSH	DE		; save system variable address.
		EX	DE,HL		; transfer to HL
		ADD	HL,BC		; add the offset
		EX	DE,HL		; back to DE
		LD	(HL),D		; load high byte
		DEC	HL		; move back
		LD	(HL),E		; load low byte
		INC	HL		; advance to high byte
		POP	DE		; restore old system variable address.

;; PTR-DONE
o167F:		INC	HL		; address next system variable.
		DEC	A		; decrease counter.
		JR	NZ,o166B	; back to PTR-NEXT if more.
		EX	DE,HL		; transfer old value of STKEND to HL.
					; Note. this has always been updated.
		POP	DE		; pop the address of the position.

		POP	AF		; pop preserved accumulator.
		AND	A		; clear carry flag preparing to subtract.

		SBC	HL,DE		; subtract position from old stkend
		LD	B,H		; to give number of data bytes
		LD	C,L		; to be moved.
		INC	BC		; increment as we also copy byte at old STKEND.
		ADD	HL,DE		; recompute old stkend.
		EX	DE,HL		; transfer to DE.
		RET			; return.



; -------------------
; Collect line number
; -------------------
; This routine extracts a line number, at an address that has previously
; been found using LINE-ADDR, and it is entered at LINE-NO. If it encounters
; the program 'end-marker' then the previous line is used and if that
; should also be unacceptable then zero is used as it must be a direct
; command. The program end-marker is the variables end-marker $80, or
; if variables exist, then the first character of any variable name.

;; LINE-ZERO
o168F:		DB	$00,$00		; dummy line number used for direct commands


;; LINE-NO-A
o1691:		EX	DE,HL		; fetch the previous line to HL and set
		LD	DE,o168F	; DE to LINE-ZERO should HL also fail.

; -> The Entry Point.

;; LINE-NO
o1695:		LD	A,(HL)		; fetch the high byte - max $2F
		AND	$C0		; mask off the invalid bits.
		JR	NZ,o1691	; to LINE-NO-A if an end-marker.

		LD	D,(HL)		; reload the high byte.
		INC	HL		; advance address.
		LD	E,(HL)		; pick up the low byte.
		RET			; return from here.

; -------------------
; Handle reserve room
; -------------------
; This is a continuation of the restart BC-SPACES

;; RESERVE
o169E:		LD	HL,(STKBOT)	; STKBOT first location of calculator stack
		DEC	HL		; make one less than new location
		CALL	o1655		; routine MAKE-ROOM creates the room.
		INC	HL		; address the first new location
		INC	HL		; advance to second
		POP	BC		; restore old WORKSP
		LD	(WORKSP),BC	; system variable WORKSP was perhaps
					; changed by POINTERS routine.
		POP	BC		; restore count for return value.
		EX	DE,HL		; switch. DE = location after first new space
		INC	HL		; HL now location after new space
		RET			; return.

; ---------------------------
; Clear various editing areas
; ---------------------------
; This routine sets the editing area, workspace and calculator stack
; to their minimum configurations as at initialization and indeed this
; routine could have been relied on to perform that task.
; This routine uses HL only and returns with that register holding
; WORKSP/STKBOT/STKEND though no use is made of this. The routines also
; reset MEM to its usual place in the systems variable area should it
; have been relocated to a FOR-NEXT variable. The main entry point
; SET-MIN is called at the start of the MAIN-EXEC loop and prior to
; displaying an error.

;; SET-MIN
o16B0:		LD	HL,(E_LINE)	; fetch E_LINE
		LD	(HL),$0D	; insert carriage return
		LD	(K_CUR),HL	; make K_CUR keyboard cursor point there.
		INC	HL		; next location
		LD	(HL),$80	; holds end-marker $80
		INC	HL		; next location becomes
		LD	(WORKSP),HL	; start of WORKSP

; This entry point is used prior to input and prior to the execution,
; or parsing, of each statement.

;; SET-WORK
o16BF:		LD	HL,(WORKSP)	; fetch WORKSP value
		LD	(STKBOT),HL	; and place in STKBOT

; This entry point is used to move the stack back to its normal place
; after temporary relocation during line entry and also from ERROR-3

;; SET-STK
o16C5:		LD	HL,(STKBOT)	; fetch STKBOT value
		LD	(STKEND),HL	; and place in STKEND.

		PUSH	HL		; perhaps an obsolete entry point.
		LD	HL,MEMBOT	; normal location of MEM-0
		LD	(MEM),HL	; is restored to system variable MEM.
		POP	HL		; saved value not required.
		RET			; return.

; ------------------
; Reclaim edit-line?
; ------------------
; This seems to be legacy code from the ZX80/ZX81 as it is
; not used in this ROM.
; That task, in fact, is performed here by the dual-area routine CLEAR-SP.
; This routine is designed to deal with something that is known to be in the
; edit buffer and not workspace.
; On entry, HL must point to the end of the something to be deleted.

;; REC-EDIT
o16D4:		LD	DE,(E_LINE)	; fetch start of edit line from E_LINE.
		JP	o19E5		; jump forward to RECLAIM-1.

; --------------------------
; The Table INDEXING routine
; --------------------------
; This routine is used to search two-byte hash tables for a character
; held in C, returning the address of the following offset byte.
; if it is known that the character is in the table e.g. for priorities,
; then the table requires no zero end-marker. If this is not known at the
; outset then a zero end-marker is required and carry is set to signal
; success.

;; INDEXER-1
o16DB:		INC	HL		; address the next pair of values.

; -> The Entry Point.

;; INDEXER
o16DC:		LD	A,(HL)		; fetch the first byte of pair
		AND	A		; is it the end-marker ?
		RET	Z		; return with carry reset if so.

		CP	C		; is it the required character ?
		INC	HL		; address next location.
		JR	NZ,o16DB	; back to INDEXER-1 if no match.

		SCF			; else set the carry flag.
		RET			; return with carry set

; --------------------------------
; The Channel and Streams Routines
; --------------------------------
; A channel is an input/output route to a hardware device
; and is identified to the system by a single letter e.g. 'K' for
; the keyboard. A channel can have an input and output route
; associated with it in which case it is bi-directional like
; the keyboard. Others like the upper screen 'S' are output
; only and the input routine usually points to a report message.
; Channels 'K' and 'S' are system channels and it would be inappropriate
; to close the associated streams so a mechanism is provided to
; re-attach them. When the re-attachment is no longer required, then
; closing these streams resets them as at initialization.
; Early adverts said that the network and RS232 were in this ROM.
; Channels 'N' and 'B' are user channels and have been removed successfully
; if, as seems possible, they existed.
; Ironically the tape streamer is not accessed through streams and
; channels.
; Early demonstrations of the Spectrum showed a single microdrive being
; controlled by the main ROM.

; ---------------------
; THE 'CLOSE #' COMMAND
; ---------------------
;   This command allows streams to be closed after use.
;   Any temporary memory areas used by the stream would be reclaimed and
;   finally flags set or reset if necessary.

;; CLOSE
o16E5:		CALL	o171E		; routine STR-DATA fetches parameter
					; from calculator stack and gets the
					; existing STRMS data pointer address in HL
					; and stream offset from CHANS in BC.

; Note. this offset could be zero if the
; stream is already closed. A check for this
; should occur now and an error should be
; generated, for example,
; Report S 'Stream status closed'.

		CALL	o1701		; routine CLOSE-2 would perform any actions
					; peculiar to that stream without disturbing
					; data pointer to STRMS entry in HL.

		LD	BC,$0000	; the stream is to be blanked.
		LD	DE,$A3E2	; the number of bytes from stream 4, $5C1E,
					; to $10000
		EX	DE,HL		; transfer offset to HL, STRMS data pointer
					; to DE.
		ADD	HL,DE		; add the offset to the data pointer.
		JR	C,o16FC		; forward to CLOSE-1 if a non-system stream.
					; i.e. higher than 3.

; proceed with a negative result.

		LD	BC,o15C6+14	; prepare the address of the byte after
					; the initial stream data in ROM. ($15D4)
		ADD	HL,BC		; index into the data table with negative value.
		LD	C,(HL)		; low byte to C
		INC	HL		; address next.
		LD	B,(HL)		; high byte to B.

;   and for streams 0 - 3 just enter the initial data back into the STRMS entry
;   streams 0 - 2 can't be closed as they are shared by the operating system.
;   -> for streams 4 - 15 then blank the entry.

;; CLOSE-1
o16FC:		EX	DE,HL		; address of stream to HL.
		LD	(HL),C		; place zero (or low byte).
		INC	HL		; next address.
		LD	(HL),B		; place zero (or high byte).
		RET			; return.

; ------------------------
; THE 'CLOSE-2' SUBROUTINE
; ------------------------
;   There is not much point in coming here.
;   The purpose was once to find the offset to a special closing routine,
;   in this ROM and within 256 bytes of the close stream look up table that
;   would reclaim any buffers associated with a stream. At least one has been
;   removed.
;   Any attempt to CLOSE streams $00 to $04, without first opening the stream,
;   will lead to either a system restart or the production of a strange report.
;   credit: Martin Wren-Hilton 1982.

;; CLOSE-2
o1701:		PUSH	HL		; * save address of stream data pointer
					; in STRMS on the machine stack.
		LD	HL,(CHANS)	; fetch CHANS address to HL
		ADD	HL,BC		; add the offset to address the second
					; byte of the output routine hopefully.
		INC	HL		; step past
		INC	HL		; the input routine.

;    Note. When the Sinclair Interface1 is fitted then an instruction fetch
;    on the next address pages this ROM out and the shadow ROM in.

;; ROM_TRAP
o1708:		INC	HL		; to address channel's letter
		LD	C,(HL)		; pick it up in C.
					; Note. but if stream is already closed we
					; get the value $10 (the byte preceding 'K').

		EX	DE,HL		; save the pointer to the letter in DE.

;   Note. The string pointer is saved but not used!!

		LD	HL,o1716	; address: cl-str-lu in ROM.
		CALL	o16DC		; routine INDEXER uses the code to get
					; the 8-bit offset from the current point to
					; the address of the closing routine in ROM.
					; Note. it won't find $10 there!

		LD	C,(HL)		; transfer the offset to C.
		LD	B,$00		; prepare to add.
		ADD	HL,BC		; add offset to point to the address of the
					; routine that closes the stream.
					; (and presumably removes any buffers that
					; are associated with it.)
		JP	(HL)		; jump to that routine.

; --------------------------------
; THE 'CLOSE STREAM LOOK-UP' TABLE
; --------------------------------
;   This table contains an entry for a letter found in the CHANS area.
;   followed by an 8-bit displacement, from that byte's address in the
;   table to the routine that performs any ancillary actions associated
;   with closing the stream of that channel.
;   The table doesn't require a zero end-marker as the letter has been
;   picked up from a channel that has an open stream.

;; cl-str-lu
o1716:		DB	"K", o171C-$-1	; offset 5 to CLOSE-STR
		DB	"S", o171C-$-1	; offset 3 to CLOSE-STR
		DB	"P", o171C-$-1	; offset 1 to CLOSE-STR


; ------------------------------
; THE 'CLOSE STREAM' SUBROUTINES
; ------------------------------
; The close stream routines in fact have no ancillary actions to perform
; which is not surprising with regard to 'K' and 'S'.

;; CLOSE-STR
o171C:		POP	HL		; * now just restore the stream data pointer
		RET			; in STRMS and return.

; -----------
; Stream data
; -----------
; This routine finds the data entry in the STRMS area for the specified
; stream which is passed on the calculator stack. It returns with HL
; pointing to this system variable and BC holding a displacement from
; the CHANS area to the second byte of the stream's channel. If BC holds
; zero, then that signifies that the stream is closed.

;; STR-DATA
o171E:		CALL	o1E94		; routine FIND-INT1 fetches parameter to A
		CP	$10		; is it less than 16d ?
		JR	C,o1727		; skip forward to STR-DATA1 if so.

;; REPORT-Ob
o1725:		RST	08H		; ERROR-1
		DB	$17		; Error Report: Invalid stream

;; STR-DATA1
o1727:		ADD	A,$03		; add the offset for 3 system streams.
					; range 00 - 15d becomes 3 - 18d.
		RLCA			; double as there are two bytes per
					; stream - now 06 - 36d
		LD	HL,STRMS	; address STRMS - the start of the streams
					; data area in system variables.
		LD	C,A		; transfer the low byte to A.
		LD	B,$00		; prepare to add offset.
		ADD	HL,BC		; add to address the data entry in STRMS.

; the data entry itself contains an offset from CHANS to the address of the
; stream

		LD	C,(HL)		; low byte of displacement to C.
		INC	HL		; address next.
		LD	B,(HL)		; high byte of displacement to B.
		DEC	HL		; step back to leave HL pointing to STRMS
					; data entry.
		RET			; return with CHANS displacement in BC
					; and address of stream data entry in HL.

; --------------------
; Handle OPEN# command
; --------------------
; Command syntax example: OPEN #5,"s"
; On entry the channel code entry is on the calculator stack with the next
; value containing the stream identifier. They have to swapped.

;; OPEN
o1736:		RST	28H		; FP-CALC    ;s,c.
		DB	$01		; exchange    ;c,s.
		DB	$38		; end-calc

		CALL	o171E		; routine STR-DATA fetches the stream off
					; the stack and returns with the CHANS
					; displacement in BC and HL addressing
					; the STRMS data entry.
		LD	A,B		; test for zero which
		OR	C		; indicates the stream is closed.
		JR	Z,o1756		; skip forward to OPEN-1 if so.

; if it is a system channel then it can re-attached.

		EX	DE,HL		; save STRMS address in DE.
		LD	HL,(CHANS)	; fetch CHANS.
		ADD	HL,BC		; add the offset to address the second
					; byte of the channel.
		INC	HL		; skip over the
		INC	HL		; input routine.
		INC	HL		; and address the letter.
		LD	A,(HL)		; pick up the letter.
		EX	DE,HL		; save letter pointer and bring back
					; the STRMS pointer.

		CP	$4B		; is it 'K' ?
		JR	Z,o1756		; forward to OPEN-1 if so

		CP	$53		; is it 'S' ?
		JR	Z,o1756		; forward to OPEN-1 if so

		CP	$50		; is it 'P' ?
		JR	NZ,o1725	; back to REPORT-Ob if not.
					; to report 'Invalid stream'.

; continue if one of the upper-case letters was found.
; and rejoin here from above if stream was closed.

;; OPEN-1
o1756:		CALL	o175D		; routine OPEN-2 opens the stream.

; it now remains to update the STRMS variable.

		LD	(HL),E		; insert or overwrite the low byte.
		INC	HL		; address high byte in STRMS.
		LD	(HL),D		; insert or overwrite the high byte.
		RET			; return.

; -----------------
; OPEN-2 Subroutine
; -----------------
; There is some point in coming here as, as well as once creating buffers,
; this routine also sets flags.

;; OPEN-2
o175D:		PUSH	HL		; * save the STRMS data entry pointer.
		CALL	o2BF1		; routine STK-FETCH now fetches the
					; parameters of the channel string.
					; start in DE, length in BC.

		LD	A,B		; test that it is not
		OR	C		; the null string.
		JR	NZ,o1767	; skip forward to OPEN-3 with 1 character
					; or more!

;; REPORT-Fb
o1765:		RST	08H		; ERROR-1
		DB	$0E		; Error Report: Invalid file name

;; OPEN-3
o1767:		PUSH	BC		; save the length of the string.
		LD	A,(DE)		; pick up the first character.
					; Note. There can be more than one character.
		AND	$DF		; make it upper-case.
		LD	C,A		; place it in C.
		LD	HL,o177A	; address: op-str-lu is loaded.
		CALL	o16DC		; routine INDEXER will search for letter.
		JR	NC,o1765	; back to REPORT-F if not found
					; 'Invalid filename'

		LD	C,(HL)		; fetch the displacement to opening routine.
		LD	B,$00		; prepare to add.
		ADD	HL,BC		; now form address of opening routine.
		POP	BC		; restore the length of string.
		JP	(HL)		; now jump forward to the relevant routine.

; -------------------------
; OPEN stream look-up table
; -------------------------
; The open stream look-up table consists of matched pairs.
; The channel letter is followed by an 8-bit displacement to the
; associated stream-opening routine in this ROM.
; The table requires a zero end-marker as the letter has been
; provided by the user and not the operating system.

;; op-str-lu
o177A:		DB	"K", o1781-$-1	; $06 offset to OPEN-K
		DB	"S", o1785-$-1	; $08 offset to OPEN-S
		DB	"P", o1789-$-1	; $0A offset to OPEN-P

		DB	$00		; end-marker.

; ----------------------------
; The Stream Opening Routines.
; ----------------------------
; These routines would have opened any buffers associated with the stream
; before jumping forward to OPEN-END with the displacement value in E
; and perhaps a modified value in BC. The strange pathing does seem to
; provide for flexibility in this respect.
;
; There is no need to open the printer buffer as it is there already
; even if you are still saving up for a ZX Printer or have moved onto
; something bigger. In any case it would have to be created after
; the system variables but apart from that it is a simple task
; and all but one of the ROM routines can handle a buffer in that position.
; (PR-ALL-6 would require an extra 3 bytes of code).
; However it wouldn't be wise to have two streams attached to the ZX Printer
; as you can now, so one assumes that if PR_CC_hi was non-zero then
; the OPEN-P routine would have refused to attach a stream if another
; stream was attached.

; Something of significance is being passed to these ghost routines in the
; second character. Strings 'RB', 'RT' perhaps or a drive/station number.
; The routine would have to deal with that and exit to OPEN_END with BC
; containing $0001 or more likely there would be an exit within the routine.
; Anyway doesn't matter, these routines are long gone.

; -----------------
; OPEN-K Subroutine
; -----------------
; Open Keyboard stream.

;; OPEN-K
o1781:		LD	E,$01		; 01 is offset to second byte of channel 'K'.
		JR	o178B		; forward to OPEN-END

; -----------------
; OPEN-S Subroutine
; -----------------
; Open Screen stream.

;; OPEN-S
o1785:		LD	E,$06		; 06 is offset to 2nd byte of channel 'S'
		JR	o178B		; to OPEN-END

; -----------------
; OPEN-P Subroutine
; -----------------
; Open Printer stream.

;; OPEN-P
o1789:		LD	E,$10		; 16d is offset to 2nd byte of channel 'P'

;; OPEN-END
o178B:		DEC	BC		; the stored length of 'K','S','P' or
					; whatever is now tested. ??
		LD	A,B		; test now if initial or residual length
		OR	C		; is one character.
		JR	NZ,o1765	; to REPORT-Fb 'Invalid file name' if not.

		LD	D,A		; load D with zero to form the displacement
					; in the DE register.
		POP	HL		; * restore the saved STRMS pointer.
		RET			; return to update STRMS entry thereby
					; signaling stream is open.

; ----------------------------------------
; Handle CAT, ERASE, FORMAT, MOVE commands
; ----------------------------------------
; These just generate an error report as the ROM is 'incomplete'.
;
; Luckily this provides a mechanism for extending these in a shadow ROM
; but without the powerful mechanisms set up in this ROM.
; An instruction fetch on $0008 may page in a peripheral ROM,
; e.g. the Sinclair Interface 1 ROM, to handle these commands.
; However that wasn't the plan.
; Development of this ROM continued for another three months until the cost
; of replacing it and the manual became unfeasible.
; The ultimate power of channels and streams died at birth.

;; CAT-ETC
o1793:		JR	o1725		; to REPORT-Ob

; -----------------
; Perform AUTO-LIST
; -----------------
; This produces an automatic listing in the upper screen.

;; AUTO-LIST
o1795:		LD	(LIST_SP),SP	; save stack pointer in LIST_SP
		LD	(IY+$02),$10	; update TV_FLAG set bit 3
		CALL	o0DAF		; routine CL-ALL.
		SET	0,(IY+$02)	; update TV_FLAG  - signal lower screen in use

		LD	B,(IY+$31)	; fetch DF_SZ to B.
		CALL	o0E44		; routine CL-LINE clears lower display
					; preserving B.
		RES	0,(IY+$02)	; update TV_FLAG  - signal main screen in use
		SET	0,(IY+$30)	; update FLAGS2 - signal will be necessary to
					; clear main screen.
		LD	HL,(E_PPC)	; fetch E_PPC current edit line to HL.
		LD	DE,(S_TOP)	; fetch S_TOP to DE, the current top line
					; (initially zero)
		AND	A		; prepare for true subtraction.
		SBC	HL,DE		; subtract and
		ADD	HL,DE		; add back.
		JR	C,o17E1		; to AUTO-L-2 if S_TOP higher than E_PPC
					; to set S_TOP to E_PPC

		PUSH	DE		; save the top line number.
		CALL	o196E		; routine LINE-ADDR gets address of E_PPC.
		LD	DE,$02C0	; prepare known number of characters in
					; the default upper screen.
		EX	DE,HL		; offset to HL, program address to DE.
		SBC	HL,DE		; subtract high value from low to obtain
					; negated result used in addition.
		EX	(SP),HL		; swap result with top line number on stack.
		CALL	o196E		; routine LINE-ADDR  gets address of that
					; top line in HL and next line in DE.
		POP	BC		; restore the result to balance stack.

;; AUTO-L-1
o17CE:		PUSH	BC		; save the result.
		CALL	o19B8		; routine NEXT-ONE gets address in HL of
					; line after auto-line (in DE).
		POP	BC		; restore result.
		ADD	HL,BC		; compute back.
		JR	C,o17E4		; to AUTO-L-3 if line 'should' appear

		EX	DE,HL		; address of next line to HL.
		LD	D,(HL)		; get line
		INC	HL		; number
		LD	E,(HL)		; in DE.
		DEC	HL		; adjust back to start.
		LD	(S_TOP),DE	; update S_TOP.
		JR	o17CE		; to AUTO-L-1 until estimate reached.

; ---

; the jump was to here if S_TOP was greater than E_PPC

;; AUTO-L-2
o17E1:		LD	(S_TOP),HL	; make S_TOP the same as E_PPC.

; continue here with valid starting point from above or good estimate
; from computation

;; AUTO-L-3
o17E4:		LD	HL,(S_TOP)	; fetch S_TOP line number to HL.
		CALL	o196E		; routine LINE-ADDR gets address in HL.
					; address of next in DE.
		JR	Z,o17ED		; to AUTO-L-4 if line exists.

		EX	DE,HL		; else use address of next line.

;; AUTO-L-4
o17ED:		CALL	o1833		; routine LIST-ALL                >>>

; The return will be to here if no scrolling occurred

		RES	4,(IY+$02)	; update TV_FLAG  - signal no auto listing.
		RET			; return.

; ------------
; Handle LLIST
; ------------
; A short form of LIST #3. The listing goes to stream 3 - default printer.

;; LLIST
o17F5:		LD	A,$03		; the usual stream for ZX Printer
		JR	o17FB		; forward to LIST-1

; -----------
; Handle LIST
; -----------
; List to any stream.
; Note. While a starting line can be specified it is
; not possible to specify an end line.
; Just listing a line makes it the current edit line.

;; LIST
o17F9:		LD	A,$02		; default is stream 2 - the upper screen.

;; LIST-1
o17FB:		LD	(IY+$02),$00	; the TV_FLAG is initialized with bit 0 reset
					; indicating upper screen in use.
		CALL	o2530		; routine SYNTAX-Z - checking syntax ?
		CALL	NZ,o1601	; routine CHAN-OPEN if in run-time.

		RST	18H		; GET-CHAR
		CALL	o2070		; routine STR-ALTER will alter if "#".
		JR	C,o181F		; forward to LIST-4 not a "#" .


		RST	18H		; GET-CHAR
		CP	$3B		; is it $3B ?
		JR	Z,o1814		; skip to LIST-2 if so.

		CP	$2C		; is it ',' ?
		JR	NZ,o181A	; forward to LIST-3 if neither separator.

; we have, say,  LIST #15, and a number must follow the separator.

;; LIST-2
o1814:		RST	20H		; NEXT-CHAR
		CALL	o1C82		; routine EXPT-1NUM
		JR	o1822		; forward to LIST-5

; ---

; the branch was here with just LIST #3 etc.

;; LIST-3
o181A:		CALL	o1CE6		; routine USE-ZERO
		JR	o1822		; forward to LIST-5

; ---

; the branch was here with LIST

;; LIST-4
o181F:		CALL	o1CDE		; routine FETCH-NUM checks if a number
					; follows else uses zero.

;; LIST-5
o1822:		CALL	o1BEE		; routine CHECK-END quits if syntax OK >>>

		CALL	o1E99		; routine FIND-INT2 fetches the number
					; from the calculator stack in run-time.
		LD	A,B		; fetch high byte of line number and
		AND	$3F		; make less than $40 so that NEXT-ONE
					; (from LINE-ADDR) doesn't lose context.
					; Note. this is not satisfactory and the typo
					; LIST 20000 will list an entirely different
					; section than LIST 2000. Such typos are not
					; available for checking if they are direct
					; commands.

		LD	H,A		; transfer the modified
		LD	L,C		; line number to HL.
		LD	(E_PPC),HL	; update E_PPC to new line number.
		CALL	o196E		; routine LINE-ADDR gets the address of the
					; line.

; This routine is called from AUTO-LIST

;; LIST-ALL
o1833:		LD	E,$01		; signal current line not yet printed

;; LIST-ALL-2
o1835:		CALL	o1855		; routine OUT-LINE outputs a BASIC line
					; using PRINT-OUT and makes an early return
					; when no more lines to print. >>>

		RST	10H		; PRINT-A prints the carriage return (in A)

		BIT	4,(IY+$02)	; test TV_FLAG  - automatic listing ?
		JR	Z,o1835		; back to LIST-ALL-2 if not
					; (loop exit is via OUT-LINE)

; continue here if an automatic listing required.

		LD	A,(DF_SZ)	; fetch DF_SZ lower display file size.
		SUB	(IY+$4F)	; subtract S_POSN_hi ithe current line number.
		JR	NZ,o1835	; back to LIST-ALL-2 if upper screen not full.

		XOR	E		; A contains zero, E contains one if the
					; current edit line has not been printed
					; or zero if it has (from OUT-LINE).
		RET	Z		; return if the screen is full and the line
					; has been printed.

; continue with automatic listings if the screen is full and the current
; edit line is missing. OUT-LINE will scroll automatically.

		PUSH	HL		; save the pointer address.
		PUSH	DE		; save the E flag.
		LD	HL,S_TOP	; fetch S_TOP the rough estimate.
		CALL	o190F		; routine LN-FETCH updates S_TOP with
					; the number of the next line.
		POP	DE		; restore the E flag.
		POP	HL		; restore the address of the next line.
		JR	o1835		; back to LIST-ALL-2.

; ------------------------
; Print a whole BASIC line
; ------------------------
; This routine prints a whole BASIC line and it is called
; from LIST-ALL to output the line to current channel
; and from ED-EDIT to 'sprint' the line to the edit buffer.

;; OUT-LINE
o1855:		LD	BC,(E_PPC)	; fetch E_PPC the current line which may be
					; unchecked and not exist.
		CALL	o1980		; routine CP-LINES finds match or line after.
		LD	D,$3E		; prepare cursor ">" in D.
		JR	Z,o1865		; to OUT-LINE1 if matched or line after.

		LD	DE,$0000	; put zero in D, to suppress line cursor.
		RL	E		; pick up carry in E if line before current
					; leave E zero if same or after.

;; OUT-LINE1
o1865:		LD	(IY+$2D),E	; save flag in BREG which is spare.
		LD	A,(HL)		; get high byte of line number.
		CP	$40		; is it too high ($2F is maximum possible) ?
		POP	BC		; drop the return address and
		RET	NC		; make an early return if so >>>

		PUSH	BC		; save return address
		CALL	o1A28		; routine OUT-NUM-2 to print addressed number
					; with leading space.
		INC	HL		; skip low number byte.
		INC	HL		; and the two
		INC	HL		; length bytes.
		RES	0,(IY+$01)	; update FLAGS - signal leading space required.
		LD	A,D		; fetch the cursor.
		AND	A		; test for zero.
		JR	Z,o1881		; to OUT-LINE3 if zero.


		RST	10H		; PRINT-A prints ">" the current line cursor.

; this entry point is called from ED-COPY

;; OUT-LINE2
o187D:		SET	0,(IY+$01)	; update FLAGS - suppress leading space.

;; OUT-LINE3
o1881:		PUSH	DE		; save flag E for a return value.
		EX	DE,HL		; save HL address in DE.
		RES	2,(IY+$30)	; update FLAGS2 - signal NOT in QUOTES.

		LD	HL,FLAGS	; point to FLAGS.
		RES	2,(HL)		; signal 'K' mode. (starts before keyword)
		BIT	5,(IY+$37)	; test FLAGX - input mode ?
		JR	Z,o1894		; forward to OUT-LINE4 if not.

		SET	2,(HL)		; signal 'L' mode. (used for input)

;; OUT-LINE4
o1894:		LD	HL,(X_PTR)	; fetch X_PTR - possibly the error pointer
					; address.
		AND	A		; clear the carry flag.
		SBC	HL,DE		; test if an error address has been reached.
		JR	NZ,o18A1	; forward to OUT-LINE5 if not.

		LD	A,$3F		; load A with "?" the error marker.
		CALL	o18C1		; routine OUT-FLASH to print flashing marker.

;; OUT-LINE5
o18A1:		CALL	o18E1		; routine OUT-CURS will print the cursor if
					; this is the right position.
		EX	DE,HL		; restore address pointer to HL.
		LD	A,(HL)		; fetch the addressed character.
		CALL	o18B6		; routine NUMBER skips a hidden floating
					; point number if present.
		INC	HL		; now increment the pointer.
		CP	$0D		; is character end-of-line ?
		JR	Z,o18B4		; to OUT-LINE6, if so, as line is finished.

		EX	DE,HL		; save the pointer in DE.
		CALL	o1937		; routine OUT-CHAR to output character/token.

		JR	o1894		; back to OUT-LINE4 until entire line is done.

; ---

;; OUT-LINE6
o18B4:		POP	DE		; bring back the flag E, zero if current
					; line printed else 1 if still to print.
		RET			; return with A holding $0D

; -------------------------
; Check for a number marker
; -------------------------
; this subroutine is called from two processes. while outputting BASIC lines
; and while searching statements within a BASIC line.
; during both, this routine will pass over an invisible number indicator
; and the five bytes floating-point number that follows it.
; Note that this causes floating point numbers to be stripped from
; the BASIC line when it is fetched to the edit buffer by OUT_LINE.
; the number marker also appears after the arguments of a DEF FN statement
; and may mask old 5-byte string parameters.

;; NUMBER
o18B6:		CP	$0E		; character fourteen ?
		RET	NZ		; return if not.

		INC	HL		; skip the character
		INC	HL		; and five bytes
		INC	HL		; following.
		INC	HL		;
		INC	HL		;
		INC	HL		;
		LD	A,(HL)		; fetch the following character
		RET			; for return value.

; --------------------------
; Print a flashing character
; --------------------------
; This subroutine is called from OUT-LINE to print a flashing error
; marker "?" or from the next routine to print a flashing cursor e.g. 'L'.
; However, this only gets called from OUT-LINE when printing the edit line
; or the input buffer to the lower screen so a direct call to $09F4 can
; be used, even though out-line outputs to other streams.
; In fact the alternate set is used for the whole routine.

;; OUT-FLASH
o18C1:		EXX

		LD	HL,(ATTR_T)	; fetch L = ATTR_T, H = MASK-T
		PUSH	HL		; save masks.
		RES	7,H		; reset flash mask bit so active.
		SET	7,L		; make attribute FLASH.
		LD	(ATTR_T),HL	; resave ATTR_T and MASK-T

		LD	HL,P_FLAG	; address P_FLAG
		LD	D,(HL)		; fetch to D
		PUSH	DE		; and save.
		LD	(HL),$00	; clear inverse, over, ink/paper 9

		CALL	o09F4		; routine PRINT-OUT outputs character
					; without the need to vector via RST 10.

		POP	HL		; pop P_FLAG to H.
		LD	(IY+$57),H	; and restore system variable P_FLAG.
		POP	HL		; restore temporary masks
		LD	(ATTR_T),HL	; and restore system variables ATTR_T/MASK_T

		EXX			; switch back to main set
		RET			; return

; ----------------
; Print the cursor
; ----------------
; This routine is called before any character is output while outputting
; a BASIC line or the input buffer. This includes listing to a printer
; or screen, copying a BASIC line to the edit buffer and printing the
; input buffer or edit buffer to the lower screen. It is only in the
; latter two cases that it has any relevance and in the last case it
; performs another very important function also.

;; OUT-CURS
o18E1:		LD	HL,(K_CUR)	; fetch K_CUR the current cursor address
		AND	A		; prepare for true subtraction.
		SBC	HL,DE		; test against pointer address in DE and
		RET	NZ		; return if not at exact position.

; the value of MODE, maintained by KEY-INPUT, is tested and if non-zero
; then this value 'E' or 'G' will take precedence.

		LD	A,(MODE)	; fetch MODE  0='KLC', 1='E', 2='G'.
		RLC	A		; double the value and set flags.
		JR	Z,o18F3		; to OUT-C-1 if still zero ('KLC').

		ADD	A,$43		; add 'C' - will become 'E' if originally 1
					; or 'G' if originally 2.
		JR	o1909		; forward to OUT-C-2 to print.

; ---

; If mode was zero then, while printing a BASIC line, bit 2 of flags has been
; set if 'THEN' or ":" was encountered as a main character and reset otherwise.
; This is now used to determine if the 'K' cursor is to be printed but this
; transient state is also now transferred permanently to bit 3 of FLAGS
; to let the interrupt routine know how to decode the next key.

;; OUT-C-1
o18F3:		LD	HL,FLAGS	; Address FLAGS
		RES	3,(HL)		; signal 'K' mode initially.
		LD	A,$4B		; prepare letter 'K'.
		BIT	2,(HL)		; test FLAGS - was the
					; previous main character ":" or 'THEN' ?
		JR	Z,o1909		; forward to OUT-C-2 if so to print.

		SET	3,(HL)		; signal 'L' mode to interrupt routine.
					; Note. transient bit has been made permanent.
		INC	A		; augment from 'K' to 'L'.

		BIT	3,(IY+$30)	; test FLAGS2 - consider caps lock ?
					; which is maintained by KEY-INPUT.
		JR	Z,o1909		; forward to OUT-C-2 if not set to print.

		LD	A,$43		; alter 'L' to 'C'.

;; OUT-C-2
o1909:		PUSH	DE		; save address pointer but OK as OUT-FLASH
					; uses alternate set without RST 10.

		CALL	o18C1		; routine OUT-FLASH to print.

		POP	DE		; restore and
		RET			; return.

; ----------------------------
; Get line number of next line
; ----------------------------
; These two subroutines are called while editing.
; This entry point is from ED-DOWN with HL addressing E_PPC
; to fetch the next line number.
; Also from AUTO-LIST with HL addressing S_TOP just to update S_TOP
; with the value of the next line number. It gets fetched but is discarded.
; These routines never get called while the editor is being used for input.

;; LN-FETCH
o190F:		LD	E,(HL)		; fetch low byte
		INC	HL		; address next
		LD	D,(HL)		; fetch high byte.
		PUSH	HL		; save system variable hi pointer.
		EX	DE,HL		; line number to HL,
		INC	HL		; increment as a starting point.
		CALL	o196E		; routine LINE-ADDR gets address in HL.
		CALL	o1695		; routine LINE-NO gets line number in DE.
		POP	HL		; restore system variable hi pointer.

; This entry point is from the ED-UP with HL addressing E_PPC_hi

;; LN-STORE
o191C:		BIT	5,(IY+$37)	; test FLAGX - input mode ?
		RET	NZ		; return if so.
					; Note. above already checked by ED-UP/ED-DOWN.

		LD	(HL),D		; save high byte of line number.
		DEC	HL		; address lower
		LD	(HL),E		; save low byte of line number.
		RET			; return.

; -----------------------------------------
; Outputting numbers at start of BASIC line
; -----------------------------------------
; This routine entered at OUT-SP-NO is used to compute then output the first
; three digits of a 4-digit BASIC line printing a space if necessary.
; The line number, or residual part, is held in HL and the BC register
; holds a subtraction value -1000, -100 or -10.
; Note. for example line number 200 -
; space(out_char), 2(out_code), 0(out_char) final number always out-code.

;; OUT-SP-2
o1925:		LD	A,E		; will be space if OUT-CODE not yet called.
					; or $FF if spaces are suppressed.
					; else $30 ("0").
					; (from the first instruction at OUT-CODE)
					; this guy is just too clever.
		AND	A		; test bit 7 of A.
		RET	M		; return if $FF, as leading spaces not
					; required. This is set when printing line
					; number and statement in MAIN-5.

		JR	o1937		; forward to exit via OUT-CHAR.

; ---

; -> the single entry point.

;; OUT-SP-NO
o192A:		XOR	A		; initialize digit to 0

;; OUT-SP-1
o192B:		ADD	HL,BC		; add negative number to HL.
		INC	A		; increment digit
		JR	C,o192B		; back to OUT-SP-1 until no carry from
					; the addition.

		SBC	HL,BC		; cancel the last addition
		DEC	A		; and decrement the digit.
		JR	Z,o1925		; back to OUT-SP-2 if it is zero.

		JP	o15EF		; jump back to exit via OUT-CODE.    ->


; -------------------------------------
; Outputting characters in a BASIC line
; -------------------------------------
; This subroutine ...

;; OUT-CHAR
o1937:		CALL	o2D1B		; routine NUMERIC tests if it is a digit ?
		JR	NC,o196C	; to OUT-CH-3 to print digit without
					; changing mode. Will be 'K' mode if digits
					; are at beginning of edit line.

		CP	$21		; less than quote character ?
		JR	C,o196C		; to OUT-CH-3 to output controls and space.

		RES	2,(IY+$01)	; initialize FLAGS to 'K' mode and leave
					; unchanged if this character would precede
					; a keyword.

		CP	$CB		; is character 'THEN' token ?
		JR	Z,o196C		; to OUT-CH-3 to output if so.

		CP	$3A		; is it ":" ?
		JR	NZ,o195A	; to OUT-CH-1 if not statement separator
					; to change mode back to 'L'.

		BIT	5,(IY+$37)	; FLAGX  - Input Mode ??
		JR	NZ,o1968	; to OUT-CH-2 if in input as no statements.
					; Note. this check should seemingly be at
					; the start. Commands seem inappropriate in
					; INPUT mode and are rejected by the syntax
					; checker anyway.
					; unless INPUT LINE is being used.

		BIT	2,(IY+$30)	; test FLAGS2 - is the ":" within quotes ?
		JR	Z,o196C		; to OUT-CH-3 if ":" is outside quoted text.

		JR	o1968		; to OUT-CH-2 as ":" is within quotes

; ---

;; OUT-CH-1
o195A:		CP	$22		; is it quote character '"'  ?
		JR	NZ,o1968	; to OUT-CH-2 with others to set 'L' mode.

		PUSH	AF		; save character.
		LD	A,(FLAGS2)	; fetch FLAGS2.
		XOR	$04		; toggle the quotes flag.
		LD	(FLAGS2),A	; update FLAGS2
		POP	AF		; and restore character.

;; OUT-CH-2
o1968:		SET	2,(IY+$01)	; update FLAGS - signal L mode if the cursor
					; is next.

;; OUT-CH-3
o196C:		RST	10H		; PRINT-A vectors the character to
					; channel 'S', 'K', 'R' or 'P'.
		RET			; return.

; -------------------------------------------
; Get starting address of line, or line after
; -------------------------------------------
; This routine is used often to get the address, in HL, of a BASIC line
; number supplied in HL, or failing that the address of the following line
; and the address of the previous line in DE.

;; LINE-ADDR
o196E:		PUSH	HL		; save line number in HL register
		LD	HL,(PROG)	; fetch start of program from PROG
		LD	D,H		; transfer address to
		LD	E,L		; the DE register pair.

;; LINE-AD-1
o1974:		POP	BC		; restore the line number to BC
		CALL	o1980		; routine CP-LINES compares with that
					; addressed by HL
		RET	NC		; return if line has been passed or matched.
					; if NZ, address of previous is in DE

		PUSH	BC		; save the current line number
		CALL	o19B8		; routine NEXT-ONE finds address of next
					; line number in DE, previous in HL.
		EX	DE,HL		; switch so next in HL
		JR	o1974		; back to LINE-AD-1 for another comparison

; --------------------
; Compare line numbers
; --------------------
; This routine compares a line number supplied in BC with an addressed
; line number pointed to by HL.

;; CP-LINES
o1980:		LD	A,(HL)		; Load the high byte of line number and
		CP	B		; compare with that of supplied line number.
		RET	NZ		; return if yet to match (carry will be set).

		INC	HL		; address low byte of
		LD	A,(HL)		; number and pick up in A.
		DEC	HL		; step back to first position.
		CP	C		; now compare.
		RET			; zero set if exact match.
					; carry set if yet to match.
					; no carry indicates a match or
					; next available BASIC line or
					; program end marker.

; -------------------
; Find each statement
; -------------------
; The single entry point EACH-STMT is used to
; 1) To find the D'th statement in a line.
; 2) To find a token in held E.

;; not-used
o1988:		INC	HL		;
		INC	HL		;
		INC	HL		;

; -> entry point.

;; EACH-STMT
o198B:		LD	(CH_ADD),HL	; save HL in CH_ADD
		LD	C,$00		; initialize quotes flag

;; EACH-S-1
o1990:		DEC	D		; decrease statement count
		RET	Z		; return if zero


		RST	20H		; NEXT-CHAR
		CP	E		; is it the search token ?
		JR	NZ,o199A	; forward to EACH-S-3 if not

		AND	A		; clear carry
		RET			; return signalling success.

; ---

;; EACH-S-2
o1998:		INC	HL		; next address
		LD	A,(HL)		; next character

;; EACH-S-3
o199A:		CALL	o18B6		; routine NUMBER skips if number marker
		LD	(CH_ADD),HL	; save in CH_ADD
		CP	$22		; is it quotes '"' ?
		JR	NZ,o19A5	; to EACH-S-4 if not

		DEC	C		; toggle bit 0 of C

;; EACH-S-4
o19A5:		CP	$3A		; is it ":"
		JR	Z,o19AD		; to EACH-S-5

		CP	$CB		; 'THEN'
		JR	NZ,o19B1	; to EACH-S-6

;; EACH-S-5
o19AD:		BIT	0,C		; is it in quotes
		JR	Z,o1990		; to EACH-S-1 if not

;; EACH-S-6
o19B1:		CP	$0D		; end of line ?
		JR	NZ,o1998	; to EACH-S-2

		DEC	D		; decrease the statement counter
					; which should be zero else
					; 'Statement Lost'.
		SCF			; set carry flag - not found
		RET			; return

; -----------------------------------------------------------------------
; Storage of variables. For full details - see chapter 24.
; ZX Spectrum BASIC Programming by Steven Vickers 1982.
; It is bits 7-5 of the first character of a variable that allow
; the six types to be distinguished. Bits 4-0 are the reduced letter.
; So any variable name is higher that $3F and can be distinguished
; also from the variables area end-marker $80.
;
; 76543210 meaning                               brief outline of format.
; -------- ------------------------              -----------------------
; 010      string variable.                      2 byte length + contents.
; 110      string array.                         2 byte length + contents.
; 100      array of numbers.                     2 byte length + contents.
; 011      simple numeric variable.              5 bytes.
; 101      variable length named numeric.        5 bytes.
; 111      for-next loop variable.               18 bytes.
; 10000000 the variables area end-marker.
;
; Note. any of the above seven will serve as a program end-marker.
;
; -----------------------------------------------------------------------

; ------------
; Get next one
; ------------
; This versatile routine is used to find the address of the next line
; in the program area or the next variable in the variables area.
; The reason one routine is made to handle two apparently unrelated tasks
; is that it can be called indiscriminately when merging a line or a
; variable.

;; NEXT-ONE
o19B8:		PUSH	HL		; save the pointer address.
		LD	A,(HL)		; get first byte.
		CP	$40		; compare with upper limit for line numbers.
		JR	C,o19D5		; forward to NEXT-O-3 if within BASIC area.

; the continuation here is for the next variable unless the supplied
; line number was erroneously over 16383. see RESTORE command.

		BIT	5,A		; is it a string or an array variable ?
		JR	Z,o19D6		; forward to NEXT-O-4 to compute length.

		ADD	A,A		; test bit 6 for single-character variables.
		JP	M,o19C7		; forward to NEXT-O-1 if so

		CCF			; clear the carry for long-named variables.
					; it remains set for for-next loop variables.

;; NEXT-O-1
o19C7:		LD	BC,$0005	; set BC to 5 for floating point number
		JR	NC,o19CE	; forward to NEXT-O-2 if not a for/next
					; variable.

		LD	C,$12		; set BC to eighteen locations.
					; value, limit, step, line and statement.

; now deal with long-named variables

;; NEXT-O-2
o19CE:		RLA
					; be set for single character variables
		INC	HL		; address next location.
		LD	A,(HL)		; and load character.
		JR	NC,o19CE	; back to NEXT-O-2 if not inverted bit.
					; forward immediately with single character
					; variable names.

		JR	o19DB		; forward to NEXT-O-5 to add length of
					; floating point number(s etc.).

; ---

; this branch is for line numbers.

;; NEXT-O-3
o19D5:		INC	HL		; increment pointer to low byte of line no.

; strings and arrays rejoin here

;; NEXT-O-4
o19D6:		INC	HL		; increment to address the length low byte.
		LD	C,(HL)		; transfer to C and
		INC	HL		; point to high byte of length.
		LD	B,(HL)		; transfer that to B
		INC	HL		; point to start of BASIC/variable contents.

; the three types of numeric variables rejoin here

;; NEXT-O-5
o19DB:		ADD	HL,BC		; add the length to give address of next
					; line/variable in HL.
		POP	DE		; restore previous address to DE.

; ------------------
; Difference routine
; ------------------
; This routine terminates the above routine and is also called from the
; start of the next routine to calculate the length to reclaim.

;; DIFFER
o19DD:		AND	A		; prepare for true subtraction.
		SBC	HL,DE		; subtract the two pointers.
		LD	B,H		; transfer result
		LD	C,L		; to BC register pair.
		ADD	HL,DE		; add back
		EX	DE,HL		; and switch pointers
		RET			; return values are the length of area in BC,
					; low pointer (previous) in HL,
					; high pointer (next) in DE.

; -----------------------
; Handle reclaiming space
; -----------------------
;

;; RECLAIM-1
o19E5:		CALL	o19DD		; routine DIFFER immediately above

;; RECLAIM-2
o19E8:		PUSH	BC		;

		LD	A,B		;
		CPL			;
		LD	B,A		;
		LD	A,C		;
		CPL			;
		LD	C,A		;
		INC	BC		;

		CALL	o1664		; routine POINTERS
		EX	DE,HL		;
		POP	HL		;

		ADD	HL,DE		;
		PUSH	DE		;
		LDIR			; copy bytes

		POP	HL		;
		RET			;

; ----------------------------------------
; Read line number of line in editing area
; ----------------------------------------
; This routine reads a line number in the editing area returning the number
; in the BC register or zero if no digits exist before commands.
; It is called from LINE-SCAN to check the syntax of the digits.
; It is called from MAIN-3 to extract the line number in preparation for
; inclusion of the line in the BASIC program area.
;
; Interestingly the calculator stack is moved from its normal place at the
; end of dynamic memory to an adequate area within the system variables area.
; This ensures that in a low memory situation, that valid line numbers can
; be extracted without raising an error and that memory can be reclaimed
; by deleting lines. If the stack was in its normal place then a situation
; arises whereby the Spectrum becomes locked with no means of reclaiming space.

;; E-LINE-NO
o19FB:		LD	HL,(E_LINE)	; load HL from system variable E_LINE.

		DEC	HL		; decrease so that NEXT_CHAR can be used
					; without skipping the first digit.

		LD	(CH_ADD),HL	; store in the system variable CH_ADD.

		RST	20H		; NEXT-CHAR skips any noise and white-space
					; to point exactly at the first digit.

		LD	HL,MEMBOT	; use MEM-0 as a temporary calculator stack
					; an overhead of three locations are needed.
		LD	(STKEND),HL	; set new STKEND.

		CALL	o2D3B		; routine INT-TO-FP will read digits till
					; a non-digit found.
		CALL	o2DA2		; routine FP-TO-BC will retrieve number
					; from stack at membot.
		JR	C,o1A15		; forward to E-L-1 if overflow i.e. > 65535.
					; 'Nonsense in BASIC'

		LD	HL,$D8F0	; load HL with value -9999
		ADD	HL,BC		; add to line number in BC

;; E-L-1
o1A15:		JP	C,o1C8A		; to REPORT-C 'Nonsense in BASIC' if over.
					; Note. As ERR_SP points to ED_ERROR
					; the report is never produced although
					; the RST $08 will update X_PTR leading to
					; the error marker being displayed when
					; the ED_LOOP is reiterated.
					; in fact, since it is immediately
					; cancelled, any report will do.

; a line in the range 0 - 9999 has been entered.

		JP	o16C5		; jump back to SET-STK to set the calculator
					; stack back to its normal place and exit
					; from there.

; ---------------------------------
; Report and line number outputting
; ---------------------------------
; Entry point OUT-NUM-1 is used by the Error Reporting code to print
; the line number and later the statement number held in BC.
; If the statement was part of a direct command then -2 is used as a
; dummy line number so that zero will be printed in the report.
; This routine is also used to print the exponent of E-format numbers.
;
; Entry point OUT-NUM-2 is used from OUT-LINE to output the line number
; addressed by HL with leading spaces if necessary.

;; OUT-NUM-1
o1A1B:		PUSH	DE		; save the
		PUSH	HL		; registers.
		XOR	A		; set A to zero.
		BIT	7,B		; is the line number minus two ?
		JR	NZ,o1A42	; forward to OUT-NUM-4 if so to print zero
					; for a direct command.

		LD	H,B		; transfer the
		LD	L,C		; number to HL.
		LD	E,$FF		; signal 'no leading zeros'.
		JR	o1A30		; forward to continue at OUT-NUM-3

; ---

; from OUT-LINE - HL addresses line number.

;; OUT-NUM-2
o1A28:		PUSH	DE		; save flags
		LD	D,(HL)		; high byte to D
		INC	HL		; address next
		LD	E,(HL)		; low byte to E
		PUSH	HL		; save pointer
		EX	DE,HL		; transfer number to HL
		LD	E,$20		; signal 'output leading spaces'

;; OUT-NUM-3
o1A30:		LD	BC,$FC18	; value -1000
		CALL	o192A		; routine OUT-SP-NO outputs space or number
		LD	BC,$FF9C	; value -100
		CALL	o192A		; routine OUT-SP-NO
		LD	C,$F6		; value -10 ( B is still $FF )
		CALL	o192A		; routine OUT-SP-NO
		LD	A,L		; remainder to A.

;; OUT-NUM-4
o1A42:		CALL	o15EF		; routine OUT-CODE for final digit.
					; else report code zero wouldn't get
					; printed.
		POP	HL		; restore the
		POP	DE		; registers and
		RET			; return.


;***************************************************
;** Part 7. BASIC LINE AND COMMAND INTERPRETATION **
;***************************************************

; ----------------
; The offset table
; ----------------
; The BASIC interpreter has found a command code $CE - $FF
; which is then reduced to range $00 - $31 and added to the base address
; of this table to give the address of an offset which, when added to
; the offset therein, gives the location in the following parameter table
; where a list of class codes, separators and addresses relevant to the
; command exists.

;; offst-tbl
o1A48:		DB	o1AF9-$		; B1 offset to Address: P-DEF-FN
		DB	o1B14-$		; CB offset to Address: P-CAT
		DB	o1B06-$		; BC offset to Address: P-FORMAT
		DB	o1B0A-$		; BF offset to Address: P-MOVE
		DB	o1B10-$		; C4 offset to Address: P-ERASE
		DB	o1AFC-$		; AF offset to Address: P-OPEN
		DB	o1B02-$		; B4 offset to Address: P-CLOSE
		DB	o1AE2-$		; 93 offset to Address: P-MERGE
		DB	o1AE1-$		; 91 offset to Address: P-VERIFY
		DB	o1AE3-$		; 92 offset to Address: P-BEEP
		DB	o1AE7-$		; 95 offset to Address: P-CIRCLE
		DB	o1AEB-$		; 98 offset to Address: P-INK
		DB	o1AEC-$		; 98 offset to Address: P-PAPER
		DB	o1AED-$		; 98 offset to Address: P-FLASH
		DB	o1AEE-$		; 98 offset to Address: P-BRIGHT
		DB	o1AEF-$		; 98 offset to Address: P-INVERSE
		DB	o1AF0-$		; 98 offset to Address: P-OVER
		DB	o1AF1-$		; 98 offset to Address: P-OUT
		DB	o1AD9-$		; 7F offset to Address: P-LPRINT
		DB	o1ADC-$		; 81 offset to Address: P-LLIST
		DB	o1A8A-$		; 2E offset to Address: P-STOP
		DB	o1AC9-$		; 6C offset to Address: P-READ
		DB	o1ACC-$		; 6E offset to Address: P-DATA
		DB	o1ACF-$		; 70 offset to Address: P-RESTORE
		DB	o1AA8-$		; 48 offset to Address: P-NEW
		DB	o1AF5-$		; 94 offset to Address: P-BORDER
		DB	o1AB8-$		; 56 offset to Address: P-CONT
		DB	o1AA2-$		; 3F offset to Address: P-DIM
		DB	o1AA5-$		; 41 offset to Address: P-REM
		DB	o1A90-$		; 2B offset to Address: P-FOR
		DB	o1A7D-$		; 17 offset to Address: P-GO-TO
		DB	o1A86-$		; 1F offset to Address: P-GO-SUB
		DB	o1A9F-$		; 37 offset to Address: P-INPUT
		DB	o1AE0-$		; 77 offset to Address: P-LOAD
		DB	o1AAE-$		; 44 offset to Address: P-LIST
		DB	o1A7A-$		; 0F offset to Address: P-LET
		DB	o1AC5-$		; 59 offset to Address: P-PAUSE
		DB	o1A98-$		; 2B offset to Address: P-NEXT
		DB	o1AB1-$		; 43 offset to Address: P-POKE
		DB	o1A9C-$		; 2D offset to Address: P-PRINT
		DB	o1AC1-$		; 51 offset to Address: P-PLOT
		DB	o1AAB-$		; 3A offset to Address: P-RUN
		DB	o1ADF-$		; 6D offset to Address: P-SAVE
		DB	o1AB5-$		; 42 offset to Address: P-RANDOM
		DB	o1A81-$		; 0D offset to Address: P-IF
		DB	o1ABE-$		; 49 offset to Address: P-CLS
		DB	o1AD2-$		; 5C offset to Address: P-DRAW
		DB	o1ABB-$		; 44 offset to Address: P-CLEAR
		DB	o1A8D-$		; 15 offset to Address: P-RETURN
		DB	o1AD6-$		; 5D offset to Address: P-COPY


; -------------------------------
; The parameter or "Syntax" table
; -------------------------------
; For each command there exists a variable list of parameters.
; If the character is greater than a space it is a required separator.
; If less, then it is a command class in the range 00 - 0B.
; Note that classes 00, 03 and 05 will fetch the addresses from this table.
; Some classes e.g. 07 and 0B have the same address in all invocations
; and the command is re-computed from the low-byte of the parameter address.
; Some e.g. 02 are only called once so a call to the command is made from
; within the class routine rather than holding the address within the table.
; Some class routines check syntax entirely and some leave this task for the
; command itself.
; Others for example CIRCLE (x,y,z) check the first part (x,y) using the
; class routine and the final part (,z) within the command.
; The last few commands appear to have been added in a rush but their syntax
; is rather simple e.g. MOVE "M1","M2"

;; P-LET
o1A7A:		DB	$01		; Class-01 - A variable is required.
		DB	$3D		; Separator:  "="
		DB	$02		; Class-02 - An expression, numeric or string,
					; must follow.

;; P-GO-TO
o1A7D:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1E67		; Address: $1E67; Address: GO-TO

;; P-IF
o1A81:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$CB		; Separator:  'THEN'
		DB	$05		; Class-05 - Variable syntax checked
					; by routine.
		DW	o1CF0		; Address: $1CF0; Address: IF

;; P-GO-SUB
o1A86:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1EED		; Address: $1EED; Address: GO-SUB

;; P-STOP
o1A8A:		DB	$00		; Class-00 - No further operands.
		DW	o1CEE		; Address: $1CEE; Address: STOP

;; P-RETURN
o1A8D:		DB	$00		; Class-00 - No further operands.
		DW	o1F23		; Address: $1F23; Address: RETURN

;; P-FOR
o1A90:		DB	$04		; Class-04 - A single character variable must
					; follow.
		DB	$3D		; Separator:  "="
		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$CC		; Separator:  'TO'
		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$05		; Class-05 - Variable syntax checked
					; by routine.
		DW	o1D03		; Address: $1D03; Address: FOR

;; P-NEXT
o1A98:		DB	$04		; Class-04 - A single character variable must
					; follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1DAB		; Address: $1DAB; Address: NEXT

;; P-PRINT
o1A9C:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o1FCD		; Address: $1FCD; Address: PRINT

;; P-INPUT
o1A9F:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o2089		; Address: $2089; Address: INPUT

;; P-DIM
o1AA2:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o2C02		; Address: $2C02; Address: DIM

;; P-REM
o1AA5:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o1BB2		; Address: $1BB2; Address: REM

;; P-NEW
o1AA8:		DB	$00		; Class-00 - No further operands.
		DW	o11B7		; Address: $11B7; Address: NEW

;; P-RUN
o1AAB:		DB	$03		; Class-03 - A numeric expression may follow
					; else default to zero.
		DW	o1EA1		; Address: $1EA1; Address: RUN

;; P-LIST
o1AAE:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o17F9		; Address: $17F9; Address: LIST

;; P-POKE
o1AB1:		DB	$08		; Class-08 - Two comma-separated numeric
					; expressions required.
		DB	$00		; Class-00 - No further operands.
		DW	o1E80		; Address: $1E80; Address: POKE

;; P-RANDOM
o1AB5:		DB	$03		; Class-03 - A numeric expression may follow
					; else default to zero.
		DW	o1E4F		; Address: $1E4F; Address: RANDOMIZE

;; P-CONT
o1AB8:		DB	$00		; Class-00 - No further operands.
		DW	o1E5F		; Address: $1E5F; Address: CONTINUE

;; P-CLEAR
o1ABB:		DB	$03		; Class-03 - A numeric expression may follow
					; else default to zero.
		DW	o1EAC		; Address: $1EAC; Address: CLEAR

;; P-CLS
o1ABE:		DB	$00		; Class-00 - No further operands.
		DW	o0D6B		; Address: $0D6B; Address: CLS

;; P-PLOT
o1AC1:		DB	$09		; Class-09 - Two comma-separated numeric
					; expressions required with optional colour
					; items.
		DB	$00		; Class-00 - No further operands.
		DW	o22DC		; Address: $22DC; Address: PLOT

;; P-PAUSE
o1AC5:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1F3A		; Address: $1F3A; Address: PAUSE

;; P-READ
o1AC9:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o1DED		; Address: $1DED; Address: READ

;; P-DATA
o1ACC:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o1E27		; Address: $1E27; Address: DATA

;; P-RESTORE
o1ACF:		DB	$03		; Class-03 - A numeric expression may follow
					; else default to zero.
		DW	o1E42		; Address: $1E42; Address: RESTORE

;; P-DRAW
o1AD2:		DB	$09		; Class-09 - Two comma-separated numeric
					; expressions required with optional colour
					; items.
		DB	$05		; Class-05 - Variable syntax checked
					; by routine.
		DW	o2382		; Address: $2382; Address: DRAW

;; P-COPY
o1AD6:		DB	$00		; Class-00 - No further operands.
		DW	o0EAC		; Address: $0EAC; Address: COPY

;; P-LPRINT
o1AD9:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o1FC9		; Address: $1FC9; Address: LPRINT

;; P-LLIST
o1ADC:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o17F5		; Address: $17F5; Address: LLIST

;; P-SAVE
o1ADF:		DB	$0B		; Class-0B - Offset address converted to tape
					; command.

;; P-LOAD
o1AE0:		DB	$0B		; Class-0B - Offset address converted to tape
					; command.

;; P-VERIFY
o1AE1:		DB	$0B		; Class-0B - Offset address converted to tape
					; command.

;; P-MERGE
o1AE2:		DB	$0B		; Class-0B - Offset address converted to tape
					; command.

;; P-BEEP
o1AE3:		DB	$08		; Class-08 - Two comma-separated numeric
					; expressions required.
		DB	$00		; Class-00 - No further operands.
		DW	o03F8		; Address: $03F8; Address: BEEP

;; P-CIRCLE
o1AE7:		DB	$09		; Class-09 - Two comma-separated numeric
					; expressions required with optional colour
					; items.
		DB	$05		; Class-05 - Variable syntax checked
					; by routine.
		DW	o2320		; Address: $2320; Address: CIRCLE

;; P-INK
o1AEB:		DB	$07		; Class-07 - Offset address is converted to
					; colour code.

;; P-PAPER
o1AEC:		DB	$07		; Class-07 - Offset address is converted to
					; colour code.

;; P-FLASH
o1AED:		DB	$07		; Class-07 - Offset address is converted to
					; colour code.

;; P-BRIGHT
o1AEE:		DB	$07		; Class-07 - Offset address is converted to
					; colour code.

;; P-INVERSE
o1AEF:		DB	$07		; Class-07 - Offset address is converted to
					; colour code.

;; P-OVER
o1AF0:		DB	$07		; Class-07 - Offset address is converted to
					; colour code.

;; P-OUT
o1AF1:		DB	$08		; Class-08 - Two comma-separated numeric
					; expressions required.
		DB	$00		; Class-00 - No further operands.
		DW	o1E7A		; Address: $1E7A; Address: OUT

;; P-BORDER
o1AF5:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o2294		; Address: $2294; Address: BORDER

;; P-DEF-FN
o1AF9:		DB	$05		; Class-05 - Variable syntax checked entirely
					; by routine.
		DW	o1F60		; Address: $1F60; Address: DEF-FN

;; P-OPEN
o1AFC:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$2C		; Separator:  ','          see Footnote *
		DB	$0A		; Class-0A - A string expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1736		; Address: $1736; Address: OPEN

;; P-CLOSE
o1B02:		DB	$06		; Class-06 - A numeric expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o16E5		; Address: $16E5; Address: CLOSE

;; P-FORMAT
o1B06:		DB	$0A		; Class-0A - A string expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1793		; Address: $1793; Address: CAT-ETC

;; P-MOVE
o1B0A:		DB	$0A		; Class-0A - A string expression must follow.
		DB	$2C		; Separator:  ','
		DB	$0A		; Class-0A - A string expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1793		; Address: $1793; Address: CAT-ETC

;; P-ERASE
o1B10:		DB	$0A		; Class-0A - A string expression must follow.
		DB	$00		; Class-00 - No further operands.
		DW	o1793		; Address: $1793; Address: CAT-ETC

;; P-CAT
o1B14:		DB	$00		; Class-00 - No further operands.
		DW	o1793		; Address: $1793; Address: CAT-ETC

; * Note that a comma is required as a separator with the OPEN command
; but the Interface 1 programmers relaxed this allowing $3B as an
; alternativo for their channels creating a confusing mixture of
; allowable syntax as it is this ROM which opens or re-opens the
; normal channels.

; -------------------------------
; Main parser (BASIC interpreter)
; -------------------------------
; This routine is called once from MAIN-2 when the BASIC line is to
; be entered or re-entered into the Program area and the syntax
; requires checking.

;; LINE-SCAN
o1B17:		RES	7,(IY+$01)	; update FLAGS - signal checking syntax
		CALL	o19FB		; routine E-LINE-NO              >>
					; fetches the line number if in range.

		XOR	A		; clear the accumulator.
		LD	(SUBPPC),A	; set statement number SUBPPC to zero.
		DEC	A		; set accumulator to $FF.
		LD	(ERR_NR),A	; set ERR_NR to 'OK' - 1.
		JR	o1B29		; forward to continue at STMT-L-1.

; --------------
; Statement loop
; --------------
;
;

;; STMT-LOOP
o1B28:		RST	20H		; NEXT-CHAR

; -> the entry point from above or LINE-RUN
;; STMT-L-1
o1B29:		CALL	o16BF		; routine SET-WORK clears workspace etc.

		INC	(IY+$0D)	; increment statement number SUBPPC
		JP	M,o1C8A		; to REPORT-C to raise
					; 'Nonsense in BASIC' if over 127.

		RST	18H		; GET-CHAR

		LD	B,$00		; set B to zero for later indexing.
					; early so any other reason ???

		CP	$0D		; is character carriage return ?
					; i.e. an empty statement.
		JR	Z,o1BB3		; forward to LINE-END if so.

		CP	$3A		; is it statement end marker ":" ?
					; i.e. another type of empty statement.
		JR	Z,o1B28		; back to STMT-LOOP if so.

		LD	HL,o1B76	; address: STMT-RET
		PUSH	HL		; is now pushed as a return address
		LD	C,A		; transfer the current character to C.

; advance CH_ADD to a position after command and test if it is a command.

		RST	20H		; NEXT-CHAR to advance pointer
		LD	A,C		; restore current character
		SUB	$CE		; subtract 'DEF FN' - first command
		JP	C,o1C8A		; jump to REPORT-C if less than a command
					; raising
					; 'Nonsense in BASIC'

		LD	C,A		; put the valid command code back in C.
					; register B is zero.
		LD	HL,o1A48	; address: offst-tbl
		ADD	HL,BC		; index into table with one of 50 commands.
		LD	C,(HL)		; pick up displacement to syntax table entry.
		ADD	HL,BC		; add to address the relevant entry.
		JR	o1B55		; forward to continue at GET-PARAM

; ----------------------
; The main scanning loop
; ----------------------
; not documented properly
;

;; SCAN-LOOP
o1B52:		LD	HL,(T_ADDR)	; fetch temporary address from T_ADDR
					; during subsequent loops.

; -> the initial entry point with HL addressing start of syntax table entry.

;; GET-PARAM
o1B55:		LD	A,(HL)		; pick up the parameter.
		INC	HL		; address next one.
		LD	(T_ADDR),HL	; save pointer in system variable T_ADDR

		LD	BC,o1B52	; address: SCAN-LOOP
		PUSH	BC		; is now pushed on stack as looping address.
		LD	C,A		; store parameter in C.
		CP	$20		; is it greater than " "  ?
		JR	NC,o1B6F	; forward to SEPARATOR to check that correct
					; separator appears in statement if so.

		LD	HL,o1C01	; address: class-tbl.
		LD	B,$00		; prepare to index into the class table.
		ADD	HL,BC		; index to find displacement to routine.
		LD	C,(HL)		; displacement to BC
		ADD	HL,BC		; add to address the CLASS routine.
		PUSH	HL		; push the address on the stack.

		RST	18H		; GET-CHAR - HL points to place in statement.

		DEC	B		; reset the zero flag - the initial state
					; for all class routines.

		RET			; and make an indirect jump to routine
					; and then SCAN-LOOP (also on stack).

; Note. one of the class routines will eventually drop the return address
; off the stack breaking out of the above seemingly endless loop.

; -----------------------
; THE 'SEPARATOR' ROUTINE
; -----------------------
;   This routine is called once to verify that the mandatory separator
;   present in the parameter table is also present in the correct
;   location following the command.  For example, the 'THEN' token after
;   the 'IF' token and expression.

;; SEPARATOR
o1B6F:		RST	18H		; GET-CHAR
		CP	C		; does it match the character in C ?
		JP	NZ,o1C8A	; jump forward to REPORT-C if not
					; 'Nonsense in BASIC'.

		RST	20H		; NEXT-CHAR advance to next character
		RET			; return.

; ------------------------------
; Come here after interpretation
; ------------------------------
;
;

;; STMT-RET
o1B76:		CALL	o1F54		; routine BREAK-KEY is tested after every
					; statement.
		JR	C,o1B7D		; step forward to STMT-R-1 if not pressed.

;; REPORT-L
o1B7B:		RST	08H		; ERROR-1
		DB	$14		; Error Report: BREAK into program

;; STMT-R-1
o1B7D:		CALL	o3A3B
		NOP
		JR	NZ,o1BF4	; forward to STMT-NEXT if a program line.

		LD	HL,(NEWPPC)	; fetch line number from NEWPPC
		BIT	7,H		; will be set if minus two - direct command(s)
		JR	Z,o1B9E		; forward to LINE-NEW if a jump is to be
					; made to a new program line/statement.

; --------------------
; Run a direct command
; --------------------
; A direct command is to be run or, if continuing from above,
; the next statement of a direct command is to be considered.

;; LINE-RUN
o1B8A:		LD	HL,$FFFE	; The dummy value minus two
		LD	(PPC),HL	; is set/reset as line number in PPC.
		LD	HL,(WORKSP)	; point to end of line + 1 - WORKSP.
		DEC	HL		; now point to $80 end-marker.
		LD	DE,(E_LINE)	; address the start of line E_LINE.
		DEC	DE		; now location before - for GET-CHAR.
		LD	A,(NSPPC)	; load statement to A from NSPPC.
		JR	o1BD1		; forward to NEXT-LINE.

; ------------------------------
; Find start address of new line
; ------------------------------
; The branch was to here if a jump is to made to a new line number
; and statement.
; That is the previous statement was a GO TO, GO SUB, RUN, RETURN, NEXT etc..

;; LINE-NEW
o1B9E:		CALL	o196E		; routine LINE-ADDR gets address of line
					; returning zero flag set if line found.
		LD	A,(NSPPC)	; fetch new statement from NSPPC
		JR	Z,o1BBF		; forward to LINE-USE if line matched.

; continue as must be a direct command.

		AND	A		; test statement which should be zero
		JR	NZ,o1BEC	; forward to REPORT-N if not.
					; 'Statement lost'

;

		LD	B,A		; save statement in B.??
		LD	A,(HL)		; fetch high byte of line number.
		AND	$C0		; test if using direct command
					; a program line is less than $3F
		LD	A,B		; retrieve statement.
					; (we can assume it is zero).
		JR	Z,o1BBF		; forward to LINE-USE if was a program line

; alternativoly a direct statement has finished correctly.

;; REPORT-0
o1BB0:		RST	08H		; ERROR-1
		DB	$FF		; Error Report: OK

; -----------------
; THE 'REM' COMMAND
; -----------------
; The REM command routine.
; The return address STMT-RET is dropped and the rest of line ignored.

;; REM
o1BB2:		POP	BC		; drop return address STMT-RET and
					; continue ignoring rest of line.

; ------------
; End of line?
; ------------
;
;

;; LINE-END
o1BB3:		CALL	o2530		; routine SYNTAX-Z  (UNSTACK-Z?)
		RET	Z		; return if checking syntax.

		LD	HL,(NXTLIN)	; fetch NXTLIN to HL.
		LD	A,$C0		; test against the
		AND	(HL)		; system limit $3F.
		RET	NZ		; return if more as must be
					; end of program.
					; (or direct command)

		XOR	A		; set statement to zero.

; and continue to set up the next following line and then consider this new one.

; ---------------------
; General line checking
; ---------------------
; The branch was here from LINE-NEW if BASIC is branching.
; or a continuation from above if dealing with a new sequential line.
; First make statement zero number one leaving others unaffected.

;; LINE-USE
o1BBF:		CP	$01		; will set carry if zero.
		ADC	A,$00		; add in any carry.

		LD	D,(HL)		; high byte of line number to D.
		INC	HL		; advance pointer.
		LD	E,(HL)		; low byte of line number to E.
		LD	(PPC),DE	; set system variable PPC.

		INC	HL		; advance pointer.
		LD	E,(HL)		; low byte of line length to E.
		INC	HL		; advance pointer.
		LD	D,(HL)		; high byte of line length to D.

		EX	DE,HL		; swap pointer to DE before
		ADD	HL,DE		; adding to address the end of line.
		INC	HL		; advance to start of next line.

; -----------------------------
; Update NEXT LINE but consider
; previous line or edit line.
; -----------------------------
; The pointer will be the next line if continuing from above or to
; edit line end-marker ($80) if from LINE-RUN.

;; NEXT-LINE
o1BD1:		LD	(NXTLIN),HL	; store pointer in system variable NXTLIN

		EX	DE,HL		; bring back pointer to previous or edit line
		LD	(CH_ADD),HL	; and update CH_ADD with character address.

		LD	D,A		; store statement in D.
		LD	E,$00		; set E to zero to suppress token searching
					; if EACH-STMT is to be called.
		LD	(IY+$0A),$FF	; set statement NSPPC to $FF signalling
					; no jump to be made.
		DEC	D		; decrement and test statement
		LD	(IY+$0D),D	; set SUBPPC to decremented statement number.
		JP	Z,o1B28		; to STMT-LOOP if result zero as statement is
					; at start of line and address is known.

		INC	D		; else restore statement.
		CALL	o198B		; routine EACH-STMT finds the D'th statement
					; address as E does not contain a token.
		JR	Z,o1BF4		; forward to STMT-NEXT if address found.

;; REPORT-N
o1BEC:		RST	08H		; ERROR-1
		DB	$16		; Error Report: Statement lost

; -----------------
; End of statement?
; -----------------
; This combination of routines is called from 20 places when
; the end of a statement should have been reached and all preceding
; syntax is in order.

;; CHECK-END
o1BEE:		CALL	o2530		; routine SYNTAX-Z
		RET	NZ		; return immediately in runtime

		POP	BC		; drop address of calling routine.
		POP	BC		; drop address STMT-RET.
					; and continue to find next statement.

; --------------------
; Go to next statement
; --------------------
; Acceptable characters at this point are carriage return and ":".
; If so go to next statement which in the first case will be on next line.

;; STMT-NEXT
o1BF4:		CALL	o3A4B
		JR	Z,o1BB3		; back to LINE-END if so.

		CP	$3A		; is it ":" ?
		JP	Z,o1B28		; jump back to STMT-LOOP to consider
					; further statements

		JP	o1C8A		; jump to REPORT-C with any other character
					; 'Nonsense in BASIC'.

; Note. the two-byte sequence 'rst 08; DB     $0B' could replace the above jp.

; -------------------
; Command class table
; -------------------
;

;; class-tbl
o1C01:		DB	o1C10-$		; 0F offset to Address: CLASS-00
		DB	o1C1F-$		; 1D offset to Address: CLASS-01
		DB	o1C4E-$		; 4B offset to Address: CLASS-02
		DB	o1C0D-$		; 09 offset to Address: CLASS-03
		DB	o1C6C-$		; 67 offset to Address: CLASS-04
		DB	o1C11-$		; 0B offset to Address: CLASS-05
		DB	o1C82-$		; 7B offset to Address: CLASS-06
		DB	o1C96-$		; 8E offset to Address: CLASS-07
		DB	o1C7A-$		; 71 offset to Address: CLASS-08
		DB	o1CBE-$		; B4 offset to Address: CLASS-09
		DB	o1C8C-$		; 81 offset to Address: CLASS-0A
		DB	o1CDB-$		; CF offset to Address: CLASS-0B


; --------------------------------
; Command classes---00, 03, and 05
; --------------------------------
; class-03 e.g. RUN or RUN 200   ;  optional operand
; class-00 e.g. CONTINUE         ;  no operand
; class-05 e.g. PRINT            ;  variable syntax checked by routine

;; CLASS-03
o1C0D:		CALL	o1CDE		; routine FETCH-NUM

;; CLASS-00

o1C10:		CP	A		; reset zero flag.

; if entering here then all class routines are entered with zero reset.

;; CLASS-05
o1C11:		POP	BC		; drop address SCAN-LOOP.
		CALL	Z,o1BEE		; if zero set then call routine CHECK-END >>>
					; as should be no further characters.

		EX	DE,HL		; save HL to DE.
		LD	HL,(T_ADDR)	; fetch T_ADDR
		LD	C,(HL)		; fetch low byte of routine
		INC	HL		; address next.
		LD	B,(HL)		; fetch high byte of routine.
		EX	DE,HL		; restore HL from DE
		PUSH	BC		; push the address
		RET			; and make an indirect jump to the command.

; --------------------------------
; Command classes---01, 02, and 04
; --------------------------------
; class-01  e.g. LET A = 2*3     ; a variable is reqd

; This class routine is also called from INPUT and READ to find the
; destination variable for an assignment.

;; CLASS-01
o1C1F:		CALL	o28B2		; routine LOOK-VARS returns carry set if not
					; found in runtime.

; ----------------------
; Variable in assignment
; ----------------------
;
;

;; VAR-A-1
o1C22:		LD	(IY+$37),$00	; set FLAGX to zero
		JR	NC,o1C30	; forward to VAR-A-2 if found or checking
					; syntax.

		SET	1,(IY+$37)	; FLAGX  - Signal a new variable
		JR	NZ,o1C46	; to VAR-A-3 if not assigning to an array
					; e.g. LET a$(3,3) = "X"

;; REPORT-2
o1C2E:		RST	08H		; ERROR-1
		DB	$01		; Error Report: Variable not found

;; VAR-A-2
o1C30:		CALL	Z,o2996		; routine STK-VAR considers a subscript/slice
		BIT	6,(IY+$01)	; test FLAGS  - Numeric or string result ?
		JR	NZ,o1C46	; to VAR-A-3 if numeric

		XOR	A		; default to array/slice - to be retained.
		CALL	o2530		; routine SYNTAX-Z
		CALL	NZ,o2BF1	; routine STK-FETCH is called in runtime
					; may overwrite A with 1.
		LD	HL,FLAGX	; address system variable FLAGX
		OR	(HL)		; set bit 0 if simple variable to be reclaimed
		LD	(HL),A		; update FLAGX
		EX	DE,HL		; start of string/subscript to DE

;; VAR-A-3
o1C46:		LD	(STRLEN),BC	; update STRLEN
		LD	(DEST),HL	; and DEST of assigned string.
		RET			; return.

; -------------------------------------------------
; class-02 e.g. LET a = 1 + 1   ; an expression must follow

;; CLASS-02
o1C4E:		POP	BC		; drop return address SCAN-LOOP
		CALL	o1C56		; routine VAL-FET-1 is called to check
					; expression and assign result in runtime
		CALL	o1BEE		; routine CHECK-END checks nothing else
					; is present in statement.
		RET			; Return

; -------------
; Fetch a value
; -------------
;
;

;; VAL-FET-1
o1C56:		LD	A,(FLAGS)	; initial FLAGS to A

;; VAL-FET-2
o1C59:		PUSH	AF		; save A briefly
		CALL	o24FB		; routine SCANNING evaluates expression.
		POP	AF		; restore A
		LD	D,(IY+$01)	; post-SCANNING FLAGS to D
		XOR	D		; xor the two sets of flags
		AND	$40		; pick up bit 6 of xored FLAGS should be zero
		JR	NZ,o1C8A	; forward to REPORT-C if not zero
					; 'Nonsense in BASIC' - results don't agree.

		BIT	7,D		; test FLAGS - is syntax being checked ?
		JP	NZ,o2AFF	; jump forward to LET to make the assignment
					; in runtime.

		RET			; but return from here if checking syntax.

; ------------------
; Command class---04
; ------------------
; class-04 e.g. FOR i            ; a single character variable must follow

;; CLASS-04
o1C6C:		CALL	o28B2		; routine LOOK-VARS
		PUSH	AF		; preserve flags.
		LD	A,C		; fetch type - should be 011xxxxx
		OR	$9F		; combine with 10011111.
		INC	A		; test if now $FF by incrementing.
		JR	NZ,o1C8A	; forward to REPORT-C if result not zero.

		POP	AF		; else restore flags.
		JR	o1C22		; back to VAR-A-1


; --------------------------------
; Expect numeric/string expression
; --------------------------------
; This routine is used to get the two coordinates of STRING$, ATTR and POINT.
; It is also called from PRINT-ITEM to get the two numeric expressions that
; follow the AT ( in PRINT AT, INPUT AT).

;; NEXT-2NUM
o1C79:		RST	20H		; NEXT-CHAR advance past 'AT' or "(".

; --------
; class-08 e.g. POKE 65535,2     ; two numeric expressions separated by comma
;; CLASS-08
;; EXPT-2NUM
o1C7A:		CALL	o1C82		; routine EXPT-1NUM is called for first
					; numeric expression
		CP	$2C		; is character ',' ?
		JR	NZ,o1C8A	; to REPORT-C if not required separator.
					; 'Nonsense in BASIC'.

		RST	20H		; NEXT-CHAR

; ->
;  class-06  e.g. GOTO a*1000   ; a numeric expression must follow
;; CLASS-06
;; EXPT-1NUM
o1C82:		CALL	o24FB		; routine SCANNING
		BIT	6,(IY+$01)	; test FLAGS  - Numeric or string result ?
		RET	NZ		; return if result is numeric.

;; REPORT-C
o1C8A:		RST	08H		; ERROR-1
		DB	$0B		; Error Report: Nonsense in BASIC

; ---------------------------------------------------------------
; class-0A e.g. ERASE "????"    ; a string expression must follow.
;                               ; these only occur in unimplemented commands
;                               ; although the routine expt-exp is called
;                               ; from SAVE-ETC

;; CLASS-0A
;; EXPT-EXP
o1C8C:		CALL	o24FB		; routine SCANNING
		BIT	6,(IY+$01)	; test FLAGS  - Numeric or string result ?
		RET	Z		; return if string result.

		JR	o1C8A		; back to REPORT-C if numeric.

; ---------------------
; Set permanent colours
; class 07
; ---------------------
; class-07 e.g. PAPER 6          ; a single class for a collection of
;                               ; similar commands. Clever.
;
; Note. these commands should ensure that current channel is 'S'

;; CLASS-07
o1C96:		BIT	7,(IY+$01)	; test FLAGS - checking syntax only ?
					; Note. there is a subroutine to do this.
		RES	0,(IY+$02)	; update TV_FLAG - signal main screen in use
		CALL	NZ,o0D4D	; routine TEMPS is called in runtime.
		POP	AF		; drop return address SCAN-LOOP
		LD	A,(T_ADDR)	; T_ADDR_lo to accumulator.
					; points to '$07' entry + 1
					; e.g. for INK points to $EC now

; Note if you move alter the syntax table next line may have to be altered.

; Note. For ZASM assembler replace following expression with SUB $13.

o1CA5:		SUB	(o1AEB-$D8)%256	; convert $EB to $D8 ('INK') etc.
					; ( is SUB $13 in standard ROM )

		CALL	o21FC		; routine CO-TEMP-4
		CALL	o1BEE		; routine CHECK-END check that nothing else
					; in statement.

; return here in runtime.

		LD	HL,(ATTR_T)	; pick up ATTR_T and MASK_T
		LD	(ATTR_P),HL	; and store in ATTR_P and MASK_P
		LD	HL,P_FLAG	; point to P_FLAG.
		LD	A,(HL)		; pick up in A
		RLCA			; rotate to left
		XOR	(HL)		; combine with HL
		AND	$AA		; 10101010
		XOR	(HL)		; only permanent bits affected
		LD	(HL),A		; reload into P_FLAG.
		RET			; return.

; ------------------
; Command class---09
; ------------------
; e.g. PLOT PAPER 0; 128,88     ; two coordinates preceded by optional
;                               ; embedded colour items.
;
; Note. this command should ensure that current channel is actually 'S'.

;; CLASS-09
o1CBE:		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o1CD6		; forward to CL-09-1 if checking syntax.

		RES	0,(IY+$02)	; update TV_FLAG - signal main screen in use
		CALL	o0D4D		; routine TEMPS is called.
		LD	HL,MASK_T	; point to MASK_T
		LD	A,(HL)		; fetch mask to accumulator.
		OR	$F8		; or with 11111000 paper/bright/flash 8
		LD	(HL),A		; mask back to MASK_T system variable.
		RES	6,(IY+$57)	; reset P_FLAG  - signal NOT PAPER 9 ?

		RST	18H		; GET-CHAR

;; CL-09-1
o1CD6:		CALL	o21E2		; routine CO-TEMP-2 deals with any embedded
					; colour items.
		JR	o1C7A		; exit via EXPT-2NUM to check for x,y.

; Note. if either of the numeric expressions contain STR$ then the flag setting
; above will be undone when the channel flags are reset during STR$.
; e.g.
; 10 BORDER 3 : PLOT VAL STR$ 128, VAL STR$ 100
; credit John Elliott.

; ------------------
; Command class---0B
; ------------------
; Again a single class for four commands.
; This command just jumps back to SAVE-ETC to handle the four tape commands.
; The routine itself works out which command has called it by examining the
; address in T_ADDR_lo. Note therefore that the syntax table has to be
; located where these and other sequential command addresses are not split
; over a page boundary.

;; CLASS-0B
o1CDB:		JP	o0605		; jump way back to SAVE-ETC

; --------------
; Fetch a number
; --------------
; This routine is called from CLASS-03 when a command may be followed by
; an optional numeric expression e.g. RUN. If the end of statement has
; been reached then zero is used as the default.
; Also called from LIST-4.

;; FETCH-NUM
o1CDE:		CP	$0D		; is character a carriage return ?
		JR	Z,o1CE6		; forward to USE-ZERO if so

		CP	$3A		; is it ":" ?
		JR	NZ,o1C82	; forward to EXPT-1NUM if not.
					; else continue and use zero.

; ----------------
; Use zero routine
; ----------------
; This routine is called four times to place the value zero on the
; calculator stack as a default value in runtime.

;; USE-ZERO
o1CE6:		CALL	o2530		; routine SYNTAX-Z  (UNSTACK-Z?)
		RET	Z		;

		RST	28H		; FP-CALC
		DB	$A0		; stk-zero       ;0.
		DB	$38		; end-calc

		RET			; return.

; -------------------
; Handle STOP command
; -------------------
; Command Syntax: STOP
; One of the shortest and least used commands. As with 'OK' not an error.

;; REPORT-9
;; STOP
o1CEE:		RST	08H		; ERROR-1
		DB	$08		; Error Report: STOP statement

; -----------------
; Handle IF command
; -----------------
; e.g. IF score>100 THEN PRINT "You Win"
; The parser has already checked the expression the result of which is on
; the calculator stack. The presence of the 'THEN' separator has also been
; checked and CH-ADD points to the command after THEN.
;

;; IF
o1CF0:		POP	BC		; drop return address - STMT-RET
		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o1D00		; forward to IF-1 if checking syntax
					; to check syntax of PRINT "You Win"


		RST	28H		; FP-CALC    score>100 (1=TRUE 0=FALSE)
		DB	$02		; delete      .
		DB	$38		; end-calc

		EX	DE,HL		; make HL point to deleted value
		CALL	o34E9		; routine TEST-ZERO
		JP	C,o1BB3		; jump to LINE-END if FALSE (0)

;; IF-1
o1D00:		JP	o1B29		; to STMT-L-1, if true (1) to execute command
					; after 'THEN' token.

; ------------------
; Handle FOR command
; ------------------
; e.g. FOR i = 0 TO 1 STEP 0.1
; Using the syntax tables, the parser has already checked for a start and
; limit value and also for the intervening separator.
; the two values v,l are on the calculator stack.
; CLASS-04 has also checked the variable and the name is in STRLEN_lo.
; The routine begins by checking for an optional STEP.

;; FOR
o1D03:		CP	$CD		; is there a 'STEP' ?
		JR	NZ,o1D10	; to F-USE-1 if not to use 1 as default.

		RST	20H		; NEXT-CHAR
		CALL	o1C82		; routine EXPT-1NUM
		CALL	o1BEE		; routine CHECK-END
		JR	o1D16		; to F-REORDER

; ---

;; F-USE-1
o1D10:		CALL	o1BEE		; routine CHECK-END

		RST	28H		; FP-CALC      v,l.
		DB	$A1		; stk-one       v,l,1=s.
		DB	$38		; end-calc


;; F-REORDER
o1D16:		RST	28H		; FP-CALC       v,l,s.
		DB	$C0		; st-mem-0       v,l,s.
		DB	$02		; delete         v,l.
		DB	$01		; exchange       l,v.
		DB	$E0		; get-mem-0      l,v,s.
		DB	$01		; exchange       l,s,v.
		DB	$38		; end-calc

		CALL	o2AFF		; routine LET assigns the initial value v to
					; the variable altering type if necessary.
		LD	(MEM),HL	; The system variable MEM is made to point to
					; the variable instead of its normal
					; location MEMBOT
		DEC	HL		; point to single-character name
		LD	A,(HL)		; fetch name
		SET	7,(HL)		; set bit 7 at location
		LD	BC,$0006	; add six to HL
		ADD	HL,BC		; to address where limit should be.
		RLCA			; test bit 7 of original name.
		JR	C,o1D34		; forward to F-L-S if already a FOR/NEXT
					; variable

		LD	C,$0D		; otherwise an additional 13 bytes are needed.
					; 5 for each value, two for line number and
					; 1 byte for looping statement.
		CALL	o1655		; routine MAKE-ROOM creates them.
		INC	HL		; make HL address limit.

;; F-L-S
o1D34:		PUSH	HL		; save position.

		RST	28H		; FP-CALC         l,s.
		DB	$02		; delete           l.
		DB	$02		; delete           .
		DB	$38		; end-calc
					; DE points to STKEND, l.

		POP	HL		; restore variable position
		EX	DE,HL		; swap pointers
		LD	C,$0A		; ten bytes to move
		LDIR			; Copy 'deleted' values to variable.
		LD	HL,(PPC)	; Load with current line number from PPC
		EX	DE,HL		; exchange pointers.
		LD	(HL),E		; save the looping line
		INC	HL		; in the next
		LD	(HL),D		; two locations.
		LD	D,(IY+$0D)	; fetch statement from SUBPPC system variable.
		INC	D		; increment statement.
		INC	HL		; and pointer
		LD	(HL),D		; and store the looping statement.
					;
		CALL	o1DDA		; routine NEXT-LOOP considers an initial
		RET	NC		; iteration. Return to STMT-RET if a loop is
					; possible to execute next statement.

; no loop is possible so execution continues after the matching 'NEXT'

		LD	B,(IY+$38)	; get single-character name from STRLEN_lo
		LD	HL,(PPC)	; get the current line from PPC
		LD	(NEWPPC),HL	; and store it in NEWPPC
		LD	A,(SUBPPC)	; fetch current statement from SUBPPC
		NEG			; Negate as counter decrements from zero
					; initially and we are in the middle of a
					; line.
		LD	D,A		; Store result in D.
		LD	HL,(CH_ADD)	; get current address from CH_ADD
		LD	E,$F3		; search will be for token 'NEXT'

;; F-LOOP
o1D64:		PUSH	BC		; save variable name.
		LD	BC,(NXTLIN)	; fetch NXTLIN
		CALL	o1D86		; routine LOOK-PROG searches for 'NEXT' token.
		LD	(NXTLIN),BC	; update NXTLIN
		POP	BC		; and fetch the letter
		JR	C,o1D84		; forward to REPORT-I if the end of program
					; was reached by LOOK-PROG.
					; 'FOR without NEXT'

		RST	20H		; NEXT-CHAR fetches character after NEXT
		OR	$20		; ensure it is upper-case.
		CP	B		; compare with FOR variable name
		JR	Z,o1D7C		; forward to F-FOUND if it matches.

; but if no match i.e. nested FOR/NEXT loops then continue search.

		RST	20H		; NEXT-CHAR
		JR	o1D64		; back to F-LOOP

; ---


;; F-FOUND
o1D7C:		RST	20H		; NEXT-CHAR
		LD	A,$01		; subtract the negated counter from 1
		SUB	D		; to give the statement after the NEXT
		LD	(NSPPC),A	; set system variable NSPPC
		RET			; return to STMT-RET to branch to new
					; line and statement. ->
					; ---

;; REPORT-I
o1D84:		RST	08H		; ERROR-1
		DB	$11		; Error Report: FOR without NEXT

; ---------
; LOOK-PROG
; ---------
; Find DATA, DEF FN or NEXT.
; This routine searches the program area for one of the above three keywords.
; On entry, HL points to start of search area.
; The token is in E, and D holds a statement count, decremented from zero.

;; LOOK-PROG
o1D86:		LD	A,(HL)		; fetch current character
		CP	$3A		; is it ":" a statement separator ?
		JR	Z,o1DA3		; forward to LOOK-P-2 if so.

; The starting point was PROG - 1 or the end of a line.

;; LOOK-P-1
o1D8B:		INC	HL		; increment pointer to address
		LD	A,(HL)		; the high byte of line number
		AND	$C0		; test for program end marker $80 or a
					; variable
		SCF			; Set Carry Flag
		RET	NZ		; return with carry set if at end
					; of program.           ->

		LD	B,(HL)		; high byte of line number to B
		INC	HL		;
		LD	C,(HL)		; low byte to C.
		LD	(NEWPPC),BC	; set system variable NEWPPC.
		INC	HL		;
		LD	C,(HL)		; low byte of line length to C.
		INC	HL		;
		LD	B,(HL)		; high byte to B.
		PUSH	HL		; save address
		ADD	HL,BC		; add length to position.
		LD	B,H		; and save result
		LD	C,L		; in BC.
		POP	HL		; restore address.
		LD	D,$00		; initialize statement counter to zero.

;; LOOK-P-2
o1DA3:		PUSH	BC		; save address of next line
		CALL	o198B		; routine EACH-STMT searches current line.
		POP	BC		; restore address.
		RET	NC		; return if match was found. ->

		JR	o1D8B		; back to LOOK-P-1 for next line.

; -------------------
; Handle NEXT command
; -------------------
; e.g. NEXT i
; The parameter tables have already evaluated the presence of a variable

;; NEXT
o1DAB:		BIT	1,(IY+$37)	; test FLAGX - handling a new variable ?
		JP	NZ,o1C2E	; jump back to REPORT-2 if so
					; 'Variable not found'

; now test if found variable is a simple variable uninitialized by a FOR.

		LD	HL,(DEST)	; load address of variable from DEST
		BIT	7,(HL)		; is it correct type ?
		JR	Z,o1DD8		; forward to REPORT-1 if not
					; 'NEXT without FOR'

		INC	HL		; step past variable name
		LD	(MEM),HL	; and set MEM to point to three 5-byte values
					; value, limit, step.

		RST	28H		; FP-CALC     add step and re-store
		DB	$E0		; get-mem-0    v.
		DB	$E2		; get-mem-2    v,s.
		DB	$0F		; addition     v+s.
		DB	$C0		; st-mem-0     v+s.
		DB	$02		; delete       .
		DB	$38		; end-calc

		CALL	o1DDA		; routine NEXT-LOOP tests against limit.
		RET	C		; return if no more iterations possible.

		LD	HL,(MEM)	; find start of variable contents from MEM.
		LD	DE,$000F	; add 3*5 to
		ADD	HL,DE		; address the looping line number
		LD	E,(HL)		; low byte to E
		INC	HL		;
		LD	D,(HL)		; high byte to D
		INC	HL		; address looping statement
		LD	H,(HL)		; and store in H
		EX	DE,HL		; swap registers
		JP	o1E73		; exit via GO-TO-2 to execute another loop.

; ---

;; REPORT-1
o1DD8:		RST	08H		; ERROR-1
		DB	$00		; Error Report: NEXT without FOR


; -----------------
; Perform NEXT loop
; -----------------
; This routine is called from the FOR command to test for an initial
; iteration and from the NEXT command to test for all subsequent iterations.
; the system variable MEM addresses the variable's contents which, in the
; latter case, have had the step, possibly negative, added to the value.

;; NEXT-LOOP
o1DDA:		RST	28H		; FP-CALC
		DB	$E1		; get-mem-1        l.
		DB	$E0		; get-mem-0        l,v.
		DB	$E2		; get-mem-2        l,v,s.
		DB	$36		; less-0           l,v,(1/0) negative step ?
		DB	$00		; jump-true        l,v.(1/0)

		DB	$02		; to o1DE2, NEXT-1 if step negative

		DB	$01		; exchange         v,l.

;; NEXT-1
o1DE2:		DB	$03		; subtract         l-v OR v-l.
		DB	$37		; greater-0        (1/0)
		DB	$00		; jump-true        .

		DB	$04		; to o1DE9, NEXT-2 if no more iterations.

		DB	$38		; end-calc         .

		AND	A		; clear carry flag signalling another loop.
		RET			; return

; ---

;; NEXT-2
o1DE9:		DB	$38		; end-calc         .

		SCF			; set carry flag signalling looping exhausted.
		RET			; return


; -------------------
; Handle READ command
; -------------------
; e.g. READ a, b$, c$(1000 TO 3000)
; A list of comma-separated variables is assigned from a list of
; comma-separated expressions.
; As it moves along the first list, the character address CH_ADD is stored
; in X_PTR while CH_ADD is used to read the second list.

;; READ-3
o1DEC:		RST	20H		; NEXT-CHAR

; -> Entry point.
;; READ
o1DED:		CALL	o1C1F		; routine CLASS-01 checks variable.
		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o1E1E		; forward to READ-2 if checking syntax


		RST	18H		; GET-CHAR
		LD	(X_PTR),HL	; save character position in X_PTR.
		LD	HL,(DATADD)	; load HL with Data Address DATADD, which is
					; the start of the program or the address
					; after the last expression that was read or
					; the address of the line number of the
					; last RESTORE command.
		LD	A,(HL)		; fetch character
		CP	$2C		; is it a comma ?
		JR	Z,o1E0A		; forward to READ-1 if so.

; else all data in this statement has been read so look for next DATA token

		LD	E,$E4		; token 'DATA'
		CALL	o1D86		; routine LOOK-PROG
		JR	NC,o1E0A	; forward to READ-1 if DATA found

; else report the error.

;; REPORT-E
o1E08:		RST	08H		; ERROR-1
		DB	$0D		; Error Report: Out of DATA

;; READ-1
o1E0A:		CALL	o0077		; routine TEMP-PTR1 advances updating CH_ADD
					; with new DATADD position.
		CALL	o1C56		; routine VAL-FET-1 assigns value to variable
					; checking type match and adjusting CH_ADD.

		RST	18H		; GET-CHAR fetches adjusted character position
		LD	(DATADD),HL	; store back in DATADD
		LD	HL,(X_PTR)	; fetch X_PTR  the original READ CH_ADD
		LD	(IY+$26),$00	; now nullify X_PTR_hi
		CALL	o0078		; routine TEMP-PTR2 restores READ CH_ADD

;; READ-2
o1E1E:		RST	18H		; GET-CHAR
		CP	$2C		; is it ',' indicating more variables to read ?
		JR	Z,o1DEC		; back to READ-3 if so

		CALL	o1BEE		; routine CHECK-END
		RET			; return from here in runtime to STMT-RET.

; -------------------
; Handle DATA command
; -------------------
; In runtime this 'command' is passed by but the syntax is checked when such
; a statement is found while parsing a line.
; e.g. DATA 1, 2, "text", score-1, a$(location, room, object), FN r(49),
;         wages - tax, TRUE, The meaning of life

;; DATA
o1E27:		CALL	o2530		; routine SYNTAX-Z to check status
		JR	NZ,o1E37	; forward to DATA-2 if in runtime

;; DATA-1
o1E2C:		CALL	o24FB		; routine SCANNING to check syntax of
					; expression
		CP	$2C		; is it a comma ?
		CALL	NZ,o1BEE	; routine CHECK-END checks that statement
					; is complete. Will make an early exit if
					; so. >>>
		RST	20H		; NEXT-CHAR
		JR	o1E2C		; back to DATA-1

; ---

;; DATA-2
o1E37:		LD	A,$E4		; set token to 'DATA' and continue into
					; the PASS-BY routine.


; ----------------------------------
; Check statement for DATA or DEF FN
; ----------------------------------
; This routine is used to backtrack to a command token and then
; forward to the next statement in runtime.

;; PASS-BY
o1E39:		LD	B,A		; Give BC enough space to find token.
		CPDR			; Compare decrement and repeat. (Only use).
					; Work backwards till keyword is found which
					; is start of statement before any quotes.
					; HL points to location before keyword.
		LD	DE,$0200	; count 1+1 statements, dummy value in E to
					; inhibit searching for a token.
		JP	o198B		; to EACH-STMT to find next statement

; -----------------------------------------------------------------------
; A General Note on Invalid Line Numbers.
; =======================================
; One of the revolutionary concepts of Sinclair BASIC was that it supported
; virtual line numbers. That is the destination of a GO TO, RESTORE etc. need
; not exist. It could be a point before or after an actual line number.
; Zero suffices for a before but the after should logically be infinity.
; Since the maximum actual line limit is 9999 then the system limit, 16383
; when variables kick in, would serve fine as a virtual end point.
; However, ironically, only the LOAD command gets it right. It will not
; autostart a program that has been saved with a line higher than 16383.
; All the other commands deal with the limit unsatisfactorily.
; LIST, RUN, GO TO, GO SUB and RESTORE have problems and the latter may
; crash the machine when supplied with an inappropriate virtual line number.
; This is puzzling as very careful consideration must have been given to
; this point when the new variable types were allocated their masks and also
; when the routine NEXT-ONE was successfully re-written to reflect this.
; An enigma.
; -------------------------------------------------------------------------

; ----------------------
; Handle RESTORE command
; ----------------------
; The restore command sets the system variable for the data address to
; point to the location before the supplied line number or first line
; thereafter.
; This alters the position where subsequent READ commands look for data.
; Note. If supplied with inappropriate high numbers the system may crash
; in the LINE-ADDR routine as it will pass the program/variables end-marker
; and then lose control of what it is looking for - variable or line number.
; - observation, Steven Vickers, 1984, Pitman.

;; RESTORE
o1E42:		CALL	o1E99		; routine FIND-INT2 puts integer in BC.
					; Note. B should be checked against limit $3F
					; and an error generated if higher.

; this entry point is used from RUN command with BC holding zero

;; REST-RUN
o1E45:		LD	H,B		; transfer the line
		LD	L,C		; number to the HL register.
		CALL	o196E		; routine LINE-ADDR to fetch the address.
		DEC	HL		; point to the location before the line.
		LD	(DATADD),HL	; update system variable DATADD.
		RET			; return to STMT-RET (or RUN)

; ------------------------
; Handle RANDOMIZE command
; ------------------------
; This command sets the SEED for the RND function to a fixed value.
; With the parameter zero, a random start point is used depending on
; how long the computer has been switched on.

;; RANDOMIZE
o1E4F:		CALL	o1E99		; routine FIND-INT2 puts parameter in BC.
		LD	A,B		; test this
		OR	C		; for zero.
		JR	NZ,o1E5A	; forward to RAND-1 if not zero.

		LD	BC,(FRAMES)	; use the lower two bytes at FRAMES1.

;; RAND-1
o1E5A:		LD	(SEED),BC	; place in SEED system variable.
		RET			; return to STMT-RET

; -----------------------
; Handle CONTINUE command
; -----------------------
; The CONTINUE command transfers the OLD (but incremented) values of
; line number and statement to the equivalent "NEW VALUE" system variables
; by using the last part of GO TO and exits indirectly to STMT-RET.

;; CONTINUE
o1E5F:		LD	HL,(OLDPPC)	; fetch OLDPPC line number.
		LD	D,(IY+$36)	; fetch OSPPC statement.
		JR	o1E73		; forward to GO-TO-2

; --------------------
; Handle GO TO command
; --------------------
; The GO TO command routine is also called by GO SUB and RUN routines
; to evaluate the parameters of both commands.
; It updates the system variables used to fetch the next line/statement.
; It is at STMT-RET that the actual change in control takes place.
; Unlike some BASICs the line number need not exist.
; Note. the high byte of the line number is incorrectly compared with $F0
; instead of $3F. This leads to commands with operands greater than 32767
; being considered as having been run from the editing area and the
; error report 'Statement Lost' is given instead of 'OK'.
; - Steven Vickers, 1984.

;; GO-TO
o1E67:		CALL	o1E99		; routine FIND-INT2 puts operand in BC
		LD	H,B		; transfer line
		LD	L,C		; number to HL.
		LD	D,$00		; set statement to 0 - first.
		LD	A,H		; compare high byte only
		CP	$F0		; to $F0 i.e. 61439 in full.
		JR	NC,o1E9F	; forward to REPORT-B if above.

; This call entry point is used to update the system variables e.g. by RETURN.

;; GO-TO-2
o1E73:		LD	(NEWPPC),HL	; save line number in NEWPPC
		LD	(IY+$0A),D	; and statement in NSPPC
		RET			; to STMT-RET (or GO-SUB command)

; ------------------
; Handle OUT command
; ------------------
; Syntax has been checked and the two comma-separated values are on the
; calculator stack.

;; OUT
o1E7A:		CALL	o1E85		; routine TWO-PARAM fetches values
					; to BC and A.
		OUT	(C),A		; perform the operation.
		RET			; return to STMT-RET.

; -------------------
; Handle POKE command
; -------------------
; This routine alters a single byte in the 64K address space.
; Happily no check is made as to whether ROM or RAM is addressed.
; Sinclair BASIC requires no poking of system variables.

;; POKE
o1E80:		CALL	o1E85		; routine TWO-PARAM fetches values
					; to BC and A.
		LD	(BC),A		; load memory location with A.
		RET			; return to STMT-RET.

; ------------------------------------
; Fetch two  parameters from calculator stack
; ------------------------------------
; This routine fetches a byte and word from the calculator stack
; producing an error if either is out of range.

;; TWO-PARAM
o1E85:		CALL	o2DD5		; routine FP-TO-A
		JR	C,o1E9F		; forward to REPORT-B if overflow occurred

		JR	Z,o1E8E		; forward to TWO-P-1 if positive

		NEG			; negative numbers are made positive

;; TWO-P-1
o1E8E:		PUSH	AF		; save the value
		CALL	o1E99		; routine FIND-INT2 gets integer to BC
		POP	AF		; restore the value
		RET			; return

; -------------
; Find integers
; -------------
; The first of these routines fetches a 8-bit integer (range 0-255) from the
; calculator stack to the accumulator and is used for colours, streams,
; durations and coordinates.
; The second routine fetches 16-bit integers to the BC register pair
; and is used to fetch command and function arguments involving line numbers
; or memory addresses and also array subscripts and tab arguments.
; ->

;; FIND-INT1
o1E94:		CALL	o2DD5		; routine FP-TO-A
		JR	o1E9C		; forward to FIND-I-1 for common exit routine.

; ---

; ->

;; FIND-INT2
o1E99:		CALL	o2DA2		; routine FP-TO-BC

;; FIND-I-1
o1E9C:		JR	C,o1E9F		; to REPORT-Bb with overflow.

		RET	Z		; return if positive.


;; REPORT-Bb
o1E9F:		RST	08H		; ERROR-1
		DB	$0A		; Error Report: Integer out of range

; ------------------
; Handle RUN command
; ------------------
; This command runs a program starting at an optional line.
; It performs a 'RESTORE 0' then CLEAR

;; RUN
o1EA1:		CALL	o1E67		; routine GO-TO puts line number in
					; system variables.
		LD	BC,$0000	; prepare to set DATADD to first line.
		CALL	o1E45		; routine REST-RUN does the 'restore'.
					; Note BC still holds zero.
		JR	o1EAF		; forward to CLEAR-RUN to clear variables
					; without disturbing RAMTOP and
					; exit indirectly to STMT-RET

; --------------------
; Handle CLEAR command
; --------------------
; This command reclaims the space used by the variables.
; It also clears the screen and the GO SUB stack.
; With an integer expression, it sets the uppermost memory
; address within the BASIC system.
; "Contrary to the manual, CLEAR doesn't execute a RESTORE" -
; Steven Vickers, Pitman Pocket Guide to the Spectrum, 1984.

;; CLEAR
o1EAC:		CALL	o1E99		; routine FIND-INT2 fetches to BC.

;; CLEAR-RUN
o1EAF:		LD	A,B		; test for
		OR	C		; zero.
		JR	NZ,o1EB7	; skip to CLEAR-1 if not zero.

		LD	BC,(RAMTOP)	; use the existing value of RAMTOP if zero.

;; CLEAR-1
o1EB7:		PUSH	BC		; save ramtop value.

		LD	DE,(VARS)	; fetch VARS
		LD	HL,(E_LINE)	; fetch E_LINE
		DEC	HL		; adjust to point at variables end-marker.
		CALL	o19E5		; routine RECLAIM-1 reclaims the space used by
					; the variables.

		CALL	o0D6B		; routine CLS to clear screen.

		LD	HL,(STKEND)	; fetch STKEND the start of free memory.
		LD	DE,$0032	; allow for another 50 bytes.
		ADD	HL,DE		; add the overhead to HL.

		POP	DE		; restore the ramtop value.
		SBC	HL,DE		; if HL is greater than the value then jump
		JR	NC,o1EDA	; forward to REPORT-M
					; 'RAMTOP no good'

		LD	HL,(P_RAMT)	; now P-RAMT ($7FFF on 16K RAM machine)
		AND	A		; exact this time.
		SBC	HL,DE		; new ramtop must be lower or the same.
		JR	NC,o1EDC	; skip to CLEAR-2 if in actual RAM.

;; REPORT-M
o1EDA:		RST	08H		; ERROR-1
		DB	$15		; Error Report: RAMTOP no good

;; CLEAR-2
o1EDC:		EX	DE,HL		; transfer ramtop value to HL.
		LD	(RAMTOP),HL	; update system variable RAMTOP.
		POP	DE		; pop the return address STMT-RET.
		POP	BC		; pop the Error Address.
		LD	(HL),$3E	; now put the GO SUB end-marker at RAMTOP.
		DEC	HL		; leave a location beneath it.
		LD	SP,HL		; initialize the machine stack pointer.
		PUSH	BC		; push the error address.
		LD	(ERR_SP),SP	; make ERR_SP point to location.
		EX	DE,HL		; put STMT-RET in HL.
		JP	(HL)		; and go there directly.

; ---------------------
; Handle GO SUB command
; ---------------------
; The GO SUB command diverts BASIC control to a new line number
; in a very similar manner to GO TO but
; the current line number and current statement + 1
; are poaced on the GO SUB stack as a RETURN point.

;; GO-SUB
o1EED:		POP	DE		; drop the address STMT-RET
		LD	H,(IY+$0D)	; fetch statement from SUBPPC and
		INC	H		; increment it
		EX	(SP),HL		; swap - error address to HL,
					; H (statement) at top of stack,
					; L (unimportant) beneath.
		INC	SP		; adjust to overwrite unimportant byte
		LD	BC,(PPC)	; fetch the current line number from PPC
		PUSH	BC		; and PUSH onto GO SUB stack.
					; the empty machine-stack can be rebuilt
		PUSH	HL		; push the error address.
		LD	(ERR_SP),SP	; make system variable ERR_SP point to it.
		PUSH	DE		; push the address STMT-RET.
		CALL	o1E67		; call routine GO-TO to update the system
					; variables NEWPPC and NSPPC.
					; then make an indirect exit to STMT-RET via
		LD	BC,$0014	; a 20-byte overhead memory check.

; ----------------------
; Check available memory
; ----------------------
; This routine is used on many occasions when extending a dynamic area
; upwards or the GO SUB stack downwards.

;; TEST-ROOM
o1F05:		LD	HL,(STKEND)	; fetch STKEND
		ADD	HL,BC		; add the supplied test value
		JR	C,o1F15		; forward to REPORT-4 if over $FFFF

		EX	DE,HL		; was less so transfer to DE
		LD	HL,$0050	; test against another 80 bytes
		ADD	HL,DE		; anyway
		JR	C,o1F15		; forward to REPORT-4 if this passes $FFFF

		SBC	HL,SP		; if less than the machine stack pointer
		RET	C		; then return - OK.

;; REPORT-4
o1F15:		LD	L,$03		; prepare 'Out of Memory'
		JP	o0055		; jump back to ERROR-3 at $0055
					; Note. this error can't be trapped at $0008

; ------------------------------
; THE 'FREE MEMORY' USER ROUTINE
; ------------------------------
; This routine is not used by the ROM but allows users to evaluate
; approximate free memory with PRINT 65536 - USR 7962.

;; free-mem
o1F1A:		LD	BC,$0000	; allow no overhead.

		CALL	o1F05		; routine TEST-ROOM.

		LD	B,H		; transfer the result
		LD	C,L		; to the BC register.
		RET			; the USR function returns value of BC.

; --------------------
; THE 'RETURN' COMMAND
; --------------------
; As with any command, there are two values on the machine stack at the time
; it is invoked.  The machine stack is below the GOSUB stack.  Both grow
; downwards, the machine stack by two bytes, the GOSUB stack by 3 bytes.
; The highest location is a statement byte followed by a two-byte line number.

;; RETURN
o1F23:		POP	BC		; drop the address STMT-RET.
		POP	HL		; now the error address.
		POP	DE		; now a possible BASIC return line.
		LD	A,D		; the high byte $00 - $27 is
		CP	$3E		; compared with the traditional end-marker $3E.
		JR	Z,o1F36		; forward to REPORT-7 with a match.
					; 'RETURN without GOSUB'

; It was not the end-marker so a single statement byte remains at the base of
; the calculator stack. It can't be popped off.

		DEC	SP		; adjust stack pointer to create room for two
					; bytes.
		EX	(SP),HL		; statement to H, error address to base of
					; new machine stack.
		EX	DE,HL		; statement to D,  BASIC line number to HL.
		LD	(ERR_SP),SP	; adjust ERR_SP to point to new stack pointer
		PUSH	BC		; now re-stack the address STMT-RET
		JP	o1E73		; to GO-TO-2 to update statement and line
					; system variables and exit indirectly to the
					; address just pushed on stack.

; ---

;; REPORT-7
o1F36:		PUSH	DE		; replace the end-marker.
		PUSH	HL		; now restore the error address
					; as will be required in a few clock cycles.

		RST	08H		; ERROR-1
		DB	$06		; Error Report: RETURN without GOSUB

; --------------------
; Handle PAUSE command
; --------------------
; The pause command takes as its parameter the number of interrupts
; for which to wait. PAUSE 50 pauses for about a second.
; PAUSE 0 pauses indefinitely.
; Both forms can be finished by pressing a key.

;; PAUSE
o1F3A:		CALL	o1E99		; routine FIND-INT2 puts value in BC

;; PAUSE-1
o1F3D:		HALT
		DEC	BC		; decrease counter.
		LD	A,B		; test if
		OR	C		; result is zero.
		JR	Z,o1F4F		; forward to PAUSE-END if so.

		LD	A,B		; test if
		AND	C		; now $FFFF
		INC	A		; that is, initially zero.
		JR	NZ,o1F49	; skip forward to PAUSE-2 if not.

		INC	BC		; restore counter to zero.

;; PAUSE-2
o1F49:		BIT	5,(IY+$01)	; test FLAGS - has a new key been pressed ?
		JR	Z,o1F3D		; back to PAUSE-1 if not.

;; PAUSE-END
o1F4F:		RES	5,(IY+$01)	; update FLAGS - signal no new key
		RET			; and return.

; -------------------
; Check for BREAK key
; -------------------
; This routine is called from COPY-LINE, when interrupts are disabled,
; to test if BREAK (SHIFT - SPACE) is being pressed.
; It is also called at STMT-RET after every statement.

;; BREAK-KEY
o1F54:		LD	A,$7F		; Input address: $7FFE
		IN	A,($FE)		; read lower right keys
		RRA			; rotate bit 0 - SPACE
		RET	C		; return if not reset

		LD	A,$FE		; Input address: $FEFE
		IN	A,($FE)		; read lower left keys
		RRA			; rotate bit 0 - SHIFT
		RET			; carry will be set if not pressed.
					; return with no carry if both keys
					; pressed.

; ---------------------
; Handle DEF FN command
; ---------------------
; e.g. DEF FN r$(a$,a) = a$(a TO )
; this 'command' is ignored in runtime but has its syntax checked
; during line-entry.

;; DEF-FN
o1F60:		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o1F6A		; forward to DEF-FN-1 if parsing

		LD	A,$CE		; else load A with 'DEF FN' and
		JP	o1E39		; jump back to PASS-BY

; ---

; continue here if checking syntax.

;; DEF-FN-1
o1F6A:		SET	6,(IY+$01)	; set FLAGS  - Assume numeric result
		CALL	o2C8D		; call routine ALPHA
		JR	NC,o1F89	; if not then to DEF-FN-4 to jump to
					; 'Nonsense in BASIC'


		RST	20H		; NEXT-CHAR
		CP	$24		; is it "$" ?
		JR	NZ,o1F7D	; to DEF-FN-2 if not as numeric.

		RES	6,(IY+$01)	; set FLAGS  - Signal string result

		RST	20H		; get NEXT-CHAR

;; DEF-FN-2
o1F7D:		CP	$28		; is it "(" ?
		JR	NZ,o1FBD	; to DEF-FN-7 'Nonsense in BASIC'


		RST	20H		; NEXT-CHAR
		CP	$29		; is it ")" ?
		JR	Z,o1FA6		; to DEF-FN-6 if null argument

;; DEF-FN-3
o1F86:		CALL	o2C8D		; routine ALPHA checks that it is the expected
					; alphabetic character.

;; DEF-FN-4
o1F89:		JP	NC,o1C8A	; to REPORT-C  if not
					; 'Nonsense in BASIC'.

		EX	DE,HL		; save pointer in DE

		RST	20H		; NEXT-CHAR re-initializes HL from CH_ADD
					; and advances.
		CP	$24		; "$" ? is it a string argument.
		JR	NZ,o1F94	; forward to DEF-FN-5 if not.

		EX	DE,HL		; save pointer to "$" in DE

		RST	20H		; NEXT-CHAR re-initializes HL and advances

;; DEF-FN-5
o1F94:		EX	DE,HL		; bring back pointer.
		LD	BC,$0006	; the function requires six hidden bytes for
					; each parameter passed.
					; The first byte will be $0E
					; then 5-byte numeric value
					; or 5-byte string pointer.

		CALL	o1655		; routine MAKE-ROOM creates space in program
					; area.

		INC	HL		; adjust HL (set by LDDR)
		INC	HL		; to point to first location.
		LD	(HL),$0E	; insert the 'hidden' marker.

; Note. these invisible storage locations hold nothing meaningful for the
; moment. They will be used every time the corresponding function is
; evaluated in runtime.
; Now consider the following character fetched earlier.

		CP	$2C		; is it ',' ? (more than one parameter)
		JR	NZ,o1FA6	; to DEF-FN-6 if not


		RST	20H		; else NEXT-CHAR
		JR	o1F86		; and back to DEF-FN-3

; ---

;; DEF-FN-6
o1FA6:		CP	$29		; should close with a ")"
		JR	NZ,o1FBD	; to DEF-FN-7 if not
					; 'Nonsense in BASIC'


		RST	20H		; get NEXT-CHAR
		CP	$3D		; is it "=" ?
		JR	NZ,o1FBD	; to DEF-FN-7 if not 'Nonsense...'


		RST	20H		; address NEXT-CHAR
		LD	A,(FLAGS)	; get FLAGS which has been set above
		PUSH	AF		; and preserve

		CALL	o24FB		; routine SCANNING checks syntax of expression
					; and also sets flags.

		POP	AF		; restore previous flags
		XOR	(IY+$01)	; xor with FLAGS - bit 6 should be same
					; therefore will be reset.
		AND	$40		; isolate bit 6.

;; DEF-FN-7
o1FBD:		JP	NZ,o1C8A	; jump back to REPORT-C if the expected result
					; is not the same type.
					; 'Nonsense in BASIC'

		CALL	o1BEE		; routine CHECK-END will return early if
					; at end of statement and move onto next
					; else produce error report. >>>

; There will be no return to here.

; -------------------------------
; Returning early from subroutine
; -------------------------------
; All routines are capable of being run in two modes - syntax checking mode
; and runtime mode.  This routine is called often to allow a routine to return
; early if checking syntax.

;; UNSTACK-Z
o1FC3:		CALL	o2530		; routine SYNTAX-Z sets zero flag if syntax
					; is being checked.

		POP	HL		; drop the return address.
		RET	Z		; return to previous call in chain if checking
					; syntax.

		JP	(HL)		; jump to return address as BASIC program is
					; actually running.

; ---------------------
; Handle LPRINT command
; ---------------------
; A simple form of 'PRINT #3' although it can output to 16 streams.
; Probably for compatibility with other BASICs particularly ZX81 BASIC.
; An extra UDG might have been better.

;; LPRINT
o1FC9:		LD	A,$03		; the printer channel
		JR	o1FCF		; forward to PRINT-1

; ---------------------
; Handle PRINT commands
; ---------------------
; The Spectrum's main stream output command.
; The default stream is stream 2 which is normally the upper screen
; of the computer. However the stream can be altered in range 0 - 15.

;; PRINT
o1FCD:		LD	A,$02		; the stream for the upper screen.

; The LPRINT command joins here.

;; PRINT-1
o1FCF:		CALL	o2530		; routine SYNTAX-Z checks if program running
		CALL	NZ,o1601	; routine CHAN-OPEN if so
		CALL	o0D4D		; routine TEMPS sets temporary colours.
		CALL	o1FDF		; routine PRINT-2 - the actual item
		CALL	o1BEE		; routine CHECK-END gives error if not at end
					; of statement
		RET			; and return >>>

; ------------------------------------
; this subroutine is called from above
; and also from INPUT.

;; PRINT-2
o1FDF:		RST	18H		; GET-CHAR gets printable character
		CALL	o2045		; routine PR-END-Z checks if more printing
		JR	Z,o1FF2		; to PRINT-4 if not     e.g. just 'PRINT :'

; This tight loop deals with combinations of positional controls and
; print items. An early return can be made from within the loop
; if the end of a print sequence is reached.

;; PRINT-3
o1FE5:		CALL	o204E		; routine PR-POSN-1 returns zero if more
					; but returns early at this point if
					; at end of statement!
					;
		JR	Z,o1FE5		; to PRINT-3 if consecutive positioners

		CALL	o1FFC		; routine PR-ITEM-1 deals with strings etc.
		CALL	o204E		; routine PR-POSN-1 for more position codes
		JR	Z,o1FE5		; loop back to PRINT-3 if so

;; PRINT-4
o1FF2:		CP	$29		; return now if this is ")" from input-item.
					; (see INPUT.)
		RET	Z		; or continue and print carriage return in
					; runtime

; ---------------------
; Print carriage return
; ---------------------
; This routine which continues from above prints a carriage return
; in run-time. It is also called once from PRINT-POSN.

;; PRINT-CR
o1FF5:		CALL	o1FC3		; routine UNSTACK-Z

		LD	A,$0D		; prepare a carriage return

		RST	10H		; PRINT-A
		RET			; return


; -----------
; Print items
; -----------
; This routine deals with print items as in
; PRINT AT 10,0;"The value of A is ";a
; It returns once a single item has been dealt with as it is part
; of a tight loop that considers sequences of positional and print items

;; PR-ITEM-1
o1FFC:		RST	18H		; GET-CHAR
		CP	$AC		; is character 'AT' ?
		JR	NZ,o200E	; forward to PR-ITEM-2 if not.

		CALL	o1C79		; routine NEXT-2NUM  check for two comma
					; separated numbers placing them on the
					; calculator stack in runtime.
		CALL	o1FC3		; routine UNSTACK-Z quits if checking syntax.

		CALL	o2307		; routine STK-TO-BC get the numbers in B and C.
		LD	A,$16		; prepare the 'at' control.
		JR	o201E		; forward to PR-AT-TAB to print the sequence.

; ---

;; PR-ITEM-2
o200E:		CP	$AD		; is character 'TAB' ?
		JR	NZ,o2024	; to PR-ITEM-3 if not


		RST	20H		; NEXT-CHAR to address next character
		CALL	o1C82		; routine EXPT-1NUM
		CALL	o1FC3		; routine UNSTACK-Z quits if checking syntax.

		CALL	o1E99		; routine FIND-INT2 puts integer in BC.
		LD	A,$17		; prepare the 'tab' control.

;; PR-AT-TAB
o201E:		RST	10H		; PRINT-A outputs the control

		LD	A,C		; first value to A
		RST	10H		; PRINT-A outputs it.

		LD	A,B		; second value
		RST	10H		; PRINT-A

		RET			; return - item finished >>>

; ---

; Now consider paper 2; #2; a$

;; PR-ITEM-3
o2024:		CALL	o21F2		; routine CO-TEMP-3 will print any colour
		RET	NC		; items - return if success.

		CALL	o2070		; routine STR-ALTER considers new stream
		RET	NC		; return if altered.

		CALL	o24FB		; routine SCANNING now to evaluate expression
		CALL	o1FC3		; routine UNSTACK-Z if not runtime.

		BIT	6,(IY+$01)	; test FLAGS  - Numeric or string result ?
		CALL	Z,o2BF1		; routine STK-FETCH if string.
					; note no flags affected.
		JP	NZ,o2DE3	; to PRINT-FP to print if numeric >>>

; It was a string expression - start in DE, length in BC
; Now enter a loop to print it

;; PR-STRING
o203C:		LD	A,B		; this tests if the
		OR	C		; length is zero and sets flag accordingly.
		DEC	BC		; this doesn't but decrements counter.
		RET	Z		; return if zero.

		LD	A,(DE)		; fetch character.
		INC	DE		; address next location.

		RST	10H		; PRINT-A.

		JR	o203C		; loop back to PR-STRING.

; ---------------
; End of printing
; ---------------
; This subroutine returns zero if no further printing is required
; in the current statement.
; The first terminator is found in  escaped input items only,
; the others in print_items.

;; PR-END-Z
o2045:		CP	$29		; is character a ")" ?
		RET	Z		; return if so -        e.g. INPUT (p$); a$

;; PR-ST-END
o2048:		CP	$0D		; is it a carriage return ?
		RET	Z		; return also -         e.g. PRINT a

		CP	$3A		; is character a ":" ?
		RET			; return - zero flag will be set if so.
					;                       e.g. PRINT a :

; --------------
; Print position
; --------------
; This routine considers a single positional character $3B, ',', '''

;; PR-POSN-1
o204E:		RST	18H		; GET-CHAR
		CP	$3B		; is it $3B ?
					; i.e. print from last position.
		JR	Z,o2067		; forward to PR-POSN-3 if so.
					; i.e. do nothing.

		CP	$2C		; is it ',' ?
					; i.e. print at next tabstop.
		JR	NZ,o2061	; forward to PR-POSN-2 if anything else.

		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o2067		; forward to PR-POSN-3 if checking syntax.

		LD	A,$06		; prepare the 'comma' control character.

		RST	10H		; PRINT-A  outputs to current channel in
					; run-time.

		JR	o2067		; skip to PR-POSN-3.

; ---

; check for newline.

;; PR-POSN-2
o2061:		CP	$27		; is character a "'" ? (newline)
		RET	NZ		; return if no match              >>>

		CALL	o1FF5		; routine PRINT-CR outputs a carriage return
					; in runtime only.

;; PR-POSN-3
o2067:		RST	20H		; NEXT-CHAR to A.
		CALL	o2045		; routine PR-END-Z checks if at end.
		JR	NZ,o206E	; to PR-POSN-4 if not.

		POP	BC		; drop return address if at end.

;; PR-POSN-4
o206E:		CP	A		; reset the zero flag.
		RET			; and return to loop or quit.

; ------------
; Alter stream
; ------------
; This routine is called from PRINT ITEMS above, and also LIST as in
; LIST #15

;; STR-ALTER
o2070:		CP	$23		; is character "#" ?
		SCF			; set carry flag.
		RET	NZ		; return if no match.


		RST	20H		; NEXT-CHAR
		CALL	o1C82		; routine EXPT-1NUM gets stream number
		AND	A		; prepare to exit early with carry reset
		CALL	o1FC3		; routine UNSTACK-Z exits early if parsing
		CALL	o1E94		; routine FIND-INT1 gets number off stack
		CP	$10		; must be range 0 - 15 decimal.
		JP	NC,o160E	; jump back to REPORT-Oa if not
					; 'Invalid stream'.

		CALL	o1601		; routine CHAN-OPEN
		AND	A		; clear carry - signal item dealt with.
		RET			; return

; -------------------
; THE 'INPUT' COMMAND
; -------------------
; This command is mysterious.
;

;; INPUT
o2089:		CALL	o2530		; routine SYNTAX-Z to check if in runtime.

		JR	Z,o2096		; forward to INPUT-1 if checking syntax.

		LD	A,$01		; select channel 'K' the keyboard for input.
		CALL	o1601		; routine CHAN-OPEN opens the channel and sets
					; bit 0 of TV_FLAG.

;   Note. As a consequence of clearing the lower screen channel 0 is made
;   the current channel so the above two instructions are superfluous.

		CALL	o0D6E		; routine CLS-LOWER clears the lower screen
					; and sets DF_SZ to two and TV_FLAG to $01.

;; INPUT-1
o2096:		LD	(IY+$02),$01	; update TV_FLAG - signal lower screen in use
					; ensuring that the correct set of system
					; variables are updated and that the border
					; colour is used.

;   Note. The Complete Spectrum ROM Disassembly incorrectly names DF-SZ as the
;   system variable that is updated above and if, as some have done, you make
;   this unnecessary alteration then there will be two blank lines between the
;   lower screen and the upper screen areas which will also scroll wrongly.

		CALL	o20C1		; routine IN-ITEM-1 to handle the input.

		CALL	o1BEE		; routine CHECK-END will make an early exit
					; if checking syntax. >>>

;   Keyboard input has been made and it remains to adjust the upper
;   screen in case the lower two lines have been extended upwards.

		LD	BC,(S_POSN)	; fetch S_POSN current line/column of
					; the upper screen.
		LD	A,(DF_SZ)	; fetch DF_SZ the display file size of
					; the lower screen.
		CP	B		; test that lower screen does not overlap
		JR	C,o20AD		; forward to INPUT-2 if not.

; the two screens overlap so adjust upper screen.

		LD	C,$21		; set column of upper screen to leftmost.
		LD	B,A		; and line to one above lower screen.
					; continue forward to update upper screen
					; print position.

;; INPUT-2
o20AD:		LD	(S_POSN),BC	; set S_POSN update upper screen line/column.
		LD	A,$19		; subtract from twenty five
		SUB	B		; the new line number.
		LD	(SCR_CT),A	; and place result in SCR_CT - scroll count.
		RES	0,(IY+$02)	; update TV_FLAG - signal main screen in use.

		CALL	o0DD9		; routine CL-SET sets the print position
					; system variables for the upper screen.

		JP	o0D6E		; jump back to CLS-LOWER and make
					; an indirect exit >>.

; ---------------------
; INPUT ITEM subroutine
; ---------------------
;   This subroutine deals with the input items and print items.
;   from  the current input channel.
;   It is only called from the above INPUT routine but was obviously
;   once called from somewhere else in another context.

;; IN-ITEM-1
o20C1:		CALL	o204E		; routine PR-POSN-1 deals with a single
					; position item at each call.
		JR	Z,o20C1		; back to IN-ITEM-1 until no more in a
					; sequence.

		CP	$28		; is character "(" ?
		JR	NZ,o20D8	; forward to IN-ITEM-2 if not.

;   any variables within braces will be treated as part, or all, of the prompt
;   instead of being used as destination variables.

		RST	20H		; NEXT-CHAR
		CALL	o1FDF		; routine PRINT-2 to output the dynamic
					; prompt.

		RST	18H		; GET-CHAR
		CP	$29		; is character a matching ")" ?
		JP	NZ,o1C8A	; jump back to REPORT-C if not.
					; 'Nonsense in BASIC'.

		RST	20H		; NEXT-CHAR
		JP	o21B2		; forward to IN-NEXT-2

; ---

;; IN-ITEM-2
o20D8:		CP	$CA		; is the character the token 'LINE' ?
		JR	NZ,o20ED	; forward to IN-ITEM-3 if not.

		RST	20H		; NEXT-CHAR - variable must come next.
		CALL	o1C1F		; routine CLASS-01 returns destination
					; address of variable to be assigned.
					; or generates an error if no variable
					; at this position.

		SET	7,(IY+$37)	; update FLAGX  - signal handling INPUT LINE
		BIT	6,(IY+$01)	; test FLAGS  - numeric or string result ?
		JP	NZ,o1C8A	; jump back to REPORT-C if not string
					; 'Nonsense in BASIC'.

		JR	o20FA		; forward to IN-PROMPT to set up workspace.

; ---

;   the jump was here for other variables.

;; IN-ITEM-3
o20ED:		CALL	o2C8D		; routine ALPHA checks if character is
					; a suitable variable name.
		JP	NC,o21AF	; forward to IN-NEXT-1 if not

		CALL	o1C1F		; routine CLASS-01 returns destination
					; address of variable to be assigned.
		RES	7,(IY+$37)	; update FLAGX  - signal not INPUT LINE.

;; IN-PROMPT
o20FA:		CALL	o2530		; routine SYNTAX-Z
		JP	Z,o21B2		; forward to IN-NEXT-2 if checking syntax.

		CALL	o16BF		; routine SET-WORK clears workspace.
		LD	HL,FLAGX	; point to system variable FLAGX
		RES	6,(HL)		; signal string result.
		SET	5,(HL)		; signal in Input Mode for editor.
		LD	BC,$0001	; initialize space required to one for
					; the carriage return.
		BIT	7,(HL)		; test FLAGX - INPUT LINE in use ?
		JR	NZ,o211C	; forward to IN-PR-2 if so as that is
					; all the space that is required.

		LD	A,(FLAGS)	; load accumulator from FLAGS
		AND	$40		; mask to test BIT 6 of FLAGS and clear
					; the other bits in A.
					; numeric result expected ?
		JR	NZ,o211A	; forward to IN-PR-1 if so

		LD	C,$03		; increase space to three bytes for the
					; pair of surrounding quotes.

;; IN-PR-1
o211A:		OR	(HL)		; if numeric result, set bit 6 of FLAGX.
		LD	(HL),A		; and update system variable

;; IN-PR-2
o211C:		RST	30H		; BC-SPACES opens 1 or 3 bytes in workspace
		LD	(HL),$0D	; insert carriage return at last new location.
		LD	A,C		; fetch the length, one or three.
		RRCA			; lose bit 0.
		RRCA			; test if quotes required.
		JR	NC,o2129	; forward to IN-PR-3 if not.

		LD	A,$22		; load the '"' character
		LD	(DE),A		; place quote in first new location at DE.
		DEC	HL		; decrease HL - from carriage return.
		LD	(HL),A		; and place a quote in second location.

;; IN-PR-3
o2129:		LD	(K_CUR),HL	; set keyboard cursor K_CUR to HL
		BIT	7,(IY+$37)	; test FLAGX  - is this INPUT LINE ??
		JR	NZ,o215E	; forward to IN-VAR-3 if so as input will
					; be accepted without checking its syntax.

		LD	HL,(CH_ADD)	; fetch CH_ADD
		PUSH	HL		; and save on stack.
		LD	HL,(ERR_SP)	; fetch ERR_SP
		PUSH	HL		; and save on stack

;; IN-VAR-1
o213A:		LD	HL,o213A	; address: IN-VAR-1 - this address
		PUSH	HL		; is saved on stack to handle errors.
		BIT	4,(IY+$30)	; test FLAGS2  - is K channel in use ?
		JR	Z,o2148		; forward to IN-VAR-2 if not using the
					; keyboard for input. (??)

		LD	(ERR_SP),SP	; set ERR_SP to point to IN-VAR-1 on stack.

;; IN-VAR-2
o2148:		LD	HL,(WORKSP)	; set HL to WORKSP - start of workspace.
		CALL	o11A7		; routine REMOVE-FP removes floating point
					; forms when looping in error condition.
		LD	(IY+$00),$FF	; set ERR_NR to 'OK' cancelling the error.
					; but X_PTR causes flashing error marker
					; to be displayed at each call to the editor.
		CALL	o0F2C		; routine EDITOR allows input to be entered
					; or corrected if this is second time around.

; if we pass to next then there are no system errors

		RES	7,(IY+$01)	; update FLAGS  - signal checking syntax
		CALL	o21B9		; routine IN-ASSIGN checks syntax using
					; the VAL-FET-2 and powerful SCANNING routines.
					; any syntax error and its back to IN-VAR-1.
					; but with the flashing error marker showing
					; where the error is.
					; Note. the syntax of string input has to be
					; checked as the user may have removed the
					; bounding quotes or escaped them as with
					; "hat" + "stand" for example.
					; proceed if syntax passed.

		JR	o2161		; jump forward to IN-VAR-4

; ---

; the jump was to here when using INPUT LINE.

;; IN-VAR-3
o215E:		CALL	o0F2C		; routine EDITOR is called for input

; when ENTER received rejoin other route but with no syntax check.

; INPUT and INPUT LINE converge here.

;; IN-VAR-4
o2161:		LD	(IY+$22),$00	; set K_CUR_hi to a low value so that the cursor
					; no longer appears in the input line.

		CALL	o21D6		; routine IN-CHAN-K tests if the keyboard
					; is being used for input.
		JR	NZ,o2174	; forward to IN-VAR-5 if using another input
					; channel.

; continue here if using the keyboard.

		CALL	o111D		; routine ED-COPY overprints the edit line
					; to the lower screen. The only visible
					; affect is that the cursor disappears.
					; if you're inputting more than one item in
					; a statement then that becomes apparent.

		LD	BC,(ECHO_E)	; fetch line and column from ECHO_E
		CALL	o0DD9		; routine CL-SET sets S-POSNL to those
					; values.

; if using another input channel rejoin here.

;; IN-VAR-5
o2174:		LD	HL,FLAGX	; point HL to FLAGX
		RES	5,(HL)		; signal not in input mode
		BIT	7,(HL)		; is this INPUT LINE ?
		RES	7,(HL)		; cancel the bit anyway.
		JR	NZ,o219B	; forward to IN-VAR-6 if INPUT LINE.

		POP	HL		; drop the looping address
		POP	HL		; drop the address of previous
					; error handler.
		LD	(ERR_SP),HL	; set ERR_SP to point to it.
		POP	HL		; drop original CH_ADD which points to
					; INPUT command in BASIC line.
		LD	(X_PTR),HL	; save in X_PTR while input is assigned.
		SET	7,(IY+$01)	; update FLAGS - Signal running program
		CALL	o21B9		; routine IN-ASSIGN is called again
					; this time the variable will be assigned
					; the input value without error.
					; Note. the previous example now
					; becomes "hatstand"

		LD	HL,(X_PTR)	; fetch stored CH_ADD value from X_PTR.
		LD	(IY+$26),$00	; set X_PTR_hi so that iy is no longer relevant.
		LD	(CH_ADD),HL	; put restored value back in CH_ADD
		JR	o21B2		; forward to IN-NEXT-2 to see if anything
					; more in the INPUT list.

; ---

; the jump was to here with INPUT LINE only

;; IN-VAR-6
o219B:		LD	HL,(STKBOT)	; STKBOT points to the end of the input.
		LD	DE,(WORKSP)	; WORKSP points to the beginning.
		SCF			; prepare for true subtraction.
		SBC	HL,DE		; subtract to get length
		LD	B,H		; transfer it to
		LD	C,L		; the BC register pair.
		CALL	o2AB2		; routine STK-STO-$ stores parameters on
					; the calculator stack.
		CALL	o2AFF		; routine LET assigns it to destination.
		JR	o21B2		; forward to IN-NEXT-2 as print items
					; not allowed with INPUT LINE.
					; Note. that "hat" + "stand" will, for
					; example, be unchanged as also would
					; 'PRINT "Iris was here"'.

; ---

; the jump was to here when ALPHA found more items while looking for
; a variable name.

;; IN-NEXT-1
o21AF:		CALL	o1FFC		; routine PR-ITEM-1 considers further items.

;; IN-NEXT-2
o21B2:		CALL	o204E		; routine PR-POSN-1 handles a position item.
		JP	Z,o20C1		; jump back to IN-ITEM-1 if the zero flag
					; indicates more items are present.

		RET			; return.

; ---------------------------
; INPUT ASSIGNMENT Subroutine
; ---------------------------
; This subroutine is called twice from the INPUT command when normal
; keyboard input is assigned. On the first occasion syntax is checked
; using SCANNING. The final call with the syntax flag reset is to make
; the assignment.

;; IN-ASSIGN
o21B9:		LD	HL,(WORKSP)	; fetch WORKSP start of input
		LD	(CH_ADD),HL	; set CH_ADD to first character

		RST	18H		; GET-CHAR ignoring leading white-space.
		CP	$E2		; is it 'STOP'
		JR	Z,o21D0		; forward to IN-STOP if so.

		LD	A,(FLAGX)	; load accumulator from FLAGX
		CALL	o1C59		; routine VAL-FET-2 makes assignment
					; or goes through the motions if checking
					; syntax. SCANNING is used.

		RST	18H		; GET-CHAR
		CP	$0D		; is it carriage return ?
		RET	Z		; return if so
					; either syntax is OK
					; or assignment has been made.

; if another character was found then raise an error.
; User doesn't see report but the flashing error marker
; appears in the lower screen.

;; REPORT-Cb
o21CE:		RST	08H		; ERROR-1
		DB	$0B		; Error Report: Nonsense in BASIC

;; IN-STOP
o21D0:		CALL	o2530		; routine SYNTAX-Z (UNSTACK-Z?)
		RET	Z		; return if checking syntax
					; as user wouldn't see error report.
					; but generate visible error report
					; on second invocation.

;; REPORT-H
o21D4:		RST	08H		; ERROR-1
		DB	$10		; Error Report: STOP in INPUT

; -----------------------------------
; THE 'TEST FOR CHANNEL K' SUBROUTINE
; -----------------------------------
;   This subroutine is called once from the keyboard INPUT command to check if
;   the input routine in use is the one for the keyboard.

;; IN-CHAN-K
o21D6:		LD	HL,(CURCHL)	; fetch address of current channel CURCHL
		INC	HL		;
		INC	HL		; advance past
		INC	HL		; input and
		INC	HL		; output streams
		LD	A,(HL)		; fetch the channel identifier.
		CP	$4B		; test for 'K'
		RET			; return with zero set if keyboard is use.

; --------------------
; Colour Item Routines
; --------------------
;
; These routines have 3 entry points -
; 1) CO-TEMP-2 to handle a series of embedded Graphic colour items.
; 2) CO-TEMP-3 to handle a single embedded print colour item.
; 3) CO TEMP-4 to handle a colour command such as FLASH 1
;
; "Due to a bug, if you bring in a peripheral channel and later use a colour
;  statement, colour controls will be sent to it by mistake." - Steven Vickers
;  Pitman Pocket Guide, 1984.
;
; To be fair, this only applies if the last channel was other than 'K', 'S'
; or 'P', which are all that are supported by this ROM, but if that last
; channel was a microdrive file, network channel etc. then
; PAPER 6; CLS will not turn the screen yellow and
; CIRCLE INK 2; 128,88,50 will not draw a red circle.
;
; This bug does not apply to embedded PRINT items as it is quite permissible
; to mix stream altering commands and colour items.
; The fix therefore would be to ensure that CLASS-07 and CLASS-09 make
; channel 'S' the current channel when not checking syntax.
; -----------------------------------------------------------------

;; CO-TEMP-1
o21E1:		RST	20H		; NEXT-CHAR

; -> Entry point from CLASS-09. Embedded Graphic colour items.
; e.g. PLOT INK 2; PAPER 8; 128,88
; Loops till all colour items output, finally addressing the coordinates.

;; CO-TEMP-2
o21E2:		CALL	o21F2		; routine CO-TEMP-3 to output colour control.
		RET	C		; return if nothing more to output. ->


		RST	18H		; GET-CHAR
		CP	$2C		; is it ',' separator ?
		JR	Z,o21E1		; back if so to CO-TEMP-1

		CP	$3B		; is it $3B separator ?
		JR	Z,o21E1		; back to CO-TEMP-1 for more.

		JP	o1C8A		; to REPORT-C (REPORT-Cb is within range)
					; 'Nonsense in BASIC'

; -------------------
; CO-TEMP-3
; -------------------
; -> this routine evaluates and outputs a colour control and parameter.
; It is called from above and also from PR-ITEM-3 to handle a single embedded
; print item e.g. PRINT PAPER 6; "Hi". In the latter case, the looping for
; multiple items is within the PR-ITEM routine.
; It is quite permissible to send these to any stream.

;; CO-TEMP-3
o21F2:		CP	$D9		; is it 'INK' ?
		RET	C		; return if less.

		CP	$DF		; compare with 'OUT'
		CCF			; Complement Carry Flag
		RET	C		; return if greater than 'OVER', $DE.

		PUSH	AF		; save the colour token.

		RST	20H		; address NEXT-CHAR
		POP	AF		; restore token and continue.

; -> this entry point used by CLASS-07. e.g. the command PAPER 6.

;; CO-TEMP-4
o21FC:		SUB	$C9		; reduce to control character $10 (INK)
					; thru $15 (OVER).
		PUSH	AF		; save control.
		CALL	o1C82		; routine EXPT-1NUM stacks addressed
					; parameter on calculator stack.
		POP	AF		; restore control.
		AND	A		; clear carry

		CALL	o1FC3		; routine UNSTACK-Z returns if checking syntax.

		PUSH	AF		; save again
		CALL	o1E94		; routine FIND-INT1 fetches parameter to A.
		LD	D,A		; transfer now to D
		POP	AF		; restore control.

		RST	10H		; PRINT-A outputs the control to current
					; channel.
		LD	A,D		; transfer parameter to A.

		RST	10H		; PRINT-A outputs parameter.
		RET			; return. ->

; -------------------------------------------------------------------------
;
;         {fl}{br}{   paper   }{  ink    }    The temporary colour attributes
;          ___ ___ ___ ___ ___ ___ ___ ___    system variable.
; ATTR_T  |   |   |   |   |   |   |   |   |
;         |   |   |   |   |   |   |   |   |
; 23695   |___|___|___|___|___|___|___|___|
;           7   6   5   4   3   2   1   0
;
;
;         {fl}{br}{   paper   }{  ink    }    The temporary mask used for
;          ___ ___ ___ ___ ___ ___ ___ ___    transparent colours. Any bit
; MASK_T  |   |   |   |   |   |   |   |   |   that is 1 shows that the
;         |   |   |   |   |   |   |   |   |   corresponding attribute is
; 23696   |___|___|___|___|___|___|___|___|   taken not from ATTR-T but from
;           7   6   5   4   3   2   1   0     what is already on the screen.
;
;
;         {paper9 }{ ink9 }{ inv1 }{ over1}   The print flags. Even bits are
;          ___ ___ ___ ___ ___ ___ ___ ___    temporary flags. The odd bits
; P_FLAG  |   |   |   |   |   |   |   |   |   are the permanent flags.
;         | p | t | p | t | p | t | p | t |
; 23697   |___|___|___|___|___|___|___|___|
;           7   6   5   4   3   2   1   0
;
; -----------------------------------------------------------------------

; ------------------------------------
;  The colour system variable handler.
; ------------------------------------
; This is an exit branch from PO-1-OPER, PO-2-OPER
; A holds control $10 (INK) to $15 (OVER)
; D holds parameter 0-9 for ink/paper 0,1 or 8 for bright/flash,
; 0 or 1 for over/inverse.

;; CO-TEMP-5
o2211:		SUB	$11		; reduce range $FF-$04
		ADC	A,$00		; add in carry if INK
		JR	Z,o2234		; forward to CO-TEMP-7 with INK and PAPER.

		SUB	$02		; reduce range $FF-$02
		ADC	A,$00		; add carry if FLASH
		JR	Z,o2273		; forward to CO-TEMP-C with FLASH and BRIGHT.

		CP	$01		; is it 'INVERSE' ?
		LD	A,D		; fetch parameter for INVERSE/OVER
		LD	B,$01		; prepare OVER mask setting bit 0.
		JR	NZ,o2228	; forward to CO-TEMP-6 if OVER

		RLCA			; shift bit 0
		RLCA			; to bit 2
		LD	B,$04		; set bit 2 of mask for inverse.

;; CO-TEMP-6
o2228:		LD	C,A		; save the A
		LD	A,D		; re-fetch parameter
		CP	$02		; is it less than 2
		JR	NC,o2244	; to REPORT-K if not 0 or 1.
					; 'Invalid colour'.

		LD	A,C		; restore A
		LD	HL,P_FLAG	; address system variable P_FLAG
		JR	o226C		; forward to exit via routine CO-CHANGE

; ---

; the branch was here with INK/PAPER and carry set for INK.

;; CO-TEMP-7
o2234:		LD	A,D		; fetch parameter
		LD	B,$07		; set ink mask 00000111
		JR	C,o223E		; forward to CO-TEMP-8 with INK

		RLCA			; shift bits 0-2
		RLCA			; to
		RLCA			; bits 3-5
		LD	B,$38		; set paper mask 00111000

; both paper and ink rejoin here

;; CO-TEMP-8
o223E:		LD	C,A		; value to C
		LD	A,D		; fetch parameter
		CP	$0A		; is it less than 10d ?
		JR	C,o2246		; forward to CO-TEMP-9 if so.

; ink 10 etc. is not allowed.

;; REPORT-K
o2244:		RST	08H		; ERROR-1
		DB	$13		; Error Report: Invalid colour

;; CO-TEMP-9
o2246:		LD	HL,ATTR_T	; address system variable ATTR_T initially.
		CP	$08		; compare with 8
		JR	C,o2258		; forward to CO-TEMP-B with 0-7.

		LD	A,(HL)		; fetch temporary attribute as no change.
		JR	Z,o2257		; forward to CO-TEMP-A with INK/PAPER 8

; it is either ink 9 or paper 9 (contrasting)

		OR	B		; or with mask to make white
		CPL			; make black and change other to dark
		AND	$24		; 00100100
		JR	Z,o2257		; forward to CO-TEMP-A if black and
					; originally light.

		LD	A,B		; else just use the mask (white)

;; CO-TEMP-A
o2257:		LD	C,A		; save A in C

;; CO-TEMP-B
o2258:		LD	A,C		; load colour to A
		CALL	o226C		; routine CO-CHANGE addressing ATTR-T

		LD	A,$07		; put 7 in accumulator
		CP	D		; compare with parameter
		SBC	A,A		; $00 if 0-7, $FF if 8
		CALL	o226C		; routine CO-CHANGE addressing MASK-T
					; mask returned in A.

; now consider P-FLAG.

		RLCA			; 01110000 or 00001110
		RLCA			; 11100000 or 00011100
		AND	$50		; 01000000 or 00010000  (AND 01010000)
		LD	B,A		; transfer to mask
		LD	A,$08		; load A with 8
		CP	D		; compare with parameter
		SBC	A,A		; $FF if was 9,  $00 if 0-8
					; continue while addressing P-FLAG
					; setting bit 4 if ink 9
					; setting bit 6 if paper 9

; -----------------------
; Handle change of colour
; -----------------------
; This routine addresses a system variable ATTR_T, MASK_T or P-FLAG in HL.
; colour value in A, mask in B.

;; CO-CHANGE
o226C:		XOR	(HL)		; impress bits specified
		AND	B		; by mask
		XOR	(HL)		; on system variable.
		LD	(HL),A		; update system variable.
		INC	HL		; address next location.
		LD	A,B		; put current value of mask in A
		RET			; return.

; ---

; the branch was here with flash and bright

;; CO-TEMP-C
o2273:		SBC	A,A		; set zero flag for bright.
		LD	A,D		; fetch original parameter 0,1 or 8
		RRCA			; rotate bit 0 to bit 7
		LD	B,$80		; mask for flash 10000000
		JR	NZ,o227D	; forward to CO-TEMP-D if flash

		RRCA			; rotate bit 7 to bit 6
		LD	B,$40		; mask for bright 01000000

;; CO-TEMP-D
o227D:		LD	C,A		; store value in C
		LD	A,D		; fetch parameter
		CP	$08		; compare with 8
		JR	Z,o2287		; forward to CO-TEMP-E if 8

		CP	$02		; test if 0 or 1
		JR	NC,o2244	; back to REPORT-K if not
					; 'Invalid colour'

;; CO-TEMP-E
o2287:		LD	A,C		; value to A
		LD	HL,ATTR_T	; address ATTR_T
		CALL	o226C		; routine CO-CHANGE addressing ATTR_T
		LD	A,C		; fetch value
		RRCA			; for flash8/bright8 complete
		RRCA			; rotations to put set bit in
		RRCA			; bit 7 (flash) bit 6 (bright)
		JR	o226C		; back to CO-CHANGE addressing MASK_T
					; and indirect return.

; ---------------------
; Handle BORDER command
; ---------------------
; Command syntax example: BORDER 7
; This command routine sets the border to one of the eight colours.
; The colours used for the lower screen are based on this.

;; BORDER
o2294:		CALL	o1E94		; routine FIND-INT1
		CP	$08		; must be in range 0 (black) to 7 (white)
		JR	NC,o2244	; back to REPORT-K if not
					; 'Invalid colour'.

		OUT	($FE),A		; outputting to port effects an immediate
					; change.
		RLCA			; shift the colour to
		RLCA			; the paper bits setting the
		RLCA			; ink colour black.
		BIT	5,A		; is the number light coloured ?
					; i.e. in the range green to white.
		JR	NZ,o22A6	; skip to BORDER-1 if so

		XOR	$07		; make the ink white.

;; BORDER-1
o22A6:		LD	(BORDCR),A	; update BORDCR with new paper/ink
		RET			; return.

; -----------------
; Get pixel address
; -----------------
;
;

;; PIXEL-ADD
o22AA:		LD	A,$AF		; load with 175 decimal.
		SUB	B		; subtract the y value.
		JP	C,o24F9		; jump forward to REPORT-Bc if greater.
					; 'Integer out of range'

; the high byte is derived from Y only.
; the first 3 bits are always 010
; the next 2 bits denote in which third of the screen the byte is.
; the last 3 bits denote in which of the 8 scan lines within a third
; the byte is located. There are 24 discrete values.


		LD	B,A		; the line number from top of screen to B.
		AND	A		; clear carry (already clear)
		RRA			;                     0xxxxxxx
		SCF			; set carry flag
		RRA			;                     10xxxxxx
		AND	A		; clear carry flag
		RRA			;                     010xxxxx

		XOR	B		;
		AND	$F8		; keep the top 5 bits 11111000
		XOR	B		;                     010xxbbb
		LD	H,A		; transfer high byte to H.

; the low byte is derived from both X and Y.

		LD	A,C		; the x value 0-255.
		RLCA			;
		RLCA			;
		RLCA			;
		XOR	B		; the y value
		AND	$C7		; apply mask             11000111
		XOR	B		; restore unmasked bits  xxyyyxxx
		RLCA			; rotate to              xyyyxxxx
		RLCA			; required position.     yyyxxxxx
		LD	L,A		; low byte to L.

; finally form the pixel position in A.

		LD	A,C		; x value to A
		AND	$07		; mod 8
		RET			; return

; ----------------
; Point Subroutine
; ----------------
; The point subroutine is called from s-point via the scanning functions
; table.

;; POINT-SUB
o22CB:		CALL	o2307		; routine STK-TO-BC
		CALL	o22AA		; routine PIXEL-ADD finds address of pixel.
		LD	B,A		; pixel position to B, 0-7.
		INC	B		; increment to give rotation count 1-8.
		LD	A,(HL)		; fetch byte from screen.

;; POINT-LP
o22D4:		RLCA
		DJNZ	o22D4		; to POINT-LP until pixel at right.

		AND	$01		; test to give zero or one.
		JP	o2D28		; jump forward to STACK-A to save result.

; -------------------
; Handle PLOT command
; -------------------
; Command Syntax example: PLOT 128,88
;

;; PLOT
o22DC:		CALL	o2307		; routine STK-TO-BC
		CALL	o22E5		; routine PLOT-SUB
		JP	o0D4D		; to TEMPS

; -------------------
; The Plot subroutine
; -------------------
; A screen byte holds 8 pixels so it is necessary to rotate a mask
; into the correct position to leave the other 7 pixels unaffected.
; However all 64 pixels in the character cell take any embedded colour
; items.
; A pixel can be reset (inverse 1), toggled (over 1), or set ( with inverse
; and over switches off). With both switches on, the byte is simply put
; back on the screen though the colours may change.

;; PLOT-SUB
o22E5:		LD	(COORDS),BC	; store new x/y values in COORDS
		CALL	o22AA		; routine PIXEL-ADD gets address in HL,
					; count from left 0-7 in B.
		LD	B,A		; transfer count to B.
		INC	B		; increase 1-8.
		LD	A,$FE		; 11111110 in A.

;; PLOT-LOOP
o22F0:		RRCA
		DJNZ	o22F0		; to PLOT-LOOP until B circular rotations.

		LD	B,A		; load mask to B
		LD	A,(HL)		; fetch screen byte to A

		LD	C,(IY+$57)	; P_FLAG to C
		BIT	0,C		; is it to be OVER 1 ?
		JR	NZ,o22FD	; forward to PL-TST-IN if so.

; was over 0

		AND	B		; combine with mask to blank pixel.

;; PL-TST-IN
o22FD:		BIT	2,C		; is it inverse 1 ?
		JR	NZ,o2303	; to PLOT-END if so.

		XOR	B		; switch the pixel
		CPL			; restore other 7 bits

;; PLOT-END
o2303:		LD	(HL),A		; load byte to the screen.
		JP	o0BDB		; exit to PO-ATTR to set colours for cell.

; ------------------------------
; Put two numbers in BC register
; ------------------------------
;
;

;; STK-TO-BC
o2307:		CALL	o2314		; routine STK-TO-A
		LD	B,A		;
		PUSH	BC		;
		CALL	o2314		; routine STK-TO-A
		LD	E,C		;
		POP	BC		;
		LD	D,C		;
		LD	C,A		;
		RET			;

; -----------------------
; Put stack in A register
; -----------------------
; This routine puts the last value on the calculator stack into the accumulator
; deleting the last value.

;; STK-TO-A
o2314:		CALL	o2DD5		; routine FP-TO-A compresses last value into
					; accumulator. e.g. PI would become 3.
					; zero flag set if positive.
		JP	C,o24F9		; jump forward to REPORT-Bc if >= 255.5.

		LD	C,$01		; prepare a positive sign byte.
		RET	Z		; return if FP-TO-BC indicated positive.

		LD	C,$FF		; prepare negative sign byte and
		RET			; return.


; --------------------
; THE 'CIRCLE' COMMAND
; --------------------
;   "Goe not Thou about to Square eyther circle" -
;   - John Donne, Cambridge educated theologian, 1624
;
;   The CIRCLE command draws a circle as a series of straight lines.
;   In some ways it can be regarded as a polygon, but the first line is drawn
;   as a tangent, taking the radius as its distance from the centre.
;
;   Both the CIRCLE algorithm and the ARC drawing algorithm make use of the
;   'ROTATION FORMULA' (see later).  It is only necessary to work out where
;   the first line will be drawn and how long it is and then the rotation
;   formula takes over and calculates all other rotated points.
;
;   All Spectrum circles consist of two vertical lines at each side and two
;   horizontal lines at the top and bottom. The number of lines is calculated
;   from the radius of the circle and is always divisible by 4. For complete
;   circles it will range from 4 for a square circle to 32 for a circle of
;   radius 87. The Spectrum can attempt larger circles e.g. CIRCLE 0,14,255
;   but these will error as they go off-screen after four lines are drawn.
;   At the opposite end, CIRCLE 128,88,1.23 will draw a circle as a perfect 3x3
;   square using 4 straight lines although very small circles are just drawn as
;   a dot on the screen.
;
;   The first chord drawn is the vertical chord on the right of the circle.
;   The starting point is at the base of this chord which is drawn upwards and
;   the circle continues in an anti-clockwise direction. As noted earlier the
;   x-coordinate of this point measured from the centre of the circle is the
;   radius.
;
;   The CIRCLE command makes extensive use of the calculator and as part of
;   process of drawing a large circle, free memory is checked 1315 times.
;   When drawing a large arc, free memory is checked 928 times.
;   A single call to 'sin' involves 63 memory checks and so values of sine
;   and cosine are pre-calculated and held in the mem locations. As a
;   clever trick 'cos' is derived from 'sin' using simple arithmetic operations
;   instead of the more expensive 'cos' function.
;
;   Initially, the syntax has been partly checked using the class for the DRAW
;   command which stacks the origin of the circle (X,Y).

;; CIRCLE
o2320:		RST	18H		; GET-CHAR              x, y.
		CP	$2C		; Is character the required comma ?
		JP	NZ,o1C8A	; Jump, if not, to REPORT-C
					; 'Nonsense in basic'

		RST	20H		; NEXT-CHAR advances the parsed character address.
		CALL	o1C82		; routine EXPT-1NUM stacks radius in runtime.
		CALL	o1BEE		; routine CHECK-END will return here in runtime
					; if nothing follows the command.

;   Now make the radius positive and ensure that it is in floating point form
;   so that the exponent byte can be accessed for quick testing.

		RST	28H		; FP-CALC              x, y, r.
		DB	$2A		; abs                   x, y, r.
		DB	$3D		; re-stack              x, y, r.
		DB	$38		; end-calc              x, y, r.

		LD	A,(HL)		; Fetch first, floating-point, exponent byte.
		CP	$81		; Compare to one.
		JR	NC,o233B	; Forward to C-R-GRE-1
					; if circle radius is greater than one.

;    The circle is no larger than a single pixel so delete the radius from the
;    calculator stack and plot a point at the centre.

		RST	28H		; FP-CALC              x, y, r.
		DB	$02		; delete                x, y.
		DB	$38		; end-calc              x, y.

		JR	o22DC		; back to PLOT routine to just plot x,y.

; ---

;   Continue when the circle's radius measures greater than one by forming
;   the angle 2 * PI radians which is 360 degrees.

;; C-R-GRE-1
o233B:		RST	28H		; FP-CALC      x, y, r
		DB	$A3		; stk-pi/2      x, y, r, pi/2.
		DB	$38		; end-calc      x, y, r, pi/2.

;   Change the exponent of pi/2 from $81 to $83 giving 2*PI the central angle.
;   This is quicker than multiplying by four.

		LD	(HL),$83	;               x, y, r, 2*PI.

;   Now store this important constant in mem-5 and delete so that other
;   parameters can be derived from it, by a routine shared with DRAW.

		RST	28H		; FP-CALC      x, y, r, 2*PI.
		DB	$C5		; st-mem-5      store 2*PI in mem-5
		DB	$02		; delete        x, y, r.
		DB	$38		; end-calc      x, y, r.

;   The parameters derived from mem-5 (A) and from the radius are set up in
;   four of the other mem locations by the CIRCLE DRAW PARAMETERS routine which
;   also returns the number of straight lines in the B register.

		CALL	o247D		; routine CD-PRMS1

; mem-0 ; A/No of lines (=a)            unused
; mem-1 ; sin(a/2)  will be moving x    var
; mem-2 ; -         will be moving y    var
; mem-3 ; cos(a)                        const
; mem-4 ; sin(a)                        const
; mem-5 ; Angle of rotation (A) (2*PI)  const
; B     ; Number of straight lines.

		PUSH	BC		; Preserve the number of lines in B.

;   Next calculate the length of half a chord by multiplying the sine of half
;   the central angle by the radius of the circle.

		RST	28H		; FP-CALC      x, y, r.
		DB	$31		; duplicate     x, y, r, r.
		DB	$E1		; get-mem-1     x, y, r, r, sin(a/2).
		DB	$04		; multiply      x, y, r, half-chord.
		DB	$38		; end-calc      x, y, r, half-chord.

		LD	A,(HL)		; fetch exponent  of the half arc to A.
		CP	$80		; compare to a half pixel
		JR	NC,o235A	; forward, if greater than .5, to C-ARC-GE1

;   If the first line is less than .5 then 4 'lines' would be drawn on the same
;   spot so tidy the calculator stack and machine stack and plot the centre.

		RST	28H		; FP-CALC      x, y, r, hc.
		DB	$02		; delete        x, y, r.
		DB	$02		; delete        x, y.
		DB	$38		; end-calc      x, y.

		POP	BC		; Balance machine stack by taking chord-count.

		JP	o22DC		; JUMP to PLOT

; ---

;   The arc is greater than 0.5 so the circle can be drawn.

;; C-ARC-GE1
o235A:		RST	28H		; FP-CALC      x, y, r, hc.
		DB	$C2		; st-mem-2      x, y, r, half chord to mem-2.
		DB	$01		; exchange      x, y, hc, r.
		DB	$C0		; st-mem-0      x, y, hc, r.
		DB	$02		; delete        x, y, hc.

;   Subtract the length of the half-chord from the absolute y coordinate to
;   give the starting y coordinate sy.
;   Note that for a circle this is also the end coordinate.

		DB	$03		; subtract      x, y-hc.  (The start y-coord)
		DB	$01		; exchange      sy, x.

;   Next simply add the radius to the x coordinate to give a fuzzy x-coordinate.
;   Strictly speaking, the radius should be multiplied by cos(a/2) first but
;   doing it this way makes the circle slightly larger.

		DB	$E0		; get-mem-0     sy, x, r.
		DB	$0F		; addition      sy, x+r.  (The start x-coord)

;   We now want three copies of this pair of values on the calculator stack.
;   The first pair remain on the stack throughout the circle routine and are
;   the end points. The next pair will be the moving absolute values of x and y
;   that are updated after each line is drawn. The final pair will be loaded
;   into the COORDS system variable so that the first vertical line starts at
;   the right place.

		DB	$C0		; st-mem-0      sy, sx.
		DB	$01		; exchange      sx, sy.
		DB	$31		; duplicate     sx, sy, sy.
		DB	$E0		; get-mem-0     sx, sy, sy, sx.
		DB	$01		; exchange      sx, sy, sx, sy.
		DB	$31		; duplicate     sx, sy, sx, sy, sy.
		DB	$E0		; get-mem-0     sx, sy, sx, sy, sy, sx.

;   Locations mem-1 and mem-2 are the relative x and y values which are updated
;   after each line is drawn. Since we are drawing a vertical line then the rx
;   value in mem-1 is zero and the ry value in mem-2 is the full chord.

		DB	$A0		; stk-zero      sx, sy, sx, sy, sy, sx, 0.
		DB	$C1		; st-mem-1      sx, sy, sx, sy, sy, sx, 0.
		DB	$02		; delete        sx, sy, sx, sy, sy, sx.

;   Although the three pairs of x/y values are the same for a circle, they
;   will be labelled terminating, absolute and start coordinates.

		DB	$38		; end-calc      tx, ty, ax, ay, sy, sx.

;   Use the exponent manipulating trick again to double the value of mem-2.

		INC	(IY+$62)	; Increment MEM-2-1st doubling half chord.

;   Note. this first vertical chord is drawn at the radius so circles are
;   slightly dispoaced to the right.
;   It is only necessary to place the values (sx) and (sy) in the system
;   variable COORDS to ensure that drawing commences at the correct pixel.
;   Note. a couple of LD (COORDS),A instructions would have been quicker, and
;   simpler, than using LD (COORDS),HL.

		CALL	o1E94		; routine FIND-INT1 fetches sx from stack to A.

		LD	L,A		; place X value in L.
		PUSH	HL		; save the holding register.

		CALL	o1E94		; routine FIND-INT1 fetches sy to A

		POP	HL		; restore the holding register.
		LD	H,A		; and place y value in high byte.

		LD	(COORDS),HL	; Update the COORDS system variable.
					;
					;               tx, ty, ax, ay.

		POP	BC		; restore the chord count
					; values 4,8,12,16,20,24,28 or 32.

		JP	o2420		; forward to DRW-STEPS
					;               tx, ty, ax, ay.

;   Note. the jump to DRW-STEPS is just to decrement B and jump into the
;   middle of the arc-drawing loop. The arc count which includes the first
;   vertical arc draws one less than the perceived number of arcs.
;   The final arc offsets are obtained by subtracting the final COORDS value
;   from the initial sx and sy values which are kept at the base of the
;   calculator stack throughout the arc loop.
;   This ensures that the final line finishes exactly at the starting pixel
;   removing the possibility of any inaccuracy.
;   Since the initial sx and sy values are not required until the final arc
;   is drawn, they are not shown until then.
;   As the calculator stack is quite busy, only the active parts are shown in
;   each section.


; ------------------
; THE 'DRAW' COMMAND
; ------------------
;   The Spectrum's DRAW command is overloaded and can take two parameters sets.
;
;   With two parameters, it simply draws an approximation to a straight line
;   at offset x,y using the LINE-DRAW routine.
;
;   With three parameters, an arc is drawn to the point at offset x,y turning
;   through an angle, in radians, supplied by the third parameter.
;   The arc will consist of 4 to 252 straight lines each one of which is drawn
;   by calls to the DRAW-LINE routine.

;; DRAW
o2382:		RST	18H		; GET-CHAR
		CP	$2C		; is it the comma character ?
		JR	Z,o238D		; forward, if so, to DR-3-PRMS

;   There are two parameters e.g. DRAW 255,175

		CALL	o1BEE		; routine CHECK-END

		JP	o2477		; jump forward to LINE-DRAW

; ---

;    There are three parameters e.g. DRAW 255, 175, .5
;    The first two are relative coordinates and the third is the angle of
;    rotation in radians (A).

;; DR-3-PRMS
o238D:		RST	20H		; NEXT-CHAR skips over the 'comma'.

		CALL	o1C82		; routine EXPT-1NUM stacks the rotation angle.

		CALL	o1BEE		; routine CHECK-END

;   Now enter the calculator and store the complete rotation angle in mem-5

		RST	28H		; FP-CALC      x, y, A.
		DB	$C5		; st-mem-5      x, y, A.

;   Test the angle for the special case of 360 degrees.

		DB	$A2		; stk-half      x, y, A, 1/2.
		DB	$04		; multiply      x, y, A/2.
		DB	$1F		; sin           x, y, sin(A/2).
		DB	$31		; duplicate     x, y, sin(A/2),sin(A/2)
		DB	$30		; not           x, y, sin(A/2), (0/1).
		DB	$30		; not           x, y, sin(A/2), (1/0).
		DB	$00		; jump-true     x, y, sin(A/2).

		DB	$06		; forward to o23A3, DR-SIN-NZ
					; if sin(r/2) is not zero.

;   The third parameter is 2*PI (or a multiple of 2*PI) so a 360 degrees turn
;   would just be a straight line.  Eliminating this case here prevents
;   division by zero at later stage.

		DB	$02		; delete        x, y.
		DB	$38		; end-calc      x, y.

		JP	o2477		; forward to LINE-DRAW

; ---

;   An arc can be drawn.

;; DR-SIN-NZ
o23A3:		DB	$C0		; st-mem-0      x, y, sin(A/2).   store mem-0
		DB	$02		; delete        x, y.

;   The next step calculates (roughly) the diameter of the circle of which the
;   arc will form part.  This value does not have to be too accurate as it is
;   only used to evaluate the number of straight lines and then discarded.
;   After all for a circle, the radius is used. Consequently, a circle of
;   radius 50 will have 24 straight lines but an arc of radius 50 will have 20
;   straight lines - when drawn in any direction.
;   So that simple arithmetic can be used, the length of the chord can be
;   calculated as X+Y rather than by Pythagoras Theorem and the sine of the
;   nearest angle within reach is used.

		DB	$C1		; st-mem-1      x, y.             store mem-1
		DB	$02		; delete        x.

		DB	$31		; duplicate     x, x.
		DB	$2A		; abs           x, x (+ve).
		DB	$E1		; get-mem-1     x, X, y.
		DB	$01		; exchange      x, y, X.
		DB	$E1		; get-mem-1     x, y, X, y.
		DB	$2A		; abs           x, y, X, Y (+ve).
		DB	$0F		; addition      x, y, X+Y.
		DB	$E0		; get-mem-0     x, y, X+Y, sin(A/2).
		DB	$05		; division      x, y, X+Y/sin(A/2).
		DB	$2A		; abs           x, y, X+Y/sin(A/2) = D.

;    Bring back sin(A/2) from mem-0 which will shortly get trashed.
;    Then bring D to the top of the stack again.

		DB	$E0		; get-mem-0     x, y, D, sin(A/2).
		DB	$01		; exchange      x, y, sin(A/2), D.

;   Note. that since the value at the top of the stack has arisen as a result
;   of division then it can no longer be in integer form and the next re-stack
;   is unnecessary. Only the Sinclair ZX80 had integer division.

		DB	$3D		; re-stack      (unnecessary)

		DB	$38		; end-calc      x, y, sin(A/2), D.

;   The next test avoids drawing 4 straight lines when the start and end pixels
;   are adjacent (or the same) but is probably best dispensed with.

		LD	A,(HL)		; fetch exponent byte of D.
		CP	$81		; compare to 1
		JR	NC,o23C1	; forward, if > 1,  to DR-PRMS

;   else delete the top two stack values and draw a simple straight line.

		RST	28H		; FP-CALC
		DB	$02		; delete
		DB	$02		; delete
		DB	$38		; end-calc      x, y.

		JP	o2477		; to LINE-DRAW

; ---

;   The ARC will consist of multiple straight lines so call the CIRCLE-DRAW
;   PARAMETERS ROUTINE to pre-calculate sine values from the angle (in mem-5)
;   and determine also the number of straight lines from that value and the
;   'diameter' which is at the top of the calculator stack.

;; DR-PRMS
o23C1:		CALL	o247D		; routine CD-PRMS1

; mem-0 ; (A)/No. of lines (=a) (step angle)
; mem-1 ; sin(a/2)
; mem-2 ; -
; mem-3 ; cos(a)                        const
; mem-4 ; sin(a)                        const
; mem-5 ; Angle of rotation (A)         in
; B     ; Count of straight lines - max 252.

		PUSH	BC		; Save the line count on the machine stack.

;   Remove the now redundant diameter value D.

		RST	28H		; FP-CALC      x, y, sin(A/2), D.
		DB	$02		; delete        x, y, sin(A/2).

;   Dividing the sine of the step angle by the sine of the total angle gives
;   the length of the initial chord on a unary circle. This factor f is used
;   to scale the coordinates of the first line which still points in the
;   direction of the end point and may be larger.

		DB	$E1		; get-mem-1     x, y, sin(A/2), sin(a/2)
		DB	$01		; exchange      x, y, sin(a/2), sin(A/2)
		DB	$05		; division      x, y, sin(a/2)/sin(A/2)
		DB	$C1		; st-mem-1      x, y. f.
		DB	$02		; delete        x, y.

;   With the factor stored, scale the x coordinate first.

		DB	$01		; exchange      y, x.
		DB	$31		; duplicate     y, x, x.
		DB	$E1		; get-mem-1     y, x, x, f.
		DB	$04		; multiply      y, x, x*f    (=xx)
		DB	$C2		; st-mem-2      y, x, xx.
		DB	$02		; delete        y. x.

;   Now scale the y coordinate.

		DB	$01		; exchange      x, y.
		DB	$31		; duplicate     x, y, y.
		DB	$E1		; get-mem-1     x, y, y, f
		DB	$04		; multiply      x, y, y*f    (=yy)

;   Note. 'sin' and 'cos' trash locations mem-0 to mem-2 so fetch mem-2 to the
;   calculator stack for safe keeping.

		DB	$E2		; get-mem-2     x, y, yy, xx.

;   Once we get the coordinates of the first straight line then the 'ROTATION
;   FORMULA' used in the arc loop will take care of all other points, but we
;   now use a variation of that formula to rotate the first arc through (A-a)/2
;   radians.
;
;       xRotated = y * sin(angle) + x * cos(angle)
;       yRotated = y * cos(angle) - x * sin(angle)
;

		DB	$E5		; get-mem-5     x, y, yy, xx, A.
		DB	$E0		; get-mem-0     x, y, yy, xx, A, a.
		DB	$03		; subtract      x, y, yy, xx, A-a.
		DB	$A2		; stk-half      x, y, yy, xx, A-a, 1/2.
		DB	$04		; multiply      x, y, yy, xx, (A-a)/2. (=angle)
		DB	$31		; duplicate     x, y, yy, xx, angle, angle.
		DB	$1F		; sin           x, y, yy, xx, angle, sin(angle)
		DB	$C5		; st-mem-5      x, y, yy, xx, angle, sin(angle)
		DB	$02		; delete        x, y, yy, xx, angle

		DB	$20		; cos           x, y, yy, xx, cos(angle).

;   Note. mem-0, mem-1 and mem-2 can be used again now...

		DB	$C0		; st-mem-0      x, y, yy, xx, cos(angle).
		DB	$02		; delete        x, y, yy, xx.

		DB	$C2		; st-mem-2      x, y, yy, xx.
		DB	$02		; delete        x, y, yy.

		DB	$C1		; st-mem-1      x, y, yy.
		DB	$E5		; get-mem-5     x, y, yy, sin(angle)
		DB	$04		; multiply      x, y, yy*sin(angle).
		DB	$E0		; get-mem-0     x, y, yy*sin(angle), cos(angle)
		DB	$E2		; get-mem-2     x, y, yy*sin(angle), cos(angle), xx.
		DB	$04		; multiply      x, y, yy*sin(angle), xx*cos(angle).
		DB	$0F		; addition      x, y, xRotated.
		DB	$E1		; get-mem-1     x, y, xRotated, yy.
		DB	$01		; exchange      x, y, yy, xRotated.
		DB	$C1		; st-mem-1      x, y, yy, xRotated.
		DB	$02		; delete        x, y, yy.

		DB	$E0		; get-mem-0     x, y, yy, cos(angle).
		DB	$04		; multiply      x, y, yy*cos(angle).
		DB	$E2		; get-mem-2     x, y, yy*cos(angle), xx.
		DB	$E5		; get-mem-5     x, y, yy*cos(angle), xx, sin(angle).
		DB	$04		; multiply      x, y, yy*cos(angle), xx*sin(angle).
		DB	$03		; subtract      x, y, yRotated.
		DB	$C2		; st-mem-2      x, y, yRotated.

;   Now the initial x and y coordinates are made positive and summed to see
;   if they measure up to anything significant.

		DB	$2A		; abs           x, y, yRotated'.
		DB	$E1		; get-mem-1     x, y, yRotated', xRotated.
		DB	$2A		; abs           x, y, yRotated', xRotated'.
		DB	$0F		; addition      x, y, yRotated+xRotated.
		DB	$02		; delete        x, y.

		DB	$38		; end-calc      x, y.

;   Although the test value has been deleted it is still above the calculator
;   stack in memory and conveniently DE which points to the first free byte
;   addresses the exponent of the test value.

		LD	A,(DE)		; Fetch exponent of the length indicator.
		CP	$81		; Compare to that for 1

		POP	BC		; Balance the machine stack

		JP	C,o2477		; forward, if the coordinates of first line
					; don't add up to more than 1, to LINE-DRAW

;   Continue when the arc will have a discernable shape.

		PUSH	BC		; Restore line counter to the machine stack.

;   The parameters of the DRAW command were relative and they are now converted
;   to absolute coordinates by adding to the coordinates of the last point
;   plotted. The first two values on the stack are the terminal tx and ty
;   coordinates.  The x-coordinate is converted first but first the last point
;   plotted is saved as it will initialize the moving ax, value.

		RST	28H		; FP-CALC      x, y.
		DB	$01		; exchange      y, x.
		DB	$38		; end-calc      y, x.

		LD	A,(COORDS)	; Fetch System Variable COORDS-x
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC      y, x, last-x.

;   Store the last point plotted to initialize the moving ax value.

		DB	$C0		; st-mem-0      y, x, last-x.
		DB	$0F		; addition      y, absolute x.
		DB	$01		; exchange      tx, y.
		DB	$38		; end-calc      tx, y.

		LD	A,($5C7E)	; Fetch System Variable COORDS-y
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC      tx, y, last-y.

;   Store the last point plotted to initialize the moving ay value.

		DB	$C5		; st-mem-5      tx, y, last-y.
		DB	$0F		; addition      tx, ty.

;   Fetch the moving ax and ay to the calculator stack.

		DB	$E0		; get-mem-0     tx, ty, ax.
		DB	$E5		; get-mem-5     tx, ty, ax, ay.
		DB	$38		; end-calc      tx, ty, ax, ay.

		POP	BC		; Restore the straight line count.

; -----------------------------------
; THE 'CIRCLE/DRAW CONVERGENCE POINT'
; -----------------------------------
;   The CIRCLE and ARC-DRAW commands converge here.
;
;   Note. for both the CIRCLE and ARC commands the minimum initial line count
;   is 4 (as set up by the CD_PARAMS routine) and so the zero flag will never
;   be set and the loop is always entered.  The first test is superfluous and
;   the jump will always be made to ARC-START.

;; DRW-STEPS
o2420:		DEC	B		; decrement the arc count (4,8,12,16...).

		JR	Z,o245F		; forward, if zero (not possible), to ARC-END

		JR	o2439		; forward to ARC-START

; --------------
; THE 'ARC LOOP'
; --------------
;
;   The arc drawing loop will draw up to 31 straight lines for a circle and up
;   251 straight lines for an arc between two points. In both cases the final
;   closing straight line is drawn at ARC_END, but it otherwise loops back to
;   here to calculate the next coordinate using the ROTATION FORMULA where (a)
;   is the previously calculated, constant CENTRAL ANGLE of the arcs.
;
;       Xrotated = x * cos(a) - y * sin(a)
;       Yrotated = x * sin(a) + y * cos(a)
;
;   The values cos(a) and sin(a) are pre-calculated and held in mem-3 and mem-4
;   for the duration of the routine.
;   Memory location mem-1 holds the last relative x value (rx) and mem-2 holds
;   the last relative y value (ry) used by DRAW.
;
;   Note. that this is a very clever twist on what is after all a very clever,
;   well-used formula.  Normally the rotation formula is used with the x and y
;   coordinates from the centre of the circle (or arc) and a supplied angle to
;   produce two new x and y coordinates in an anticlockwise direction on the
;   circumference of the circle.
;   What is being used here, instead, is the relative X and Y parameters from
;   the last point plotted that are required to get to the current point and
;   the formula returns the next relative coordinates to use.

;; ARC-LOOP
o2425:		RST	28H		; FP-CALC
		DB	$E1		; get-mem-1     rx.
		DB	$31		; duplicate     rx, rx.
		DB	$E3		; get-mem-3     cos(a)
		DB	$04		; multiply      rx, rx*cos(a).
		DB	$E2		; get-mem-2     rx, rx*cos(a), ry.
		DB	$E4		; get-mem-4     rx, rx*cos(a), ry, sin(a).
		DB	$04		; multiply      rx, rx*cos(a), ry*sin(a).
		DB	$03		; subtract      rx, rx*cos(a) - ry*sin(a)
		DB	$C1		; st-mem-1      rx, new relative x rotated.
		DB	$02		; delete        rx.

		DB	$E4		; get-mem-4     rx, sin(a).
		DB	$04		; multiply      rx*sin(a)
		DB	$E2		; get-mem-2     rx*sin(a), ry.
		DB	$E3		; get-mem-3     rx*sin(a), ry, cos(a).
		DB	$04		; multiply      rx*sin(a), ry*cos(a).
		DB	$0F		; addition      rx*sin(a) + ry*cos(a).
		DB	$C2		; st-mem-2      new relative y rotated.
		DB	$02		; delete        .
		DB	$38		; end-calc      .

;   Note. the calculator stack actually holds   tx, ty, ax, ay
;   and the last absolute values of x and y
;   are now brought into play.
;
;   Magically, the two new rotated coordinates rx and ry are all that we would
;   require to draw a circle or arc - on paper!
;   The Spectrum DRAW routine draws to the rounded x and y coordinate and so
;   repetitions of values like 3.49 would mean that the fractional parts
;   would be lost until eventually the draw coordinates might differ from the
;   floating point values used above by several pixels.
;   For this reason the accurate offsets calculated above are added to the
;   accurate, absolute coordinates maintained in ax and ay and these new
;   coordinates have the integer coordinates of the last plot position
;   ( from System Variable COORDS ) subtracted from them to give the relative
;   coordinates required by the DRAW routine.

;   The mid entry point.

;; ARC-START
o2439:		PUSH	BC		; Preserve the arc counter on the machine stack.

;   Store the absolute ay in temporary variable mem-0 for the moment.

		RST	28H		; FP-CALC      ax, ay.
		DB	$C0		; st-mem-0      ax, ay.
		DB	$02		; delete        ax.

;   Now add the fractional relative x coordinate to the fractional absolute
;   x coordinate to obtain a new fractional x-coordinate.

		DB	$E1		; get-mem-1     ax, xr.
		DB	$0F		; addition      ax+xr (= new ax).
		DB	$31		; duplicate     ax, ax.
		DB	$38		; end-calc      ax, ax.

		LD	A,(COORDS)	; COORDS-x      last x    (integer ix 0-255)
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC      ax, ax, ix.
		DB	$03		; subtract      ax, ax-ix  = relative DRAW Dx.

;   Having calculated the x value for DRAW do the same for the y value.

		DB	$E0		; get-mem-0     ax, Dx, ay.
		DB	$E2		; get-mem-2     ax, Dx, ay, ry.
		DB	$0F		; addition      ax, Dx, ay+ry (= new ay).
		DB	$C0		; st-mem-0      ax, Dx, ay.
		DB	$01		; exchange      ax, ay, Dx,
		DB	$E0		; get-mem-0     ax, ay, Dx, ay.
		DB	$38		; end-calc      ax, ay, Dx, ay.

		LD	A,($5C7E)	; COORDS-y      last y (integer iy 0-175)
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC      ax, ay, Dx, ay, iy.
		DB	$03		; subtract      ax, ay, Dx, ay-iy ( = Dy).
		DB	$38		; end-calc      ax, ay, Dx, Dy.

		CALL	o24B7		; Routine DRAW-LINE draws (Dx,Dy) relative to
					; the last pixel plotted leaving absolute x
					; and y on the calculator stack.
					;               ax, ay.

		POP	BC		; Restore the arc counter from the machine stack.

		DJNZ	o2425		; Decrement and loop while > 0 to ARC-LOOP

; -------------
; THE 'ARC END'
; -------------

;   To recap the full calculator stack is       tx, ty, ax, ay.

;   Just as one would do if drawing the curve on paper, the final line would
;   be drawn by joining the last point plotted to the initial start point
;   in the case of a CIRCLE or to the calculated end point in the case of
;   an ARC.
;   The moving absolute values of x and y are no longer required and they
;   can be deleted to expose the closing coordinates.

;; ARC-END
o245F:		RST	28H		; FP-CALC      tx, ty, ax, ay.
		DB	$02		; delete        tx, ty, ax.
		DB	$02		; delete        tx, ty.
		DB	$01		; exchange      ty, tx.
		DB	$38		; end-calc      ty, tx.

;   First calculate the relative x coordinate to the end-point.

		LD	A,(COORDS)	; COORDS-x
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC      ty, tx, coords_x.
		DB	$03		; subtract      ty, rx.

;   Next calculate the relative y coordinate to the end-point.

		DB	$01		; exchange      rx, ty.
		DB	$38		; end-calc      rx, ty.

		LD	A,($5C7E)	; COORDS-y
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC      rx, ty, coords_y
		DB	$03		; subtract      rx, ry.
		DB	$38		; end-calc      rx, ry.

;   Finally draw the last straight line.

;; LINE-DRAW
o2477:		CALL	o24B7		; routine DRAW-LINE draws to the relative
					; coordinates (rx, ry).

		JP	o0D4D		; jump back and exit via TEMPS          >>>


; --------------------------------------------
; THE 'INITIAL CIRCLE/DRAW PARAMETERS' ROUTINE
; --------------------------------------------
;   Begin by calculating the number of chords which will be returned in B.
;   A rule of thumb is employed that uses a value z which for a circle is the
;   radius and for an arc is the diameter with, as it happens, a pinch more if
;   the arc is on a slope.
;
;   NUMBER OF STRAIGHT LINES = ANGLE OF ROTATION * SQUARE ROOT ( Z ) / 2

;; CD-PRMS1
o247D:		RST	28H		; FP-CALC      z.
		DB	$31		; duplicate     z, z.
		DB	$28		; sqr           z, sqr(z).
		DB	$34		; stk-data      z, sqr(z), 2.
		DB	$32		; Exponent: $82, Bytes: 1
		DB	$00		; (+00,+00,+00)
		DB	$01		; exchange      z, 2, sqr(z).
		DB	$05		; division      z, 2/sqr(z).
		DB	$E5		; get-mem-5     z, 2/sqr(z), ANGLE.
		DB	$01		; exchange      z, ANGLE, 2/sqr (z)
		DB	$05		; division      z, ANGLE*sqr(z)/2 (= No. of lines)
		DB	$2A		; abs           (for arc only)
		DB	$38		; end-calc      z, number of lines.

;    As an example for a circle of radius 87 the number of lines will be 29.

		CALL	o2DD5		; routine FP-TO-A

;    The value is compressed into A register, no carry with valid circle.

		JR	C,o2495		; forward, if over 256, to USE-252

;    now make a multiple of 4 e.g. 29 becomes 28

		AND	$FC		; AND 252

;    Adding 4 could set carry for arc, for the circle example, 28 becomes 32.

		ADD	A,$04		; adding 4 could set carry if result is 256.

		JR	NC,o2497	; forward if less than 256 to DRAW-SAVE

;    For an arc, a limit of 252 is imposed.

;; USE-252
o2495:		LD	A,$FC		; Use a value of 252 (for arc).


;   For both arcs and circles, constants derived from the central angle are
;   stored in the 'mem' locations.  Some are not relevant for the circle.

;; DRAW-SAVE
o2497:		PUSH	AF		; Save the line count (A) on the machine stack.

		CALL	o2D28		; Routine STACK-A stacks the modified count(A).

		RST	28H		; FP-CALC      z, A.
		DB	$E5		; get-mem-5     z, A, ANGLE.
		DB	$01		; exchange      z, ANGLE, A.
		DB	$05		; division      z, ANGLE/A. (Angle/count = a)
		DB	$31		; duplicate     z, a, a.

;  Note. that cos (a) could be formed here directly using 'cos' and stored in
;  mem-3 but that would spoil a good story and be slightly slower, as also
;  would using square roots to form cos (a) from sin (a).

		DB	$1F		; sin           z, a, sin(a)
		DB	$C4		; st-mem-4      z, a, sin(a)
		DB	$02		; delete        z, a.
		DB	$31		; duplicate     z, a, a.
		DB	$A2		; stk-half      z, a, a, 1/2.
		DB	$04		; multiply      z, a, a/2.
		DB	$1F		; sin           z, a, sin(a/2).

;   Note. after second sin, mem-0 and mem-1 become free.

		DB	$C1		; st-mem-1      z, a, sin(a/2).
		DB	$01		; exchange      z, sin(a/2), a.
		DB	$C0		; st-mem-0      z, sin(a/2), a.  (for arc only)

;   Now form cos(a) from sin(a/2) using the 'DOUBLE ANGLE FORMULA'.

		DB	$02		; delete        z, sin(a/2).
		DB	$31		; duplicate     z, sin(a/2), sin(a/2).
		DB	$04		; multiply      z, sin(a/2)*sin(a/2).
		DB	$31		; duplicate     z, sin(a/2)*sin(a/2),
					;;                           sin(a/2)*sin(a/2).
		DB	$0F		; addition      z, 2*sin(a/2)*sin(a/2).
		DB	$A1		; stk-one       z, 2*sin(a/2)*sin(a/2), 1.
		DB	$03		; subtract      z, 2*sin(a/2)*sin(a/2)-1.

		DB	$1B		; negate        z, 1-2*sin(a/2)*sin(a/2).

		DB	$C3		; st-mem-3      z, cos(a).
		DB	$02		; delete        z.
		DB	$38		; end-calc      z.

;   The radius/diameter is left on the calculator stack.

		POP	BC		; Restore the line count to the B register.

		RET			; Return.

; --------------------------
; THE 'DOUBLE ANGLE FORMULA'
; --------------------------
;   This formula forms cos(a) from sin(a/2) using simple arithmetic.
;
;   THE GEOMETRIC PROOF OF FORMULA   cos (a) = 1 - 2 * sin(a/2) * sin(a/2)
;
;
;                                            A
;
;                                         . /|\
;                                     .    / | \
;                                  .      /  |  \
;                               .        /   |a/2\
;                            .          /    |    \
;                         .          1 /     |     \
;                      .              /      |      \
;                   .                /       |       \
;                .                  /        |        \
;             .  a/2             D / a      E|-+       \
;          B ---------------------/----------+-+--------\ C
;            <-         1       -><-       1           ->
;
;   cos a = 1 - 2 * sin(a/2) * sin(a/2)
;
;   The figure shows a right triangle that inscribes a circle of radius 1 with
;   centre, or origin, D.  Line BC is the diameter of length 2 and A is a point
;   on the circle. The periphery angle BAC is therefore a right angle by the
;   Rule of Thales.
;   Line AC is a chord touching two points on the circle and the angle at the
;   centre is (a).
;   Since the vertex of the largest triangle B touches the circle, the
;   inscribed angle (a/2) is half the central angle (a).
;   The cosine of (a) is the length DE as the hypotenuse is of length 1.
;   This can also be expressed as 1-length CE.  Examining the triangle at the
;   right, the top angle is also (a/2) as angle BAE and EBA add to give a right
;   angle as do BAE and EAC.
;   So cos (a) = 1 - AC * sin(a/2)
;   Looking at the largest triangle, side AC can be expressed as
;   AC = 2 * sin(a/2)   and so combining these we get
;   cos (a) = 1 - 2 * sin(a/2) * sin(a/2).
;
;   "I will be sufficiently rewarded if when telling it to others, you will
;    not claim the discovery as your own, but will say it is mine."
;   - Thales, 640 - 546 B.C.
;
; --------------------------
; THE 'LINE DRAWING' ROUTINE
; --------------------------
;
;

;; DRAW-LINE
o24B7:		CALL	o2307		; routine STK-TO-BC
		LD	A,C		;
		CP	B		;
		JR	NC,o24C4	; to DL-X-GE-Y

		LD	L,C		;
		PUSH	DE		;
		XOR	A		;
		LD	E,A		;
		JR	o24CB		; to DL-LARGER

; ---

;; DL-X-GE-Y
o24C4:		OR	C		;
		RET	Z		;

		LD	L,B		;
		LD	B,C		;
		PUSH	DE		;
		LD	D,$00		;

;; DL-LARGER
o24CB:		LD	H,B		;
		LD	A,B		;
		RRA			;

;; D-L-LOOP
o24CE:		ADD	A,L		;
		JR	C,o24D4		; to D-L-DIAG

		CP	H		;
		JR	C,o24DB		; to D-L-HR-VT

;; D-L-DIAG
o24D4:		SUB	H		;
		LD	C,A		;
		EXX			;
		POP	BC		;
		PUSH	BC		;
		JR	o24DF		; to D-L-STEP

; ---

;; D-L-HR-VT
o24DB:		LD	C,A		;
		PUSH	DE		;
		EXX			;
		POP	BC		;

;; D-L-STEP
o24DF:		LD	HL,(COORDS)	; COORDS
		LD	A,B		;
		ADD	A,H		;
		LD	B,A		;
		LD	A,C		;
		INC	A		;
		ADD	A,L		;
		JR	C,o24F7		; to D-L-RANGE

		JR	Z,o24F9		; to REPORT-Bc

;; D-L-PLOT
o24EC:		DEC	A		;
		LD	C,A		;
		CALL	o22E5		; routine PLOT-SUB
		EXX			;
		LD	A,C		;
		DJNZ	o24CE		; to D-L-LOOP

		POP	DE		;
		RET			;

; ---

;; D-L-RANGE
o24F7:		JR	Z,o24EC		; to D-L-PLOT


;; REPORT-Bc
o24F9:		RST	08H		; ERROR-1
		DB	$0A		; Error Report: Integer out of range



;***********************************
;** Part 8. EXPRESSION EVALUATION **
;***********************************
;
; It is a this stage of the ROM that the Spectrum ceases altogether to be
; just a colourful novelty. One remarkable feature is that in all previous
; commands when the Spectrum is expecting a number or a string then an
; expression of the same type can be substituted ad infinitum.
; This is the routine that evaluates that expression.
; This is what causes 2 + 2 to give the answer 4.
; That is quite easy to understand. However you don't have to make it much
; more complex to start a remarkable juggling act.
; e.g. PRINT 2 * (VAL "2+2" + TAN 3)
; In fact, provided there is enough free RAM, the Spectrum can evaluate
; an expression of unlimited complexity.
; Apart from a couple of minor glitches, which you can now correct, the
; system is remarkably robust.


; ---------------------------------
; Scan expression or sub-expression
; ---------------------------------
;
;

;; SCANNING
o24FB:		RST	18H		; GET-CHAR
		LD	B,$00		; priority marker zero is pushed on stack
					; to signify end of expression when it is
					; popped off again.
		PUSH	BC		; put in on stack.
					; and proceed to consider the first character
					; of the expression.

;; S-LOOP-1
o24FF:		LD	C,A		; store the character while a look up is done.
		LD	HL,o2596	; Address: scan-func
		CALL	o16DC		; routine INDEXER is called to see if it is
					; part of a limited range '+', "(", 'ATTR' etc.

		LD	A,C		; fetch the character back
		JP	NC,o2684	; jump forward to S-ALPHNUM if not in primary
					; operators and functions to consider in the
					; first instance a digit or a variable and
					; then anything else.                >>>

		LD	B,$00		; but here if it was found in table so
		LD	C,(HL)		; fetch offset from table and make B zero.
		ADD	HL,BC		; add the offset to position found
		JP	(HL)		; and jump to the routine e.g. S-BIN
					; making an indirect exit from there.

; -------------------------------------------------------------------------
; The four service subroutines for routines in the scanning function table
; -------------------------------------------------------------------------

; PRINT """Hooray!"" he cried."

;; S-QUOTE-S
o250F:		CALL	o0074		; routine CH-ADD+1 points to next character
					; and fetches that character.
		INC	BC		; increase length counter.
		CP	$0D		; is it carriage return ?
					; inside a quote.
		JP	Z,o1C8A		; jump back to REPORT-C if so.
					; 'Nonsense in BASIC'.

		CP	$22		; is it a quote '"' ?
		JR	NZ,o250F	; back to S-QUOTE-S if not for more.

		CALL	o0074		; routine CH-ADD+1
		CP	$22		; compare with possible adjacent quote
		RET			; return. with zero set if two together.

; ---

; This subroutine is used to get two coordinate expressions for the three
; functions SCREEN$, ATTR and POINT that have two fixed parameters and
; therefore require surrounding braces.

;; S-2-COORD
o2522:		RST	20H		; NEXT-CHAR
		CP	$28		; is it the opening "(" ?
		JR	NZ,o252D	; forward to S-RPORT-C if not
					; 'Nonsense in BASIC'.

		CALL	o1C79		; routine NEXT-2NUM gets two comma-separated
					; numeric expressions. Note. this could cause
					; many more recursive calls to SCANNING but
					; the parent function will be evaluated fully
					; before rejoining the main juggling act.

		RST	18H		; GET-CHAR
		CP	$29		; is it the closing ")" ?

;; S-RPORT-C
o252D:		JP	NZ,o1C8A	; jump back to REPORT-C if not.
					; 'Nonsense in BASIC'.

; ------------
; Check syntax
; ------------
; This routine is called on a number of occasions to check if syntax is being
; checked or if the program is being run. To test the flag inline would use
; four bytes of code, but a call instruction only uses 3 bytes of code.

;; SYNTAX-Z
o2530:		BIT	7,(IY+$01)	; test FLAGS  - checking syntax only ?
		RET			; return.

; ----------------
; Scanning SCREEN$
; ----------------
; This function returns the code of a bit-mapped character at screen
; position at line C, column B. It is unable to detect the mosaic characters
; which are not bit-mapped but detects the ASCII 32 - 127 range.
; The bit-mapped UDGs are ignored which is curious as it requires only a
; few extra bytes of code. As usual, anything to do with CHARS is weird.
; If no match is found a null string is returned.
; No actual check on ranges is performed - that's up to the BASIC programmer.
; No real harm can come from SCREEN$(255,255) although the BASIC manual
; says that invalid values will be trapped.
; Interestingly, in the Pitman pocket guide, 1984, Vickers says that the
; range checking will be performed.

;; S-SCRN$-S
o2535:		CALL	o2307		; routine STK-TO-BC.
		LD	HL,(CHARS)	; fetch address of CHARS.
		LD	DE,$0100	; fetch offset to chr$ 32
		ADD	HL,DE		; and find start of bitmaps.
					; Note. not inc h. ??
		LD	A,C		; transfer line to A.
		RRCA			; multiply
		RRCA			; by
		RRCA			; thirty-two.
		AND	$E0		; and with 11100000
		XOR	B		; combine with column $00 - $1F
		LD	E,A		; to give the low byte of top line
		LD	A,C		; column to A range 00000000 to 00011111
		AND	$18		; and with 00011000
		XOR	$40		; xor with 01000000 (high byte screen start)
		LD	D,A		; register DE now holds start address of cell.
		LD	B,$60		; there are 96 characters in ASCII set.

;; S-SCRN-LP
o254F:		PUSH	BC		; save count
		PUSH	DE		; save screen start address
		PUSH	HL		; save bitmap start
		LD	A,(DE)		; first byte of screen to A
		XOR	(HL)		; xor with corresponding character byte
		JR	Z,o255A		; forward to S-SC-MTCH if they match
					; if inverse result would be $FF
					; if any other then mismatch

		INC	A		; set to $00 if inverse
		JR	NZ,o2573	; forward to S-SCR-NXT if a mismatch

		DEC	A		; restore $FF

; a match has been found so seven more to test.

;; S-SC-MTCH
o255A:		LD	C,A		; load C with inverse mask $00 or $FF
		LD	B,$07		; count seven more bytes

;; S-SC-ROWS
o255D:		INC	D		; increment screen address.
		INC	HL		; increment bitmap address.
		LD	A,(DE)		; byte to A
		XOR	(HL)		; will give $00 or $FF (inverse)
		XOR	C		; xor with inverse mask
		JR	NZ,o2573	; forward to S-SCR-NXT if no match.

		DJNZ	o255D		; back to S-SC-ROWS until all eight matched.

; continue if a match of all eight bytes was found

		POP	BC		; discard the
		POP	BC		; saved
		POP	BC		; pointers
		LD	A,$80		; the endpoint of character set
		SUB	B		; subtract the counter
					; to give the code 32-127
		LD	BC,$0001	; make one space in workspace.

		RST	30H		; BC-SPACES creates the space sliding
					; the calculator stack upwards.
		LD	(DE),A		; start is addressed by DE, so insert code
		JR	o257D		; forward to S-SCR-STO

; ---

; the jump was here if no match and more bitmaps to test.

;; S-SCR-NXT
o2573:		POP	HL		; restore the last bitmap start
		LD	DE,$0008	; and prepare to add 8.
		ADD	HL,DE		; now addresses next character bitmap.
		POP	DE		; restore screen address
		POP	BC		; and character counter in B
		DJNZ	o254F		; back to S-SCRN-LP if more characters.

		LD	C,B		; B is now zero, so BC now zero.

;; S-SCR-STO
o257D:		JP	o2AB2		; to STK-STO-$ to store the string in
					; workspace or a string with zero length.
					; (value of DE doesn't matter in last case)

; Note. this exit seems correct but the general-purpose routine S-STRING
; that calls this one will also stack any of its string results so this
; leads to a double storing of the result in this case.
; The instruction at o257D should just be a RET.
; credit Stephen Kelly and others, 1982.

; -------------
; Scanning ATTR
; -------------
; This function subroutine returns the attributes of a screen location -
; a numeric result.
; Again it's up to the BASIC programmer to supply valid values of line/column.

;; S-ATTR-S
o2580:		CALL	o2307		; routine STK-TO-BC fetches line to C,
					; and column to B.
		LD	A,C		; line to A $00 - $17   (max 00010111)
		RRCA			; rotate
		RRCA			; bits
		RRCA			; left.
		LD	C,A		; store in C as an intermediate value.

		AND	$E0		; pick up bits 11100000 ( was 00011100 )
		XOR	B		; combine with column $00 - $1F
		LD	L,A		; low byte now correct.

		LD	A,C		; bring back intermediate result from C
		AND	$03		; mask to give correct third of
					; screen $00 - $02
		XOR	$58		; combine with base address.
		LD	H,A		; high byte correct.
		LD	A,(HL)		; pick up the colour attribute.
		JP	o2D28		; forward to STACK-A to store result
					; and make an indirect exit.

; -----------------------
; Scanning function table
; -----------------------
; This table is used by INDEXER routine to find the offsets to
; four operators and eight functions. e.g. $A8 is the token 'FN'.
; This table is used in the first instance for the first character of an
; expression or by a recursive call to SCANNING for the first character of
; any sub-expression. It eliminates functions that have no argument or
; functions that can have more than one argument and therefore require
; braces. By eliminating and dealing with these now it can later take a
; simplistic approach to all other functions and assume that they have
; one argument.
; Similarly by eliminating BIN and '.' now it is later able to assume that
; all numbers begin with a digit and that the presence of a number or
; variable can be detected by a call to ALPHANUM.
; By default all expressions are positive and the spurious '+' is eliminated
; now as in print +2. This should not be confused with the operator '+'.
; Note. this does allow a degree of nonsense to be accepted as in
; PRINT +"3 is the greatest.".
; An acquired programming skill is the ability to include brackets where
; they are not necessary.
; A bracket at the start of a sub-expression may be spurious or necessary
; to denote that the contained expression is to be evaluated as an entity.
; In either case this is dealt with by recursive calls to SCANNING.
; An expression that begins with a quote requires special treatment.

;; scan-func
o2596:		DB	$22, o25B3-$-1	; $1C offset to S-QUOTE
		DB	"(", o25E8-$-1	; $4F offset to S-BRACKET
		DB	'.', o268D-$-1	; $F2 offset to S-DECIMAL
		DB	'+', o25AF-$-1	; $12 offset to S-U-PLUS

		DB	$A8, o25F5-$-1	; $56 offset to S-FN
		DB	$A5, o25F8-$-1	; $57 offset to S-RND
		DB	$A7, o2627-$-1	; $84 offset to S-PI
		DB	$A6, o2634-$-1	; $8F offset to S-INKEY$
		DB	$C4, o268D-$-1	; $E6 offset to S-BIN
		DB	$AA, o2668-$-1	; $BF offset to S-SCREEN$
		DB	$AB, o2672-$-1	; $C7 offset to S-ATTR
		DB	$A9, o267B-$-1	; $CE offset to S-POINT

		DB	$00		; zero end marker

; --------------------------
; Scanning function routines
; --------------------------
; These are the 11 subroutines accessed by the above table.
; S-BIN and S-DECIMAL are the same
; The 1-byte offset limits their location to within 255 bytes of their
; entry in the table.

; ->
;; S-U-PLUS
o25AF:		RST	20H		; NEXT-CHAR just ignore
		JP	o24FF		; to S-LOOP-1

; ---

; ->
;; S-QUOTE
o25B3:		RST	18H		; GET-CHAR
		INC	HL		; address next character (first in quotes)
		PUSH	HL		; save start of quoted text.
		LD	BC,$0000	; initialize length of string to zero.
		CALL	o250F		; routine S-QUOTE-S
		JR	NZ,o25D9	; forward to S-Q-PRMS if

;; S-Q-AGAIN
o25BE:		CALL	o250F		; routine S-QUOTE-S copies string until a
					; quote is encountered
		JR	Z,o25BE		; back to S-Q-AGAIN if two quotes WERE
					; together.

; but if just an isolated quote then that terminates the string.

		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o25D9		; forward to S-Q-PRMS if checking syntax.


		RST	30H		; BC-SPACES creates the space for true
					; copy of string in workspace.
		POP	HL		; re-fetch start of quoted text.
		PUSH	DE		; save start in workspace.

;; S-Q-COPY
o25CB:		LD	A,(HL)		; fetch a character from source.
		INC	HL		; advance source address.
		LD	(DE),A		; place in destination.
		INC	DE		; advance destination address.
		CP	$22		; was it a '"' just copied ?
		JR	NZ,o25CB	; back to S-Q-COPY to copy more if not

		LD	A,(HL)		; fetch adjacent character from source.
		INC	HL		; advance source address.
		CP	$22		; is this '"' ? - i.e. two quotes together ?
		JR	Z,o25CB		; to S-Q-COPY if so including just one of the
					; pair of quotes.

; proceed when terminating quote encountered.

;; S-Q-PRMS
o25D9:		DEC	BC		; decrease count by 1.
		POP	DE		; restore start of string in workspace.

;; S-STRING
o25DB:		LD	HL,FLAGS	; Address FLAGS system variable.
		RES	6,(HL)		; signal string result.
		BIT	7,(HL)		; is syntax being checked.
		CALL	NZ,o2AB2	; routine STK-STO-$ is called in runtime.
		JP	o2712		; jump forward to S-CONT-2          ===>

; ---

; ->
;; S-BRACKET
o25E8:		RST	20H		; NEXT-CHAR
		CALL	o24FB		; routine SCANNING is called recursively.
		CP	$29		; is it the closing ")" ?
		JP	NZ,o1C8A	; jump back to REPORT-C if not
					; 'Nonsense in BASIC'

		RST	20H		; NEXT-CHAR
		JP	o2712		; jump forward to S-CONT-2          ===>

; ---

; ->
;; S-FN
o25F5:		JP	o27BD		; jump forward to S-FN-SBRN.

; --------------------------------------------------------------------
;
;   RANDOM THEORY from the ZX81 manual by Steven Vickers
;
;   (same algorithm as the ZX Spectrum).
;
;   Chapter 5. Exercise 6. (For mathematicians only.)
;
;   Let p be a [large] prime, & let a be a primitive root modulo p.
;   Then if b_i is the residue of a^i modulo p (1<=b_i<p-1), the
;   sequence
;
;                           (b_i-1)/(p-1)
;
;   is a cyclical sequence of p-1 distinct numbers in the range 0 to 1
;   (excluding 1). By choosing a suitably, these can be made to look
;   fairly random.
;
;     65537 is a Mersenne prime 2^16-1. Note.
;
;   Use this, & Gauss' law of quadratic reciprocity, to show that 75
;   is a primitive root modulo 65537.
;
;     The ZX81 uses p=65537 & a=75, & stores some b_i-1 in memory.
;   The function RND involves replacing b_i-1 in memory by b_(i+1)-1,
;   & yielding the result (b_(i+1)-1)/(p-1). RAND n (with 1<=n<=65535)
;   makes b_i equal to n+1.
;
; --------------------------------------------------------------------
;
; Steven Vickers writing in comp.sys.sinclair on 20-DEC-1993
;
;   Note. (Of course, 65537 is 2^16 + 1, not -1.)
;
;   Consider arithmetic modulo a prime p. There are p residue classes, and the
;   non-zero ones are all invertible. Hence under multiplication they form a
;   group (Fp*, say) of order p-1; moreover (and not so obvious) Fp* is cyclic.
;   Its generators are the "primitive roots". The "quadratic residues modulo p"
;   are the squares in Fp*, and the "Legendre symbol" (d/p) is defined (when p
;   does not divide d) as +1 or -1, according as d is or is not a quadratic
;   residue mod p.
;
;   In the case when p = 65537, we can show that d is a primitive root if and
;   only if it's not a quadratic residue. For let w be a primitive root, d
;   congruent to w^r (mod p). If d is not primitive, then its order is a proper
;   factor of 65536: hence w^{32768*r} = 1 (mod p), so 65536 divides 32768*r,
;   and hence r is even and d is a square (mod p). Conversely, the squares in
;   Fp* form a subgroup of (Fp*)^2 of index 2, and so cannot be generators.
;
;   Hence to check whether 75 is primitive mod 65537, we want to calculate that
;   (75/65537) = -1. There is a multiplicative formula (ab/p) = (a/p)(b/p) (mod
;   p), so (75/65537) = (5/65537)^2 * (3/65537) = (3/65537). Now the law of
;   quadratic reciprocity says that if p and q are distinct odd primes, then
;
;    (p/q)(q/p) = (-1)^{(p-1)(q-1)/4}
;
;   Hence (3/65537) = (65537/3) * (-1)^{65536*2/4} = (65537/3)
;            = (2/3)  (because 65537 = 2 mod 3)
;            = -1
;
;   (I referred to Pierre Samuel's "Algebraic Theory of Numbers".)
;
; ->

;; S-RND
o25F8:		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o2625		; forward to S-RND-END if checking syntax.

		LD	BC,(SEED)	; fetch system variable SEED
		CALL	o2D2B		; routine STACK-BC places on calculator stack

		RST	28H		; FP-CALC           ;s.
		DB	$A1		; stk-one            ;s,1.
		DB	$0F		; addition           ;s+1.
		DB	$34		; stk-data           ;
		DB	$37		; Exponent: $87,
					;;Bytes: 1
		DB	$16		; (+00,+00,+00)      ;s+1,75.
		DB	$04		; multiply           ;(s+1)*75 = v
		DB	$34		; stk-data           ;v.
		DB	$80		; Bytes: 3
		DB	$41		; Exponent $91
		DB	$00,$00,$80	; (+00)              ;v,65537.
		DB	$32		; n-mod-m            ;remainder, result.
		DB	$02		; delete             ;remainder.
		DB	$A1		; stk-one            ;remainder, 1.
		DB	$03		; subtract           ;remainder - 1. = rnd
		DB	$31		; duplicate          ;rnd,rnd.
		DB	$38		; end-calc

		CALL	o2DA2		; routine FP-TO-BC
		LD	(SEED),BC	; store in SEED for next starting point.
		LD	A,(HL)		; fetch exponent
		AND	A		; is it zero ?
		JR	Z,o2625		; forward if so to S-RND-END

		SUB	$10		; reduce exponent by 2^16
		LD	(HL),A		; place back

;; S-RND-END
o2625:		JR	o2630		; forward to S-PI-END

; ---

; the number PI 3.14159...

; ->
;; S-PI
o2627:		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o2630		; to S-PI-END if checking syntax.

		RST	28H		; FP-CALC
		DB	$A3		; stk-pi/2                          pi/2.
		DB	$38		; end-calc

		INC	(HL)		; increment the exponent leaving pi
					; on the calculator stack.

;; S-PI-END
o2630:		RST	20H		; NEXT-CHAR
		JP	o26C3		; jump forward to S-NUMERIC

; ---

; ->
;; S-INKEY$
o2634:		LD	BC,$105A	; priority $10, operation code $1A ('read-in')
					; +$40 for string result, numeric operand.
					; set this up now in case we need to use the
					; calculator.
		RST	20H		; NEXT-CHAR
		CP	$23		; "#" ?
		JP	Z,o270D		; to S-PUSH-PO if so to use the calculator
					; single operation
					; to read from network/RS232 etc. .

; else read a key from the keyboard.

		LD	HL,FLAGS	; fetch FLAGS
		RES	6,(HL)		; signal string result.
		BIT	7,(HL)		; checking syntax ?
		JR	Z,o2665		; forward to S-INK$-EN if so

		JP	o3A5A
		LD	C,$00		; the length of an empty string
		JR	NZ,o2660	; to S-IK$-STK to store empty string if
					; no key returned.

		CALL	o031E		; routine K-TEST get main code in A
		JR	NC,o2660	; to S-IK$-STK to stack null string if
					; invalid

		DEC	D		; D is expected to be FLAGS so set bit 3 $FF
					; 'L' Mode so no keywords.
		LD	E,A		; main key to A
					; C is MODE 0 'KLC' from above still.
		CALL	o0333		; routine K-DECODE
o2657:		PUSH	AF		; save the code
		LD	BC,$0001	; make room for one character

		RST	30H		; BC-SPACES
		POP	AF		; bring the code back
		LD	(DE),A		; put the key in workspace
		LD	C,$01		; set C length to one

;; S-IK$-STK
o2660:		LD	B,$00		; set high byte of length to zero
		CALL	o2AB2		; routine STK-STO-$

;; S-INK$-EN
o2665:		JP	o2712		; to S-CONT-2            ===>

; ---

; ->
;; S-SCREEN$
o2668:		CALL	o2522		; routine S-2-COORD
		CALL	NZ,o2535	; routine S-SCRN$-S

		RST	20H		; NEXT-CHAR
		JP	o25DB		; forward to S-STRING to stack result

; ---

; ->
;; S-ATTR
o2672:		CALL	o2522		; routine S-2-COORD
		CALL	NZ,o2580	; routine S-ATTR-S

		RST	20H		; NEXT-CHAR
		JR	o26C3		; forward to S-NUMERIC

; ---

; ->
;; S-POINT
o267B:		CALL	o2522		; routine S-2-COORD
		CALL	NZ,o22CB	; routine POINT-SUB

		RST	20H		; NEXT-CHAR
		JR	o26C3		; forward to S-NUMERIC

; -----------------------------

; ==> The branch was here if not in table.

;; S-ALPHNUM
o2684:		CALL	o2C88		; routine ALPHANUM checks if variable or
					; a digit.
		JR	NC,o26DF	; forward to S-NEGATE if not to consider
					; a '-' character then functions.

		CP	$41		; compare 'A'
		JR	NC,o26C9	; forward to S-LETTER if alpha       ->
					; else must have been numeric so continue
					; into that routine.

; This important routine is called during runtime and from LINE-SCAN
; when a BASIC line is checked for syntax. It is this routine that
; inserts, during syntax checking, the invisible floating point numbers
; after the numeric expression. During runtime it just picks these
; numbers up. It also handles BIN format numbers.

; ->
;; S-BIN
;; S-DECIMAL
o268D:		CALL	o2530		; routine SYNTAX-Z
		JR	NZ,o26B5	; to S-STK-DEC in runtime

; this route is taken when checking syntax.

		CALL	o2C9B		; routine DEC-TO-FP to evaluate number

		RST	18H		; GET-CHAR to fetch HL
		LD	BC,$0006	; six locations required
		CALL	o1655		; routine MAKE-ROOM
		INC	HL		; to first new location
		LD	(HL),$0E	; insert number marker
		INC	HL		; address next
		EX	DE,HL		; make DE destination.
		LD	HL,(STKEND)	; STKEND points to end of stack.
		LD	C,$05		; result is five locations lower
		AND	A		; prepare for true subtraction
		SBC	HL,BC		; point to start of value.
		LD	(STKEND),HL	; update STKEND as we are taking number.
		LDIR			; Copy five bytes to program location
		EX	DE,HL		; transfer pointer to HL
		DEC	HL		; adjust
		CALL	o0077		; routine TEMP-PTR1 sets CH-ADD
		JR	o26C3		; to S-NUMERIC to record nature of result

; ---

; branch here in runtime.

;; S-STK-DEC
o26B5:		RST	18H		; GET-CHAR positions HL at digit.

;; S-SD-SKIP
o26B6:		INC	HL		; advance pointer
		LD	A,(HL)		; until we find
		CP	$0E		; chr 14d - the number indicator
		JR	NZ,o26B6	; to S-SD-SKIP until a match
					; it has to be here.

		INC	HL		; point to first byte of number
		CALL	o33B4		; routine STACK-NUM stacks it
		LD	(CH_ADD),HL	; update system variable CH_ADD

;; S-NUMERIC
o26C3:		SET	6,(IY+$01)	; update FLAGS  - Signal numeric result
		JR	o26DD		; forward to S-CONT-1               ===>
					; actually S-CONT-2 is destination but why
					; waste a byte on a jump when a JR will do.
					; Actually a JR o2712 can be used. Rats.

; end of functions accessed from scanning functions table.

; --------------------------
; Scanning variable routines
; --------------------------
;
;

;; S-LETTER
o26C9:		CALL	o28B2		; routine LOOK-VARS

		JP	C,o1C2E		; jump back to REPORT-2 if variable not found
					; 'Variable not found'
					; but a variable is always 'found' if syntax
					; is being checked.

		CALL	Z,o2996		; routine STK-VAR considers a subscript/slice
		LD	A,(FLAGS)	; fetch FLAGS value
		CP	$C0		; compare 11000000
		JR	C,o26DD		; step forward to S-CONT-1 if string  ===>

		INC	HL		; advance pointer
		CALL	o33B4		; routine STACK-NUM

;; S-CONT-1
o26DD:		JR	o2712		; forward to S-CONT-2                 ===>

; ----------------------------------------
; -> the scanning branch was here if not alphanumeric.
; All the remaining functions will be evaluated by a single call to the
; calculator. The correct priority for the operation has to be poaced in
; the B register and the operation code, calculator literal in the C register.
; the operation code has bit 7 set if result is numeric and bit 6 is
; set if operand is numeric. so
; $C0 = numeric result, numeric operand.            e.g. 'sin'
; $80 = numeric result, string operand.             e.g. 'code'
; $40 = string result, numeric operand.             e.g. 'str$'
; $00 = string result, string operand.              e.g. 'val$'

;; S-NEGATE
o26DF:		LD	BC,$09DB	; prepare priority 09, operation code $C0 +
					; 'negate' ($1B) - bits 6 and 7 set for numeric
					; result and numeric operand.

		CP	$2D		; is it '-' ?
		JR	Z,o270D		; forward if so to S-PUSH-PO

		LD	BC,$1018	; prepare priority $10, operation code 'val$' -
					; bits 6 and 7 reset for string result and
					; string operand.

		CP	$AE		; is it 'VAL$' ?
		JR	Z,o270D		; forward if so to S-PUSH-PO

		SUB	$AF		; subtract token 'CODE' value to reduce
					; functions 'CODE' to 'NOT' although the
					; upper range is, as yet, unchecked.
					; valid range would be $00 - $14.

		JP	C,o1C8A		; jump back to REPORT-C with anything else
					; 'Nonsense in BASIC'

		LD	BC,$04F0	; prepare priority $04, operation $C0 +
					; 'not' ($30)

		CP	$14		; is it 'NOT'
		JR	Z,o270D		; forward to S-PUSH-PO if so

		JP	NC,o1C8A	; to REPORT-C if higher
					; 'Nonsense in BASIC'

		LD	B,$10		; priority $10 for all the rest
		ADD	A,$DC		; make range $DC - $EF
					; $C0 + 'code'($1C) thru 'chr$' ($2F)

		LD	C,A		; transfer 'function' to C
		CP	$DF		; is it 'sin' ?
		JR	NC,o2707	; forward to S-NO-TO-$  with 'sin' through
					; 'chr$' as operand is numeric.

; all the rest 'cos' through 'chr$' give a numeric result except 'str$'
; and 'chr$'.

		RES	6,C		; signal string operand for 'code', 'val' and
					; 'len'.

;; S-NO-TO-$
o2707:		CP	$EE		; compare 'str$'
		JR	C,o270D		; forward to S-PUSH-PO if lower as result
					; is numeric.

		RES	7,C		; reset bit 7 of op code for 'str$', 'chr$'
					; as result is string.

; >> This is where they were all headed for.

;; S-PUSH-PO
o270D:		PUSH	BC		; push the priority and calculator operation
					; code.

		RST	20H		; NEXT-CHAR
		JP	o24FF		; jump back to S-LOOP-1 to go round the loop
					; again with the next character.

; --------------------------------

; ===>  there were many branches forward to here

;   An important step after the evaluation of an expression is to test for
;   a string expression and allow it to be sliced.  If a numeric expression is
;   followed by a "(" then the numeric expression is complete.
;   Since a string slice can itself be sliced then loop repeatedly
;   e.g. (STR$ PI) (3 TO) (TO 2)    or "nonsense" (4 TO )

;; S-CONT-2
o2712:		RST	18H		; GET-CHAR

;; S-CONT-3
o2713:		CP	$28		; is it "(" ?
		JR	NZ,o2723	; forward, if not, to S-OPERTR

		BIT	6,(IY+$01)	; test FLAGS - numeric or string result ?
		JR	NZ,o2734	; forward, if numeric, to S-LOOP

;   if a string expression preceded the "(" then slice it.

		CALL	o2A52		; routine SLICING

		RST	20H		; NEXT-CHAR
		JR	o2713		; loop back to S-CONT-3

; ---------------------------

;   the branch was here when possibility of a "(" has been excluded.

;; S-OPERTR
o2723:		LD	B,$00		; prepare to add
		LD	C,A		; possible operator to C
		LD	HL,o2795	; Address: $2795 - tbl-of-ops
		CALL	o16DC		; routine INDEXER
		JR	NC,o2734	; forward to S-LOOP if not in table

;   but if found in table the priority has to be looked up.

		LD	C,(HL)		; operation code to C ( B is still zero )
		LD	HL,o27B0-$C3	; $26ED is base of table
		ADD	HL,BC		; index into table.
		LD	B,(HL)		; priority to B.

; ------------------
; Scanning main loop
; ------------------
; the juggling act

;; S-LOOP
o2734:		POP	DE		; fetch last priority and operation
		LD	A,D		; priority to A
		CP	B		; compare with this one
		JR	C,o2773		; forward to S-TIGHTER to execute the
					; last operation before this one as it has
					; higher priority.

; the last priority was greater or equal this one.

		AND	A		; if it is zero then so is this
		JP	Z,o0018		; jump to exit via get-char pointing at
					; next character.
					; This may be the character after the
					; expression or, if exiting a recursive call,
					; the next part of the expression to be
					; evaluated.

		PUSH	BC		; save current priority/operation
					; as it has lower precedence than the one
					; now in DE.

; the 'USR' function is special in that it is overloaded to give two types
; of result.

		LD	HL,FLAGS	; address FLAGS
		LD	A,E		; new operation to A register
		CP	$ED		; is it $C0 + 'usr-no' ($2D)  ?
		JR	NZ,o274C	; forward to S-STK-LST if not

		BIT	6,(HL)		; string result expected ?
					; (from the lower priority operand we've
					; just pushed on stack )
		JR	NZ,o274C	; forward to S-STK-LST if numeric
					; as operand bits match.

		LD	E,$99		; reset bit 6 and substitute $19 'usr-$'
					; for string operand.

;; S-STK-LST
o274C:		PUSH	DE		; now stack this priority/operation
		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o275B		; forward to S-SYNTEST if checking syntax.

		LD	A,E		; fetch the operation code
		AND	$3F		; mask off the result/operand bits to leave
					; a calculator literal.
		LD	B,A		; transfer to B register

; now use the calculator to perform the single operation - operand is on
; the calculator stack.
; Note. although the calculator is performing a single operation most
; functions e.g. TAN are written using other functions and literals and
; these in turn are written using further strings of calculator literals so
; another level of magical recursion joins the juggling act for a while
; as the calculator too is calling itself.

		RST	28H		; FP-CALC
		DB	$3B		; fp-calc-2
o2758:		DB	$38		; end-calc

		JR	o2764		; forward to S-RUNTEST

; ---

; the branch was here if checking syntax only.

;; S-SYNTEST
o275B:		LD	A,E		; fetch the operation code to accumulator
		XOR	(IY+$01)	; compare with bits of FLAGS
		AND	$40		; bit 6 will be zero now if operand
					; matched expected result.

;; S-RPORT-C2
o2761:		JP	NZ,o1C8A	; to REPORT-C if mismatch
					; 'Nonsense in BASIC'
					; else continue to set flags for next

; the branch is to here in runtime after a successful operation.

;; S-RUNTEST
o2764:		POP	DE		; fetch the last operation from stack
		LD	HL,FLAGS	; address FLAGS
		SET	6,(HL)		; set default to numeric result in FLAGS
		BIT	7,E		; test the operational result
		JR	NZ,o2770	; forward to S-LOOPEND if numeric

		RES	6,(HL)		; reset bit 6 of FLAGS to show string result.

;; S-LOOPEND
o2770:		POP	BC		; fetch the previous priority/operation
		JR	o2734		; back to S-LOOP to perform these

; ---

; the branch was here when a stacked priority/operator had higher priority
; than the current one.

;; S-TIGHTER
o2773:		PUSH	DE		; save high priority op on stack again
		LD	A,C		; fetch lower priority operation code
		BIT	6,(IY+$01)	; test FLAGS - Numeric or string result ?
		JR	NZ,o2790	; forward to S-NEXT if numeric result

; if this is lower priority yet has string then must be a comparison.
; Since these can only be evaluated in context and were defaulted to
; numeric in operator look up they must be changed to string equivalents.

		AND	$3F		; mask to give true calculator literal
		ADD	A,$08		; augment numeric literals to string
					; equivalents.
					; 'no-&-no'  => 'str-&-no'
					; 'no-l-eql' => 'str-l-eql'
					; 'no-gr-eq' => 'str-gr-eq'
					; 'nos-neql' => 'strs-neql'
					; 'no-grtr'  => 'str-grtr'
					; 'no-less'  => 'str-less'
					; 'nos-eql'  => 'strs-eql'
					; 'addition' => 'strs-add'
		LD	C,A		; put modified comparison operator back
		CP	$10		; is it now 'str-&-no' ?
		JR	NZ,o2788	; forward to S-NOT-AND  if not.

		SET	6,C		; set numeric operand bit
		JR	o2790		; forward to S-NEXT

; ---

;; S-NOT-AND
o2788:		JR	C,o2761		; back to S-RPORT-C2 if less
					; 'Nonsense in BASIC'.
					; e.g. a$ * b$

		CP	$17		; is it 'strs-add' ?
		JR	Z,o2790		; forward to S-NEXT if so
					; (bit 6 and 7 are reset)

		SET	7,C		; set numeric (Boolean) result for all others

;; S-NEXT
o2790:		PUSH	BC		; now save this priority/operation on stack

		RST	20H		; NEXT-CHAR
		JP	o24FF		; jump back to S-LOOP-1

; ------------------
; Table of operators
; ------------------
; This table is used to look up the calculator literals associated with
; the operator character. The thirteen calculator operations $03 - $0F
; have bits 6 and 7 set to signify a numeric result.
; Some of these codes and bits may be altered later if the context suggests
; a string comparison or operation.
; that is '+', "=", ">", "<", '<=', '>=' or '<>'.

;; tbl-of-ops
o2795:		DB	'+', $CF	;        $C0 + 'addition'
		DB	'-', $C3	;        $C0 + 'subtract'
		DB	"*", $C4	;        $C0 + 'multiply'
		DB	"/", $C5	;        $C0 + 'division'
		DB	'^', $C6	;        $C0 + 'to-power'
		DB	"=", $CE	;        $C0 + 'nos-eql'
		DB	">", $CC	;        $C0 + 'no-grtr'
		DB	"<", $CD	;        $C0 + 'no-less'

		DB	$C7,$C9		; '<='   $C0 + 'no-l-eql'
		DB	$C8,$CA		; '>='   $C0 + 'no-gr-eql'
		DB	$C9,$CB		; '<>'   $C0 + 'nos-neql'
		DB	$C5,$C7		; 'OR'   $C0 + 'or'
		DB	$C6,$C8		; 'AND'  $C0 + 'no-&-no'

		DB	$00		; zero end-marker.


; -------------------
; Table of priorities
; -------------------
; This table is indexed with the operation code obtained from the above
; table $C3 - $CF to obtain the priority for the respective operation.

;; tbl-priors
o27B0:		DB	$06		; '-'   opcode $C3
		DB	$08		; "*"   opcode $C4
		DB	$08		; "/"   opcode $C5
		DB	$0A		; '^'   opcode $C6
		DB	$02		; 'OR'  opcode $C7
		DB	$03		; 'AND' opcode $C8
		DB	$05		; '<='  opcode $C9
		DB	$05		; '>='  opcode $CA
		DB	$05		; '<>'  opcode $CB
		DB	$05		; ">"   opcode $CC
		DB	$05		; "<"   opcode $CD
		DB	$05		; "="   opcode $CE
		DB	$06		; '+'   opcode $CF

; ----------------------
; Scanning function (FN)
; ----------------------
; This routine deals with user-defined functions.
; The definition can be anywhere in the program area but these are best
; poaced near the start of the program as we shall see.
; The evaluation process is quite complex as the Spectrum has to parse two
; statements at the same time. Syntax of both has been checked previously
; and hidden locations have been created immediately after each argument
; of the DEF FN statement. Each of the arguments of the FN function is
; evaluated by SCANNING and poaced in the hidden locations. Then the
; expression to the right of the DEF FN "=" is evaluated by SCANNING and for
; any variables encountered, a search is made in the DEF FN variable list
; in the program area before searching in the normal variables area.
;
; Recursion is not allowed: i.e. the definition of a function should not use
; the same function, either directly or indirectly ( through another function).
; You'll normally get error 4, ('Out of memory'), although sometimes the system
; will crash. - Vickers, Pitman 1984.
;
; As the definition is just an expression, there would seem to be no means
; of breaking out of such recursion.
; However, by the clever use of string expressions and VAL, such recursion is
; possible.
; e.g. DEF FN a(n) = VAL "n+FN a(n-1)+0" ((n<1) * 10 + 1 TO )
; will evaluate the full 11-character expression for all values where n is
; greater than zero but just the 11th character, "0", when n drops to zero
; thereby ending the recursion producing the correct result.
; Recursive string functions are possible using VAL$ instead of VAL and the
; null string as the final addend.
; - from a turn of the century newsgroup discussion initiated by Mike Wynne.

;; S-FN-SBRN
o27BD:		CALL	o2530		; routine SYNTAX-Z
		JR	NZ,o27F7	; forward to SF-RUN in runtime


		RST	20H		; NEXT-CHAR
		CALL	o2C8D		; routine ALPHA check for letters A-Z a-z
		JP	NC,o1C8A	; jump back to REPORT-C if not
					; 'Nonsense in BASIC'


		RST	20H		; NEXT-CHAR
		CP	$24		; is it "$" ?
		PUSH	AF		; save character and flags
		JR	NZ,o27D0	; forward to SF-BRKT-1 with numeric function


		RST	20H		; NEXT-CHAR

;; SF-BRKT-1
o27D0:		CP	$28		; is "(" ?
		JR	NZ,o27E6	; forward to SF-RPRT-C if not
					; 'Nonsense in BASIC'


		RST	20H		; NEXT-CHAR
		CP	$29		; is it ")" ?
		JR	Z,o27E9		; forward to SF-FLAG-6 if no arguments.

;; SF-ARGMTS
o27D9:		CALL	o24FB		; routine SCANNING checks each argument
					; which may be an expression.

		RST	18H		; GET-CHAR
		CP	$2C		; is it a ',' ?
		JR	NZ,o27E4	; forward if not to SF-BRKT-2 to test bracket


		RST	20H		; NEXT-CHAR if a comma was found
		JR	o27D9		; back to SF-ARGMTS to parse all arguments.

; ---

;; SF-BRKT-2
o27E4:		CP	$29		; is character the closing ")" ?

;; SF-RPRT-C
o27E6:		JP	NZ,o1C8A	; jump to REPORT-C
					; 'Nonsense in BASIC'

; at this point any optional arguments have had their syntax checked.

;; SF-FLAG-6
o27E9:		RST	20H		; NEXT-CHAR
		LD	HL,FLAGS	; address system variable FLAGS
		RES	6,(HL)		; signal string result
		POP	AF		; restore test against "$".
		JR	Z,o27F4		; forward to SF-SYN-EN if string function.

		SET	6,(HL)		; signal numeric result

;; SF-SYN-EN
o27F4:		JP	o2712		; jump back to S-CONT-2 to continue scanning.

; ---

; the branch was here in runtime.

;; SF-RUN
o27F7:		RST	20H		; NEXT-CHAR fetches name
		AND	$DF		; AND 11101111 - reset bit 5 - upper-case.
		LD	B,A		; save in B

		RST	20H		; NEXT-CHAR
		SUB	$24		; subtract "$"
		LD	C,A		; save result in C
		JR	NZ,o2802	; forward if not "$" to SF-ARGMT1

		RST	20H		; NEXT-CHAR advances to bracket

;; SF-ARGMT1
o2802:		RST	20H		; NEXT-CHAR advances to start of argument
		PUSH	HL		; save address
		LD	HL,(PROG)	; fetch start of program area from PROG
		DEC	HL		; the search starting point is the previous
					; location.

;; SF-FND-DF
o2808:		LD	DE,$00CE	; search is for token 'DEF FN' in E,
					; statement count in D.
		PUSH	BC		; save C the string test, and B the letter.
		CALL	o1D86		; routine LOOK-PROG will search for token.
		POP	BC		; restore BC.
		JR	NC,o2814	; forward to SF-CP-DEF if a match was found.


;; REPORT-P
o2812:		RST	08H		; ERROR-1
		DB	$18		; Error Report: FN without DEF

;; SF-CP-DEF
o2814:		PUSH	HL		; save address of DEF FN
		CALL	o28AB		; routine FN-SKPOVR skips over white-space etc.
					; without disturbing CH-ADD.
		AND	$DF		; make fetched character upper-case.
		CP	B		; compare with FN name
		JR	NZ,o2825	; forward to SF-NOT-FD if no match.

; the letters match so test the type.

		CALL	o28AB		; routine FN-SKPOVR skips white-space
		SUB	$24		; subtract "$" from fetched character
		CP	C		; compare with saved result of same operation
					; on FN name.
		JR	Z,o2831		; forward to SF-VALUES with a match.

; the letters matched but one was string and the other numeric.

;; SF-NOT-FD
o2825:		POP	HL		; restore search point.
		DEC	HL		; make location before
		LD	DE,$0200	; the search is to be for the end of the
					; current definition - 2 statements forward.
		PUSH	BC		; save the letter/type
		CALL	o198B		; routine EACH-STMT steps past rejected
					; definition.
		POP	BC		; restore letter/type
		JR	o2808		; back to SF-FND-DF to continue search

; ---

; Success!
; the branch was here with matching letter and numeric/string type.

;; SF-VALUES
o2831:		AND	A		; test A ( will be zero if string "$" - "$" )

		CALL	Z,o28AB		; routine FN-SKPOVR advances HL past "$".

		POP	DE		; discard pointer to 'DEF FN'.
		POP	DE		; restore pointer to first FN argument.
		LD	(CH_ADD),DE	; save in CH_ADD

		CALL	o28AB		; routine FN-SKPOVR advances HL past "("
		PUSH	HL		; save start address in DEF FN  ***
		CP	$29		; is character a ")" ?
		JR	Z,o2885		; forward to SF-R-BR-2 if no arguments.

;; SF-ARG-LP
o2843:		INC	HL		; point to next character.
		LD	A,(HL)		; fetch it.
		CP	$0E		; is it the number marker
		LD	D,$40		; signal numeric in D.
		JR	Z,o2852		; forward to SF-ARG-VL if numeric.

		DEC	HL		; back to letter
		CALL	o28AB		; routine FN-SKPOVR skips any white-space
		INC	HL		; advance past the expected "$" to
					; the 'hidden' marker.
		LD	D,$00		; signal string.

;; SF-ARG-VL
o2852:		INC	HL		; now address first of 5-byte location.
		PUSH	HL		; save address in DEF FN statement
		PUSH	DE		; save D - result type

		CALL	o24FB		; routine SCANNING evaluates expression in
					; the FN statement setting FLAGS and leaving
					; result as last value on calculator stack.

		POP	AF		; restore saved result type to A

		XOR	(IY+$01)	; xor with FLAGS
		AND	$40		; and with 01000000 to test bit 6
		JR	NZ,o288B	; forward to REPORT-Q if type mismatch.
					; 'Parameter error'

		POP	HL		; pop the start address in DEF FN statement
		EX	DE,HL		; transfer to DE ?? pop straight into de ?

		LD	HL,(STKEND)	; set HL to STKEND location after value
		LD	BC,$0005	; five bytes to move
		SBC	HL,BC		; decrease HL by 5 to point to start.
		LD	(STKEND),HL	; set STKEND 'removing' value from stack.

		LDIR			; copy value into DEF FN statement
		EX	DE,HL		; set HL to location after value in DEF FN
		DEC	HL		; step back one
		CALL	o28AB		; routine FN-SKPOVR gets next valid character
		CP	$29		; is it ")" end of arguments ?
		JR	Z,o2885		; forward to SF-R-BR-2 if so.

; a comma separator has been encountered in the DEF FN argument list.

		PUSH	HL		; save position in DEF FN statement

		RST	18H		; GET-CHAR from FN statement
		CP	$2C		; is it ',' ?
		JR	NZ,o288B	; forward to REPORT-Q if not
					; 'Parameter error'

		RST	20H		; NEXT-CHAR in FN statement advances to next
					; argument.

		POP	HL		; restore DEF FN pointer
		CALL	o28AB		; routine FN-SKPOVR advances to corresponding
					; argument.

		JR	o2843		; back to SF-ARG-LP looping until all
					; arguments are passed into the DEF FN
					; hidden locations.

; ---

; the branch was here when all arguments passed.

;; SF-R-BR-2
o2885:		PUSH	HL		; save location of ")" in DEF FN

		RST	18H		; GET-CHAR gets next character in FN
		CP	$29		; is it a ")" also ?
		JR	Z,o288D		; forward to SF-VALUE if so.


;; REPORT-Q
o288B:		RST	08H		; ERROR-1
		DB	$19		; Error Report: Parameter error

;; SF-VALUE
o288D:		POP	DE		; location of ")" in DEF FN to DE.
		EX	DE,HL		; now to HL, FN ")" pointer to DE.
		LD	(CH_ADD),HL	; initialize CH_ADD to this value.

; At this point the start of the DEF FN argument list is on the machine stack.
; We also have to consider that this defined function may form part of the
; definition of another defined function (though not itself).
; As this defined function may be part of a hierarchy of defined functions
; currently being evaluated by recursive calls to SCANNING, then we have to
; preserve the original value of DEFADD and not assume that it is zero.

		LD	HL,(DEFADD)	; get original DEFADD address
		EX	(SP),HL		; swap with DEF FN address on stack ***
		LD	(DEFADD),HL	; set DEFADD to point to this argument list
					; during scanning.

		PUSH	DE		; save FN ")" pointer.

		RST	20H		; NEXT-CHAR advances past ")" in define

		RST	20H		; NEXT-CHAR advances past "=" to expression

		CALL	o24FB		; routine SCANNING evaluates but searches
					; initially for variables at DEFADD

		POP	HL		; pop the FN ")" pointer
		LD	(CH_ADD),HL	; set CH_ADD to this
		POP	HL		; pop the original DEFADD value
		LD	(DEFADD),HL	; and re-insert into DEFADD system variable.

		RST	20H		; NEXT-CHAR advances to character after ")"
		JP	o2712		; to S-CONT-2 - to continue current
					; invocation of scanning

; --------------------
; Used to parse DEF FN
; --------------------
; e.g. DEF FN     s $ ( x )     =  b     $ (  TO  x  ) : REM exaggerated
;
; This routine is used 10 times to advance along a DEF FN statement
; skipping spaces and colour control codes. It is similar to NEXT-CHAR
; which is, at the same time, used to skip along the corresponding FN function
; except the latter has to deal with AT and TAB characters in string
; expressions. These cannot occur in a program area so this routine is
; simpler as both colour controls and their parameters are less than space.

;; FN-SKPOVR
o28AB:		INC	HL		; increase pointer
		LD	A,(HL)		; fetch addressed character
		CP	$21		; compare with space + 1
		JR	C,o28AB		; back to FN-SKPOVR if less

		RET			; return pointing to a valid character.

; ---------
; LOOK-VARS
; ---------
;
;

;; LOOK-VARS
o28B2:		SET	6,(IY+$01)	; update FLAGS - presume numeric result

		RST	18H		; GET-CHAR
		CALL	o2C8D		; routine ALPHA tests for A-Za-z
		JP	NC,o1C8A	; jump to REPORT-C if not.
					; 'Nonsense in BASIC'

		PUSH	HL		; save pointer to first letter       ^1
		AND	$1F		; mask lower bits, 1 - 26 decimal     000xxxxx
		LD	C,A		; store in C.

		RST	20H		; NEXT-CHAR
		PUSH	HL		; save pointer to second character   ^2
		CP	$28		; is it "(" - an array ?
		JR	Z,o28EF		; forward to V-RUN/SYN if so.

		SET	6,C		; set 6 signaling string if solitary  010
		CP	$24		; is character a "$" ?
		JR	Z,o28DE		; forward to V-STR-VAR

		SET	5,C		; signal numeric                       011
		CALL	o2C88		; routine ALPHANUM sets carry if second
					; character is alphanumeric.
		JR	NC,o28E3	; forward to V-TEST-FN if just one character

; It is more than one character but re-test current character so that 6 reset
; This loop renders the similar loop at V-PASS redundant.

;; V-CHAR
o28D4:		CALL	o2C88		; routine ALPHANUM
		JR	NC,o28EF	; to V-RUN/SYN when no more

		RES	6,C		; make long named type                 001

		RST	20H		; NEXT-CHAR
		JR	o28D4		; loop back to V-CHAR

; ---


;; V-STR-VAR
o28DE:		RST	20H		; NEXT-CHAR advances past "$"
		RES	6,(IY+$01)	; update FLAGS - signal string result.

;; V-TEST-FN
o28E3:		LD	A,($5C0C)	; load A with DEFADD_hi
		AND	A		; and test for zero.
		JR	Z,o28EF		; forward to V-RUN/SYN if a defined function
					; is not being evaluated.

; Note.

		CALL	o2530		; routine SYNTAX-Z
		JP	NZ,o2951	; JUMP to STK-F-ARG in runtime and then
					; back to this point if no variable found.

;; V-RUN/SYN
o28EF:		LD	B,C		; save flags in B
		CALL	o2530		; routine SYNTAX-Z
		JR	NZ,o28FD	; to V-RUN to look for the variable in runtime

; if checking syntax the letter is not returned

		LD	A,C		; copy letter/flags to A
		AND	$E0		; and with 11100000 to get rid of the letter
		SET	7,A		; use spare bit to signal checking syntax.
		LD	C,A		; and transfer to C.
		JR	o2934		; forward to V-SYNTAX

; ---

; but in runtime search for the variable.

;; V-RUN
o28FD:		LD	HL,(VARS)	; set HL to start of variables from VARS

;; V-EACH
o2900:		LD	A,(HL)		; get first character
		AND	$7F		; and with 01111111
					; ignoring bit 7 which distinguishes
					; arrays or for/next variables.

		JR	Z,o2932		; to V-80-BYTE if zero as must be 10000000
					; the variables end-marker.

		CP	C		; compare with supplied value.
		JR	NZ,o292A	; forward to V-NEXT if no match.

		RLA			; destructively test
		ADD	A,A		; bits 5 and 6 of A
					; jumping if bit 5 reset or 6 set

		JP	P,o293F		; to V-FOUND-2  strings and arrays

		JR	C,o293F		; to V-FOUND-2  simple and for next

; leaving long name variables.

		POP	DE		; pop pointer to 2nd. char
		PUSH	DE		; save it again
		PUSH	HL		; save variable first character pointer

;; V-MATCHES
o2912:		INC	HL		; address next character in vars area

;; V-SPACES
o2913:		LD	A,(DE)		; pick up letter from prog area
		INC	DE		; and advance address
		CP	$20		; is it a space
		JR	Z,o2913		; back to V-SPACES until non-space

		OR	$20		; convert to range 1 - 26.
		CP	(HL)		; compare with addressed variables character
		JR	Z,o2912		; loop back to V-MATCHES if a match on an
					; intermediate letter.

		OR	$80		; now set bit 7 as last character of long
					; names are inverted.
		CP	(HL)		; compare again
		JR	NZ,o2929	; forward to V-GET-PTR if no match

; but if they match check that this is also last letter in prog area

		LD	A,(DE)		; fetch next character
		CALL	o2C88		; routine ALPHANUM sets carry if not alphanum
		JR	NC,o293E	; forward to V-FOUND-1 with a full match.

;; V-GET-PTR
o2929:		POP	HL		; pop saved pointer to char 1

;; V-NEXT
o292A:		PUSH	BC		; save flags
		CALL	o19B8		; routine NEXT-ONE gets next variable in DE
		EX	DE,HL		; transfer to HL.
		POP	BC		; restore the flags
		JR	o2900		; loop back to V-EACH
					; to compare each variable

; ---

;; V-80-BYTE
o2932:		SET	7,B		; will signal not found

; the branch was here when checking syntax

;; V-SYNTAX
o2934:		POP	DE		; discard the pointer to 2nd. character  v2
					; in BASIC line/workspace.

		RST	18H		; GET-CHAR gets character after variable name.
		CP	$28		; is it "(" ?
		JR	Z,o2943		; forward to V-PASS
					; Note. could go straight to V-END ?

		SET	5,B		; signal not an array
		JR	o294B		; forward to V-END

; ---------------------------

; the jump was here when a long name matched and HL pointing to last character
; in variables area.

;; V-FOUND-1
o293E:		POP	DE		; discard pointer to first var letter

; the jump was here with all other matches HL points to first var char.

;; V-FOUND-2
o293F:		POP	DE		; discard pointer to 2nd prog char       v2
		POP	DE		; drop pointer to 1st prog char          v1
		PUSH	HL		; save pointer to last char in vars

		RST	18H		; GET-CHAR

;; V-PASS
o2943:		CALL	o2C88		; routine ALPHANUM
		JR	NC,o294B	; forward to V-END if not

; but it never will be as we advanced past long-named variables earlier.

		RST	20H		; NEXT-CHAR
		JR	o2943		; back to V-PASS

; ---

;; V-END
o294B:		POP	HL		; pop the pointer to first character in
					; BASIC line/workspace.
		RL	B		; rotate the B register left
					; bit 7 to carry
		BIT	6,B		; test the array indicator bit.
		RET			; return

; -----------------------
; Stack function argument
; -----------------------
; This branch is taken from LOOK-VARS when a defined function is currently
; being evaluated.
; Scanning is evaluating the expression after the "=" and the variable
; found could be in the argument list to the left of the "=" or in the
; normal place after the program. Preference will be given to the former.
; The variable name to be matched is in C.

;; STK-F-ARG
o2951:		LD	HL,(DEFADD)	; set HL to DEFADD
		LD	A,(HL)		; load the first character
		CP	$29		; is it ")" ?
		JP	Z,o28EF		; JUMP back to V-RUN/SYN, if so, as there are
					; no arguments.

; but proceed to search argument list of defined function first if not empty.

;; SFA-LOOP
o295A:		LD	A,(HL)		; fetch character again.
		OR	$60		; or with 01100000 presume a simple variable.
		LD	B,A		; save result in B.
		INC	HL		; address next location.
		LD	A,(HL)		; pick up byte.
		CP	$0E		; is it the number marker ?
		JR	Z,o296B		; forward to SFA-CP-VR if so.

; it was a string. White-space may be present but syntax has been checked.

		DEC	HL		; point back to letter.
		CALL	o28AB		; routine FN-SKPOVR skips to the "$"
		INC	HL		; now address the hidden marker.
		RES	5,B		; signal a string variable.

;; SFA-CP-VR
o296B:		LD	A,B		; transfer found variable letter to A.
		CP	C		; compare with expected.
		JR	Z,o2981		; forward to SFA-MATCH with a match.

		INC	HL		; step
		INC	HL		; past
		INC	HL		; the
		INC	HL		; five
		INC	HL		; bytes.

		CALL	o28AB		; routine FN-SKPOVR skips to next character
		CP	$29		; is it ")" ?
		JP	Z,o28EF		; jump back if so to V-RUN/SYN to look in
					; normal variables area.

		CALL	o28AB		; routine FN-SKPOVR skips past the ','
					; all syntax has been checked and these
					; things can be taken as read.
		JR	o295A		; back to SFA-LOOP while there are more
					; arguments.

; ---

;; SFA-MATCH
o2981:		BIT	5,C		; test if numeric
		JR	NZ,o2991	; to SFA-END if so as will be stacked
					; by scanning

		INC	HL		; point to start of string descriptor
		LD	DE,(STKEND)	; set DE to STKEND
		CALL	o33C0		; routine MOVE-FP puts parameters on stack.
		EX	DE,HL		; new free location to HL.
		LD	(STKEND),HL	; use it to set STKEND system variable.

;; SFA-END
o2991:		POP	DE		; discard
		POP	DE		; pointers.
		XOR	A		; clear carry flag.
		INC	A		; and zero flag.
		RET			; return.

; ------------------------
; Stack variable component
; ------------------------
; This is called to evaluate a complex structure that has been found, in
; runtime, by LOOK-VARS in the variables area.
; In this case HL points to the initial letter, bits 7-5
; of which indicate the type of variable.
; 010 - simple string, 110 - string array, 100 - array of numbers.
;
; It is called from CLASS-01 when assigning to a string or array including
; a slice.
; It is called from SCANNING to isolate the required part of the structure.
;
; An important part of the runtime process is to check that the number of
; dimensions of the variable match the number of subscripts supplied in the
; BASIC line.
;
; If checking syntax,
; the B register, which counts dimensions is set to zero (256) to allow
; the loop to continue till all subscripts are checked. While doing this it
; is reading dimension sizes from some arbitrary area of memory. Although
; these are meaningless it is of no concern as the limit is never checked by
; int-exp during syntax checking.
;
; The routine is also called from the syntax path of DIM command to check the
; syntax of both string and numeric arrays definitions except that bit 6 of C
; is reset so both are checked as numeric arrays. This ruse avoids a terminal
; slice being accepted as part of the DIM command.
; All that is being checked is that there are a valid set of comma-separated
; expressions before a terminal ")", although, as above, it will still go
; through the motions of checking dummy dimension sizes.

;; STK-VAR
o2996:		XOR	A		; clear A
		LD	B,A		; and B, the syntax dimension counter (256)
		BIT	7,C		; checking syntax ?
		JR	NZ,o29E7	; forward to SV-COUNT if so.

; runtime evaluation.

		BIT	7,(HL)		; will be reset if a simple string.
		JR	NZ,o29AE	; forward to SV-ARRAYS otherwise

		INC	A		; set A to 1, simple string.

;; SV-SIMPLE$
o29A1:		INC	HL		; address length low
		LD	C,(HL)		; place in C
		INC	HL		; address length high
		LD	B,(HL)		; place in B
		INC	HL		; address start of string
		EX	DE,HL		; DE = start now.
		CALL	o2AB2		; routine STK-STO-$ stacks string parameters
					; DE start in variables area,
					; BC length, A=1 simple string

; the only thing now is to consider if a slice is required.

		RST	18H		; GET-CHAR puts character at CH_ADD in A
		JP	o2A49		; jump forward to SV-SLICE? to test for "("

; --------------------------------------------------------

; the branch was here with string and numeric arrays in runtime.

;; SV-ARRAYS
o29AE:		INC	HL		; step past
		INC	HL		; the total length
		INC	HL		; to address Number of dimensions.
		LD	B,(HL)		; transfer to B overwriting zero.
		BIT	6,C		; a numeric array ?
		JR	Z,o29C0		; forward to SV-PTR with numeric arrays

		DEC	B		; ignore the final element of a string array
					; the fixed string size.

		JR	Z,o29A1		; back to SV-SIMPLE$ if result is zero as has
					; been created with DIM a$(10) for instance
					; and can be treated as a simple string.

; proceed with multi-dimensioned string arrays in runtime.

		EX	DE,HL		; save pointer to dimensions in DE

		RST	18H		; GET-CHAR looks at the BASIC line
		CP	$28		; is character "(" ?
		JR	NZ,o2A20	; to REPORT-3 if not
					; 'Subscript wrong'

		EX	DE,HL		; dimensions pointer to HL to synchronize
					; with next instruction.

; runtime numeric arrays path rejoins here.

;; SV-PTR
o29C0:		EX	DE,HL		; save dimension pointer in DE
		JR	o29E7		; forward to SV-COUNT with true no of dims
					; in B. As there is no initial comma the
					; loop is entered at the midpoint.

; ----------------------------------------------------------
; the dimension counting loop which is entered at mid-point.

;; SV-COMMA
o29C3:		PUSH	HL		; save counter

		RST	18H		; GET-CHAR

		POP	HL		; pop counter
		CP	$2C		; is character ',' ?
		JR	Z,o29EA		; forward to SV-LOOP if so

; in runtime the variable definition indicates a comma should appear here

		BIT	7,C		; checking syntax ?
		JR	Z,o2A20		; forward to REPORT-3 if not
					; 'Subscript error'

; proceed if checking syntax of an array?

		BIT	6,C		; array of strings
		JR	NZ,o29D8	; forward to SV-CLOSE if so

; an array of numbers.

		CP	$29		; is character ")" ?
		JR	NZ,o2A12	; forward to SV-RPT-C if not
					; 'Nonsense in BASIC'

		RST	20H		; NEXT-CHAR moves CH-ADD past the statement
		RET			; return ->

; ---

; the branch was here with an array of strings.

;; SV-CLOSE
o29D8:		CP	$29		; as above ")" could follow the expression
		JR	Z,o2A48		; forward to SV-DIM if so

		CP	$CC		; is it 'TO' ?
		JR	NZ,o2A12	; to SV-RPT-C with anything else
					; 'Nonsense in BASIC'

; now backtrack CH_ADD to set up for slicing routine.
; Note. in a BASIC line we can safely backtrack to a colour parameter.

;; SV-CH-ADD
o29E0:		RST	18H		; GET-CHAR
		DEC	HL		; backtrack HL
		LD	(CH_ADD),HL	; to set CH_ADD up for slicing routine
		JR	o2A45		; forward to SV-SLICE and make a return
					; when all slicing complete.

; ----------------------------------------
; -> the mid-point entry point of the loop

;; SV-COUNT
o29E7:		LD	HL,$0000	; initialize data pointer to zero.

;; SV-LOOP
o29EA:		PUSH	HL		; save the data pointer.

		RST	20H		; NEXT-CHAR in BASIC area points to an
					; expression.

		POP	HL		; restore the data pointer.
		LD	A,C		; transfer name/type to A.
		CP	$C0		; is it 11000000 ?
					; Note. the letter component is absent if
					; syntax checking.
		JR	NZ,o29FB	; forward to SV-MULT if not an array of
					; strings.

; proceed to check string arrays during syntax.

		RST	18H		; GET-CHAR
		CP	$29		; ")"  end of subscripts ?
		JR	Z,o2A48		; forward to SV-DIM to consider further slice

		CP	$CC		; is it 'TO' ?
		JR	Z,o29E0		; back to SV-CH-ADD to consider a slice.
					; (no need to repeat get-char at o29E0)

; if neither, then an expression is required so rejoin runtime loop ??
; registers HL and DE only point to somewhere meaningful in runtime so
; comments apply to that situation.

;; SV-MULT
o29FB:		PUSH	BC		; save dimension number.
		PUSH	HL		; push data pointer/rubbish.
					; DE points to current dimension.
		CALL	o2AEE		; routine DE,(DE+1) gets next dimension in DE
					; and HL points to it.
		EX	(SP),HL		; dim pointer to stack, data pointer to HL (*)
		EX	DE,HL		; data pointer to DE, dim size to HL.

		CALL	o2ACC		; routine INT-EXP1 checks integer expression
					; and gets result in BC in runtime.
		JR	C,o2A20		; to REPORT-3 if > HL
					; 'Subscript out of range'

		DEC	BC		; adjust returned result from 1-x to 0-x
		CALL	o2AF4		; routine GET-HL*DE multiplies data pointer by
					; dimension size.
		ADD	HL,BC		; add the integer returned by expression.
		POP	DE		; pop the dimension pointer.                              ***
		POP	BC		; pop dimension counter.
		DJNZ	o29C3		; back to SV-COMMA if more dimensions
					; Note. during syntax checking, unless there
					; are more than 256 subscripts, the branch
					; back to SV-COMMA is always taken.

		BIT	7,C		; are we checking syntax ?
					; then we've got a joker here.

;; SV-RPT-C
o2A12:		JR	NZ,o2A7A	; forward to SL-RPT-C if so
					; 'Nonsense in BASIC'
					; more than 256 subscripts in BASIC line.

; but in runtime the number of subscripts are at least the same as dims

		PUSH	HL		; save data pointer.
		BIT	6,C		; is it a string array ?
		JR	NZ,o2A2C	; forward to SV-ELEM$ if so.

; a runtime numeric array subscript.

		LD	B,D		; register DE has advanced past all dimensions
		LD	C,E		; and points to start of data in variable.
					; transfer it to BC.

		RST	18H		; GET-CHAR checks BASIC line
		CP	$29		; must be a ")" ?
		JR	Z,o2A22		; skip to SV-NUMBER if so

; else more subscripts in BASIC line than the variable definition.

;; REPORT-3
o2A20:		RST	08H		; ERROR-1
		DB	$02		; Error Report: Subscript wrong

; continue if subscripts matched the numeric array.

;; SV-NUMBER
o2A22:		RST	20H		; NEXT-CHAR moves CH_ADD to next statement
					; - finished parsing.

		POP	HL		; pop the data pointer.
		LD	DE,$0005	; each numeric element is 5 bytes.
		CALL	o2AF4		; routine GET-HL*DE multiplies.
		ADD	HL,BC		; now add to start of data in the variable.

		RET			; return with HL pointing at the numeric
					; array subscript.                       ->

; ---------------------------------------------------------------

; the branch was here for string subscripts when the number of subscripts
; in the BASIC line was one less than in variable definition.

;; SV-ELEM$
o2A2C:		CALL	o2AEE		; routine DE,(DE+1) gets final dimension
					; the length of strings in this array.
		EX	(SP),HL		; start pointer to stack, data pointer to HL.
		CALL	o2AF4		; routine GET-HL*DE multiplies by element
					; size.
		POP	BC		; the start of data pointer is added
		ADD	HL,BC		; in - now points to location before.
		INC	HL		; point to start of required string.
		LD	B,D		; transfer the length (final dimension size)
		LD	C,E		; from DE to BC.
		EX	DE,HL		; put start in DE.
		CALL	o2AB1		; routine STK-ST-0 stores the string parameters
					; with A=0 - a slice or subscript.

; now check that there were no more subscripts in the BASIC line.

		RST	18H		; GET-CHAR
		CP	$29		; is it ")" ?
		JR	Z,o2A48		; forward to SV-DIM to consider a separate
					; subscript or/and a slice.

		CP	$2C		; a comma is allowed if the final subscript
					; is to be sliced e.g. a$(2,3,4 TO 6).
		JR	NZ,o2A20	; to REPORT-3 with anything else
					; 'Subscript error'

;; SV-SLICE
o2A45:		CALL	o2A52		; routine SLICING slices the string.

; but a slice of a simple string can itself be sliced.

;; SV-DIM
o2A48:		RST	20H		; NEXT-CHAR

;; SV-SLICE?
o2A49:		CP	$28		; is character "(" ?
		JR	Z,o2A45		; loop back if so to SV-SLICE

		RES	6,(IY+$01)	; update FLAGS  - Signal string result
		RET			; and return.

; ---

; The above section deals with the flexible syntax allowed.
; DIM a$(3,3,10) can be considered as two dimensional array of ten-character
; strings or a 3-dimensional array of characters.
; a$(1,1) will return a 10-character string as will a$(1,1,1 TO 10)
; a$(1,1,1) will return a single character.
; a$(1,1) (1 TO 6) is the same as a$(1,1,1 TO 6)
; A slice can itself be sliced ad infinitum
; b$ () () () () () () (2 TO 10) (2 TO 9) (3) is the same as b$(5)



; -------------------------
; Handle slicing of strings
; -------------------------
; The syntax of string slicing is very natural and it is as well to reflect
; on the permutations possible.
; a$() and a$( TO ) indicate the entire string although just a$ would do
; and would avoid coming here.
; h$(16) indicates the single character at position 16.
; a$( TO 32) indicates the first 32 characters.
; a$(257 TO) indicates all except the first 256 characters.
; a$(19000 TO 19999) indicates the thousand characters at position 19000.
; Also a$(9 TO 5) returns a null string not an error.
; This enables a$(2 TO) to return a null string if the passed string is
; of length zero or 1.
; A string expression in brackets can be sliced. e.g. (STR$ PI) (3 TO )
; We arrived here from SCANNING with CH-ADD pointing to the initial "("
; or from above.

;; SLICING
o2A52:		CALL	o2530		; routine SYNTAX-Z
		CALL	NZ,o2BF1	; routine STK-FETCH fetches parameters of
					; string at runtime, start in DE, length
					; in BC. This could be an array subscript.

		RST	20H		; NEXT-CHAR
		CP	$29		; is it ")" ?     e.g. a$()
		JR	Z,o2AAD		; forward to SL-STORE to store entire string.

		PUSH	DE		; else save start address of string

		XOR	A		; clear accumulator to use as a running flag.
		PUSH	AF		; and save on stack before any branching.

		PUSH	BC		; save length of string to be sliced.
		LD	DE,$0001	; default the start point to position 1.

		RST	18H		; GET-CHAR

		POP	HL		; pop length to HL as default end point
					; and limit.

		CP	$CC		; is it 'TO' ?    e.g. a$( TO 10000)
		JR	Z,o2A81		; to SL-SECOND to evaluate second parameter.

		POP	AF		; pop the running flag.

		CALL	o2ACD		; routine INT-EXP2 fetches first parameter.

		PUSH	AF		; save flag (will be $FF if parameter>limit)

		LD	D,B		; transfer the start
		LD	E,C		; to DE overwriting 0001.
		PUSH	HL		; save original length.

		RST	18H		; GET-CHAR
		POP	HL		; pop the limit length.
		CP	$CC		; is it 'TO' after a start ?
		JR	Z,o2A81		; to SL-SECOND to evaluate second parameter

		CP	$29		; is it ")" ?       e.g. a$(365)

;; SL-RPT-C
o2A7A:		JP	NZ,o1C8A	; jump to REPORT-C with anything else
					; 'Nonsense in BASIC'

		LD	H,D		; copy start
		LD	L,E		; to end - just a one character slice.
		JR	o2A94		; forward to SL-DEFINE.

; ---------------------

;; SL-SECOND
o2A81:		PUSH	HL		; save limit length.

		RST	20H		; NEXT-CHAR

		POP	HL		; pop the length.

		CP	$29		; is character ")" ?        e.g. a$(7 TO )
		JR	Z,o2A94		; to SL-DEFINE using length as end point.

		POP	AF		; else restore flag.
		CALL	o2ACD		; routine INT-EXP2 gets second expression.

		PUSH	AF		; save the running flag.

		RST	18H		; GET-CHAR

		LD	H,B		; transfer second parameter
		LD	L,C		; to HL.              e.g. a$(42 to 99)
		CP	$29		; is character a ")" ?
		JR	NZ,o2A7A	; to SL-RPT-C if not
					; 'Nonsense in BASIC'

; we now have start in DE and an end in HL.

;; SL-DEFINE
o2A94:		POP	AF		; pop the running flag.
		EX	(SP),HL		; put end point on stack, start address to HL
		ADD	HL,DE		; add address of string to the start point.
		DEC	HL		; point to first character of slice.
		EX	(SP),HL		; start address to stack, end point to HL (*)
		AND	A		; prepare to subtract.
		SBC	HL,DE		; subtract start point from end point.
		LD	BC,$0000	; default the length result to zero.
		JR	C,o2AA8		; forward to SL-OVER if start > end.

		INC	HL		; increment the length for inclusive byte.

		AND	A		; now test the running flag.
		JP	M,o2A20		; jump back to REPORT-3 if $FF.
					; 'Subscript out of range'

		LD	B,H		; transfer the length
		LD	C,L		; to BC.

;; SL-OVER
o2AA8:		POP	DE		; restore start address from machine stack ***
		RES	6,(IY+$01)	; update FLAGS - signal string result for
					; syntax.

;; SL-STORE
o2AAD:		CALL	o2530		; routine SYNTAX-Z  (UNSTACK-Z?)
		RET	Z		; return if checking syntax.
					; but continue to store the string in runtime.

; ------------------------------------
; other than from above, this routine is called from STK-VAR to stack
; a known string array element.
; ------------------------------------

;; STK-ST-0
o2AB1:		XOR	A		; clear to signal a sliced string or element.

; -------------------------
; this routine is called from chr$, scrn$ etc. to store a simple string result.
; --------------------------

;; STK-STO-$
o2AB2:		RES	6,(IY+$01)	; update FLAGS - signal string result.
					; and continue to store parameters of string.

; ---------------------------------------
; Pass five registers to calculator stack
; ---------------------------------------
; This subroutine puts five registers on the calculator stack.

;; STK-STORE
o2AB6:		PUSH	BC		; save two registers
		CALL	o33A9		; routine TEST-5-SP checks room and puts 5
					; in BC.
		POP	BC		; fetch the saved registers.
		LD	HL,(STKEND)	; make HL point to first empty location STKEND
		LD	(HL),A		; place the 5 registers.
		INC	HL		;
		LD	(HL),E		;
		INC	HL		;
		LD	(HL),D		;
		INC	HL		;
		LD	(HL),C		;
		INC	HL		;
		LD	(HL),B		;
		INC	HL		;
		LD	(STKEND),HL	; update system variable STKEND.
		RET			; and return.

; -------------------------------------------
; Return result of evaluating next expression
; -------------------------------------------
; This clever routine is used to check and evaluate an integer expression
; which is returned in BC, setting A to $FF, if greater than a limit supplied
; in HL. It is used to check array subscripts, parameters of a string slice
; and the arguments of the DIM command. In the latter case, the limit check
; is not required and H is set to $FF. When checking optional string slice
; parameters, it is entered at the second entry point so as not to disturb
; the running flag A, which may be $00 or $FF from a previous invocation.

;; INT-EXP1
o2ACC:		XOR	A		; set result flag to zero.

; -> The entry point is here if A is used as a running flag.

;; INT-EXP2
o2ACD:		PUSH	DE		; preserve DE register throughout.
		PUSH	HL		; save the supplied limit.
		PUSH	AF		; save the flag.

		CALL	o1C82		; routine EXPT-1NUM evaluates expression
					; at CH_ADD returning if numeric result,
					; with value on calculator stack.

		POP	AF		; pop the flag.
		CALL	o2530		; routine SYNTAX-Z
		JR	Z,o2AEB		; forward to I-RESTORE if checking syntax so
					; avoiding a comparison with supplied limit.

		PUSH	AF		; save the flag.

		CALL	o1E99		; routine FIND-INT2 fetches value from
					; calculator stack to BC producing an error
					; if too high.

		POP	DE		; pop the flag to D.
		LD	A,B		; test value for zero and reject
		OR	C		; as arrays and strings begin at 1.
		SCF			; set carry flag.
		JR	Z,o2AE8		; forward to I-CARRY if zero.

		POP	HL		; restore the limit.
		PUSH	HL		; and save.
		AND	A		; prepare to subtract.
		SBC	HL,BC		; subtract value from limit.

;; I-CARRY
o2AE8:		LD	A,D		; move flag to accumulator $00 or $FF.
		SBC	A,$00		; will set to $FF if carry set.

;; I-RESTORE
o2AEB:		POP	HL		; restore the limit.
		POP	DE		; and DE register.
		RET			; return.


; -----------------------
; LD DE,(DE+1) Subroutine
; -----------------------
; This routine just loads the DE register with the contents of the two
; locations following the location addressed by DE.
; It is used to step along the 16-bit dimension sizes in array definitions.
; Note. Such code is made into subroutines to make programs easier to
; write and it would use less space to include the five instructions in-line.
; However, there are so many exchanges going on at the places this is invoked
; that to implement it in-line would make the code hard to follow.
; It probably had a zippier label though as the intention is to simplify the
; program.

;; DE,(DE+1)
o2AEE:		EX	DE,HL		;
		INC	HL		;
		LD	E,(HL)		;
		INC	HL		;
		LD	D,(HL)		;
		RET			;

; -------------------
; HL=HL*DE Subroutine
; -------------------
; This routine calls the mathematical routine to multiply HL by DE in runtime.
; It is called from STK-VAR and from DIM. In the latter case syntax is not
; being checked so the entry point could have been at the second CALL
; instruction to save a few clock-cycles.

;; GET-HL*DE
o2AF4:		CALL	o2530		; routine SYNTAX-Z.
		RET	Z		; return if checking syntax.

		CALL	o30A9		; routine HL-HL*DE.
		JP	C,o1F15		; jump back to REPORT-4 if over 65535.

		RET			; else return with 16-bit result in HL.

; -----------------
; THE 'LET' COMMAND
; -----------------
; Sinclair BASIC adheres to the ANSI-78 standard and a LET is required in
; assignments e.g. LET a = 1  :   LET h$ = "hat".
;
; Long names may contain spaces but not colour controls (when assigned).
; a substring can appear to the left of the equals sign.

; An earlier mathematician Lewis Carroll may have been pleased that
; 10 LET Babies cannot manage crocodiles = Babies are illogical AND
;    Nobody is despised who can manage a crocodile AND Illogical persons
;    are despised
; does not give the 'Nonsense..' error if the three variables exist.
; I digress.

;; LET
o2AFF:		LD	HL,(DEST)	; fetch system variable DEST to HL.
		BIT	1,(IY+$37)	; test FLAGX - handling a new variable ?
		JR	Z,o2B66		; forward to L-EXISTS if not.

; continue for a new variable. DEST points to start in BASIC line.
; from the CLASS routines.

		LD	BC,$0005	; assume numeric and assign an initial 5 bytes

;; L-EACH-CH
o2B0B:		INC	BC		; increase byte count for each relevant
					; character

;; L-NO-SP
o2B0C:		INC	HL		; increase pointer.
		LD	A,(HL)		; fetch character.
		CP	$20		; is it a space ?
		JR	Z,o2B0C		; back to L-NO-SP is so.

		JR	NC,o2B1F	; forward to L-TEST-CH if higher.

		CP	$10		; is it $00 - $0F ?
		JR	C,o2B29		; forward to L-SPACES if so.

		CP	$16		; is it $16 - $1F ?
		JR	NC,o2B29	; forward to L-SPACES if so.

; it was $10 - $15  so step over a colour code.

		INC	HL		; increase pointer.
		JR	o2B0C		; loop back to L-NO-SP.

; ---

; the branch was to here if higher than space.

;; L-TEST-CH
o2B1F:		CALL	o2C88		; routine ALPHANUM sets carry if alphanumeric
		JR	C,o2B0B		; loop back to L-EACH-CH for more if so.

		CP	$24		; is it "$" ?
		JP	Z,o2BC0		; jump forward if so, to L-NEW$
					; with a new string.

;; L-SPACES
o2B29:		LD	A,C		; save length lo in A.
		LD	HL,(E_LINE)	; fetch E_LINE to HL.
		DEC	HL		; point to location before, the variables
					; end-marker.
		CALL	o1655		; routine MAKE-ROOM creates BC spaces
					; for name and numeric value.
		INC	HL		; advance to first new location.
		INC	HL		; then to second.
		EX	DE,HL		; set DE to second location.
		PUSH	DE		; save this pointer.
		LD	HL,(DEST)	; reload HL with DEST.
		DEC	DE		; point to first.
		SUB	$06		; subtract six from length_lo.
		LD	B,A		; save count in B.
		JR	Z,o2B4F		; forward to L-SINGLE if it was just
					; one character.

; HL points to start of variable name after 'LET' in BASIC line.

;; L-CHAR
o2B3E:		INC	HL		; increase pointer.
		LD	A,(HL)		; pick up character.
		CP	$21		; is it space or higher ?
		JR	C,o2B3E		; back to L-CHAR with space and less.

		OR	$20		; make variable lower-case.
		INC	DE		; increase destination pointer.
		LD	(DE),A		; and load to edit line.
		DJNZ	o2B3E		; loop back to L-CHAR until B is zero.

		OR	$80		; invert the last character.
		LD	(DE),A		; and overwrite that in edit line.

; now consider first character which has bit 6 set

		LD	A,$C0		; set A 11000000 is xor mask for a long name.
					; %101      is xor/or  result

; single character numerics rejoin here with %00000000 in mask.
;                                            %011      will be xor/or result

;; L-SINGLE
o2B4F:		LD	HL,(DEST)	; fetch DEST - HL addresses first character.
		XOR	(HL)		; apply variable type indicator mask (above).
		OR	$20		; make lowercase - set bit 5.
		POP	HL		; restore pointer to 2nd character.
		CALL	o2BEA		; routine L-FIRST puts A in first character.
					; and returns with HL holding
					; new E_LINE-1  the $80 vars end-marker.

;; L-NUMERIC
o2B59:		PUSH	HL		; save the pointer.

; the value of variable is deleted but remains after calculator stack.

		RST	28H		; FP-CALC
		DB	$02		; delete      ; delete variable value
		DB	$38		; end-calc

; DE (STKEND) points to start of value.

		POP	HL		; restore the pointer.
		LD	BC,$0005	; start of number is five bytes before.
		AND	A		; prepare for true subtraction.
		SBC	HL,BC		; HL points to start of value.
		JR	o2BA6		; forward to L-ENTER  ==>

; ---


; the jump was to here if the variable already existed.

;; L-EXISTS
o2B66:		BIT	6,(IY+$01)	; test FLAGS - numeric or string result ?
		JR	Z,o2B72		; skip forward to L-DELETE$   -*->
					; if string result.

; A numeric variable could be simple or an array element.
; They are treated the same and the old value is overwritten.

		LD	DE,$0006	; six bytes forward points to loc past value.
		ADD	HL,DE		; add to start of number.
		JR	o2B59		; back to L-NUMERIC to overwrite value.

; ---

; -*-> the branch was here if a string existed.

;; L-DELETE$
o2B72:		LD	HL,(DEST)	; fetch DEST to HL.
					; (still set from first instruction)
		LD	BC,(STRLEN)	; fetch STRLEN to BC.
		BIT	0,(IY+$37)	; test FLAGX - handling a complete simple
					; string ?
		JR	NZ,o2BAF	; forward to L-ADD$ if so.

; must be a string array or a slice in workspace.
; Note. LET a$(3 TO 6) = h$   will assign "hat " if h$ = "hat"
;                                  and    "hats" if h$ = "hatstand".
;
; This is known as Procrustean lengthening and shortening after a
; character Procrustes in Greek legend who made travellers sleep in his bed,
; cutting off their feet or stretching them so they fitted the bed perfectly.
; The bloke was hatstand and slain by Theseus.

		LD	A,B		; test if length
		OR	C		; is zero and
		RET	Z		; return if so.

		PUSH	HL		; save pointer to start.

		RST	30H		; BC-SPACES creates room.
		PUSH	DE		; save pointer to first new location.
		PUSH	BC		; and length            (*)
		LD	D,H		; set DE to point to last location.
		LD	E,L		;
		INC	HL		; set HL to next location.
		LD	(HL),$20	; place a space there.
		LDDR			; copy bytes filling with spaces.

		PUSH	HL		; save pointer to start.
		CALL	o2BF1		; routine STK-FETCH start to DE,
					; length to BC.
		POP	HL		; restore the pointer.
		EX	(SP),HL		; (*) length to HL, pointer to stack.
		AND	A		; prepare for true subtraction.
		SBC	HL,BC		; subtract old length from new.
		ADD	HL,BC		; and add back.
		JR	NC,o2B9B	; forward if it fits to L-LENGTH.

		LD	B,H		; otherwise set
		LD	C,L		; length to old length.
					; "hatstand" becomes "hats"

;; L-LENGTH
o2B9B:		EX	(SP),HL		; (*) length to stack, pointer to HL.
		EX	DE,HL		; pointer to DE, start of string to HL.
		LD	A,B		; is the length zero ?
		OR	C		;
		JR	Z,o2BA3		; forward to L-IN-W/S if so
					; leaving prepared spaces.

		LDIR			; else copy bytes overwriting some spaces.

;; L-IN-W/S
o2BA3:		POP	BC		; pop the new length.  (*)
		POP	DE		; pop pointer to new area.
		POP	HL		; pop pointer to variable in assignment.
					; and continue copying from workspace
					; to variables area.

; ==> branch here from  L-NUMERIC

;; L-ENTER
o2BA6:		EX	DE,HL		; exchange pointers HL=STKEND DE=end of vars.
		LD	A,B		; test the length
		OR	C		; and make a
		RET	Z		; return if zero (strings only).

		PUSH	DE		; save start of destination.
		LDIR			; copy bytes.
		POP	HL		; address the start.
		RET			; and return.

; ---

; the branch was here from L-DELETE$ if an existing simple string.
; register HL addresses start of string in variables area.

;; L-ADD$
o2BAF:		DEC	HL		; point to high byte of length.
		DEC	HL		; to low byte.
		DEC	HL		; to letter.
		LD	A,(HL)		; fetch masked letter to A.
		PUSH	HL		; save the pointer on stack.
		PUSH	BC		; save new length.
		CALL	o2BC6		; routine L-STRING adds new string at end
					; of variables area.
					; if no room we still have old one.
		POP	BC		; restore length.
		POP	HL		; restore start.
		INC	BC		; increase
		INC	BC		; length by three
		INC	BC		; to include character and length bytes.
		JP	o19E8		; jump to indirect exit via RECLAIM-2
					; deleting old version and adjusting pointers.

; ---

; the jump was here with a new string variable.

;; L-NEW$
o2BC0:		LD	A,$DF		; indicator mask %11011111 for
					;                %010xxxxx will be result
		LD	HL,(DEST)	; address DEST first character.
		AND	(HL)		; combine mask with character.

;; L-STRING
o2BC6:		PUSH	AF		; save first character and mask.
		CALL	o2BF1		; routine STK-FETCH fetches parameters of
					; the string.
		EX	DE,HL		; transfer start to HL.
		ADD	HL,BC		; add to length.
		PUSH	BC		; save the length.
		DEC	HL		; point to end of string.
		LD	(DEST),HL	; save pointer in DEST.
					; (updated by POINTERS if in workspace)
		INC	BC		; extra byte for letter.
		INC	BC		; two bytes
		INC	BC		; for the length of string.
		LD	HL,(E_LINE)	; address E_LINE.
		DEC	HL		; now end of VARS area.
		CALL	o1655		; routine MAKE-ROOM makes room for string.
					; updating pointers including DEST.
		LD	HL,(DEST)	; pick up pointer to end of string from DEST.
		POP	BC		; restore length from stack.
		PUSH	BC		; and save again on stack.
		INC	BC		; add a byte.
		LDDR			; copy bytes from end to start.
		EX	DE,HL		; HL addresses length low
		INC	HL		; increase to address high byte
		POP	BC		; restore length to BC
		LD	(HL),B		; insert high byte
		DEC	HL		; address low byte location
		LD	(HL),C		; insert that byte
		POP	AF		; restore character and mask

;; L-FIRST
o2BEA:		DEC	HL		; address variable name
		LD	(HL),A		; and insert character.
		LD	HL,(E_LINE)	; load HL with E_LINE.
		DEC	HL		; now end of VARS area.
		RET			; return

; ------------------------------------
; Get last value from calculator stack
; ------------------------------------
;
;

;; STK-FETCH
o2BF1:		LD	HL,(STKEND)	; STKEND
		DEC	HL		;
		LD	B,(HL)		;
		DEC	HL		;
		LD	C,(HL)		;
		DEC	HL		;
		LD	D,(HL)		;
		DEC	HL		;
		LD	E,(HL)		;
		DEC	HL		;
		LD	A,(HL)		;
		LD	(STKEND),HL	; STKEND
		RET			;

; ------------------
; Handle DIM command
; ------------------
; e.g. DIM a(2,3,4,7): DIM a$(32) : DIM b$(20,2,768) : DIM c$(20000)
; the only limit to dimensions is memory so, for example,
; DIM a(2,2,2,2,2,2,2,2,2,2,2,2,2) is possible and creates a multi-
; dimensional array of zeros. String arrays are initialized to spaces.
; It is not possible to erase an array, but it can be re-dimensioned to
; a minimal size of 1, after use, to free up memory.

;; DIM
o2C02:		CALL	o28B2		; routine LOOK-VARS

;; D-RPORT-C
o2C05:		JP	NZ,o1C8A	; jump to REPORT-C if a long-name variable.
					; DIM lottery numbers(49) doesn't work.

		CALL	o2530		; routine SYNTAX-Z
		JR	NZ,o2C15	; forward to D-RUN in runtime.

		RES	6,C		; signal 'numeric' array even if string as
					; this simplifies the syntax checking.

		CALL	o2996		; routine STK-VAR checks syntax.
		CALL	o1BEE		; routine CHECK-END performs early exit ->

; the branch was here in runtime.

;; D-RUN
o2C15:		JR	C,o2C1F		; skip to D-LETTER if variable did not exist.
					; else reclaim the old one.

		PUSH	BC		; save type in C.
		CALL	o19B8		; routine NEXT-ONE find following variable
					; or position of $80 end-marker.
		CALL	o19E8		; routine RECLAIM-2 reclaims the
					; space between.
		POP	BC		; pop the type.

;; D-LETTER
o2C1F:		SET	7,C		; signal array.
		LD	B,$00		; initialize dimensions to zero and
		PUSH	BC		; save with the type.
		LD	HL,$0001	; make elements one character presuming string
		BIT	6,C		; is it a string ?
		JR	NZ,o2C2D	; forward to D-SIZE if so.

		LD	L,$05		; make elements 5 bytes as is numeric.

;; D-SIZE
o2C2D:		EX	DE,HL		; save the element size in DE.

; now enter a loop to parse each of the integers in the list.

;; D-NO-LOOP
o2C2E:		RST	20H		; NEXT-CHAR
		LD	H,$FF		; disable limit check by setting HL high
		CALL	o2ACC		; routine INT-EXP1
		JP	C,o2A20		; to REPORT-3 if > 65280 and then some
					; 'Subscript out of range'

		POP	HL		; pop dimension counter, array type
		PUSH	BC		; save dimension size                     ***
		INC	H		; increment the dimension counter
		PUSH	HL		; save the dimension counter
		LD	H,B		; transfer size
		LD	L,C		; to HL
		CALL	o2AF4		; routine GET-HL*DE multiplies dimension by
					; running total of size required initially
					; 1 or 5.
		EX	DE,HL		; save running total in DE

		RST	18H		; GET-CHAR
		CP	$2C		; is it ',' ?
		JR	Z,o2C2E		; loop back to D-NO-LOOP until all dimensions
					; have been considered

; when loop complete continue.

		CP	$29		; is it ")" ?
		JR	NZ,o2C05	; to D-RPORT-C with anything else
					; 'Nonsense in BASIC'


		RST	20H		; NEXT-CHAR advances to next statement/CR

		POP	BC		; pop dimension counter/type
		LD	A,C		; type to A

; now calculate space required for array variable

		LD	L,B		; dimensions to L since these require 16 bits
					; then this value will be doubled
		LD	H,$00		; set high byte to zero

; another four bytes are required for letter(1), total length(2), number of
; dimensions(1) but since we have yet to double allow for two

		INC	HL		; increment
		INC	HL		; increment

		ADD	HL,HL		; now double giving 4 + dimensions * 2

		ADD	HL,DE		; add to space required for array contents

		JP	C,o1F15		; to REPORT-4 if > 65535
					; 'Out of memory'

		PUSH	DE		; save data space
		PUSH	BC		; save dimensions/type
		PUSH	HL		; save total space
		LD	B,H		; total space
		LD	C,L		; to BC
		LD	HL,(E_LINE)	; address E_LINE - first location after
					; variables area
		DEC	HL		; point to location before - the $80 end-marker
		CALL	o1655		; routine MAKE-ROOM creates the space if
					; memory is available.

		INC	HL		; point to first new location and
		LD	(HL),A		; store letter/type

		POP	BC		; pop total space
		DEC	BC		; exclude name
		DEC	BC		; exclude the 16-bit
		DEC	BC		; counter itself
		INC	HL		; point to next location the 16-bit counter
		LD	(HL),C		; insert low byte
		INC	HL		; address next
		LD	(HL),B		; insert high byte

		POP	BC		; pop the number of dimensions.
		LD	A,B		; dimensions to A
		INC	HL		; address next
		LD	(HL),A		; and insert "No. of dims"

		LD	H,D		; transfer DE space + 1 from make-room
		LD	L,E		; to HL
		DEC	DE		; set DE to next location down.
		LD	(HL),$00	; presume numeric and insert a zero
		BIT	6,C		; test bit 6 of C. numeric or string ?
		JR	Z,o2C7C		; skip to DIM-CLEAR if numeric

		LD	(HL),$20	; place a space character in HL

;; DIM-CLEAR
o2C7C:		POP	BC		; pop the data length

		LDDR			; LDDR sets to zeros or spaces

; The number of dimensions is still in A.
; A loop is now entered to insert the size of each dimension that was pushed
; during the D-NO-LOOP working downwards from position before start of data.

;; DIM-SIZES
o2C7F:		POP	BC		; pop a dimension size                    ***
		LD	(HL),B		; insert high byte at position
		DEC	HL		; next location down
		LD	(HL),C		; insert low byte
		DEC	HL		; next location down
		DEC	A		; decrement dimension counter
		JR	NZ,o2C7F	; back to DIM-SIZES until all done.

		RET			; return.

; -----------------------------
; Check whether digit or letter
; -----------------------------
; This routine checks that the character in A is alphanumeric
; returning with carry set if so.

;; ALPHANUM
o2C88:		CALL	o2D1B		; routine NUMERIC will reset carry if so.
		CCF			; Complement Carry Flag
		RET	C		; Return if numeric else continue into
					; next routine.

; This routine checks that the character in A is alphabetic

;; ALPHA
o2C8D:		CP	$41		; less than 'A' ?
		CCF			; Complement Carry Flag
		RET	NC		; return if so

		CP	$5B		; less than 'Z'+1 ?
		RET	C		; is within first range

		CP	$61		; less than 'a' ?
		CCF			; Complement Carry Flag
		RET	NC		; return if so.

		CP	$7B		; less than 'z'+1 ?
		RET			; carry set if within a-z.

; -------------------------
; Decimal to floating point
; -------------------------
; This routine finds the floating point number represented by an expression
; beginning with BIN, '.' or a digit.
; Note that BIN need not have any "0"s or "1"s after it.
; BIN is really just a notational symbol and not a function.

;; DEC-TO-FP
o2C9B:		CP	$C4		; 'BIN' token ?
		JR	NZ,o2CB8	; to NOT-BIN if not

		LD	DE,$0000	; initialize 16 bit buffer register.

;; BIN-DIGIT
o2CA2:		RST	20H		; NEXT-CHAR
		SUB	$31		; "1"
		ADC	A,$00		; will be zero if "1" or "0"
					; carry will be set if was "0"
		JR	NZ,o2CB3	; forward to BIN-END if result not zero

		EX	DE,HL		; buffer to HL
		CCF			; Carry now set if originally "1"
		ADC	HL,HL		; shift the carry into HL
		JP	C,o31AD		; to REPORT-6 if overflow - too many digits
					; after first "1". There can be an unlimited
					; number of leading zeros.
					; 'Number too big' - raise an error

		EX	DE,HL		; save the buffer
		JR	o2CA2		; back to BIN-DIGIT for more digits

; ---

;; BIN-END
o2CB3:		LD	B,D		; transfer 16 bit buffer
		LD	C,E		; to BC register pair.
		JP	o2D2B		; JUMP to STACK-BC to put on calculator stack

; ---

; continue here with .1,  42, 3.14, 5., 2.3 E -4

;; NOT-BIN
o2CB8:		CP	$2E		; '.' - leading decimal point ?
		JR	Z,o2CCB		; skip to DECIMAL if so.

		CALL	o2D3B		; routine INT-TO-FP to evaluate all digits
					; This number 'x' is poaced on stack.
		CP	$2E		; '.' - mid decimal point ?

		JR	NZ,o2CEB	; to E-FORMAT if not to consider that format

		RST	20H		; NEXT-CHAR
		CALL	o2D1B		; routine NUMERIC returns carry reset if 0-9

		JR	C,o2CEB		; to E-FORMAT if not a digit e.g. '1.'

		JR	o2CD5		; to DEC-STO-1 to add the decimal part to 'x'

; ---

; a leading decimal point has been found in a number.

;; DECIMAL
o2CCB:		RST	20H		; NEXT-CHAR
		CALL	o2D1B		; routine NUMERIC will reset carry if digit

;; DEC-RPT-C
o2CCF:		JP	C,o1C8A		; to REPORT-C if just a '.'
					; raise 'Nonsense in BASIC'

; since there is no leading zero put one on the calculator stack.

		RST	28H		; FP-CALC
		DB	$A0		; stk-zero  ; 0.
		DB	$38		; end-calc

; If rejoining from earlier there will be a value 'x' on stack.
; If continuing from above the value zero.
; Now store 1 in mem-0.
; Note. At each pass of the digit loop this will be divided by ten.

;; DEC-STO-1
o2CD5:		RST	28H		; FP-CALC
		DB	$A1		; stk-one   ;x or 0,1.
		DB	$C0		; st-mem-0  ;x or 0,1.
		DB	$02		; delete    ;x or 0.
		DB	$38		; end-calc


;; NXT-DGT-1
o2CDA:		RST	18H		; GET-CHAR
		CALL	o2D22		; routine STK-DIGIT stacks single digit 'd'
		JR	C,o2CEB		; exit to E-FORMAT when digits exhausted  >


		RST	28H		; FP-CALC   ;x or 0,d.           first pass.
		DB	$E0		; get-mem-0  ;x or 0,d,1.
		DB	$A4		; stk-ten    ;x or 0,d,1,10.
		DB	$05		; division   ;x or 0,d,1/10.
		DB	$C0		; st-mem-0   ;x or 0,d,1/10.
		DB	$04		; multiply   ;x or 0,d/10.
		DB	$0F		; addition   ;x or 0 + d/10.
		DB	$38		; end-calc   last value.

		RST	20H		; NEXT-CHAR  moves to next character
		JR	o2CDA		; back to NXT-DGT-1

; ---

; although only the first pass is shown it can be seen that at each pass
; the new less significant digit is multiplied by an increasingly smaller
; factor (1/100, 1/1000, 1/10000 ... ) before being added to the previous
; last value to form a new last value.

; Finally see if an exponent has been input.

;; E-FORMAT
o2CEB:		CP	$45		; is character 'E' ?
		JR	Z,o2CF2		; to SIGN-FLAG if so

		CP	$65		; 'e' is acceptable as well.
		RET	NZ		; return as no exponent.

;; SIGN-FLAG
o2CF2:		LD	B,$FF		; initialize temporary sign byte to $FF

		RST	20H		; NEXT-CHAR
		CP	$2B		; is character '+' ?
		JR	Z,o2CFE		; to SIGN-DONE

		CP	$2D		; is character '-' ?
		JR	NZ,o2CFF	; to ST-E-PART as no sign

		INC	B		; set sign to zero

; now consider digits of exponent.
; Note. incidentally this is the only occasion in Spectrum BASIC when an
; expression may not be used when a number is expected.

;; SIGN-DONE
o2CFE:		RST	20H		; NEXT-CHAR

;; ST-E-PART
o2CFF:		CALL	o2D1B		; routine NUMERIC
		JR	C,o2CCF		; to DEC-RPT-C if not
					; raise 'Nonsense in BASIC'.

		PUSH	BC		; save sign (in B)
		CALL	o2D3B		; routine INT-TO-FP places exponent on stack
		CALL	o2DD5		; routine FP-TO-A  transfers it to A
		POP	BC		; restore sign
		JP	C,o31AD		; to REPORT-6 if overflow (over 255)
					; raise 'Number too big'.

		AND	A		; set flags
		JP	M,o31AD		; to REPORT-6 if over '127'.
					; raise 'Number too big'.
					; 127 is still way too high and it is
					; impossible to enter an exponent greater
					; than 39 from the keyboard. The error gets
					; raised later in E-TO-FP so two different
					; error messages depending how high A is.

		INC	B		; $FF to $00 or $00 to $01 - expendable now.
		JR	Z,o2D18		; forward to E-FP-JUMP if exponent positive

		NEG			; Negate the exponent.

;; E-FP-JUMP
o2D18:		JP	o2D4F		; JUMP forward to E-TO-FP to assign to
					; last value x on stack x * 10 to power A
					; a relative jump would have done.

; ---------------------
; Check for valid digit
; ---------------------
; This routine checks that the ASCII character in A is numeric
; returning with carry reset if so.

;; NUMERIC
o2D1B:		CP	$30		; "0"
		RET	C		; return if less than zero character.

		CP	$3A		; The upper test is "9"
		CCF			; Complement Carry Flag
		RET			; Return - carry clear if character "0" - "9"

; -----------
; Stack Digit
; -----------
; This subroutine is called from INT-TO-FP and DEC-TO-FP to stack a digit
; on the calculator stack.

;; STK-DIGIT
o2D22:		CALL	o2D1B		; routine NUMERIC
		RET	C		; return if not numeric character

		SUB	$30		; convert from ASCII to digit

; -----------------
; Stack accumulator
; -----------------
;
;

;; STACK-A
o2D28:		LD	C,A		; transfer to C
		LD	B,$00		; and make B zero

; ----------------------
; Stack BC register pair
; ----------------------
;

;; STACK-BC
o2D2B:		LD	IY,ERR_NR	; re-initialize ERR_NR

		XOR	A		; clear to signal small integer
		LD	E,A		; place in E for sign
		LD	D,C		; LSB to D
		LD	C,B		; MSB to C
		LD	B,A		; last byte not used
		CALL	o2AB6		; routine STK-STORE

		RST	28H		; FP-CALC
		DB	$38		; end-calc  make HL = STKEND-5

		AND	A		; clear carry
		RET			; before returning

; -------------------------
; Integer to floating point
; -------------------------
; This routine places one or more digits found in a BASIC line
; on the calculator stack multiplying the previous value by ten each time
; before adding in the new digit to form a last value on calculator stack.

;; INT-TO-FP
o2D3B:		PUSH	AF		; save first character

		RST	28H		; FP-CALC
		DB	$A0		; stk-zero    ; v=0. initial value
		DB	$38		; end-calc

		POP	AF		; fetch first character back.

;; NXT-DGT-2
o2D40:		CALL	o2D22		; routine STK-DIGIT puts 0-9 on stack
		RET	C		; will return when character is not numeric >

		RST	28H		; FP-CALC    ; v, d.
		DB	$01		; exchange    ; d, v.
		DB	$A4		; stk-ten     ; d, v, 10.
		DB	$04		; multiply    ; d, v*10.
		DB	$0F		; addition    ; d + v*10 = newvalue
		DB	$38		; end-calc    ; v.

		CALL	o0074		; routine CH-ADD+1 get next character
		JR	o2D40		; back to NXT-DGT-2 to process as a digit


;*********************************
;** Part 9. ARITHMETIC ROUTINES **
;*********************************

; --------------------------
; E-format to floating point
; --------------------------
; This subroutine is used by the PRINT-FP routine and the decimal to FP
; routines to stack a number expressed in exponent format.
; Note. Though not used by the ROM as such, it has also been set up as
; a unary calculator literal but this will not work as the accumulator
; is not available from within the calculator.

; on entry there is a value x on the calculator stack and an exponent of ten
; in A.    The required value is x + 10 ^ A

;; e-to-fp
;; E-TO-FP
o2D4F:		RLCA
		RRCA			; carry if bit 7 is set

		JR	NC,o2D55	; to E-SAVE  if positive.

		CPL			; make negative positive
		INC	A		; without altering carry.

;; E-SAVE
o2D55:		PUSH	AF		; save positive exp and sign in carry

		LD	HL,MEMBOT	; address MEM-0

		CALL	o350B		; routine FP-0/1
					; places an integer zero, if no carry,
					; else a one in mem-0 as a sign flag

		RST	28H		; FP-CALC
		DB	$A4		; stk-ten                    x, 10.
		DB	$38		; end-calc

		POP	AF		; pop the exponent.

; now enter a loop

;; E-LOOP
o2D60:		SRL	A		; 0>76543210>C

		JR	NC,o2D71	; forward to E-TST-END if no bit

		PUSH	AF		; save shifted exponent.

		RST	28H		; FP-CALC
		DB	$C1		; st-mem-1                   x, 10.
		DB	$E0		; get-mem-0                  x, 10, (0/1).
		DB	$00		; jump-true

		DB	$04		; to o2D6D, E-DIVSN

		DB	$04		; multiply                   x*10.
		DB	$33		; jump

		DB	$02		; to o2D6E, E-FETCH

;; E-DIVSN
o2D6D:		DB	$05		; division                   x/10.

;; E-FETCH
o2D6E:		DB	$E1		; get-mem-1                  x/10 or x*10, 10.
		DB	$38		; end-calc                   new x, 10.

		POP	AF		; restore shifted exponent

; the loop branched to here with no carry

;; E-TST-END
o2D71:		JR	Z,o2D7B		; forward to E-END  if A emptied of bits

		PUSH	AF		; re-save shifted exponent

		RST	28H		; FP-CALC
		DB	$31		; duplicate                  new x, 10, 10.
		DB	$04		; multiply                   new x, 100.
		DB	$38		; end-calc

		POP	AF		; restore shifted exponent
		JR	o2D60		; back to E-LOOP  until all bits done.

; ---

; although only the first pass is shown it can be seen that for each set bit
; representing a power of two, x is multiplied or divided by the
; corresponding power of ten.

;; E-END
o2D7B:		RST	28H		; FP-CALC                   final x, factor.
		DB	$02		; delete                     final x.
		DB	$38		; end-calc                   x.

		RET			; return




; -------------
; Fetch integer
; -------------
; This routine is called by the mathematical routines - FP-TO-BC, PRINT-FP,
; mult, re-stack and negate to fetch an integer from address HL.
; HL points to the stack or a location in MEM and no deletion occurs.
; If the number is negative then a similar process to that used in INT-STORE
; is used to restore the twos complement number to normal in DE and a sign
; in C.

;; INT-FETCH
o2D7F:		INC	HL		; skip zero indicator.
		LD	C,(HL)		; fetch sign to C
		INC	HL		; address low byte
		LD	A,(HL)		; fetch to A
		XOR	C		; two's complement
		SUB	C		;
		LD	E,A		; place in E
		INC	HL		; address high byte
		LD	A,(HL)		; fetch to A
		ADC	A,C		; two's complement
		XOR	C		;
		LD	D,A		; place in D
		RET			; return

; ------------------------
; Store a positive integer
; ------------------------
; This entry point is not used in this ROM but would
; store any integer as positive.

;; p-int-sto
o2D8C:		LD	C,$00		; make sign byte positive and continue

; -------------
; Store integer
; -------------
; this routine stores an integer in DE at address HL.
; It is called from mult, truncate, negate and sgn.
; The sign byte $00 +ve or $FF -ve is in C.
; If negative, the number is stored in 2's complement form so that it is
; ready to be added.

;; INT-STORE
o2D8E:		PUSH	HL		; preserve HL

		LD	(HL),$00	; first byte zero shows integer not exponent
		INC	HL		;
		LD	(HL),C		; then store the sign byte
		INC	HL		;
					; e.g.             +1             -1
		LD	A,E		; fetch low byte   00000001       00000001
		XOR	C		; xor sign         00000000   or  11111111
					; gives            00000001   or  11111110
		SUB	C		; sub sign         00000000   or  11111111
					; gives            00000001>0 or  11111111>C
		LD	(HL),A		; store 2's complement.
		INC	HL		;
		LD	A,D		; high byte        00000000       00000000
		ADC	A,C		; sign             00000000<0     11111111<C
					; gives            00000000   or  00000000
		XOR	C		; xor sign         00000000       11111111
		LD	(HL),A		; store 2's complement.
		INC	HL		;
		LD	(HL),$00	; last byte always zero for integers.
					; is not used and need not be looked at when
					; testing for zero but comes into play should
					; an integer be converted to fp.
		POP	HL		; restore HL
		RET			; return.


; -----------------------------
; Floating point to BC register
; -----------------------------
; This routine gets a floating point number e.g. 127.4 from the calculator
; stack to the BC register.

;; FP-TO-BC
o2DA2:		RST	28H		; FP-CALC            set HL to
		DB	$38		; end-calc            point to last value.

		LD	A,(HL)		; get first of 5 bytes
		AND	A		; and test
		JR	Z,o2DAD		; forward to FP-DELETE if an integer

; The value is first rounded up and then converted to integer.

		RST	28H		; FP-CALC           x.
		DB	$A2		; stk-half           x. 1/2.
		DB	$0F		; addition           x + 1/2.
		DB	$27		; int                int(x + .5)
		DB	$38		; end-calc

; now delete but leave HL pointing at integer

;; FP-DELETE
o2DAD:		RST	28H		; FP-CALC
		DB	$02		; delete
		DB	$38		; end-calc

		PUSH	HL		; save pointer.
		PUSH	DE		; and STKEND.
		EX	DE,HL		; make HL point to exponent/zero indicator
		LD	B,(HL)		; indicator to B
		CALL	o2D7F		; routine INT-FETCH
					; gets int in DE sign byte to C
					; but meaningless values if a large integer

		XOR	A		; clear A
		SUB	B		; subtract indicator byte setting carry
					; if not a small integer.

		BIT	7,C		; test a bit of the sign byte setting zero
					; if positive.

		LD	B,D		; transfer int
		LD	C,E		; to BC
		LD	A,E		; low byte to A as a useful return value.

		POP	DE		; pop STKEND
		POP	HL		; and pointer to last value
		RET			; return
					; if carry is set then the number was too big.

; ------------
; LOG(2^A)
; ------------
; This routine is used when printing floating point numbers to calculate
; the number of digits before the decimal point.

; first convert a one-byte signed integer to its five byte form.

;; LOG(2^A)
o2DC1:		LD	D,A		; store a copy of A in D.
		RLA			; test sign bit of A.
		SBC	A,A		; now $FF if negative or $00
		LD	E,A		; sign byte to E.
		LD	C,A		; and to C
		XOR	A		; clear A
		LD	B,A		; and B.
		CALL	o2AB6		; routine STK-STORE stacks number AEDCB

;  so 00 00 XX 00 00 (positive) or 00 FF XX FF 00 (negative).
;  i.e. integer indicator, sign byte, low, high, unused.

; now multiply exponent by log to the base 10 of two.

		RST	28H		; FP-CALC

		DB	$34		; stk-data                      .30103 (log 2)
		DB	$EF		; Exponent: $7F, Bytes: 4
		DB	$1A,$20,$9A,$85	;
		DB	$04		; multiply

		DB	$27		; int

		DB	$38		; end-calc

; -------------------
; Floating point to A
; -------------------
; this routine collects a floating point number from the stack into the
; accumulator returning carry set if not in range 0 - 255.
; Not all the calling routines raise an error with overflow so no attempt
; is made to produce an error report here.

;; FP-TO-A
o2DD5:		CALL	o2DA2		; routine FP-TO-BC returns with C in A also.
		RET	C		; return with carry set if > 65535, overflow

		PUSH	AF		; save the value and flags
		DEC	B		; and test that
		INC	B		; the high byte is zero.
		JR	Z,o2DE1		; forward  FP-A-END if zero

; else there has been 8-bit overflow

		POP	AF		; retrieve the value
		SCF			; set carry flag to show overflow
		RET			; and return.

; ---

;; FP-A-END
o2DE1:		POP	AF		; restore value and success flag and
		RET			; return.


; -----------------------------
; Print a floating point number
; -----------------------------
; Not a trivial task.
; Begin by considering whether to print a leading sign for negative numbers.

;; PRINT-FP
o2DE3:		RST	28H		; FP-CALC
		DB	$31		; duplicate
		DB	$36		; less-0
		DB	$00		; jump-true

		DB	$0B		; to o2DF2, PF-NEGTVE

		DB	$31		; duplicate
		DB	$37		; greater-0
		DB	$00		; jump-true

		DB	$0D		; to o2DF8, PF-POSTVE

; must be zero itself

		DB	$02		; delete
		DB	$38		; end-calc

		LD	A,$30		; prepare the character "0"

		RST	10H		; PRINT-A
		RET			; return.                 ->
					; ---

;; PF-NEGTVE
o2DF2:		DB	$2A		; abs
		DB	$38		; end-calc

		LD	A,$2D		; the character '-'

		RST	10H		; PRINT-A

; and continue to print the now positive number.

		RST	28H		; FP-CALC

;; PF-POSTVE
o2DF8:		DB	$A0		; stk-zero     x,0.     begin by
		DB	$C3		; st-mem-3     x,0.     clearing a temporary
		DB	$C4		; st-mem-4     x,0.     output buffer to
		DB	$C5		; st-mem-5     x,0.     fifteen zeros.
		DB	$02		; delete       x.
		DB	$38		; end-calc     x.

		EXX			; in case called from 'str$' then save the
		PUSH	HL		; pointer to whatever comes after
		EXX			; str$ as H'L' will be used.

; now enter a loop?

;; PF-LOOP
o2E01:		RST	28H		; FP-CALC
		DB	$31		; duplicate    x,x.
		DB	$27		; int          x,int x.
		DB	$C2		; st-mem-2     x,int x.
		DB	$03		; subtract     x-int x.     fractional part.
		DB	$E2		; get-mem-2    x-int x, int x.
		DB	$01		; exchange     int x, x-int x.
		DB	$C2		; st-mem-2     int x, x-int x.
		DB	$02		; delete       int x.
		DB	$38		; end-calc     int x.
					;
					; mem-2 holds the fractional part.

; HL points to last value int x

		LD	A,(HL)		; fetch exponent of int x.
		AND	A		; test
		JR	NZ,o2E56	; forward to PF-LARGE if a large integer
					; > 65535

; continue with small positive integer components in range 0 - 65535
; if original number was say .999 then this integer component is zero.

		CALL	o2D7F		; routine INT-FETCH gets x in DE
					; (but x is not deleted)

		LD	B,$10		; set B, bit counter, to 16d

		LD	A,D		; test if
		AND	A		; high byte is zero
		JR	NZ,o2E1E	; forward to PF-SAVE if 16-bit integer.

; and continue with integer in range 0 - 255.

		OR	E		; test the low byte for zero
					; i.e. originally just point something or other.
		JR	Z,o2E24		; forward if so to PF-SMALL

;

		LD	D,E		; transfer E to D
		LD	B,$08		; and reduce the bit counter to 8.

;; PF-SAVE
o2E1E:		PUSH	DE		; save the part before decimal point.
		EXX			;
		POP	DE		; and pop in into D'E'
		EXX			;
		JR	o2E7B		; forward to PF-BITS

; ---------------------

; the branch was here when 'int x' was found to be zero as in say 0.5.
; The zero has been fetched from the calculator stack but not deleted and
; this should occur now. This omission leaves the stack unbalanced and while
; that causes no problems with a simple PRINT statement, it will if str$ is
; being used in an expression e.g. "2" + STR$ 0.5 gives the result "0.5"
; instead of the expected result "20.5".
; credit Tony Stratton, 1982.
; A DB     02 delete is required immediately on using the calculator.

;; PF-SMALL
o2E24:		RST	28H		; FP-CALC       int x = 0.
o2E25:		DB	$E2		; get-mem-2      int x = 0, x-int x.
		DB	$38		; end-calc

		LD	A,(HL)		; fetch exponent of positive fractional number
		SUB	$7E		; subtract

		CALL	o2DC1		; routine LOG(2^A) calculates leading digits.

		LD	D,A		; transfer count to D
		LD	A,($5CAC)	; fetch total MEM-5-1
		SUB	D		;
		LD	($5CAC),A	; MEM-5-1
		LD	A,D		;
		CALL	o2D4F		; routine E-TO-FP

		RST	28H		; FP-CALC
		DB	$31		; duplicate
		DB	$27		; int
		DB	$C1		; st-mem-1
		DB	$03		; subtract
		DB	$E1		; get-mem-1
		DB	$38		; end-calc

		CALL	o2DD5		; routine FP-TO-A

		PUSH	HL		; save HL
		LD	($5CA1),A	; MEM-3-1
		DEC	A		;
		RLA			;
		SBC	A,A		;
		INC	A		;

		LD	HL,$5CAB	; address MEM-5-1 leading digit counter
		LD	(HL),A		; store counter
		INC	HL		; address MEM-5-2 total digits
		ADD	A,(HL)		; add counter to contents
		LD	(HL),A		; and store updated value
		POP	HL		; restore HL

		JP	o2ECF		; JUMP forward to PF-FRACTN

; ---

; Note. while it would be pedantic to comment on every occasion a JP
; instruction could be repoaced with a JR instruction, this applies to the
; above, which is useful if you wish to correct the unbalanced stack error
; by inserting a 'DB     02 delete' at o2E25, and maintain main addresses.

; the branch was here with a large positive integer > 65535 e.g. 123456789
; the accumulator holds the exponent.

;; PF-LARGE
o2E56:		SUB	$80		; make exponent positive
		CP	$1C		; compare to 28
		JR	C,o2E6F		; to PF-MEDIUM if integer <= 2^27

		CALL	o2DC1		; routine LOG(2^A)
		SUB	$07		;
		LD	B,A		;
		LD	HL,$5CAC	; address MEM-5-1 the leading digits counter.
		ADD	A,(HL)		; add A to contents
		LD	(HL),A		; store updated value.
		LD	A,B		;
		NEG			; negate
		CALL	o2D4F		; routine E-TO-FP
		JR	o2E01		; back to PF-LOOP

; ----------------------------

;; PF-MEDIUM
o2E6F:		EX	DE,HL		;
		CALL	o2FBA		; routine FETCH-TWO
		EXX			;
		SET	7,D		;
		LD	A,L		;
		EXX			;
		SUB	$80		;
		LD	B,A		;

; the branch was here to handle bits in DE with 8 or 16 in B  if small int
; and integer in D'E', 6 nibbles will accommodate 065535 but routine does
; 32-bit numbers as well from above

;; PF-BITS
o2E7B:		SLA	E		;  C<xxxxxxxx<0
		RL	D		;  C<xxxxxxxx<C
		EXX			;
		RL	E		;  C<xxxxxxxx<C
		RL	D		;  C<xxxxxxxx<C
		EXX			;

		LD	HL,$5CAA	; set HL to mem-4-5th last byte of buffer
		LD	C,$05		; set byte count to 5 -  10 nibbles

;; PF-BYTES
o2E8A:		LD	A,(HL)		; fetch 0 or prev value
		ADC	A,A		; shift left add in carry    C<xxxxxxxx<C

		DAA			; Decimal Adjust Accumulator.
					; if greater than 9 then the left hand
					; nibble is incremented. If greater than
					; 99 then adjusted and carry set.
					; so if we'd built up 7 and a carry came in
					;      0000 0111 < C
					;      0000 1111
					; daa     1 0101  which is 15 in BCD

		LD	(HL),A		; put back
		DEC	HL		; work down thru mem 4
		DEC	C		; decrease the 5 counter.
		JR	NZ,o2E8A	; back to PF-BYTES until the ten nibbles rolled

		DJNZ	o2E7B		; back to PF-BITS until 8 or 16 (or 32) done

; at most 9 digits for 32-bit number will have been loaded with digits
; each of the 9 nibbles in mem 4 is poaced into ten bytes in mem-3 and mem 4
; unless the nibble is zero as the buffer is already zero.
; ( or in the case of mem-5 will become zero as a result of RLD instruction )

		XOR	A		; clear to accept
		LD	HL,$5CA6	; address MEM-4-0 byte destination.
		LD	DE,$5CA1	; address MEM-3-0 nibble source.
		LD	B,$09		; the count is 9 (not ten) as the first
					; nibble is known to be blank.

		RLD			; shift RH nibble to left in (HL)
					;    A           (HL)
					; 0000 0000 < 0000 3210
					; 0000 0000   3210 0000
					; A picks up the blank nibble


		LD	C,$FF		; set a flag to indicate when a significant
					; digit has been encountered.

;; PF-DIGITS
o2EA1:		RLD
					;    A           (HL)
					; 0000 0000 < 7654 3210
					; 0000 7654   3210 0000


		JR	NZ,o2EA9	; to PF-INSERT if non-zero value picked up.

		DEC	C		; test
		INC	C		; flag
		JR	NZ,o2EB3	; skip forward to PF-TEST-2 if flag still $FF
					; indicating this is a leading zero.

; but if the zero is a significant digit e.g. 10 then include in digit totals.
; the path for non-zero digits rejoins here.

;; PF-INSERT
o2EA9:		LD	(DE),A		; insert digit at destination
		INC	DE		; increase the destination pointer
		INC	(IY+$71)	; increment MEM-5-1st  digit counter
		INC	(IY+$72)	; increment MEM-5-2nd  leading digit counter
		LD	C,$00		; set flag to zero indicating that any
					; subsequent zeros are significant and not
					; leading.

;; PF-TEST-2
o2EB3:		BIT	0,B		; test if the nibble count is even
		JR	Z,o2EB8		; skip to PF-ALL-9 if so to deal with the
					; other nibble in the same byte

		INC	HL		; point to next source byte if not

;; PF-ALL-9
o2EB8:		DJNZ	o2EA1		; decrement the nibble count, back to PF-DIGITS
					; if all nine not done.

; For 8-bit integers there will be at most 3 digits.
; For 16-bit integers there will be at most 5 digits.
; but for larger integers there could be nine leading digits.
; if nine digits complete then the last one is rounded up as the number will
; be printed using E-format notation

		LD	A,($5CAB)	; fetch digit count from MEM-5-1st
		SUB	$09		; subtract 9 - max possible
		JR	C,o2ECB		; forward if less to PF-MORE

		DEC	(IY+$71)	; decrement digit counter MEM-5-1st to 8
		LD	A,$04		; load A with the value 4.
		CP	(IY+$6F)	; compare with MEM-4-4th - the ninth digit
		JR	o2F0C		; forward to PF-ROUND
					; to consider rounding.

; ---------------------------------------

; now delete int x from calculator stack and fetch fractional part.

;; PF-MORE
o2ECB:		RST	28H		; FP-CALC        int x.
		DB	$02		; delete          .
		DB	$E2		; get-mem-2       x - int x = f.
		DB	$38		; end-calc        f.

;; PF-FRACTN
o2ECF:		EX	DE,HL		;
		CALL	o2FBA		; routine FETCH-TWO
		EXX			;
		LD	A,$80		;
		SUB	L		;
		LD	L,$00		;
		SET	7,D		;
		EXX			;
		CALL	o2FDD		; routine SHIFT-FP

;; PF-FRN-LP
o2EDF:		LD	A,(IY+$71)	; MEM-5-1st
		CP	$08		;
		JR	C,o2EEC		; to PF-FR-DGT

		EXX			;
		RL	D		;
		EXX			;
		JR	o2F0C		; to PF-ROUND

; ---

;; PF-FR-DGT
o2EEC:		LD	BC,$0200	;

;; PF-FR-EXX
o2EEF:		LD	A,E		;
		CALL	o2F8B		; routine CA-10*A+C
		LD	E,A		;
		LD	A,D		;
		CALL	o2F8B		; routine CA-10*A+C
		LD	D,A		;
		PUSH	BC		;
		EXX			;
		POP	BC		;
		DJNZ	o2EEF		; to PF-FR-EXX

		LD	HL,$5CA1	; MEM-3
		LD	A,C		;
		LD	C,(IY+$71)	; MEM-5-1st
		ADD	HL,BC		;
		LD	(HL),A		;
		INC	(IY+$71)	; MEM-5-1st
		JR	o2EDF		; to PF-FRN-LP

; ----------------

; 1) with 9 digits but 8 in mem-5-1 and A holding 4, carry set if rounding up.
; e.g.
;      999999999 is printed as 1E+9
;      100000001 is printed as 1E+8
;      100000009 is printed as 1.0000001E+8

;; PF-ROUND
o2F0C:		PUSH	AF		; save A and flags
		LD	HL,$5CA1	; address MEM-3 start of digits
		LD	C,(IY+$71)	; MEM-5-1st No. of digits to C
		LD	B,$00		; prepare to add
		ADD	HL,BC		; address last digit + 1
		LD	B,C		; No. of digits to B counter
		POP	AF		; restore A and carry flag from comparison.

;; PF-RND-LP
o2F18:		DEC	HL		; address digit at rounding position.
		LD	A,(HL)		; fetch it
		ADC	A,$00		; add carry from the comparison
		LD	(HL),A		; put back result even if $0A.
		AND	A		; test A
		JR	Z,o2F25		; skip to PF-R-BACK if ZERO?

		CP	$0A		; compare to 'ten' - overflow
		CCF			; complement carry flag so that set if ten.
		JR	NC,o2F2D	; forward to PF-COUNT with 1 - 9.

;; PF-R-BACK
o2F25:		DJNZ	o2F18		; loop back to PF-RND-LP

; if B counts down to zero then we've rounded right back as in 999999995.
; and the first 8 locations all hold $0A.


		LD	(HL),$01	; load first location with digit 1.
		INC	B		; make B hold 1 also.
					; could save an instruction byte here.
		INC	(IY+$72)	; make MEM-5-2nd hold 1.
					; and proceed to initialize total digits to 1.

;; PF-COUNT
o2F2D:		LD	(IY+$71),B	; MEM-5-1st

; now balance the calculator stack by deleting  it

		RST	28H		; FP-CALC
		DB	$02		; delete
		DB	$38		; end-calc

; note if used from str$ then other values may be on the calculator stack.
; we can also restore the next literal pointer from its position on the
; machine stack.

		EXX			;
		POP	HL		; restore next literal pointer.
		EXX			;

		LD	BC,($5CAB)	; set C to MEM-5-1st digit counter.
					; set B to MEM-5-2nd leading digit counter.
		LD	HL,$5CA1	; set HL to start of digits at MEM-3-1
		LD	A,B		;
		CP	$09		;
		JR	C,o2F46		; to PF-NOT-E

		CP	$FC		;
		JR	C,o2F6C		; to PF-E-FRMT

;; PF-NOT-E
o2F46:		AND	A		; test for zero leading digits as in .123

		CALL	Z,o15EF		; routine OUT-CODE prints a zero e.g. 0.123

;; PF-E-SBRN
o2F4A:		XOR	A		;
		SUB	B		;
		JP	M,o2F52		; skip forward to PF-OUT-LP if originally +ve

		LD	B,A		; else negative count now +ve
		JR	o2F5E		; forward to PF-DC-OUT       ->

; ---

;; PF-OUT-LP
o2F52:		LD	A,C		; fetch total digit count
		AND	A		; test for zero
		JR	Z,o2F59		; forward to PF-OUT-DT if so

		LD	A,(HL)		; fetch digit
		INC	HL		; address next digit
		DEC	C		; decrease total digit counter

;; PF-OUT-DT
o2F59:		CALL	o15EF		; routine OUT-CODE outputs it.
		DJNZ	o2F52		; loop back to PF-OUT-LP until B leading
					; digits output.

;; PF-DC-OUT
o2F5E:		LD	A,C		; fetch total digits and
		AND	A		; test if also zero
		RET	Z		; return if so              -->

;

		INC	B		; increment B
		LD	A,$2E		; prepare the character '.'

;; PF-DEC-0S
o2F64:		RST	10H		; PRINT-A outputs the character '.' or "0"

		LD	A,$30		; prepare the character "0"
					; (for cases like .000012345678)
		DJNZ	o2F64		; loop back to PF-DEC-0S for B times.

		LD	B,C		; load B with now trailing digit counter.
		JR	o2F52		; back to PF-OUT-LP

; ---------------------------------

; the branch was here for E-format printing e.g. 123456789 => 1.2345679e+8

;; PF-E-FRMT
o2F6C:		LD	D,B		; counter to D
		DEC	D		; decrement
		LD	B,$01		; load B with 1.

		CALL	o2F4A		; routine PF-E-SBRN above

		LD	A,$45		; prepare character 'e'
		RST	10H		; PRINT-A

		LD	C,D		; exponent to C
		LD	A,C		; and to A
		AND	A		; test exponent
		JP	P,o2F83		; to PF-E-POS if positive

		NEG			; negate
		LD	C,A		; positive exponent to C
		LD	A,$2D		; prepare character '-'
		JR	o2F85		; skip to PF-E-SIGN

; ---

;; PF-E-POS
o2F83:		LD	A,$2B		; prepare character '+'

;; PF-E-SIGN
o2F85:		RST	10H		; PRINT-A outputs the sign

		LD	B,$00		; make the high byte zero.
		JP	o1A1B		; exit via OUT-NUM-1 to print exponent in BC

; ------------------------------
; Handle printing floating point
; ------------------------------
; This subroutine is called twice from above when printing floating-point
; numbers. It returns 10*A +C in registers C and A

;; CA-10*A+C
o2F8B:		PUSH	DE		; preserve DE.
		LD	L,A		; transfer A to L
		LD	H,$00		; zero high byte.
		LD	E,L		; copy HL
		LD	D,H		; to DE.
		ADD	HL,HL		; double (*2)
		ADD	HL,HL		; double (*4)
		ADD	HL,DE		; add DE (*5)
		ADD	HL,HL		; double (*10)
		LD	E,C		; copy C to E    (D is 0)
		ADD	HL,DE		; and add to give required result.
		LD	C,H		; transfer to
		LD	A,L		; destination registers.
		POP	DE		; restore DE
		RET			; return with result.

; --------------
; Prepare to add
; --------------
; This routine is called twice by addition to prepare the two numbers. The
; exponent is picked up in A and the location made zero. Then the sign bit
; is tested before being set to the implied state. Negative numbers are twos
; complemented.

;; PREP-ADD
o2F9B:		LD	A,(HL)		; pick up exponent
		LD	(HL),$00	; make location zero
		AND	A		; test if number is zero
		RET	Z		; return if so

		INC	HL		; address mantissa
		BIT	7,(HL)		; test the sign bit
		SET	7,(HL)		; set it to implied state
		DEC	HL		; point to exponent
		RET	Z		; return if positive number.

		PUSH	BC		; preserve BC
		LD	BC,$0005	; length of number
		ADD	HL,BC		; point HL past end
		LD	B,C		; set B to 5 counter
		LD	C,A		; store exponent in C
		SCF			; set carry flag

;; NEG-BYTE
o2FAF:		DEC	HL		; work from LSB to MSB
		LD	A,(HL)		; fetch byte
		CPL			; complement
		ADC	A,$00		; add in initial carry or from prev operation
		LD	(HL),A		; put back
		DJNZ	o2FAF		; loop to NEG-BYTE till all 5 done

		LD	A,C		; stored exponent to A
		POP	BC		; restore original BC
		RET			; return

; -----------------
; Fetch two numbers
; -----------------
; This routine is called twice when printing floating point numbers and also
; to fetch two numbers by the addition, multiply and division routines.
; HL addresses the first number, DE addresses the second number.
; For arithmetic only, A holds the sign of the result which is stored in
; the second location.

;; FETCH-TWO
o2FBA:		PUSH	HL		; save pointer to first number, result if math.
		PUSH	AF		; save result sign.

		LD	C,(HL)		;
		INC	HL		;

		LD	B,(HL)		;
		LD	(HL),A		; store the sign at correct location in
					; destination 5 bytes for arithmetic only.
		INC	HL		;

		LD	A,C		;
		LD	C,(HL)		;
		PUSH	BC		;
		INC	HL		;
		LD	C,(HL)		;
		INC	HL		;
		LD	B,(HL)		;
		EX	DE,HL		;
		LD	D,A		;
		LD	E,(HL)		;
		PUSH	DE		;
		INC	HL		;
		LD	D,(HL)		;
		INC	HL		;
		LD	E,(HL)		;
		PUSH	DE		;
		EXX			;
		POP	DE		;
		POP	HL		;
		POP	BC		;
		EXX			;
		INC	HL		;
		LD	D,(HL)		;
		INC	HL		;
		LD	E,(HL)		;

		POP	AF		; restore possible result sign.
		POP	HL		; and pointer to possible result.
		RET			; return.

; ---------------------------------
; Shift floating point number right
; ---------------------------------
;
;

;; SHIFT-FP
o2FDD:		AND	A		;
		RET	Z		;

		CP	$21		;
		JR	NC,o2FF9	; to ADDEND-0

		PUSH	BC		;
		LD	B,A		;

;; ONE-SHIFT
o2FE5:		EXX
		SRA	L		;
		RR	D		;
		RR	E		;
		EXX			;
		RR	D		;
		RR	E		;
		DJNZ	o2FE5		; to ONE-SHIFT

		POP	BC		;
		RET	NC		;

		CALL	o3004		; routine ADD-BACK
		RET	NZ		;

;; ADDEND-0
o2FF9:		EXX
		XOR	A		;

;; ZEROS-4/5
o2FFB:		LD	L,$00		;
		LD	D,A		;
		LD	E,L		;
		EXX			;
		LD	DE,$0000	;
		RET			;

; ------------------
; Add back any carry
; ------------------
;
;

;; ADD-BACK
o3004:		INC	E		;
		RET	NZ		;

		INC	D		;
		RET	NZ		;

		EXX			;
		INC	E		;
		JR	NZ,o300D	; to ALL-ADDED

		INC	D		;

;; ALL-ADDED
o300D:		EXX
		RET			;

; -----------------------
; Handle subtraction (03)
; -----------------------
; Subtraction is done by switching the sign byte/bit of the second number
; which may be integer of floating point and continuing into addition.

;; subtract
o300F:		EX	DE,HL		; address second number with HL

		CALL	o346E		; routine NEGATE switches sign

		EX	DE,HL		; address first number again
					; and continue.

; --------------------
; Handle addition (0F)
; --------------------
; HL points to first number, DE to second.
; If they are both integers, then go for the easy route.

;; addition
o3014:		LD	A,(DE)		; fetch first byte of second
		OR	(HL)		; combine with first byte of first
		JR	NZ,o303E	; forward to FULL-ADDN if at least one was
					; in floating point form.

; continue if both were small integers.

		PUSH	DE		; save pointer to lowest number for result.

		INC	HL		; address sign byte and
		PUSH	HL		; push the pointer.

		INC	HL		; address low byte
		LD	E,(HL)		; to E
		INC	HL		; address high byte
		LD	D,(HL)		; to D
		INC	HL		; address unused byte

		INC	HL		; address known zero indicator of 1st number
		INC	HL		; address sign byte

		LD	A,(HL)		; sign to A, $00 or $FF

		INC	HL		; address low byte
		LD	C,(HL)		; to C
		INC	HL		; address high byte
		LD	B,(HL)		; to B

		POP	HL		; pop result sign pointer
		EX	DE,HL		; integer to HL

		ADD	HL,BC		; add to the other one in BC
					; setting carry if overflow.

		EX	DE,HL		; save result in DE bringing back sign pointer

		ADC	A,(HL)		; if pos/pos A=01 with overflow else 00
					; if neg/neg A=FF with overflow else FE
					; if mixture A=00 with overflow else FF

		RRCA			; bit 0 to (C)

		ADC	A,$00		; both acceptable signs now zero

		JR	NZ,o303C	; forward to ADDN-OFLW if not

		SBC	A,A		; restore a negative result sign

		LD	(HL),A		;
		INC	HL		;
		LD	(HL),E		;
		INC	HL		;
		LD	(HL),D		;
		DEC	HL		;
		DEC	HL		;
		DEC	HL		;

		POP	DE		; STKEND
		RET			;

; ---

;; ADDN-OFLW
o303C:		DEC	HL		;
		POP	DE		;

;; FULL-ADDN
o303E:		CALL	o3293		; routine RE-ST-TWO
		EXX			;
		PUSH	HL		;
		EXX			;
		PUSH	DE		;
		PUSH	HL		;
		CALL	o2F9B		; routine PREP-ADD
		LD	B,A		;
		EX	DE,HL		;
		CALL	o2F9B		; routine PREP-ADD
		LD	C,A		;
		CP	B		;
		JR	NC,o3055	; to SHIFT-LEN

		LD	A,B		;
		LD	B,C		;
		EX	DE,HL		;

;; SHIFT-LEN
o3055:		PUSH	AF		;
		SUB	B		;
		CALL	o2FBA		; routine FETCH-TWO
		CALL	o2FDD		; routine SHIFT-FP
		POP	AF		;
		POP	HL		;
		LD	(HL),A		;
		PUSH	HL		;
		LD	L,B		;
		LD	H,C		;
		ADD	HL,DE		;
		EXX			;
		EX	DE,HL		;
		ADC	HL,BC		;
		EX	DE,HL		;
		LD	A,H		;
		ADC	A,L		;
		LD	L,A		;
		RRA			;
		XOR	L		;
		EXX			;
		EX	DE,HL		;
		POP	HL		;
		RRA			;
		JR	NC,o307C	; to TEST-NEG

		LD	A,$01		;
		CALL	o2FDD		; routine SHIFT-FP
		INC	(HL)		;
		JR	Z,o309F		; to ADD-REP-6

;; TEST-NEG
o307C:		EXX
		LD	A,L		;
		AND	$80		;
		EXX			;
		INC	HL		;
		LD	(HL),A		;
		DEC	HL		;
		JR	Z,o30A5		; to GO-NC-MLT

		LD	A,E		;
		NEG			; Negate
		CCF			; Complement Carry Flag
		LD	E,A		;
		LD	A,D		;
		CPL			;
		ADC	A,$00		;
		LD	D,A		;
		EXX			;
		LD	A,E		;
		CPL			;
		ADC	A,$00		;
		LD	E,A		;
		LD	A,D		;
		CPL			;
		ADC	A,$00		;
		JR	NC,o30A3	; to END-COMPL

		RRA			;
		EXX			;
		INC	(HL)		;

;; ADD-REP-6
o309F:		JP	Z,o31AD		; to REPORT-6

		EXX			;

;; END-COMPL
o30A3:		LD	D,A		;
		EXX			;

;; GO-NC-MLT
o30A5:		XOR	A		;
		JP	o3155		; to TEST-NORM

; -----------------------------
; Used in 16 bit multiplication
; -----------------------------
; This routine is used, in the first instance, by the multiply calculator
; literal to perform an integer multiplication in preference to
; 32-bit multiplication to which it will resort if this overflows.
;
; It is also used by STK-VAR to calculate array subscripts and by DIM to
; calculate the space required for multi-dimensional arrays.

;; HL-HL*DE
o30A9:		PUSH	BC		; preserve BC throughout
		LD	B,$10		; set B to 16
		LD	A,H		; save H in A high byte
		LD	C,L		; save L in C low byte
		LD	HL,$0000	; initialize result to zero

; now enter a loop.

;; HL-LOOP
o30B1:		ADD	HL,HL		; double result
		JR	C,o30BE		; to HL-END if overflow

		RL	C		; shift AC left into carry
		RLA			;
		JR	NC,o30BC	; to HL-AGAIN to skip addition if no carry

		ADD	HL,DE		; add in DE
		JR	C,o30BE		; to HL-END if overflow

;; HL-AGAIN
o30BC:		DJNZ	o30B1		; back to HL-LOOP for all 16 bits

;; HL-END
o30BE:		POP	BC		; restore preserved BC
		RET			; return with carry reset if successful
					; and result in HL.

; ----------------------------------------------
; THE 'PREPARE TO MULTIPLY OR DIVIDE' SUBROUTINE
; ----------------------------------------------
;   This routine is called in succession from multiply and divide to prepare
;   two mantissas by setting the leftmost bit that is used for the sign.
;   On the first call A holds zero and picks up the sign bit. On the second
;   call the two bits are XORed to form the result sign - minus * minus giving
;   plus etc. If either number is zero then this is flagged.
;   HL addresses the exponent.

;; PREP-M/D
o30C0:		CALL	o34E9		; routine TEST-ZERO  preserves accumulator.
		RET	C		; return carry set if zero

		INC	HL		; address first byte of mantissa
		XOR	(HL)		; pick up the first or xor with first.
		SET	7,(HL)		; now set to give true 32-bit mantissa
		DEC	HL		; point to exponent
		RET			; return with carry reset

; ----------------------
; THE 'MULTIPLY' ROUTINE
; ----------------------
; (offset: $04 'multiply')
;
;
;   "He said go forth and something about mathematics, I wasn't really
;    listening" - overheard conversation between two unicorns.
;    [ The Odd Streak ].

;; multiply
o30CA:		LD	A,(DE)		;
		OR	(HL)		;
		JR	NZ,o30F0	; to MULT-LONG

		PUSH	DE		;
		PUSH	HL		;
		PUSH	DE		;
		CALL	o2D7F		; routine INT-FETCH
		EX	DE,HL		;
		EX	(SP),HL		;
		LD	B,C		;
		CALL	o2D7F		; routine INT-FETCH
		LD	A,B		;
		XOR	C		;
		LD	C,A		;
		POP	HL		;
		CALL	o30A9		; routine HL-HL*DE
		EX	DE,HL		;
		POP	HL		;
		JR	C,o30EF		; to MULT-OFLW

		LD	A,D		;
		OR	E		;
		JR	NZ,o30EA	; to MULT-RSLT

		LD	C,A		;

;; MULT-RSLT
o30EA:		CALL	o2D8E		; routine INT-STORE
		POP	DE		;
		RET			;

; ---

;; MULT-OFLW
o30EF:		POP	DE		;

;; MULT-LONG
o30F0:		CALL	o3293		; routine RE-ST-TWO
		XOR	A		;
		CALL	o30C0		; routine PREP-M/D
		RET	C		;

		EXX			;
		PUSH	HL		;
		EXX			;
		PUSH	DE		;
		EX	DE,HL		;
		CALL	o30C0		; routine PREP-M/D
		EX	DE,HL		;
		JR	C,o315D		; to ZERO-RSLT

		PUSH	HL		;
		CALL	o2FBA		; routine FETCH-TWO
		LD	A,B		;
		AND	A		;
		SBC	HL,HL		;
		EXX			;
		PUSH	HL		;
		SBC	HL,HL		;
		EXX			;
		LD	B,$21		;
		JR	o3125		; to STRT-MLT

; ---

;; MLT-LOOP
o3114:		JR	NC,o311B	; to NO-ADD

		ADD	HL,DE		;
		EXX			;
		ADC	HL,DE		;
		EXX			;

;; NO-ADD
o311B:		EXX
		RR	H		;
		RR	L		;
		EXX			;
		RR	H		;
		RR	L		;

;; STRT-MLT
o3125:		EXX
		RR	B		;
		RR	C		;
		EXX			;
		RR	C		;
		RRA			;
		DJNZ	o3114		; to MLT-LOOP

		EX	DE,HL		;
		EXX			;
		EX	DE,HL		;
		EXX			;
		POP	BC		;
		POP	HL		;
		LD	A,B		;
		ADD	A,C		;
		JR	NZ,o313B	; to MAKE-EXPT

		AND	A		;

;; MAKE-EXPT
o313B:		DEC	A		;
		CCF			; Complement Carry Flag

;; DIVN-EXPT
o313D:		RLA
		CCF			; Complement Carry Flag
		RRA			;
		JP	P,o3146		; to OFLW1-CLR

		JR	NC,o31AD	; to REPORT-6

		AND	A		;

;; OFLW1-CLR
o3146:		INC	A		;
		JR	NZ,o3151	; to OFLW2-CLR

		JR	C,o3151		; to OFLW2-CLR

		EXX			;
		BIT	7,D		;
		EXX			;
		JR	NZ,o31AD	; to REPORT-6

;; OFLW2-CLR
o3151:		LD	(HL),A		;
		EXX			;
		LD	A,B		;
		EXX			;

;; TEST-NORM
o3155:		JR	NC,o316C	; to NORMALISE

		LD	A,(HL)		;
		AND	A		;

;; NEAR-ZERO
o3159:		LD	A,$80		;
		JR	Z,o315E		; to SKIP-ZERO

;; ZERO-RSLT
o315D:		XOR	A		;

;; SKIP-ZERO
o315E:		EXX
		AND	D		;
		CALL	o2FFB		; routine ZEROS-4/5
		RLCA			;
		LD	(HL),A		;
		JR	C,o3195		; to OFLOW-CLR

		INC	HL		;
		LD	(HL),A		;
		DEC	HL		;
		JR	o3195		; to OFLOW-CLR

; ---

;; NORMALISE
o316C:		LD	B,$20		;

;; SHIFT-ONE
o316E:		EXX
		BIT	7,D		;
		EXX			;
		JR	NZ,o3186	; to NORML-NOW

		RLCA			;
		RL	E		;
		RL	D		;
		EXX			;
		RL	E		;
		RL	D		;
		EXX			;
		DEC	(HL)		;
		JR	Z,o3159		; to NEAR-ZERO

		DJNZ	o316E		; to SHIFT-ONE

		JR	o315D		; to ZERO-RSLT

; ---

;; NORML-NOW
o3186:		RLA
		JR	NC,o3195	; to OFLOW-CLR

		CALL	o3004		; routine ADD-BACK
		JR	NZ,o3195	; to OFLOW-CLR

		EXX			;
		LD	D,$80		;
		EXX			;
		INC	(HL)		;
		JR	Z,o31AD		; to REPORT-6

;; OFLOW-CLR
o3195:		PUSH	HL		;
		INC	HL		;
		EXX			;
		PUSH	DE		;
		EXX			;
		POP	BC		;
		LD	A,B		;
		RLA			;
		RL	(HL)		;
		RRA			;
		LD	(HL),A		;
		INC	HL		;
		LD	(HL),C		;
		INC	HL		;
		LD	(HL),D		;
		INC	HL		;
		LD	(HL),E		;
		POP	HL		;
		POP	DE		;
		EXX			;
		POP	HL		;
		EXX			;
		RET			;

; ---

;; REPORT-6
o31AD:		RST	08H		; ERROR-1
		DB	$05		; Error Report: Number too big

; ----------------------
; THE 'DIVISION' ROUTINE
; ----------------------
; (offset: $05 'division')
;
;   "He who can properly define and divide is to be considered a god"
;   - Plato,  429 - 347 B.C.

;; division
o31AF:		CALL	o3293		; routine RE-ST-TWO
		EX	DE,HL		;
		XOR	A		;
		CALL	o30C0		; routine PREP-M/D
		JR	C,o31AD		; to REPORT-6

		EX	DE,HL		;
		CALL	o30C0		; routine PREP-M/D
		RET	C		;

		EXX			;
		PUSH	HL		;
		EXX			;
		PUSH	DE		;
		PUSH	HL		;
		CALL	o2FBA		; routine FETCH-TWO
		EXX			;
		PUSH	HL		;
		LD	H,B		;
		LD	L,C		;
		EXX			;
		LD	H,C		;
		LD	L,B		;
		XOR	A		;
		LD	B,$DF		;
		JR	o31E2		; to DIV-START

; ---

;; DIV-LOOP
o31D2:		RLA
		RL	C		;
		EXX			;
		RL	C		;
		RL	B		;
		EXX			;

;; div-34th
o31DB:		ADD	HL,HL		;
		EXX			;
		ADC	HL,HL		;
		EXX			;
		JR	C,o31F2		; to SUBN-ONLY

;; DIV-START
o31E2:		SBC	HL,DE		;
		EXX			;
		SBC	HL,DE		;
		EXX			;
		JR	NC,o31F9	; to NO-RSTORE

		ADD	HL,DE		;
		EXX			;
		ADC	HL,DE		;
		EXX			;
		AND	A		;
		JR	o31FA		; to COUNT-ONE

; ---

;; SUBN-ONLY
o31F2:		AND	A		;
		SBC	HL,DE		;
		EXX			;
		SBC	HL,DE		;
		EXX			;

;; NO-RSTORE
o31F9:		SCF

;; COUNT-ONE
o31FA:		INC	B		;
		JP	M,o31D2		; to DIV-LOOP

		PUSH	AF		;
		JR	Z,o31E2		; to DIV-START

;
;
;
;

		LD	E,A		;
		LD	D,C		;
		EXX			;
		LD	E,C		;
		LD	D,B		;
		POP	AF		;
		RR	B		;
		POP	AF		;
		RR	B		;
		EXX			;
		POP	BC		;
		POP	HL		;
		LD	A,B		;
		SUB	C		;
		JP	o313D		; jump back to DIVN-EXPT

; ------------------------------------
; Integer truncation towards zero ($3A)
; ------------------------------------
;
;

;; truncate
o3214:		LD	A,(HL)		;
		AND	A		;
		RET	Z		;

		CP	$81		;
		JR	NC,o3221	; to T-GR-ZERO

		LD	(HL),$00	;
		LD	A,$20		;
		JR	o3272		; to NIL-BYTES

; ---

;; T-GR-ZERO
o3221:		CP	$91		;
		JR	NZ,o323F	; to T-SMALL

		INC	HL		;
		INC	HL		;
		INC	HL		;
		LD	A,$80		;
		AND	(HL)		;
		DEC	HL		;
		OR	(HL)		;
		DEC	HL		;
		JR	NZ,o3233	; to T-FIRST

		LD	A,$80		;
		XOR	(HL)		;

;; T-FIRST
o3233:		DEC	HL		;
		JR	NZ,o326C	; to T-EXPNENT

		LD	(HL),A		;
		INC	HL		;
		LD	(HL),$FF	;
		DEC	HL		;
		LD	A,$18		;
		JR	o3272		; to NIL-BYTES

; ---

;; T-SMALL
o323F:		JR	NC,o326D	; to X-LARGE

		PUSH	DE		;
		CPL			;
		ADD	A,$91		;
		INC	HL		;
		LD	D,(HL)		;
		INC	HL		;
		LD	E,(HL)		;
		DEC	HL		;
		DEC	HL		;
		LD	C,$00		;
		BIT	7,D		;
		JR	Z,o3252		; to T-NUMERIC

		DEC	C		;

;; T-NUMERIC
o3252:		SET	7,D		;
		LD	B,$08		;
		SUB	B		;
		ADD	A,B		;
		JR	C,o325E		; to T-TEST

		LD	E,D		;
		LD	D,$00		;
		SUB	B		;

;; T-TEST
o325E:		JR	Z,o3267		; to T-STORE

		LD	B,A		;

;; T-SHIFT
o3261:		SRL	D		;
		RR	E		;
		DJNZ	o3261		; to T-SHIFT

;; T-STORE
o3267:		CALL	o2D8E		; routine INT-STORE
		POP	DE		;
		RET			;

; ---

;; T-EXPNENT
o326C:		LD	A,(HL)		;

;; X-LARGE
o326D:		SUB	$A0		;
		RET	P		;

		NEG			; Negate

;; NIL-BYTES
o3272:		PUSH	DE		;
		EX	DE,HL		;
		DEC	HL		;
		LD	B,A		;
		SRL	B		;
		SRL	B		;
		SRL	B		;
		JR	Z,o3283		; to BITS-ZERO

;; BYTE-ZERO
o327E:		LD	(HL),$00	;
		DEC	HL		;
		DJNZ	o327E		; to BYTE-ZERO

;; BITS-ZERO
o3283:		AND	$07		;
		JR	Z,o3290		; to IX-END

		LD	B,A		;
		LD	A,$FF		;

;; LESS-MASK
o328A:		SLA	A		;
		DJNZ	o328A		; to LESS-MASK

		AND	(HL)		;
		LD	(HL),A		;

;; IX-END
o3290:		EX	DE,HL		;
		POP	DE		;
		RET			;

; ----------------------------------
; Storage of numbers in 5 byte form.
; ==================================
; Both integers and floating-point numbers can be stored in five bytes.
; Zero is a special case stored as 5 zeros.
; For integers the form is
; Byte 1 - zero,
; Byte 2 - sign byte, $00 +ve, $FF -ve.
; Byte 3 - Low byte of integer.
; Byte 4 - High byte
; Byte 5 - unused but always zero.
;
; it seems unusual to store the low byte first but it is just as easy either
; way. Statistically it just increases the chances of trailing zeros which
; is an advantage elsewhere in saving ROM code.
;
;             zero     sign     low      high    unused
; So +1 is  00000000 00000000 00000001 00000000 00000000
;
; and -1 is 00000000 11111111 11111111 11111111 00000000
;
; much of the arithmetic found in BASIC lines can be done using numbers
; in this form using the Z80's 16 bit register operation ADD.
; (multiplication is done by a sequence of additions).
;
; Storing -ve integers in two's complement form, means that they are ready for
; addition and you might like to add the numbers above to prove that the
; answer is zero. If, as in this case, the carry is set then that denotes that
; the result is positive. This only applies when the signs don't match.
; With positive numbers a carry denotes the result is out of integer range.
; With negative numbers a carry denotes the result is within range.
; The exception to the last rule is when the result is -65536
;
; Floating point form is an alternativo method of storing numbers which can
; be used for integers and larger (or fractional) numbers.
;
; In this form 1 is stored as
;           10000001 00000000 00000000 00000000 00000000
;
; When a small integer is converted to a floating point number the last two
; bytes are always blank so they are omitted in the following steps
;
; first make exponent +1 +16d  (bit 7 of the exponent is set if positive)

; 10010001 00000000 00000001
; 10010000 00000000 00000010 <-  now shift left and decrement exponent
; ...
; 10000010 01000000 00000000 <-  until a 1 abuts the imaginary point
; 10000001 10000000 00000000     to the left of the mantissa.
;
; however since the leftmost bit of the mantissa is always set then it can
; be used to denote the sign of the mantissa and put back when needed by the
; PREP routines which gives
;
; 10000001 00000000 00000000

; ----------------------------------------------
; THE 'RE-STACK TWO "SMALL" INTEGERS' SUBROUTINE
; ----------------------------------------------
;   This routine is called to re-stack two numbers in full floating point form
;   e.g. from mult when integer multiplication has overflowed.

;; RE-ST-TWO
o3293:		CALL	o3296		; routine RESTK-SUB  below and continue
					; into the routine to do the other one.

;; RESTK-SUB
o3296:		EX	DE,HL		; swap pointers

; ---------------------------------------------
; THE 'RE-STACK ONE "SMALL" INTEGER' SUBROUTINE
; ---------------------------------------------
; (offset: $3D 're-stack')
;   This routine re-stacks an integer, usually on the calculator stack, in full
;   floating point form.  HL points to first byte.

;; re-stack
o3297:		LD	A,(HL)		; Fetch Exponent byte to A
		AND	A		; test it
		RET	NZ		; return if not zero as already in full
					; floating-point form.

		PUSH	DE		; preserve DE.
		CALL	o2D7F		; routine INT-FETCH
					; integer to DE, sign to C.

; HL points to 4th byte.

		XOR	A		; clear accumulator.
		INC	HL		; point to 5th.
		LD	(HL),A		; and blank.
		DEC	HL		; point to 4th.
		LD	(HL),A		; and blank.

		LD	B,$91		; set exponent byte +ve $81
					; and imaginary dec point 16 bits to right
					; of first bit.

;   we could skip to normalize now but it's quicker to avoid normalizing
;   through an empty D.

		LD	A,D		; fetch the high byte D
		AND	A		; is it zero ?
		JR	NZ,o32B1	; skip to RS-NRMLSE if not.

		OR	E		; low byte E to A and test for zero
		LD	B,D		; set B exponent to 0
		JR	Z,o32BD		; forward to RS-STORE if value is zero.

		LD	D,E		; transfer E to D
		LD	E,B		; set E to 0
		LD	B,$89		; reduce the initial exponent by eight.


;; RS-NRMLSE
o32B1:		EX	DE,HL		; integer to HL, addr of 4th byte to DE.

;; RSTK-LOOP
o32B2:		DEC	B		; decrease exponent
		ADD	HL,HL		; shift DE left
		JR	NC,o32B2	; loop back to RSTK-LOOP
					; until a set bit pops into carry

		RRC	C		; now rotate the sign byte $00 or $FF
					; into carry to give a sign bit

		RR	H		; rotate the sign bit to left of H
		RR	L		; rotate any carry into L

		EX	DE,HL		; address 4th byte, normalized int to DE

;; RS-STORE
o32BD:		DEC	HL		; address 3rd byte
		LD	(HL),E		; place E
		DEC	HL		; address 2nd byte
		LD	(HL),D		; place D
		DEC	HL		; address 1st byte
		LD	(HL),B		; store the exponent

		POP	DE		; restore initial DE.
		RET			; return.

;****************************************
;** Part 10. FLOATING-POINT CALCULATOR **
;****************************************

; As a general rule the calculator avoids using the IY register.
; exceptions are val, val$ and str$.
; So an assembly language programmer who has disabled interrupts to use
; IY for other purposes can still use the calculator for mathematical
; purposes.


; ------------------------
; THE 'TABLE OF CONSTANTS'
; ------------------------
;
;

; used 11 times
;; stk-zero                                                 00 00 00 00 00
o32C5:		DB	$00		; Bytes: 1
		DB	$B0		; Exponent $00
		DB	$00		; (+00,+00,+00)

; used 19 times
;; stk-one                                                  00 00 01 00 00
o32C8:		DB	$40		; Bytes: 2
		DB	$B0		; Exponent $00
		DB	$00,$01		; (+00,+00)

; used 9 times
;; stk-half                                                 80 00 00 00 00
o32CC:		DB	$30		; Exponent: $80, Bytes: 1
		DB	$00		; (+00,+00,+00)

; used 4 times.
;; stk-pi/2                                                 81 49 0F DA A2
o32CE:		DB	$F1		; Exponent: $81, Bytes: 4
		DB	$49,$0F,$DA,$A2	;

; used 3 times.
;; stk-ten                                                  00 00 0A 00 00
o32D3:		DB	$40		; Bytes: 2
		DB	$B0		; Exponent $00
		DB	$00,$0A		; (+00,+00)


; ------------------------
; THE 'TABLE OF ADDRESSES'
; ------------------------
;  "Each problem that I solved became a rule which served afterwards to solve
;   other problems" - Rene Descartes 1596 - 1650.
;
;   Starts with binary operations which have two operands and one result.
;   Three pseudo binary operations first.

;; tbl-addrs
o32D7:		DW	o368F		; $00 Address: $368F - jump-true
		DW	o343C		; $01 Address: $343C - exchange
		DW	o33A1		; $02 Address: $33A1 - delete

;   True binary operations.

		DW	o300F		; $03 Address: $300F - subtract
		DW	o30CA		; $04 Address: $30CA - multiply
		DW	o31AF		; $05 Address: $31AF - division
		DW	o3851		; $06 Address: $3851 - to-power
		DW	o351B		; $07 Address: $351B - or

		DW	o3524		; $08 Address: $3524 - no-&-no
		DW	o353B		; $09 Address: $353B - no-l-eql
		DW	o353B		; $0A Address: $353B - no-gr-eql
		DW	o353B		; $0B Address: $353B - nos-neql
		DW	o353B		; $0C Address: $353B - no-grtr
		DW	o353B		; $0D Address: $353B - no-less
		DW	o353B		; $0E Address: $353B - nos-eql
		DW	o3014		; $0F Address: $3014 - addition

		DW	o352D		; $10 Address: $352D - str-&-no
		DW	o353B		; $11 Address: $353B - str-l-eql
		DW	o353B		; $12 Address: $353B - str-gr-eql
		DW	o353B		; $13 Address: $353B - strs-neql
		DW	o353B		; $14 Address: $353B - str-grtr
		DW	o353B		; $15 Address: $353B - str-less
		DW	o353B		; $16 Address: $353B - strs-eql
		DW	o359C		; $17 Address: $359C - strs-add

;   Unary follow.

		DW	o35DE		; $18 Address: $35DE - val$
		DW	o34BC		; $19 Address: $34BC - usr-$
		DW	o3645		; $1A Address: $3645 - read-in
		DW	o346E		; $1B Address: $346E - negate

		DW	o3669		; $1C Address: $3669 - code
		DW	o35DE		; $1D Address: $35DE - val
		DW	o3674		; $1E Address: $3674 - len
		DW	o37B5		; $1F Address: $37B5 - sin
		DW	o37AA		; $20 Address: $37AA - cos
		DW	o37DA		; $21 Address: $37DA - tan
		DW	o3833		; $22 Address: $3833 - asn
		DW	o3843		; $23 Address: $3843 - acs
		DW	o37E2		; $24 Address: $37E2 - atn
		DW	o3713		; $25 Address: $3713 - ln
		DW	o36C4		; $26 Address: $36C4 - exp
		DW	o36AF		; $27 Address: $36AF - int
		DW	o384A		; $28 Address: $384A - sqr
		DW	o3492		; $29 Address: $3492 - sgn
		DW	o346A		; $2A Address: $346A - abs
		DW	o34AC		; $2B Address: $34AC - peek
		DW	o34A5		; $2C Address: $34A5 - in
		DW	o34B3		; $2D Address: $34B3 - usr-no
		DW	o361F		; $2E Address: $361F - str$
		DW	o35C9		; $2F Address: $35C9 - chrs
		DW	o3501		; $30 Address: $3501 - not

;   End of true unary.

		DW	o33C0		; $31 Address: $33C0 - duplicate
		DW	o36A0		; $32 Address: $36A0 - n-mod-m
		DW	o3686		; $33 Address: $3686 - jump
		DW	o33C6		; $34 Address: $33C6 - stk-data
		DW	o367A		; $35 Address: $367A - dec-jr-nz
		DW	o3506		; $36 Address: $3506 - less-0
		DW	o34F9		; $37 Address: $34F9 - greater-0
		DW	o369B		; $38 Address: $369B - end-calc
		DW	o3783		; $39 Address: $3783 - get-argt
		DW	o3214		; $3A Address: $3214 - truncate
		DW	o33A2		; $3B Address: $33A2 - fp-calc-2
		DW	o2D4F		; $3C Address: $2D4F - e-to-fp
		DW	o3297		; $3D Address: $3297 - re-stack

;   The following are just the next available slots for the 128 compound
;   literals which are in range $80 - $FF.

		DW	o3449		;     Address: $3449 - series-xx    $80 - $9F.
		DW	o341B		;     Address: $341B - stk-const-xx $A0 - $BF.
		DW	o342D		;     Address: $342D - st-mem-xx    $C0 - $DF.
		DW	o340F		;     Address: $340F - get-mem-xx   $E0 - $FF.

;   Aside: 3E - 3F are therefore unused calculator literals.
;   If the literal has to be also usable as a function then bits 6 and 7 are
;   used to show type of arguments and result.

; --------------
; The Calculator
; --------------
;  "A good calculator does not need artificial aids"
;  Lao Tze 604 - 531 B.C.

;; CALCULATE
o335B:		CALL	o35BF		; routine STK-PNTRS is called to set up the
					; calculator stack pointers for a default
					; unary operation. HL = last value on stack.
					; DE = STKEND first location after stack.

; the calculate routine is called at this point by the series generator...

;; GEN-ENT-1
o335E:		LD	A,B		; fetch the Z80 B register to A
		LD	(BREG),A	; and store value in system variable BREG.
					; this will be the counter for dec-jr-nz
					; or if used from fp-calc2 the calculator
					; instruction.

; ... and again later at this point

;; GEN-ENT-2
o3362:		EXX
		EX	(SP),HL		; and store the address of next instruction,
					; the return address, in H'L'.
					; If this is a recursive call the H'L'
					; of the previous invocation goes on stack.
					; c.f. end-calc.
		EXX			; switch back to main set

; this is the re-entry looping point when handling a string of literals.

;; RE-ENTRY
o3365:		LD	(STKEND),DE	; save end of stack in system variable STKEND
		EXX			; switch to alt
		LD	A,(HL)		; get next literal
		INC	HL		; increase pointer'

; single operation jumps back to here

;; SCAN-ENT
o336C:		PUSH	HL		; save pointer on stack
		AND	A		; now test the literal
		JP	P,o3380		; forward to FIRST-3D if in range $00 - $3D
					; anything with bit 7 set will be one of
					; 128 compound literals.

; compound literals have the following format.
; bit 7 set indicates compound.
; bits 6-5 the subgroup 0-3.
; bits 4-0 the embedded parameter $00 - $1F.
; The subgroup 0-3 needs to be manipulated to form the next available four
; address places after the simple literals in the address table.

		LD	D,A		; save literal in D
		AND	$60		; and with 01100000 to isolate subgroup
		RRCA			; rotate bits
		RRCA			; 4 places to right
		RRCA			; not five as we need offset * 2
		RRCA			; 00000xx0
		ADD	A,$7C		; add ($3E * 2) to give correct offset.
					; alter above if you add more literals.
		LD	L,A		; store in L for later indexing.
		LD	A,D		; bring back compound literal
		AND	$1F		; use mask to isolate parameter bits
		JR	o338E		; forward to ENT-TABLE

; ---

; the branch was here with simple literals.

;; FIRST-3D
o3380:		CP	$18		; compare with first unary operations.
		JR	NC,o338C	; to DOUBLE-A with unary operations

; it is binary so adjust pointers.

		EXX			;
		LD	BC,$FFFB	; the value -5
		LD	D,H		; transfer HL, the last value, to DE.
		LD	E,L		;
		ADD	HL,BC		; subtract 5 making HL point to second
					; value.
		EXX			;

;; DOUBLE-A
o338C:		RLCA
		LD	L,A		; and store in L for indexing

;; ENT-TABLE
o338E:		LD	DE,o32D7	; Address: tbl-addrs
		LD	H,$00		; prepare to index
		ADD	HL,DE		; add to get address of routine
		LD	E,(HL)		; low byte to E
		INC	HL		;
		LD	D,(HL)		; high byte to D
		LD	HL,o3365	; Address: RE-ENTRY
		EX	(SP),HL		; goes to stack
		PUSH	DE		; now address of routine
		EXX			; main set
					; avoid using IY register.
		LD	BC,($5C66)	; STKEND_hi
					; nothing much goes to C but BREG to B
					; and continue into next ret instruction
					; which has a dual identity


; ------------------
; Handle delete (02)
; ------------------
; A simple return but when used as a calculator literal this
; deletes the last value from the calculator stack.
; On entry, as always with binary operations,
; HL=first number, DE=second number
; On exit, HL=result, DE=stkend.
; So nothing to do

;; delete
o33A1:		RET

; ---------------------
; Single operation (3B)
; ---------------------
;   This single operation is used, in the first instance, to evaluate most
;   of the mathematical and string functions found in BASIC expressions.

;; fp-calc-2
o33A2:		POP	AF		; drop return address.
		LD	A,(BREG)	; load accumulator from system variable BREG
					; value will be literal e.g. 'tan'
		EXX			; switch to alt
		JR	o336C		; back to SCAN-ENT
					; next literal will be end-calc at o2758

; ---------------------------------
; THE 'TEST FIVE SPACES' SUBROUTINE
; ---------------------------------
;   This routine is called from MOVE-FP, STK-CONST and STK-STORE to test that
;   there is enough space between the calculator stack and the machine stack
;   for another five-byte value.  It returns with BC holding the value 5 ready
;   for any subsequent LDIR.

;; TEST-5-SP
o33A9:		PUSH	DE		; save
		PUSH	HL		; registers
		LD	BC,$0005	; an overhead of five bytes
		CALL	o1F05		; routine TEST-ROOM tests free RAM raising
					; an error if not.
		POP	HL		; else restore
		POP	DE		; registers.
		RET			; return with BC set at 5.

; -----------------------------
; THE 'STACK NUMBER' SUBROUTINE
; -----------------------------
;   This routine is called to stack a hidden floating point number found in
;   a BASIC line.  It is also called to stack a numeric variable value, and
;   from BEEP, to stack an entry in the semi-tone table.  It is not part of the
;   calculator suite of routines.  On entry, HL points to the number to be
;   stacked.

;; STACK-NUM
o33B4:		LD	DE,(STKEND)	; Load destination from STKEND system variable.

		CALL	o33C0		; Routine MOVE-FP puts on calculator stack
					; with a memory check.
		LD	(STKEND),DE	; Set STKEND to next free location.

		RET			; Return.

; ---------------------------------
; Move a floating point number (31)
; ---------------------------------

; This simple routine is a 5-byte LDIR instruction
; that incorporates a memory check.
; When used as a calculator literal it duplicates the last value on the
; calculator stack.
; Unary so on entry HL points to last value, DE to stkend

;; duplicate
;; MOVE-FP
o33C0:		CALL	o33A9		; routine TEST-5-SP test free memory
					; and sets BC to 5.
		LDIR			; copy the five bytes.
		RET			; return with DE addressing new STKEND
					; and HL addressing new last value.

; -------------------
; Stack literals ($34)
; -------------------
; When a calculator subroutine needs to put a value on the calculator
; stack that is not a regular constant this routine is called with a
; variable number of following data bytes that convey to the routine
; the integer or floating point form as succinctly as is possible.

;; stk-data
o33C6:		LD	H,D		; transfer STKEND
		LD	L,E		; to HL for result.

;; STK-CONST
o33C8:		CALL	o33A9		; routine TEST-5-SP tests that room exists
					; and sets BC to $05.

		EXX			; switch to alternate set
		PUSH	HL		; save the pointer to next literal on stack
		EXX			; switch back to main set

		EX	(SP),HL		; pointer to HL, destination to stack.

		PUSH	BC		; save BC - value 5 from test room ??.

		LD	A,(HL)		; fetch the byte following 'stk-data'
		AND	$C0		; isolate bits 7 and 6
		RLCA			; rotate
		RLCA			; to bits 1 and 0  range $00 - $03.
		LD	C,A		; transfer to C
		INC	C		; and increment to give number of bytes
					; to read. $01 - $04
		LD	A,(HL)		; reload the first byte
		AND	$3F		; mask off to give possible exponent.
		JR	NZ,o33DE	; forward to FORM-EXP if it was possible to
					; include the exponent.

; else byte is just a byte count and exponent comes next.

		INC	HL		; address next byte and
		LD	A,(HL)		; pick up the exponent ( - $50).

;; FORM-EXP
o33DE:		ADD	A,$50		; now add $50 to form actual exponent
		LD	(DE),A		; and load into first destination byte.
		LD	A,$05		; load accumulator with $05 and
		SUB	C		; subtract C to give count of trailing
					; zeros plus one.
		INC	HL		; increment source
		INC	DE		; increment destination
		LD	B,$00		; prepare to copy
		LDIR			; copy C bytes

		POP	BC		; restore 5 counter to BC ??.

		EX	(SP),HL		; put HL on stack as next literal pointer
					; and the stack value - result pointer -
					; to HL.

		EXX			; switch to alternate set.
		POP	HL		; restore next literal pointer from stack
					; to H'L'.
		EXX			; switch back to main set.

		LD	B,A		; zero count to B
		XOR	A		; clear accumulator

;; STK-ZEROS
o33F1:		DEC	B		; decrement B counter
		RET	Z		; return if zero.          >>
					; DE points to new STKEND
					; HL to new number.

		LD	(DE),A		; else load zero to destination
		INC	DE		; increase destination
		JR	o33F1		; loop back to STK-ZEROS until done.

; -------------------------------
; THE 'SKIP CONSTANTS' SUBROUTINE
; -------------------------------
;   This routine traverses variable-length entries in the table of constants,
;   stacking intermediate, unwanted constants onto a dummy calculator stack,
;   in the first five bytes of ROM.  The destination DE normally points to the
;   end of the calculator stack which might be in the normal place or in the
;   system variables area during E-LINE-NO; INT-TO-FP; stk-ten.  In any case,
;   it would be simpler all round if the routine just shoved unwanted values
;   where it is going to stick the wanted value.  The instruction LD DE, $0000
;   can be removed.

;; SKIP-CONS
o33F7:		AND	A		; test if initially zero.

;; SKIP-NEXT
o33F8:		RET	Z		; return if zero.          >>

		PUSH	AF		; save count.
		PUSH	DE		; and normal STKEND

		LD	DE,$0000	; dummy value for STKEND at start of ROM
					; Note. not a fault but this has to be
					; moved elsewhere when running in RAM.
					; e.g. with Expandor Systems 'Soft ROM'.
					; Better still, write to the normal place.
		CALL	o33C8		; routine STK-CONST works through variable
					; length records.

		POP	DE		; restore real STKEND
		POP	AF		; restore count
		DEC	A		; decrease
		JR	o33F8		; loop back to SKIP-NEXT

; ------------------------------
; THE 'LOCATE MEMORY' SUBROUTINE
; ------------------------------
;   This routine, when supplied with a base address in HL and an index in A,
;   will calculate the address of the A'th entry, where each entry occupies
;   five bytes.  It is used for reading the semi-tone table and addressing
;   floating-point numbers in the calculator's memory area.
;   It is not possible to use this routine for the table of constants as these
;   six values are held in compressed format.

;; LOC-MEM
o3406:		LD	C,A		; store the original number $00-$1F.
		RLCA			; X2 - double.
		RLCA			; X4 - quadruple.
		ADD	A,C		; X5 - now add original to multiply by five.

		LD	C,A		; place the result in the low byte.
		LD	B,$00		; set high byte to zero.
		ADD	HL,BC		; add to form address of start of number in HL.

		RET			; return.

; ------------------------------
; Get from memory area ($E0 etc.)
; ------------------------------
; Literals $E0 to $FF
; A holds $00-$1F offset.
; The calculator stack increases by 5 bytes.

;; get-mem-xx
o340F:		PUSH	DE		; save STKEND
		LD	HL,(MEM)	; MEM is base address of the memory cells.
		CALL	o3406		; routine LOC-MEM so that HL = first byte
		CALL	o33C0		; routine MOVE-FP moves 5 bytes with memory
					; check.
					; DE now points to new STKEND.
		POP	HL		; original STKEND is now RESULT pointer.
		RET			; return.

; --------------------------
; Stack a constant (A0 etc.)
; --------------------------
; This routine allows a one-byte instruction to stack up to 32 constants
; held in short form in a table of constants. In fact only 5 constants are
; required. On entry the A register holds the literal ANDed with 1F.
; It isn't very efficient and it would have been better to hold the
; numbers in full, five byte form and stack them in a similar manner
; to that used for semi-tone table values.

;; stk-const-xx
o341B:		LD	H,D		; save STKEND - required for result
		LD	L,E		;
		EXX			; swap
		PUSH	HL		; save pointer to next literal
		LD	HL,o32C5	; Address: stk-zero - start of table of
					; constants
		EXX			;
		CALL	o33F7		; routine SKIP-CONS
		CALL	o33C8		; routine STK-CONST
		EXX			;
		POP	HL		; restore pointer to next literal.
		EXX			;
		RET			; return.

; --------------------------------
; Store in a memory area ($C0 etc.)
; --------------------------------
; Offsets $C0 to $DF
; Although 32 memory storage locations can be addressed, only six
; $C0 to $C5 are required by the ROM and only the thirty bytes (6*5)
; required for these are allocated. Spectrum programmers who wish to
; use the floating point routines from assembly language may wish to
; alter the system variable MEM to point to 160 bytes of RAM to have
; use the full range available.
; A holds the derived offset $00-$1F.
; This is a unary operation, so on entry HL points to the last value and DE
; points to STKEND.

;; st-mem-xx
o342D:		PUSH	HL		; save the result pointer.
		EX	DE,HL		; transfer to DE.
		LD	HL,(MEM)	; fetch MEM the base of memory area.
		CALL	o3406		; routine LOC-MEM sets HL to the destination.
		EX	DE,HL		; swap - HL is start, DE is destination.
		CALL	o33C0		; routine MOVE-FP.
					; note. a short ld bc,5; ldir
					; the embedded memory check is not required
					; so these instructions would be faster.
		EX	DE,HL		; DE = STKEND
		POP	HL		; restore original result pointer
		RET			; return.

; -------------------------
; THE 'EXCHANGE' SUBROUTINE
; -------------------------
; (offset: $01 'exchange')
;   This routine swaps the last two values on the calculator stack.
;   On entry, as always with binary operations,
;   HL=first number, DE=second number
;   On exit, HL=result, DE=stkend.

;; exchange
o343C:		LD	B,$05		; there are five bytes to be swapped

; start of loop.

;; SWAP-BYTE
o343E:		LD	A,(DE)		; each byte of second
		LD	C,(HL)		; each byte of first
		EX	DE,HL		; swap pointers
		LD	(DE),A		; store each byte of first
		LD	(HL),C		; store each byte of second
		INC	HL		; advance both
		INC	DE		; pointers.
		DJNZ	o343E		; loop back to SWAP-BYTE until all 5 done.

		EX	DE,HL		; even up the exchanges so that DE addresses
					; STKEND.

		RET			; return.

; ------------------------------
; THE 'SERIES GENERATOR' ROUTINE
; ------------------------------
; (offset: $86 'series-06')
; (offset: $88 'series-08')
; (offset: $8C 'series-0C')
;   The Spectrum uses Chebyshev polynomials to generate approximations for
;   SIN, ATN, LN and EXP.  These are named after the Russian mathematician
;   Pafnuty Chebyshev, born in 1821, who did much pioneering work on numerical
;   series.  As far as calculators are concerned, Chebyshev polynomials have an
;   advantage over other series, for example the Taylor series, as they can
;   reach an approximation in just six iterations for SIN, eight for EXP and
;   twelve for LN and ATN.  The mechanics of the routine are interesting but
;   for full treatment of how these are generated with demonstrations in
;   Sinclair BASIC see "The Complete Spectrum ROM Disassembly" by Dr Ian Logan
;   and Dr Frank O'Hara, published 1983 by Melbourne House.

;; series-xx
o3449:		LD	B,A		; parameter $00 - $1F to B counter
		CALL	o335E		; routine GEN-ENT-1 is called.
					; A recursive call to a special entry point
					; in the calculator that puts the B register
					; in the system variable BREG. The return
					; address is the next location and where
					; the calculator will expect its first
					; instruction - now pointed to by HL'.
					; The previous pointer to the series of
					; five-byte numbers goes on the machine stack.

; The initialization phase.

		DB	$31		; duplicate       x,x
		DB	$0F		; addition        x+x
		DB	$C0		; st-mem-0        x+x
		DB	$02		; delete          .
		DB	$A0		; stk-zero        0
		DB	$C2		; st-mem-2        0

; a loop is now entered to perform the algebraic calculation for each of
; the numbers in the series

;; G-LOOP
o3453:		DB	$31		; duplicate       v,v.
		DB	$E0		; get-mem-0       v,v,x+2
		DB	$04		; multiply        v,v*x+2
		DB	$E2		; get-mem-2       v,v*x+2,v
		DB	$C1		; st-mem-1
		DB	$03		; subtract
		DB	$38		; end-calc

; the previous pointer is fetched from the machine stack to H'L' where it
; addresses one of the numbers of the series following the series literal.

		CALL	o33C6		; routine STK-DATA is called directly to
					; push a value and advance H'L'.
		CALL	o3362		; routine GEN-ENT-2 recursively re-enters
					; the calculator without disturbing
					; system variable BREG
					; H'L' value goes on the machine stack and is
					; then loaded as usual with the next address.

		DB	$0F		; addition
		DB	$01		; exchange
		DB	$C2		; st-mem-2
		DB	$02		; delete

		DB	$35		; dec-jr-nz
		DB	$EE		; back to o3453, G-LOOP

; when the counted loop is complete the final subtraction yields the result
; for example SIN X.

		DB	$E1		; get-mem-1
		DB	$03		; subtract
		DB	$38		; end-calc

		RET			; return with H'L' pointing to location
					; after last number in series.

; ---------------------------------
; THE 'ABSOLUTE MAGNITUDE' FUNCTION
; ---------------------------------
; (offset: $2A 'abs')
;   This calculator literal finds the absolute value of the last value,
;   integer or floating point, on calculator stack.

;; abs
o346A:		LD	B,$FF		; signal abs
		JR	o3474		; forward to NEG-TEST

; ---------------------------
; THE 'UNARY MINUS' OPERATION
; ---------------------------
; (offset: $1B 'negate')
;   Unary so on entry HL points to last value, DE to STKEND.

;; NEGATE
;; negate
o346E:		CALL	o34E9		; call routine TEST-ZERO and
		RET	C		; return if so leaving zero unchanged.

		LD	B,$00		; signal negate required before joining
					; common code.

;; NEG-TEST
o3474:		LD	A,(HL)		; load first byte and
		AND	A		; test for zero
		JR	Z,o3483		; forward to INT-CASE if a small integer

; for floating point numbers a single bit denotes the sign.

		INC	HL		; address the first byte of mantissa.
		LD	A,B		; action flag $FF=abs, $00=neg.
		AND	$80		; now         $80      $00
		OR	(HL)		; sets bit 7 for abs
		RLA			; sets carry for abs and if number negative
		CCF			; complement carry flag
		RRA			; and rotate back in altering sign
		LD	(HL),A		; put the altered adjusted number back
		DEC	HL		; HL points to result
		RET			; return with DE unchanged

; ---

; for integer numbers an entire byte denotes the sign.

;; INT-CASE
o3483:		PUSH	DE		; save STKEND.

		PUSH	HL		; save pointer to the last value/result.

		CALL	o2D7F		; routine INT-FETCH puts integer in DE
					; and the sign in C.

		POP	HL		; restore the result pointer.

		LD	A,B		; $FF=abs, $00=neg
		OR	C		; $FF for abs, no change neg
		CPL			; $00 for abs, switched for neg
		LD	C,A		; transfer result to sign byte.

		CALL	o2D8E		; routine INT-STORE to re-write the integer.

		POP	DE		; restore STKEND.
		RET			; return.

; ---------------------
; THE 'SIGNUM' FUNCTION
; ---------------------
; (offset: $29 'sgn')
;   This routine replaces the last value on the calculator stack,
;   which may be in floating point or integer form, with the integer values
;   zero if zero, with one if positive and  with -minus one if negative.

;; sgn
o3492:		CALL	o34E9		; call routine TEST-ZERO and
		RET	C		; exit if so as no change is required.

		PUSH	DE		; save pointer to STKEND.

		LD	DE,$0001	; the result will be 1.
		INC	HL		; skip over the exponent.
		RL	(HL)		; rotate the sign bit into the carry flag.
		DEC	HL		; step back to point to the result.
		SBC	A,A		; byte will be $FF if negative, $00 if positive.
		LD	C,A		; store the sign byte in the C register.
		CALL	o2D8E		; routine INT-STORE to overwrite the last
					; value with 0001 and sign.

		POP	DE		; restore STKEND.
		RET			; return.

; -----------------
; THE 'IN' FUNCTION
; -----------------
; (offset: $2C 'in')
;   This function reads a byte from an input port.

;; in
o34A5:		CALL	o1E99		; Routine FIND-INT2 puts port address in BC.
					; All 16 bits are put on the address line.

		IN	A,(C)		; Read the port.

		JR	o34B0		; exit to STACK-A (via IN-PK-STK to save a byte
					; of instruction code).

; -------------------
; THE 'PEEK' FUNCTION
; -------------------
; (offset: $2B 'peek')
;   This function returns the contents of a memory address.
;   The entire address space can be peeked including the ROM.

;; peek
o34AC:		CALL	o1E99		; routine FIND-INT2 puts address in BC.
		LD	A,(BC)		; load contents into A register.

;; IN-PK-STK
o34B0:		JP	o2D28		; exit via STACK-A to put the value on the
					; calculator stack.

; ------------------
; THE 'USR' FUNCTION
; ------------------
; (offset: $2D 'usr-no')
;   The USR function followed by a number 0-65535 is the method by which
;   the Spectrum invokes machine code programs. This function returns the
;   contents of the BC register pair.
;   Note. that STACK-BC re-initializes the IY register if a user-written
;   program has altered it.

;; usr-no
o34B3:		CALL	o1E99		; routine FIND-INT2 to fetch the
					; supplied address into BC.

		LD	HL,o2D2B	; address: STACK-BC is
		PUSH	HL		; pushed onto the machine stack.
		PUSH	BC		; then the address of the machine code
					; routine.

		RET			; make an indirect jump to the routine
					; and, hopefully, to STACK-BC also.

; -------------------------
; THE 'USR STRING' FUNCTION
; -------------------------
; (offset: $19 'usr-$')
;   The user function with a one-character string argument, calculates the
;   address of the User Defined Graphic character that is in the string.
;   As an alternativo, the ASCII equivalent, upper or lower case,
;   may be supplied. This provides a user-friendly method of redefining
;   the 21 User Definable Graphics e.g.
;   POKE USR "a", BIN 10000000 will put a dot in the top left corner of the
;   character 144.
;   Note. the curious double check on the range. With 26 UDGs the first check
;   only is necessary. With anything less the second check only is required.
;   It is highly likely that the first check was written by Steven Vickers.

;; usr-$
o34BC:		CALL	o2BF1		; routine STK-FETCH fetches the string
					; parameters.
		DEC	BC		; decrease BC by
		LD	A,B		; one to test
		OR	C		; the length.
		JR	NZ,o34E7	; to REPORT-A if not a single character.

		LD	A,(DE)		; fetch the character
		CALL	o2C8D		; routine ALPHA sets carry if 'A-Z' or 'a-z'.
		JR	C,o34D3		; forward to USR-RANGE if ASCII.

		SUB	$90		; make UDGs range 0-20d
		JR	C,o34E7		; to REPORT-A if too low. e.g. usr " ".

		CP	$15		; Note. this test is not necessary.
		JR	NC,o34E7	; to REPORT-A if higher than 20.

		INC	A		; make range 1-21d to match LSBs of ASCII

;; USR-RANGE
o34D3:		DEC	A		; make range of bits 0-4 start at zero
		ADD	A,A		; multiply by eight
		ADD	A,A		; and lose any set bits
		ADD	A,A		; range now 0 - 25*8
		CP	$A8		; compare to 21*8
		JR	NC,o34E7	; to REPORT-A if originally higher
					; than 'U','u' or graphics U.

		LD	BC,(UDG)	; fetch the UDG system variable value.
		ADD	A,C		; add the offset to character
		LD	C,A		; and store back in register C.
		JR	NC,o34E4	; forward to USR-STACK if no overflow.

		INC	B		; increment high byte.

;; USR-STACK
o34E4:		JP	o2D2B		; jump back and exit via STACK-BC to store

; ---

;; REPORT-A
o34E7:		RST	08H		; ERROR-1
		DB	$09		; Error Report: Invalid argument

; ------------------------------
; THE 'TEST FOR ZERO' SUBROUTINE
; ------------------------------
;   Test if top value on calculator stack is zero.  The carry flag is set if
;   the last value is zero but no registers are altered.
;   All five bytes will be zero but first four only need be tested.
;   On entry, HL points to the exponent the first byte of the value.

;; TEST-ZERO
o34E9:		PUSH	HL		; preserve HL which is used to address.
		PUSH	BC		; preserve BC which is used as a store.
		LD	B,A		; preserve A in B.

		LD	A,(HL)		; load first byte to accumulator
		INC	HL		; advance.
		OR	(HL)		; OR with second byte and clear carry.
		INC	HL		; advance.
		OR	(HL)		; OR with third byte.
		INC	HL		; advance.
		OR	(HL)		; OR with fourth byte.

		LD	A,B		; restore A without affecting flags.
		POP	BC		; restore the saved
		POP	HL		; registers.

		RET	NZ		; return if not zero and with carry reset.

		SCF			; set the carry flag.
		RET			; return with carry set if zero.

; --------------------------------
; THE 'GREATER THAN ZERO' OPERATOR
; --------------------------------
; (offset: $37 'greater-0' )
;   Test if the last value on the calculator stack is greater than zero.
;   This routine is also called directly from the end-tests of the comparison
;   routine.

;; GREATER-0
;; greater-0
o34F9:		CALL	o34E9		; routine TEST-ZERO
		RET	C		; return if was zero as this
					; is also the Boolean 'false' value.

		LD	A,$FF		; prepare XOR mask for sign bit
		JR	o3507		; forward to SIGN-TO-C
					; to put sign in carry
					; (carry will become set if sign is positive)
					; and then overwrite location with 1 or 0
					; as appropriate.

; ------------------
; THE 'NOT' FUNCTION
; ------------------
; (offset: $30 'not')
;   This overwrites the last value with 1 if it was zero else with zero
;   if it was any other value.
;
;   e.g. NOT 0 returns 1, NOT 1 returns 0, NOT -3 returns 0.
;
;   The subroutine is also called directly from the end-tests of the comparison
;   operator.

;; NOT
;; not
o3501:		CALL	o34E9		; routine TEST-ZERO sets carry if zero

		JR	o350B		; to FP-0/1 to overwrite operand with
					; 1 if carry is set else to overwrite with zero.

; ------------------------------
; THE 'LESS THAN ZERO' OPERATION
; ------------------------------
; (offset: $36 'less-0' )
;   Destructively test if last value on calculator stack is less than zero.
;   Bit 7 of second byte will be set if so.

;; less-0
o3506:		XOR	A		; set XOR mask to zero
					; (carry will become set if sign is negative).

;   transfer sign of mantissa to Carry Flag.

;; SIGN-TO-C
o3507:		INC	HL		; address 2nd byte.
		XOR	(HL)		; bit 7 of HL will be set if number is negative.
		DEC	HL		; address 1st byte again.
		RLCA			; rotate bit 7 of A to carry.

; ----------------------------
; THE 'ZERO OR ONE' SUBROUTINE
; ----------------------------
;   This routine places an integer value of zero or one at the addressed
;   location of the calculator stack or MEM area.  The value one is written if
;   carry is set on entry else zero.

;; FP-0/1
o350B:		PUSH	HL		; save pointer to the first byte
		LD	A,$00		; load accumulator with zero - without
					; disturbing flags.
		LD	(HL),A		; zero to first byte
		INC	HL		; address next
		LD	(HL),A		; zero to 2nd byte
		INC	HL		; address low byte of integer
		RLA			; carry to bit 0 of A
		LD	(HL),A		; load one or zero to low byte.
		RRA			; restore zero to accumulator.
		INC	HL		; address high byte of integer.
		LD	(HL),A		; put a zero there.
		INC	HL		; address fifth byte.
		LD	(HL),A		; put a zero there.
		POP	HL		; restore pointer to the first byte.
		RET			; return.

; -----------------
; THE 'OR' OPERATOR
; -----------------
; (offset: $07 'or' )
; The Boolean OR operator. e.g. X OR Y
; The result is zero if both values are zero else a non-zero value.
;
; e.g.    0 OR 0  returns 0.
;        -3 OR 0  returns -3.
;         0 OR -3 returns 1.
;        -3 OR 2  returns 1.
;
; A binary operation.
; On entry HL points to first operand (X) and DE to second operand (Y).

;; or
o351B:		EX	DE,HL		; make HL point to second number
		CALL	o34E9		; routine TEST-ZERO
		EX	DE,HL		; restore pointers
		RET	C		; return if result was zero - first operand,
					; now the last value, is the result.

		SCF			; set carry flag
		JR	o350B		; back to FP-0/1 to overwrite the first operand
					; with the value 1.


; ---------------------------------
; THE 'NUMBER AND NUMBER' OPERATION
; ---------------------------------
; (offset: $08 'no-&-no')
;   The Boolean AND operator.
;
;   e.g.    -3 AND 2  returns -3.
;           -3 AND 0  returns 0.
;            0 and -2 returns 0.
;            0 and 0  returns 0.
;
;   Compare with OR routine above.

;; no-&-no
o3524:		EX	DE,HL		; make HL address second operand.

		CALL	o34E9		; routine TEST-ZERO sets carry if zero.

		EX	DE,HL		; restore pointers.
		RET	NC		; return if second non-zero, first is result.

;

		AND	A		; else clear carry.
		JR	o350B		; back to FP-0/1 to overwrite first operand
					; with zero for return value.

; ---------------------------------
; THE 'STRING AND NUMBER' OPERATION
; ---------------------------------
; (offset: $10 'str-&-no')
;   e.g. "You Win" AND score>99 will return the string if condition is true
;   or the null string if false.

;; str-&-no
o352D:		EX	DE,HL		; make HL point to the number.
		CALL	o34E9		; routine TEST-ZERO.
		EX	DE,HL		; restore pointers.
		RET	NC		; return if number was not zero - the string
					; is the result.

;   if the number was zero (false) then the null string must be returned by
;   altering the length of the string on the calculator stack to zero.

		PUSH	DE		; save pointer to the now obsolete number
					; (which will become the new STKEND)

		DEC	DE		; point to the 5th byte of string descriptor.
		XOR	A		; clear the accumulator.
		LD	(DE),A		; place zero in high byte of length.
		DEC	DE		; address low byte of length.
		LD	(DE),A		; place zero there - now the null string.

		POP	DE		; restore pointer - new STKEND.
		RET			; return.

; ---------------------------
; THE 'COMPARISON' OPERATIONS
; ---------------------------
; (offset: $0A 'no-gr-eql')
; (offset: $0B 'nos-neql')
; (offset: $0C 'no-grtr')
; (offset: $0D 'no-less')
; (offset: $0E 'nos-eql')
; (offset: $11 'str-l-eql')
; (offset: $12 'str-gr-eql')
; (offset: $13 'strs-neql')
; (offset: $14 'str-grtr')
; (offset: $15 'str-less')
; (offset: $16 'strs-eql')

;   True binary operations.
;   A single entry point is used to evaluate six numeric and six string
;   comparisons. On entry, the calculator literal is in the B register and
;   the two numeric values, or the two string parameters, are on the
;   calculator stack.
;   The individual bits of the literal are manipulated to group similar
;   operations although the SUB 8 instruction does nothing useful and merely
;   alters the string test bit.
;   Numbers are compared by subtracting one from the other, strings are
;   compared by comparing every character until a mismatch, or the end of one
;   or both, is reached.
;
;   Numeric Comparisons.
;   --------------------
;   The 'x>y' example is the easiest as it employs straight-thru logic.
;   Number y is subtracted from x and the result tested for greater-0 yielding
;   a final value 1 (true) or 0 (false).
;   For 'x<y' the same logic is used but the two values are first swapped on the
;   calculator stack.
;   For 'x=y' NOT is applied to the subtraction result yielding true if the
;   difference was zero and false with anything else.
;   The first three numeric comparisons are just the opposite of the last three
;   so the same processing steps are used and then a final NOT is applied.
;
; literal    Test   No  sub 8       ExOrNot  1st RRCA  exch sub  ?   End-Tests
; =========  ====   == ======== === ======== ========  ==== ===  =  === === ===
; no-l-eql   x<=y   09 00000001 dec 00000000 00000000  ---- x-y  ?  --- >0? NOT
; no-gr-eql  x>=y   0A 00000010 dec 00000001 10000000c swap y-x  ?  --- >0? NOT
; nos-neql   x<>y   0B 00000011 dec 00000010 00000001  ---- x-y  ?  NOT --- NOT
; no-grtr    x>y    0C 00000100  -  00000100 00000010  ---- x-y  ?  --- >0? ---
; no-less    x<y    0D 00000101  -  00000101 10000010c swap y-x  ?  --- >0? ---
; nos-eql    x=y    0E 00000110  -  00000110 00000011  ---- x-y  ?  NOT --- ---
;
;                                                           comp -> C/F
;                                                           ====    ===
; str-l-eql  x$<=y$ 11 00001001 dec 00001000 00000100  ---- x$y$ 0  !or >0? NOT
; str-gr-eql x$>=y$ 12 00001010 dec 00001001 10000100c swap y$x$ 0  !or >0? NOT
; strs-neql  x$<>y$ 13 00001011 dec 00001010 00000101  ---- x$y$ 0  !or >0? NOT
; str-grtr   x$>y$  14 00001100  -  00001100 00000110  ---- x$y$ 0  !or >0? ---
; str-less   x$<y$  15 00001101  -  00001101 10000110c swap y$x$ 0  !or >0? ---
; strs-eql   x$=y$  16 00001110  -  00001110 00000111  ---- x$y$ 0  !or >0? ---
;
;   String comparisons are a little different in that the eql/neql carry flag
;   from the 2nd RRCA is, as before, fed into the first of the end tests but
;   along the way it gets modified by the comparison process. The result on the
;   stack always starts off as zero and the carry fed in determines if NOT is
;   applied to it. So the only time the greater-0 test is applied is if the
;   stack holds zero which is not very efficient as the test will always yield
;   zero. The most likely explanation is that there were once separate end tests
;   for numbers and strings.

;; no-l-eql,etc.
o353B:		LD	A,B		; transfer literal to accumulator.
		SUB	$08		; subtract eight - which is not useful.

		BIT	2,A		; isolate ">", "<", "=".

		JR	NZ,o3543	; skip to EX-OR-NOT with these.

		DEC	A		; else make $00-$02, $08-$0A to match bits 0-2.

;; EX-OR-NOT
o3543:		RRCA
		JR	NC,o354E	; forward to NU-OR-STR with other 8 cases

; for the other 4 cases the two values on the calculator stack are exchanged.

		PUSH	AF		; save A and carry.
		PUSH	HL		; save HL - pointer to first operand.
					; (DE points to second operand).

		CALL	o343C		; routine exchange swaps the two values.
					; (HL = second operand, DE = STKEND)

		POP	DE		; DE = first operand
		EX	DE,HL		; as we were.
		POP	AF		; restore A and carry.

; Note. it would be better if the 2nd RRCA preceded the string test.
; It would save two duplicate bytes and if we also got rid of that sub 8
; at the beginning we wouldn't have to alter which bit we test.

;; NU-OR-STR
o354E:		BIT	2,A		; test if a string comparison.
		JR	NZ,o3559	; forward to STRINGS if so.

; continue with numeric comparisons.

		RRCA			; 2nd RRCA causes eql/neql to set carry.
		PUSH	AF		; save A and carry

		CALL	o300F		; routine subtract leaves result on stack.
		JR	o358C		; forward to END-TESTS

; ---

;; STRINGS
o3559:		RRCA
		PUSH	AF		; save A and carry.

		CALL	o2BF1		; routine STK-FETCH gets 2nd string params
		PUSH	DE		; save start2 *.
		PUSH	BC		; and the length.

		CALL	o2BF1		; routine STK-FETCH gets 1st string
					; parameters - start in DE, length in BC.
		POP	HL		; restore length of second to HL.

; A loop is now entered to compare, by subtraction, each corresponding character
; of the strings. For each successful match, the pointers are incremented and
; the lengths decreased and the branch taken back to here. If both string
; remainders become null at the same time, then an exact match exists.

;; BYTE-COMP
o3564:		LD	A,H		; test if the second string
		OR	L		; is the null string and hold flags.

		EX	(SP),HL		; put length2 on stack, bring start2 to HL *.
		LD	A,B		; hi byte of length1 to A

		JR	NZ,o3575	; forward to SEC-PLUS if second not null.

		OR	C		; test length of first string.

;; SECND-LOW
o356B:		POP	BC		; pop the second length off stack.
		JR	Z,o3572		; forward to BOTH-NULL if first string is also
					; of zero length.

; the true condition - first is longer than second (SECND-LESS)

		POP	AF		; restore carry (set if eql/neql)
		CCF			; complement carry flag.
					; Note. equality becomes false.
					; Inequality is true. By swapping or applying
					; a terminal 'not', all comparisons have been
					; manipulated so that this is success path.
		JR	o3588		; forward to leave via STR-TEST

; ---
; the branch was here with a match

;; BOTH-NULL
o3572:		POP	AF		; restore carry - set for eql/neql
		JR	o3588		; forward to STR-TEST

; ---
; the branch was here when 2nd string not null and low byte of first is yet
; to be tested.


;; SEC-PLUS
o3575:		OR	C		; test the length of first string.
		JR	Z,o3585		; forward to FRST-LESS if length is zero.

; both strings have at least one character left.

		LD	A,(DE)		; fetch character of first string.
		SUB	(HL)		; subtract with that of 2nd string.
		JR	C,o3585		; forward to FRST-LESS if carry set

		JR	NZ,o356B	; back to SECND-LOW and then STR-TEST
					; if not exact match.

		DEC	BC		; decrease length of 1st string.
		INC	DE		; increment 1st string pointer.

		INC	HL		; increment 2nd string pointer.
		EX	(SP),HL		; swap with length on stack
		DEC	HL		; decrement 2nd string length
		JR	o3564		; back to BYTE-COMP

; ---
; the false condition.

;; FRST-LESS
o3585:		POP	BC		; discard length
		POP	AF		; pop A
		AND	A		; clear the carry for false result.

; ---
; exact match and x$>y$ rejoin here

;; STR-TEST
o3588:		PUSH	AF		; save A and carry

		RST	28H		; FP-CALC
		DB	$A0		; stk-zero      an initial false value.
		DB	$38		; end-calc

; both numeric and string paths converge here.

;; END-TESTS
o358C:		POP	AF		; pop carry  - will be set if eql/neql
		PUSH	AF		; save it again.

		CALL	C,o3501		; routine NOT sets true(1) if equal(0)
					; or, for strings, applies true result.

		POP	AF		; pop carry and
		PUSH	AF		; save A

		CALL	NC,o34F9	; routine GREATER-0 tests numeric subtraction
					; result but also needlessly tests the string
					; value for zero - it must be.

		POP	AF		; pop A
		RRCA			; the third RRCA - test for '<=', '>=' or '<>'.
		CALL	NC,o3501	; apply a terminal NOT if so.
		RET			; return.

; ------------------------------------
; THE 'STRING CONCATENATION' OPERATION
; ------------------------------------
; (offset: $17 'strs-add')
;   This literal combines two strings into one e.g. LET a$ = b$ + c$
;   The two parameters of the two strings to be combined are on the stack.

;; strs-add
o359C:		CALL	o2BF1		; routine STK-FETCH fetches string parameters
					; and deletes calculator stack entry.
		PUSH	DE		; save start address.
		PUSH	BC		; and length.

		CALL	o2BF1		; routine STK-FETCH for first string
		POP	HL		; re-fetch first length
		PUSH	HL		; and save again
		PUSH	DE		; save start of second string
		PUSH	BC		; and its length.

		ADD	HL,BC		; add the two lengths.
		LD	B,H		; transfer to BC
		LD	C,L		; and create
		RST	30H		; BC-SPACES in workspace.
					; DE points to start of space.

		CALL	o2AB2		; routine STK-STO-$ stores parameters
					; of new string updating STKEND.

		POP	BC		; length of first
		POP	HL		; address of start
		LD	A,B		; test for
		OR	C		; zero length.
		JR	Z,o35B7		; to OTHER-STR if null string

		LDIR			; copy string to workspace.

;; OTHER-STR
o35B7:		POP	BC		; now second length
		POP	HL		; and start of string
		LD	A,B		; test this one
		OR	C		; for zero length
		JR	Z,o35BF		; skip forward to STK-PNTRS if so as complete.

		LDIR			; else copy the bytes.
					; and continue into next routine which
					; sets the calculator stack pointers.

; -----------------------------------
; THE 'SET STACK POINTERS' SUBROUTINE
; -----------------------------------
;   Register DE is set to STKEND and HL, the result pointer, is set to five
;   locations below this.
;   This routine is used when it is inconvenient to save these values at the
;   time the calculator stack is manipulated due to other activity on the
;   machine stack.
;   This routine is also used to terminate the VAL and READ-IN  routines for
;   the same reason and to initialize the calculator stack at the start of
;   the CALCULATE routine.

;; STK-PNTRS
o35BF:		LD	HL,(STKEND)	; fetch STKEND value from system variable.
		LD	DE,$FFFB	; the value -5
		PUSH	HL		; push STKEND value.

		ADD	HL,DE		; subtract 5 from HL.

		POP	DE		; pop STKEND to DE.
		RET			; return.

; -------------------
; THE 'CHR$' FUNCTION
; -------------------
; (offset: $2F 'chr$')
;   This function returns a single character string that is a result of
;   converting a number in the range 0-255 to a string e.g. CHR$ 65 = "A".

;; chrs
o35C9:		CALL	o2DD5		; routine FP-TO-A puts the number in A.

		JR	C,o35DC		; forward to REPORT-Bd if overflow
		JR	NZ,o35DC	; forward to REPORT-Bd if negative

		PUSH	AF		; save the argument.

		LD	BC,$0001	; one space required.
		RST	30H		; BC-SPACES makes DE point to start

		POP	AF		; restore the number.

		LD	(DE),A		; and store in workspace

		CALL	o2AB2		; routine STK-STO-$ stacks descriptor.

		EX	DE,HL		; make HL point to result and DE to STKEND.
		RET			; return.

; ---

;; REPORT-Bd
o35DC:		RST	08H		; ERROR-1
		DB	$0A		; Error Report: Integer out of range

; ----------------------------
; THE 'VAL and VAL$' FUNCTIONS
; ----------------------------
; (offset: $1D 'val')
; (offset: $18 'val$')
;   VAL treats the characters in a string as a numeric expression.
;   e.g. VAL "2.3" = 2.3, VAL "2+4" = 6, VAL ("2" + "4") = 24.
;   VAL$ treats the characters in a string as a string expression.
;   e.g. VAL$ (z$+"(2)") = a$(2) if z$ happens to be "a$".

;; val
;; val$
o35DE:		LD	HL,(CH_ADD)	; fetch value of system variable CH_ADD
		PUSH	HL		; and save on the machine stack.
		LD	A,B		; fetch the literal (either $1D or $18).
		ADD	A,$E3		; add $E3 to form $00 (setting carry) or $FB.
		SBC	A,A		; now form $FF bit 6 = numeric result
					; or $00 bit 6 = string result.
		PUSH	AF		; save this mask on the stack

		CALL	o2BF1		; routine STK-FETCH fetches the string operand
					; from calculator stack.

		PUSH	DE		; save the address of the start of the string.
		INC	BC		; increment the length for a carriage return.

		RST	30H		; BC-SPACES creates the space in workspace.
		POP	HL		; restore start of string to HL.
		LD	(CH_ADD),DE	; load CH_ADD with start DE in workspace.

		PUSH	DE		; save the start in workspace
		LDIR			; copy string from program or variables or
					; workspace to the workspace area.
		EX	DE,HL		; end of string + 1 to HL
		DEC	HL		; decrement HL to point to end of new area.
		LD	(HL),$0D	; insert a carriage return at end.
		RES	7,(IY+$01)	; update FLAGS  - signal checking syntax.
		CALL	o24FB		; routine SCANNING evaluates string
					; expression and result.

		RST	18H		; GET-CHAR fetches next character.
		CP	$0D		; is it the expected carriage return ?
		JR	NZ,o360C	; forward to V-RPORT-C if not
					; 'Nonsense in BASIC'.

		POP	HL		; restore start of string in workspace.
		POP	AF		; restore expected result flag (bit 6).
		XOR	(IY+$01)	; xor with FLAGS now updated by SCANNING.
		AND	$40		; test bit 6 - should be zero if result types
					; match.

;; V-RPORT-C
o360C:		JP	NZ,o1C8A	; jump back to REPORT-C with a result mismatch.

		LD	(CH_ADD),HL	; set CH_ADD to the start of the string again.
		SET	7,(IY+$01)	; update FLAGS  - signal running program.
		CALL	o24FB		; routine SCANNING evaluates the string
					; in full leaving result on calculator stack.

		POP	HL		; restore saved character address in program.
		LD	(CH_ADD),HL	; and reset the system variable CH_ADD.

		JR	o35BF		; back to exit via STK-PNTRS.
					; resetting the calculator stack pointers
					; HL and DE from STKEND as it wasn't possible
					; to preserve them during this routine.

; -------------------
; THE 'STR$' FUNCTION
; -------------------
; (offset: $2E 'str$')
;   This function produces a string comprising the characters that would appear
;   if the numeric argument were printed.
;   e.g. STR$ (1/10) produces "0.1".

;; str$
o361F:		LD	BC,$0001	; create an initial byte in workspace
		RST	30H		; using BC-SPACES restart.

		LD	(K_CUR),HL	; set system variable K_CUR to new location.
		PUSH	HL		; and save start on machine stack also.

		LD	HL,(CURCHL)	; fetch value of system variable CURCHL
		PUSH	HL		; and save that too.

		LD	A,$FF		; select system channel 'R'.
		CALL	o1601		; routine CHAN-OPEN opens it.
		CALL	o2DE3		; routine PRINT-FP outputs the number to
					; workspace updating K-CUR.

		POP	HL		; restore current channel.
		CALL	o1615		; routine CHAN-FLAG resets flags.

		POP	DE		; fetch saved start of string to DE.
		LD	HL,(K_CUR)	; load HL with end of string from K_CUR.

		AND	A		; prepare for true subtraction.
		SBC	HL,DE		; subtract start from end to give length.
		LD	B,H		; transfer the length to
		LD	C,L		; the BC register pair.

		CALL	o2AB2		; routine STK-STO-$ stores string parameters
					; on the calculator stack.

		EX	DE,HL		; HL = last value, DE = STKEND.
		RET			; return.

; ------------------------
; THE 'READ-IN' SUBROUTINE
; ------------------------
; (offset: $1A 'read-in')
;   This is the calculator literal used by the INKEY$ function when a "#"
;   is encountered after the keyword.
;   INKEY$ # does not interact correctly with the keyboard, #0 or #1, and
;   its uses are for other channels.

;; read-in
o3645:		CALL	o1E94		; routine FIND-INT1 fetches stream to A
		CP	$10		; compare with 16 decimal.
		JP	NC,o1E9F	; JUMP to REPORT-Bb if not in range 0 - 15.
					; 'Integer out of range'
					; (REPORT-Bd is within range)

		LD	HL,(CURCHL)	; fetch current channel CURCHL
		PUSH	HL		; save it

		CALL	o1601		; routine CHAN-OPEN opens channel

		CALL	o15E6		; routine INPUT-AD - the channel must have an
					; input stream or else error here from stream
					; stub.
		LD	BC,$0000	; initialize length of string to zero
		JR	NC,o365F	; forward to R-I-STORE if no key detected.

		INC	C		; increase length to one.

		RST	30H		; BC-SPACES creates space for one character
					; in workspace.
		LD	(DE),A		; the character is inserted.

;; R-I-STORE
o365F:		CALL	o2AB2		; routine STK-STO-$ stacks the string
					; parameters.
		POP	HL		; restore current channel address

		CALL	o1615		; routine CHAN-FLAG resets current channel
					; system variable and flags.

		JP	o35BF		; jump back to STK-PNTRS

; -------------------
; THE 'CODE' FUNCTION
; -------------------
; (offset: $1C 'code')
;   Returns the ASCII code of a character or first character of a string
;   e.g. CODE "Aardvark" = 65, CODE "" = 0.

;; code
o3669:		CALL	o2BF1		; routine STK-FETCH to fetch and delete the
					; string parameters.
					; DE points to the start, BC holds the length.

		LD	A,B		; test length
		OR	C		; of the string.
		JR	Z,o3671		; skip to STK-CODE with zero if the null string.

		LD	A,(DE)		; else fetch the first character.

;; STK-CODE
o3671:		JP	o2D28		; jump back to STACK-A (with memory check)

; ------------------
; THE 'LEN' FUNCTION
; ------------------
; (offset: $1E 'len')
;   Returns the length of a string.
;   In Sinclair BASIC strings can be more than twenty thousand characters long
;   so a sixteen-bit register is required to store the length

;; len
o3674:		CALL	o2BF1		; Routine STK-FETCH to fetch and delete the
					; string parameters from the calculator stack.
					; Register BC now holds the length of string.

		JP	o2D2B		; Jump back to STACK-BC to save result on the
					; calculator stack (with memory check).

; -------------------------------------
; THE 'DECREASE THE COUNTER' SUBROUTINE
; -------------------------------------
; (offset: $35 'dec-jr-nz')
;   The calculator has an instruction that decrements a single-byte
;   pseudo-register and makes consequential relative jumps just like
;   the Z80's DJNZ instruction.

;; dec-jr-nz
o367A:		EXX

		PUSH	HL		; save pointer to offset byte
		LD	HL,BREG		; address BREG in system variables
		DEC	(HL)		; decrement it
		POP	HL		; restore pointer

		JR	NZ,o3687	; to JUMP-2 if not zero

		INC	HL		; step past the jump length.
		EXX			; switch in the main set.
		RET			; return.

; Note. as a general rule the calculator avoids using the IY register
; otherwise the cumbersome 4 instructions in the middle could be repoaced by
; dec (iy+$2D) - three bytes instead of six.


; ---------------------
; THE 'JUMP' SUBROUTINE
; ---------------------
; (offset: $33 'jump')
;   This enables the calculator to perform relative jumps just like the Z80
;   chip's JR instruction.

;; jump
;; JUMP
o3686:		EXX

;; JUMP-2
o3687:		LD	E,(HL)		; the jump byte 0-127 forward, 128-255 back.
		LD	A,E		; transfer to accumulator.
		RLA			; if backward jump, carry is set.
		SBC	A,A		; will be $FF if backward or $00 if forward.
		LD	D,A		; transfer to high byte.
		ADD	HL,DE		; advance calculator pointer forward or back.

		EXX			; switch back.
		RET			; return.

; --------------------------
; THE 'JUMP-TRUE' SUBROUTINE
; --------------------------
; (offset: $00 'jump-true')
;   This enables the calculator to perform conditional relative jumps dependent
;   on whether the last test gave a true result.

;; jump-true
o368F:		INC	DE		; Collect the
		INC	DE		; third byte
		LD	A,(DE)		; of the test
		DEC	DE		; result and
		DEC	DE		; backtrack.

		AND	A		; Is result 0 or 1 ?
		JR	NZ,o3686	; Back to JUMP if true (1).

		EXX			; Else switch in the pointer set.
		INC	HL		; Step past the jump length.
		EXX			; Switch in the main set.
		RET			; Return.

; -------------------------
; THE 'END-CALC' SUBROUTINE
; -------------------------
; (offset: $38 'end-calc')
;   The end-calc literal terminates a mini-program written in the Spectrum's
;   internal language.

;; end-calc
o369B:		POP	AF		; Drop the calculator return address RE-ENTRY
		EXX			; Switch to the other set.

		EX	(SP),HL		; Transfer H'L' to machine stack for the
					; return address.
					; When exiting recursion, then the previous
					; pointer is transferred to H'L'.

		EXX			; Switch back to main set.
		RET			; Return.


; ------------------------
; THE 'MODULUS' SUBROUTINE
; ------------------------
; (offset: $32 'n-mod-m')
; (n1,n2 -- r,q)
;   Similar to FORTH's 'divide mod' /MOD
;   On the Spectrum, this is only used internally by the RND function and could
;   have been implemented inline.  On the ZX81, this calculator routine was also
;   used by PRINT-FP.

;; n-mod-m
o36A0:		RST	28H		; FP-CALC          17, 3.
		DB	$C0		; st-mem-0          17, 3.
		DB	$02		; delete            17.
		DB	$31		; duplicate         17, 17.
		DB	$E0		; get-mem-0         17, 17, 3.
		DB	$05		; division          17, 17/3.
		DB	$27		; int               17, 5.
		DB	$E0		; get-mem-0         17, 5, 3.
		DB	$01		; exchange          17, 3, 5.
		DB	$C0		; st-mem-0          17, 3, 5.
		DB	$04		; multiply          17, 15.
		DB	$03		; subtract          2.
		DB	$E0		; get-mem-0         2, 5.
		DB	$38		; end-calc          2, 5.

		RET			; return.


; ------------------
; THE 'INT' FUNCTION
; ------------------
; (offset $27: 'int' )
; This function returns the integer of x, which is just the same as truncate
; for positive numbers. The truncate literal truncates negative numbers
; upwards so that -3.4 gives -3 whereas the BASIC INT function has to
; truncate negative numbers down so that INT -3.4 is -4.
; It is best to work through using, say, +-3.4 as examples.

;; int
o36AF:		RST	28H		; FP-CALC              x.    (= 3.4 or -3.4).
		DB	$31		; duplicate             x, x.
		DB	$36		; less-0                x, (1/0)
		DB	$00		; jump-true             x, (1/0)
		DB	$04		; to o36B7, X-NEG

		DB	$3A		; truncate              trunc 3.4 = 3.
		DB	$38		; end-calc              3.

		RET			; return with + int x on stack.

; ---


;; X-NEG
o36B7:		DB	$31		; duplicate             -3.4, -3.4.
		DB	$3A		; truncate              -3.4, -3.
		DB	$C0		; st-mem-0              -3.4, -3.
		DB	$03		; subtract              -.4
		DB	$E0		; get-mem-0             -.4, -3.
		DB	$01		; exchange              -3, -.4.
		DB	$30		; not                   -3, (0).
		DB	$00		; jump-true             -3.
		DB	$03		; to o36C2, EXIT        -3.

		DB	$A1		; stk-one               -3, 1.
		DB	$03		; subtract              -4.

;; EXIT
o36C2:		DB	$38		; end-calc              -4.

		RET			; return.


; ------------------
; THE 'EXP' FUNCTION
; ------------------
; (offset $26: 'exp')
;   The exponential function EXP x is equal to e^x, where e is the mathematical
;   name for a number approximated to 2.718281828.
;   ERROR 6 if argument is more than about 88.

;; EXP
;; exp
o36C4:		RST	28H		; FP-CALC
		DB	$3D		; re-stack      (not required - mult will do)
		DB	$34		; stk-data
		DB	$F1		; Exponent: $81, Bytes: 4
		DB	$38,$AA,$3B,$29	;
		DB	$04		; multiply
		DB	$31		; duplicate
		DB	$27		; int
		DB	$C3		; st-mem-3
		DB	$03		; subtract
		DB	$31		; duplicate
		DB	$0F		; addition
		DB	$A1		; stk-one
		DB	$03		; subtract
		DB	$88		; series-08
		DB	$13		; Exponent: $63, Bytes: 1
		DB	$36		; (+00,+00,+00)
		DB	$58		; Exponent: $68, Bytes: 2
		DB	$65,$66		; (+00,+00)
		DB	$9D		; Exponent: $6D, Bytes: 3
		DB	$78,$65,$40	; (+00)
		DB	$A2		; Exponent: $72, Bytes: 3
		DB	$60,$32,$C9	; (+00)
		DB	$E7		; Exponent: $77, Bytes: 4
		DB	$21,$F7,$AF,$24	;
		DB	$EB		; Exponent: $7B, Bytes: 4
		DB	$2F,$B0,$B0,$14	;
		DB	$EE		; Exponent: $7E, Bytes: 4
		DB	$7E,$BB,$94,$58	;
		DB	$F1		; Exponent: $81, Bytes: 4
		DB	$3A,$7E,$F8,$CF	;
		DB	$E3		; get-mem-3
		DB	$38		; end-calc

		CALL	o2DD5		; routine FP-TO-A
		JR	NZ,o3705	; to N-NEGTV

		JR	C,o3703		; to REPORT-6b
					; 'Number too big'

		ADD	A,(HL)		;
		JR	NC,o370C	; to RESULT-OK


;; REPORT-6b
o3703:		RST	08H		; ERROR-1
		DB	$05		; Error Report: Number too big

; ---

;; N-NEGTV
o3705:		JR	C,o370E		; to RSLT-ZERO

		SUB	(HL)		;
		JR	NC,o370E	; to RSLT-ZERO

		NEG			; Negate

;; RESULT-OK
o370C:		LD	(HL),A		;
		RET			; return.

; ---


;; RSLT-ZERO
o370E:		RST	28H		; FP-CALC
		DB	$02		; delete
		DB	$A0		; stk-zero
		DB	$38		; end-calc

		RET			; return.


; --------------------------------
; THE 'NATURAL LOGARITHM' FUNCTION
; --------------------------------
; (offset $25: 'ln')
;   Function to calculate the natural logarithm (to the base e ).
;   Natural logarithms were devised in 1614 by well-traveled Scotsman John
;   Napier who noted
;   "Nothing doth more molest and hinder calculators than the multiplications,
;    divisions, square and cubical extractions of great numbers".
;
;   Napier's logarithms enabled the above operations to be accomplished by
;   simple addition and subtraction simplifying the navigational and
;   astronomical calculations which beset his age.
;   Napier's logarithms were quickly overtaken by logarithms to the base 10
;   devised, in conjunction with Napier, by Henry Briggs a Cambridge-educated
;   professor of Geometry at Oxford University. These simplified the layout
;   of the tables enabling humans to easily scale calculations.
;
;   It is only recently with the introduction of pocket calculators and machines
;   like the ZX Spectrum that natural logarithms are once more at the fore,
;   although some computers retain logarithms to the base ten.
;
;   'Natural' logarithms are powers to the base 'e', which like 'pi' is a
;   naturally occurring number in branches of mathematics.
;   Like 'pi' also, 'e' is an irrational number and starts 2.718281828...
;
;   The tabular use of logarithms was that to multiply two numbers one looked
;   up their two logarithms in the tables, added them together and then looked
;   for the result in a table of antilogarithms to give the desired product.
;
;   The EXP function is the BASIC equivalent of a calculator's 'antiln' function
;   and by picking any two numbers, 1.72 and 6.89 say,
;     10 PRINT EXP ( LN 1.72 + LN 6.89 )
;   will give just the same result as
;     20 PRINT 1.72 * 6.89.
;   Division is accomplished by subtracting the two logs.
;
;   Napier also mentioned "square and cubicle extractions".
;   To raise a number to the power 3, find its 'ln', multiply by 3 and find the
;   'antiln'.  e.g. PRINT EXP( LN 4 * 3 )  gives 64.
;   Similarly to find the n'th root divide the logarithm by 'n'.
;   The ZX81 ROM used PRINT EXP ( LN 9 / 2 ) to find the square root of the
;   number 9. The Napieran square root function is just a special case of
;   the 'to_power' function. A cube root or indeed any root/power would be just
;   as simple.

;   First test that the argument to LN is a positive, non-zero number.
;   Error A if the argument is 0 or negative.

;; ln
o3713:		RST	28H		; FP-CALC
		DB	$3D		; re-stack
		DB	$31		; duplicate
		DB	$37		; greater-0
		DB	$00		; jump-true
		DB	$04		; to o371C, VALID

		DB	$38		; end-calc


;; REPORT-Ab
o371A:		RST	08H		; ERROR-1
		DB	$09		; Error Report: Invalid argument

;; VALID
o371C:		DB	$A0		; stk-zero              Note. not
		DB	$02		; delete                necessary.
		DB	$38		; end-calc
		LD	A,(HL)		;

		LD	(HL),$80	;
		CALL	o2D28		; routine STACK-A

		RST	28H		; FP-CALC
		DB	$34		; stk-data
		DB	$38		; Exponent: $88, Bytes: 1
		DB	$00		; (+00,+00,+00)
		DB	$03		; subtract
		DB	$01		; exchange
		DB	$31		; duplicate
		DB	$34		; stk-data
		DB	$F0		; Exponent: $80, Bytes: 4
		DB	$4C,$CC,$CC,$CD	;
		DB	$03		; subtract
		DB	$37		; greater-0
		DB	$00		; jump-true
		DB	$08		; to o373D, GRE.8

		DB	$01		; exchange
		DB	$A1		; stk-one
		DB	$03		; subtract
		DB	$01		; exchange
		DB	$38		; end-calc

		INC	(HL)		;

		RST	28H		; FP-CALC

;; GRE.8
o373D:		DB	$01		; exchange
		DB	$34		; stk-data
		DB	$F0		; Exponent: $80, Bytes: 4
		DB	$31,$72,$17,$F8	;
		DB	$04		; multiply
		DB	$01		; exchange
		DB	$A2		; stk-half
		DB	$03		; subtract
		DB	$A2		; stk-half
		DB	$03		; subtract
		DB	$31		; duplicate
		DB	$34		; stk-data
		DB	$32		; Exponent: $82, Bytes: 1
		DB	$20		; (+00,+00,+00)
		DB	$04		; multiply
		DB	$A2		; stk-half
		DB	$03		; subtract
		DB	$8C		; series-0C
		DB	$11		; Exponent: $61, Bytes: 1
		DB	$AC		; (+00,+00,+00)
		DB	$14		; Exponent: $64, Bytes: 1
		DB	$09		; (+00,+00,+00)
		DB	$56		; Exponent: $66, Bytes: 2
		DB	$DA,$A5		; (+00,+00)
		DB	$59		; Exponent: $69, Bytes: 2
		DB	$30,$C5		; (+00,+00)
		DB	$5C		; Exponent: $6C, Bytes: 2
		DB	$90,$AA		; (+00,+00)
		DB	$9E		; Exponent: $6E, Bytes: 3
		DB	$70,$6F,$61	; (+00)
		DB	$A1		; Exponent: $71, Bytes: 3
		DB	$CB,$DA,$96	; (+00)
		DB	$A4		; Exponent: $74, Bytes: 3
		DB	$31,$9F,$B4	; (+00)
		DB	$E7		; Exponent: $77, Bytes: 4
		DB	$A0,$FE,$5C,$FC	;
		DB	$EA		; Exponent: $7A, Bytes: 4
		DB	$1B,$43,$CA,$36	;
		DB	$ED		; Exponent: $7D, Bytes: 4
		DB	$A7,$9C,$7E,$5E	;
		DB	$F0		; Exponent: $80, Bytes: 4
		DB	$6E,$23,$80,$93	;
		DB	$04		; multiply
		DB	$0F		; addition
		DB	$38		; end-calc

		RET			; return.


; -----------------------------
; THE 'TRIGONOMETRIC' FUNCTIONS
; -----------------------------
; Trigonometry is rocket science. It is also used by carpenters and pyramid
; builders.
; Some uses can be quite abstract but the principles can be seen in simple
; right-angled triangles. Triangles have some special properties -
;
; 1) The sum of the three angles is always PI radians (180 degrees).
;    Very helpful if you know two angles and wish to find the third.
; 2) In any right-angled triangle the sum of the squares of the two shorter
;    sides is equal to the square of the longest side opposite the right-angle.
;    Very useful if you know the length of two sides and wish to know the
;    length of the third side.
; 3) Functions sine, cosine and tangent enable one to calculate the length
;    of an unknown side when the length of one other side and an angle is
;    known.
; 4) Functions arcsin, arccosine and arctan enable one to calculate an unknown
;    angle when the length of two of the sides is known.

; --------------------------------
; THE 'REDUCE ARGUMENT' SUBROUTINE
; --------------------------------
; (offset $39: 'get-argt')
;
; This routine performs two functions on the angle, in radians, that forms
; the argument to the sine and cosine functions.
; First it ensures that the angle 'wraps round'. That if a ship turns through
; an angle of, say, 3*PI radians (540 degrees) then the net effect is to turn
; through an angle of PI radians (180 degrees).
; Secondly it converts the angle in radians to a fraction of a right angle,
; depending within which quadrant the angle lies, with the periodicity
; resembling that of the desired sine value.
; The result lies in the range -1 to +1.
;
;                     90 deg.
;
;                     (pi/2)
;              II       +1        I
;                       |
;        sin+      |\   |   /|    sin+
;        cos-      | \  |  / |    cos+
;        tan-      |  \ | /  |    tan+
;                  |   \|/)  |
; 180 deg. (pi) 0 -|----+----|-- 0  (0)   0 degrees
;                  |   /|\   |
;        sin-      |  / | \  |    sin-
;        cos-      | /  |  \ |    cos+
;        tan+      |/   |   \|    tan-
;                       |
;              III      -1       IV
;                     (3pi/2)
;
;                     270 deg.
;

;; get-argt
o3783:		RST	28H		; FP-CALC      X.
		DB	$3D		; re-stack      (not rquired done by mult)
		DB	$34		; stk-data
		DB	$EE		; Exponent: $7E,
					;;Bytes: 4
		DB	$22,$F9,$83,$6E	;              X, 1/(2*PI)
		DB	$04		; multiply      X/(2*PI) = fraction
		DB	$31		; duplicate
		DB	$A2		; stk-half
		DB	$0F		; addition
		DB	$27		; int

		DB	$03		; subtract      now range -.5 to .5

		DB	$31		; duplicate
		DB	$0F		; addition      now range -1 to 1.
		DB	$31		; duplicate
		DB	$0F		; addition      now range -2 to +2.

; quadrant I (0 to +1) and quadrant IV (-1 to 0) are now correct.
; quadrant II ranges +1 to +2.
; quadrant III ranges -2 to -1.

		DB	$31		; duplicate     Y, Y.
		DB	$2A		; abs           Y, abs(Y).    range 1 to 2
		DB	$A1		; stk-one       Y, abs(Y), 1.
		DB	$03		; subtract      Y, abs(Y)-1.  range 0 to 1
		DB	$31		; duplicate     Y, Z, Z.
		DB	$37		; greater-0     Y, Z, (1/0).

		DB	$C0		; st-mem-0         store as possible sign
					;;                 for cosine function.

		DB	$00		; jump-true
		DB	$04		; to o37A1, ZPLUS  with quadrants II and III.

; else the angle lies in quadrant I or IV and value Y is already correct.

		DB	$02		; delete        Y.   delete the test value.
		DB	$38		; end-calc      Y.

		RET			; return.       with Q1 and Q4           >>>

; ---

; the branch was here with quadrants II (0 to 1) and III (1 to 0).
; Y will hold -2 to -1 if this is quadrant III.

;; ZPLUS
o37A1:		DB	$A1		; stk-one         Y, Z, 1.
		DB	$03		; subtract        Y, Z-1.       Q3 = 0 to -1
		DB	$01		; exchange        Z-1, Y.
		DB	$36		; less-0          Z-1, (1/0).
		DB	$00		; jump-true       Z-1.
		DB	$02		; to o37A8, YNEG
					;;if angle in quadrant III

; else angle is within quadrant II (-1 to 0)

		DB	$1B		; negate          range +1 to 0.

;; YNEG
o37A8:		DB	$38		; end-calc        quadrants II and III correct.

		RET			; return.


; ---------------------
; THE 'COSINE' FUNCTION
; ---------------------
; (offset $20: 'cos')
; Cosines are calculated as the sine of the opposite angle rectifying the
; sign depending on the quadrant rules.
;
;
;           /|
;        h /y|
;         /  |o
;        /x  |
;       /----|
;         a
;
; The cosine of angle x is the adjacent side (a) divided by the hypotenuse 1.
; However if we examine angle y then a/h is the sine of that angle.
; Since angle x plus angle y equals a right-angle, we can find angle y by
; subtracting angle x from pi/2.
; However it's just as easy to reduce the argument first and subtract the
; reduced argument from the value 1 (a reduced right-angle).
; It's even easier to subtract 1 from the angle and rectify the sign.
; In fact, after reducing the argument, the absolute value of the argument
; is used and rectified using the test result stored in mem-0 by 'get-argt'
; for that purpose.
;

;; cos
o37AA:		RST	28H		; FP-CALC              angle in radians.
		DB	$39		; get-argt              X     reduce -1 to +1

		DB	$2A		; abs                   ABS X.   0 to 1
		DB	$A1		; stk-one               ABS X, 1.
		DB	$03		; subtract              now opposite angle
					;;                      although sign is -ve.

		DB	$E0		; get-mem-0             fetch the sign indicator
		DB	$00		; jump-true
		DB	$06		; fwd to o37B7, C-ENT
					;;forward to common code if in QII or QIII.

		DB	$1B		; negate                else make sign +ve.
		DB	$33		; jump
		DB	$03		; fwd to o37B7, C-ENT
					;; with quadrants I and IV.

; -------------------
; THE 'SINE' FUNCTION
; -------------------
; (offset $1F: 'sin')
; This is a fundamental transcendental function from which others such as cos
; and tan are directly, or indirectly, derived.
; It uses the series generator to produce Chebyshev polynomials.
;
;
;           /|
;        1 / |
;         /  |x
;        /a  |
;       /----|
;         y
;
; The 'get-argt' function is designed to modify the angle and its sign
; in line with the desired sine value and afterwards it can launch straight
; into common code.

;; sin
o37B5:		RST	28H		; FP-CALC      angle in radians
		DB	$39		; get-argt      reduce - sign now correct.

;; C-ENT
o37B7:		DB	$31		; duplicate
		DB	$31		; duplicate
		DB	$04		; multiply
		DB	$31		; duplicate
		DB	$0F		; addition
		DB	$A1		; stk-one
		DB	$03		; subtract

		DB	$86		; series-06
		DB	$14		; Exponent: $64, Bytes: 1
		DB	$E6		; (+00,+00,+00)
		DB	$5C		; Exponent: $6C, Bytes: 2
		DB	$1F,$0B		; (+00,+00)
		DB	$A3		; Exponent: $73, Bytes: 3
		DB	$8F,$38,$EE	; (+00)
		DB	$E9		; Exponent: $79, Bytes: 4
		DB	$15,$63,$BB,$23	;
		DB	$EE		; Exponent: $7E, Bytes: 4
		DB	$92,$0D,$CD,$ED	;
		DB	$F1		; Exponent: $81, Bytes: 4
		DB	$23,$5D,$1B,$EA	;
		DB	$04		; multiply
		DB	$38		; end-calc

		RET			; return.

; ----------------------
; THE 'TANGENT' FUNCTION
; ----------------------
; (offset $21: 'tan')
;
; Evaluates tangent x as    sin(x) / cos(x).
;
;
;           /|
;        h / |
;         /  |o
;        /x  |
;       /----|
;         a
;
; the tangent of angle x is the ratio of the length of the opposite side
; divided by the length of the adjacent side. As the opposite length can
; be calculates using sin(x) and the adjacent length using cos(x) then
; the tangent can be defined in terms of the previous two functions.

; Error 6 if the argument, in radians, is too close to one like pi/2
; which has an infinite tangent. e.g. PRINT TAN (PI/2)  evaluates as 1/0.
; Similarly PRINT TAN (3*PI/2), TAN (5*PI/2) etc.

;; tan
o37DA:		RST	28H		; FP-CALC          x.
		DB	$31		; duplicate         x, x.
		DB	$1F		; sin               x, sin x.
		DB	$01		; exchange          sin x, x.
		DB	$20		; cos               sin x, cos x.
		DB	$05		; division          sin x/cos x (= tan x).
		DB	$38		; end-calc          tan x.

		RET			; return.

; ---------------------
; THE 'ARCTAN' FUNCTION
; ---------------------
; (Offset $24: 'atn')
; the inverse tangent function with the result in radians.
; This is a fundamental transcendental function from which others such as asn
; and acs are directly, or indirectly, derived.
; It uses the series generator to produce Chebyshev polynomials.

;; atn
o37E2:		CALL	o3297		; routine re-stack
		LD	A,(HL)		; fetch exponent byte.
		CP	$81		; compare to that for 'one'
		JR	C,o37F8		; forward, if less, to SMALL

		RST	28H		; FP-CALC
		DB	$A1		; stk-one
		DB	$1B		; negate
		DB	$01		; exchange
		DB	$05		; division
		DB	$31		; duplicate
		DB	$36		; less-0
		DB	$A3		; stk-pi/2
		DB	$01		; exchange
		DB	$00		; jump-true
		DB	$06		; to o37FA, CASES

		DB	$1B		; negate
		DB	$33		; jump
		DB	$03		; to o37FA, CASES

;; SMALL
o37F8:		RST	28H		; FP-CALC
		DB	$A0		; stk-zero

;; CASES
o37FA:		DB	$01		; exchange
		DB	$31		; duplicate
		DB	$31		; duplicate
		DB	$04		; multiply
		DB	$31		; duplicate
		DB	$0F		; addition
		DB	$A1		; stk-one
		DB	$03		; subtract
		DB	$8C		; series-0C
		DB	$10		; Exponent: $60, Bytes: 1
		DB	$B2		; (+00,+00,+00)
		DB	$13		; Exponent: $63, Bytes: 1
		DB	$0E		; (+00,+00,+00)
		DB	$55		; Exponent: $65, Bytes: 2
		DB	$E4,$8D		; (+00,+00)
		DB	$58		; Exponent: $68, Bytes: 2
		DB	$39,$BC		; (+00,+00)
		DB	$5B		; Exponent: $6B, Bytes: 2
		DB	$98,$FD		; (+00,+00)
		DB	$9E		; Exponent: $6E, Bytes: 3
		DB	$00,$36,$75	; (+00)
		DB	$A0		; Exponent: $70, Bytes: 3
		DB	$DB,$E8,$B4	; (+00)
		DB	$63		; Exponent: $73, Bytes: 2
		DB	$42,$C4		; (+00,+00)
		DB	$E6		; Exponent: $76, Bytes: 4
		DB	$B5,$09,$36,$BE	;
		DB	$E9		; Exponent: $79, Bytes: 4
		DB	$36,$73,$1B,$5D	;
		DB	$EC		; Exponent: $7C, Bytes: 4
		DB	$D8,$DE,$63,$BE	;
		DB	$F0		; Exponent: $80, Bytes: 4
		DB	$61,$A1,$B3,$0C	;
		DB	$04		; multiply
		DB	$0F		; addition
		DB	$38		; end-calc

		RET			; return.


; ---------------------
; THE 'ARCSIN' FUNCTION
; ---------------------
; (Offset $22: 'asn')
;   The inverse sine function with result in radians.
;   Derived from arctan function above.
;   Error A unless the argument is between -1 and +1 inclusive.
;   Uses an adaptation of the formula asn(x) = atn(x/sqr(1-x*x))
;
;
;                 /|
;                / |
;              1/  |x
;              /a  |
;             /----|
;               y
;
;   e.g. We know the opposite side (x) and hypotenuse (1)
;   and we wish to find angle a in radians.
;   We can derive length y by Pythagoras and then use ATN instead.
;   Since y*y + x*x = 1*1 (Pythagoras Theorem) then
;   y=sqr(1-x*x)                         - no need to multiply 1 by itself.
;   So, asn(a) = atn(x/y)
;   or more fully,
;   asn(a) = atn(x/sqr(1-x*x))

;   Close but no cigar.

;   While PRINT ATN (x/SQR (1-x*x)) gives the same results as PRINT ASN x,
;   it leads to division by zero when x is 1 or -1.
;   To overcome this, 1 is added to y giving half the required angle and the
;   result is then doubled.
;   That is, PRINT ATN (x/(SQR (1-x*x) +1)) *2
;
;   GEOMETRIC PROOF.
;
;
;               . /|
;            .  c/ |
;         .     /1 |x
;      . c   b /a  |
;    ---------/----|
;      1      y
;
;   By creating an isosceles triangle with two equal sides of 1, angles c and
;   c are also equal. If b+c+c = 180 degrees and b+a = 180 degrees then c=a/2.
;
;   A value higher than 1 gives the required error as attempting to find  the
;   square root of a negative number generates an error in Sinclair BASIC.

;; asn
o3833:		RST	28H		; FP-CALC      x.
		DB	$31		; duplicate     x, x.
		DB	$31		; duplicate     x, x, x.
		DB	$04		; multiply      x, x*x.
		DB	$A1		; stk-one       x, x*x, 1.
		DB	$03		; subtract      x, x*x-1.
		DB	$1B		; negate        x, 1-x*x.
		DB	$28		; sqr           x, sqr(1-x*x) = y
		DB	$A1		; stk-one       x, y, 1.
		DB	$0F		; addition      x, y+1.
		DB	$05		; division      x/y+1.
		DB	$24		; atn           a/2       (half the angle)
		DB	$31		; duplicate     a/2, a/2.
		DB	$0F		; addition      a.
		DB	$38		; end-calc      a.

		RET			; return.


; ---------------------
; THE 'ARCCOS' FUNCTION
; ---------------------
; (Offset $23: 'acs')
; the inverse cosine function with the result in radians.
; Error A unless the argument is between -1 and +1.
; Result in range 0 to pi.
; Derived from asn above which is in turn derived from the preceding atn.
; It could have been derived directly from atn using acs(x) = atn(sqr(1-x*x)/x).
; However, as sine and cosine are horizontal translations of each other,
; uses acs(x) = pi/2 - asn(x)

; e.g. the arccosine of a known x value will give the required angle b in
; radians.
; We know, from above, how to calculate the angle a using asn(x).
; Since the three angles of any triangle add up to 180 degrees, or pi radians,
; and the largest angle in this case is a right-angle (pi/2 radians), then
; we can calculate angle b as pi/2 (both angles) minus asn(x) (angle a).
;
;
;           /|
;        1 /b|
;         /  |x
;        /a  |
;       /----|
;         y
;

;; acs
o3843:		RST	28H		; FP-CALC      x.
		DB	$22		; asn           asn(x).
		DB	$A3		; stk-pi/2      asn(x), pi/2.
		DB	$03		; subtract      asn(x) - pi/2.
		DB	$1B		; negate        pi/2 -asn(x)  =  acs(x).
		DB	$38		; end-calc      acs(x).

		RET			; return.


; --------------------------
; THE 'SQUARE ROOT' FUNCTION
; --------------------------
; (Offset $28: 'sqr')
; This routine is remarkable for its brevity - 7 bytes.
; It wasn't written here but in the ZX81 where the programmers had to squeeze
; a bulky operating system into an 8K ROM. It simply calculates
; the square root by stacking the value .5 and continuing into the 'to-power'
; routine. With more space available the much faster Newton-Raphson method
; could have been used as on the Jupiter Ace.

;; sqr
o384A:		RST	28H		; FP-CALC
		DB	$31		; duplicate
		DB	$30		; not
		DB	$00		; jump-true
		DB	$1E		; to o386C, LAST

		DB	$A2		; stk-half
		DB	$38		; end-calc


; ------------------------------
; THE 'EXPONENTIATION' OPERATION
; ------------------------------
; (Offset $06: 'to-power')
; This raises the first number X to the power of the second number Y.
; As with the ZX80,
; 0 ^ 0 = 1.
; 0 ^ +n = 0.
; 0 ^ -n = arithmetic overflow.
;

;; to-power
o3851:		RST	28H		; FP-CALC              X, Y.
		DB	$01		; exchange              Y, X.
		DB	$31		; duplicate             Y, X, X.
		DB	$30		; not                   Y, X, (1/0).
		DB	$00		; jump-true
		DB	$07		; to o385D, XIS0   if X is zero.

;   else X is non-zero. Function 'ln' will catch a negative value of X.

		DB	$25		; ln                    Y, LN X.
		DB	$04		; multiply              Y * LN X.
		DB	$38		; end-calc

		JP	o36C4		; jump back to EXP routine   ->

; ---

;   these routines form the three simple results when the number is zero.
;   begin by deleting the known zero to leave Y the power factor.

;; XIS0
o385D:		DB	$02		; delete                Y.
		DB	$31		; duplicate             Y, Y.
		DB	$30		; not                   Y, (1/0).
		DB	$00		; jump-true
		DB	$09		; to o386A, ONE         if Y is zero.

		DB	$A0		; stk-zero              Y, 0.
		DB	$01		; exchange              0, Y.
		DB	$37		; greater-0             0, (1/0).
		DB	$00		; jump-true             0.
		DB	$06		; to o386C, LAST        if Y was any positive
					;;                      number.

;   else force division by zero thereby raising an Arithmetic overflow error.
;   There are some one and two-byte alternativos but perhaps the most formal
;   might have been to use end-calc; rst 08; DB     05.

		DB	$A1		; stk-one               0, 1.
		DB	$01		; exchange              1, 0.
		DB	$05		; division              1/0        ouch!

; ---

;; ONE
o386A:		DB	$02		; delete                .
		DB	$A1		; stk-one               1.

;; LAST
o386C:		DB	$38		; end-calc              last value is 1 or 0.

		RET			; return.

;   "Everything should be made as simple as possible, but not simpler"
;   - Albert Einstein, 1879-1955.

; ---------------------
; THE 'SPARE' LOCATIONS
; ---------------------

;; spare
o386E:		PUSH	IX		; save IX (why?)
		CALL	o02BF		; scan keyboard as with 48K ROM
		BIT	4,(IY+$01)
		JR	Z,o387C		; check bit 4 of FLAGS
		CALL	o387F		; check disk motor if true
o387C:		POP	IX
		RET

; Subroutine to check disk motor timeout

o387F:		LD	BC,PBANKM
		LD	A,(BANKM)
		OR	$07
		OUT	(C),A		; page in page 7
		LD	A,($E600)
		OR	A
		JR	Z,o38AC		; move on if motor already off
		LD	A,(FRAMES)
		BIT	0,A
		JR	NZ,o38AC	; only decrement timeout every other time
		LD	A,($E600)
		DEC	A		; decrement timeout
		LD	($E600),A
		JR	NZ,o38AC	; move on if not yet zero
		LD	BC,PBANK678
		LD	A,(BANK678)
		AND	$F7
		LD	(BANK678),A
		OUT	(C),A		; switch motor off
o38AC:		LD	BC,PBANKM
		LD	A,(BANKM)
		OUT	(C),A		; page back page 0
		RET


;**** disponible ****
		/*
		1)	averiguar cual pagina esta arriba
		2)	averiguar si estoy en modo 128 o 48
		3)	si estoy en 48 retornar
		4)	si estoy en 128 preguntar si
		(tapl_stat1)=1
		si	es asi cargar desde disco
		y	si no retornar

		5)	si cargo desde disco los registros
		deben	tener los mismos valores
		que	tendrian si hubiese cargado desde
		cinta,	especialmente IX
		*/

FREE_ROM3_0:	EQU	$

NEW_LOAD:	EX	AF,AF'
		PUSH	AF		; GUARDA AF ALTERNATIVO
		EX	AF,AF'
		PUSH	AF		; GUARDA AF NORMAL (ESTO TIENE EL FLAG CARGAR/VERIFICAR)
		PUSH	DE
		PUSH	HL
		PUSH	IX

;ESTOY SEGURO QUE LAS INT. ESTAN deshabilitadas
;de lo contario el usuario tampoco podría cargar
;desde cinta

;puedo usar HL y BC con tranquilidad

		LD	HL,($C000)	; GUARDO LOS CONTENIDOS DE C000-C003
		PUSH	HL
		LD	HL,($C002)
		PUSH	HL
		LD	HL,$5446	; *INSERTO* A PARTIR DE $C000 LA CADENA "FTAP" (POR FIND TAP)
		LD	($C000),HL
		LD	HL,$5041
		LD	($C002),HL


		LD	A,$10		; A PARTIR DE AQUI AVERIGUO QUE PAGINA TENGO ARRIBA
		LD	BC,PBANKM	; BUSCANDO EN CADA UNA DE ELLAS LA CADENA "FTAP" EN C000-C003
LFIND_UPPERMEM:	OUT	(C),A		; Y ME DETENGO CUANDO LA ENCUENTRE
		LD	HL,($C000)
		AND	A
		LD	DE,$5446
		SBC	HL,DE
		JR	NZ,NOMEMORY
		LD	HL,($C002)
		AND	A
		LD	DE,$5041
		SBC	HL,DE
		JR	Z,ENCONTRADA
NOMEMORY:		INC	A
		CP	$18
		JR	NZ,LFIND_UPPERMEM

ENCONTRADA:	EX	AF,AF'		; EN A ME QUEDA LA PAGINA QUE TENGO ARRIBA
					; USO AF' PARA NO PERDER AF DE MOMENTO

		POP	HL		; RECUPERO LOS BYTES DE C000-C003
		LD	($C002),HL
		POP	HL
		LD	($C000),HL


		LD	HL,$C000
		LD	A,$10		; AVERIGUO SI ESTAN ACCESIBLES LOS 128K
		OUT	(C),A
		LD	D,(HL)
		LD	(HL),A

		INC	A		; LD     A,$11
		OUT	(C),A
		LD	E,(HL)
		LD	(HL),A

		DEC	A		; LD     A,$10
		OUT	(C),A
		CP	(HL)
		LD	(HL),D
		JR	Z,ES128K

		LD	(HL),E		; RECUPERO EL BYTE NO ESTOY EN MODO 128 ASI QUE NO PUEDE HABER
RETLOAD2:	POP	IX		; CARGA DE TAPs DESDE +3DOS
		POP	HL
		POP	DE
		POP	AF
		EX	AF,AF'
		POP	AF
		EX	AF,AF'
RET_NEW_LOAD:		IN	A,($FE)
		RRA
		RET


ES128K:		LD	A,$11		; RECUPERO LOS BYTES (SOLO EL QUE CORRESPONDIA A PAG.1 EL DE PAG 0 YA FUE RECUPERADO)
		OUT	(C),A
		LD	(HL),E

		LD	A,$17		; AVERIGUO SI TENGO QUE CARGAR DESDE DISCO
		OUT	(C),A		; SI tapl_stat1==1 SIGNIFICA QUE HUBO
		LD	A,(tapl_stat1)	; UN COMANDO PREVIO SPECTRUM "xxxxxxxx.TAP"
		LD	D,A		; QUE YA PREPARO LAS COSAS

		EX	AF,AF'		; PONER LA PAGINA DE RAM AVERIGUADA MAS ARRIBA
		LD	(CONFRAM),A	; LA GUARDO EN ESTA VAR DE PAGINA 7
		OUT	(C),A
		LD	A,D
		CP	$01
		JR	NZ,RETLOAD2

SIMULAR_TAPE:	LD	A,$17		;puesto que voy a poner IM2 cuando use rutinas de +3DOS asi el propio +3DOS
		OUT	(C),A		;no me despedaza la zona de variables - 5C78
		LD	HL,SWAP
		LD	DE,save_buffer
		LD	BC,1024
		LDIR
		CALL	STANDAR_RAM	; pongo los valores "estandar" en BANKM y BANK678
		LD	A,I		; porque a lo mejor la INT llamada en el HALT siguiente
		LD	(save_REGI),A	; se le arma lio
		LD	A,$FF
		LD	(SWAP),A
		LD	A,$5B
		LD	I,A
		LD	HL,DUMMY_IM2
		LD	(TSTACK),HL
		XOR	A
		LD	(EXCEPCIONES),A
		LD	A,(CONFRAM)
		LD	BC,PBANKM
		OUT	(C),A
		EI
		HALT			; despues de que se ejecute la interrupcion puede pasar 2 cosas
		DI			; 1) que (SWAP)==0 lo que significaria que estaba IM2
					; 2) que (SWAP)==FF lo que significaria que estaba IM1


		LD	A,$17		; por las dudas si alguna interrupcion cabrona me quito pag 7
		LD	BC,PBANKM
		OUT	(C),A
		XOR	A
		LD	(end_LOAD),A

		LD	A,(save_REGI)
		LD	I,A
		LD	A,(SWAP)
		LD	(save_INTERR),A	; ENTONCES esta VAR me indica si es $FF = IM1 y si es $00 = IM2

		LD	HL,save_buffer
		LD	DE,SWAP
		LD	BC,1024
		LDIR

		LD	A,(CONFRAM)
		LD	BC,PBANKM
		OUT	(C),A

		CALL	SWP_RTALERT

		POP	IX		; CARGA DE TAPs DESDE +3DOS
		POP	HL
		POP	DE
		POP	AF
		EX	AF,AF'
		POP	AF
		EX	AF,AF'

		CALL	GET_BYTE_TAPE
		PUSH	AF
		CALL	GET_BYTE_TAPE
		LD	H,A
		POP	AF
		LD	L,A		; en HL tengo la cantidad de bytes a leer del bloque actual
					; ej: una cabecera son 19 bytes, (17+2)4

		CALL	SET_POS_SIG

		CALL	GET_BYTE_TAPE	; CARGO EL FLAG
		LD	C,A
		EX	AF,AF'
		LD	B,A
		EX	AF,AF'

		LD	A,B
		CP	C
		JR	NZ,ERROR_LFLAG	; SIMULO ERROR DE CARGA

; (!! AVERIGUAR EL PUNTERO ACTUAL Y SUMARLE HL PARA SABER EN DONDE
; COMIENZE EL SIGUIENTE BLOQUE DEL TAP PARA CUANDO NECESITES PASAR
; AL SIGUIENTE BLOQUE)


		DEC	HL		; ya cargué el flag
		DEC	HL
		AND	A
		SBC	HL,DE
		JR	NZ,BAD_SIZE

		LD	H,C		; EL control de parity
					; "comienza" con el mismo
					; valor del flag

LOOP_LOAD_TAP:	CALL	GET_BYTE_TAPE
		JP	NC,END_LDTAPE

		LD	C,A
		INC	A
		AND	$07
		OUT	($FE),A
		LD	A,C

		EX	AF,AF'
		JR	C,SET_LOAD

VERIF:		EX	AF,AF'
		CP	(IX+0)
		JR	NZ,ERROR_VERIF
		JR	CONT_NEXT_BYTE

SET_LOAD:	EX	AF,AF'
		LD	(IX+0),A

CONT_NEXT_BYTE:		INC	IX
		DEC	DE

		LD	L,A		; calc parity
		LD	A,H
		XOR	L
		LD	H,A

		LD	A,D
		OR	E
		JR	NZ,LOOP_LOAD_TAP

		JP	CONTINUARA


ERROR_LFLAG:	JR	TAPE_ERROR	; cuando implementes retornar NZ y quedate posicionado al comienzo
					; del siguiente bloque del TAP

ERROR_PARITY:	JR	TAPE_ERROR

ERROR_VERIF:	JR	TAPE_ERROR

BAD_SIZE:	JR	TAPE_ERROR	; significa que no me coincide lo que mandaron en DE y lo que puedo
					; yo ofrecer desde el TAP, asi que lo considero de "antemano" error
					; de carga y muevo el puntero del TAP al siguiente bloque

TAPE_ERROR:	JP	MOVE_NEXT_BLQ

GET_BYTE_TAPE:	CALL	READ_BYTE
		RET	NC
		CALL	CHECK_TERMINAR
		RET

		;...
		;...

R3_FREE_0:	EQU	331-($-FREE_ROM3_0)
ROM3_SPARE0:	DS	R3_FREE_0

; The printer input (o3a00) and output (o3a05) routines
; Channel information for "P" channel points here

o3A00:		LD	HL,$3D03	; input routine in ROM 1
		JR	o3A08
o3A05:		LD	HL,$3D06	; output routine in ROM 1
o3A08:		EX	AF,AF'
		LD	BC,PBANK678
		LD	A,(BANK678)
		PUSH	AF
		AND	$FB		; select ROM 1
		DI
		LD	(BANK678),A
		OUT	(C),A		; at this point, routine continues in ROM 1
		JP	FONT
o3A1B:		EX	AF,AF'
		POP	AF
		LD	BC,PBANK678
		DI
		LD	(BANK678),A
		OUT	(C),A
		EI			; control returns to this ROM here
		EX	AF,AF'
		RET

; Patch to print error message routine

o3A29:		BIT	4,(IY+$01)	; check bit 4 of FLAGS
		JR	NZ,o3A34	; move on if in +3 BASIC
		XOR	A
		LD	DE,$1536	; else exit to do standard "comma" message
		RET
o3A34:		LD	HL,$010F
o3A37:		EX	(SP),HL
		JP	SWAP		; call routine in ROM 0
					; note that all these routines seem to enter
					; during the reset routine! Or am I missing
					; something...

; Patch to "STMT-RET" routine

o3A3B:		BIT	4,(IY+$01)	; check bit 4 of FLAGS
		JR	NZ,o3A46	; move on if in +3 BASIC
		BIT	7,(IY+$0A)	; else exit with normal 48K ROM check done
		RET
o3A46:		LD	HL,$0112
		JR	o3A37		; go to call routine in ROM 0


; Patch to "STMT-NEXT" routine

o3A4B:		BIT	4,(IY+$01)	; check bit 4 of FLAGS
		JR	NZ,o3A55	; move on if in +3 BASIC
		RST	18H
		CP	$0D		; else exit with normal 48K ROM check done
		RET
o3A55:		LD	HL,$0115
		JR	o3A37		; go to call routine in ROM 0


; Patch to INKEY$ function routine
; Presumably, in earlier 128K spectrums this was used to read the
; external keypad, but it effectively does nothing different to the
; usual routine on the +3.

o3A5A:		CALL	o028E		; do normal call to get key-value in DE
		LD	C,$00
		JR	NZ,o3A6E	; move on if too many keys pressed
		CALL	o031E		; test key value
		JR	NC,o3A6E	; move on if unsatisfactory
		DEC	D		; D=$FF (L-mode)
		LD	E,A		; E=key value
		CALL	o0333		; decode
		JP	o2657		; jump back into INKEY$ routine with keycode
o3A6E:		BIT	4,(IY+$01)	; check bit 4 of FLAGS
		JP	Z,o2660		; jump back into INKEY$ if in 48K BASIC
		DI
		EI
		JR	o3A79
o3A79:		LD	C,$00
		JP	o2660		; jump back into INKEY$ routine


; Patch to "print a character" routine

o3A7E:		CP	$A3
		JR	Z,o3A8E		; move on for "SPECTRUM"
		CP	$A4
		JR	Z,o3A8E		; move on for "PLAY"
o3A86:		SUB	$A5
		JP	NC,o0B5F	; else rejoin print character routine
		JP	o0B56		; with normal test done
o3A8E:		BIT	4,(IY+$01)	; check bit 4 of FLAGS
		JR	Z,o3A86		; move back if in 48K mode
		LD	DE,o3AA8
		PUSH	DE		; stack address to return to in this routine
		SUB	$A3
		LD	DE,o3AB1	; address of "SPECTRUM"
		JR	Z,o3AA2		; move on if SPECTRUM
		LD	DE,o3AB9	; address of "PLAY"
o3AA2:		LD	A,$04
		PUSH	AF		; stack $04 to get a trailing space
		JP	o0C17		; output the token & return to next instruction
o3AA8:		SCF
		BIT	1,(IY+$01)
		RET	NZ		; exit if handling the printer
		JP	o0B03		; else jump back into print routine

o3AB1:		DM	"SPECTRU","M"  + $80
o3AB9:		DM	"PLA","Y" + $80

		JP	o3C01		; what's this for???
					; (yo me hago la misma pregunta amigo)

; **** disponible ****

FREE_ROM3_1:	EQU	$

CONTINUARA:	CALL	GET_BYTE_TAPE	; cargo parity
		CP	H
		JP	NZ,ERROR_PARITY
RET_BASIC:	INC	SP
		INC	SP
		EX	AF,AF'
		RET

READ_BYTE:	PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	IX
		EX	AF,AF'
		PUSH	AF
		EX	AF,AF'

RETRY_RBYTE:	LD	A,$7F		; retorno si encontre SPACE presionada
		IN	A,($FE)
		RRA
		JR	NC,RET_READ_BYTE

		LD	A,$17		; PONGO PAGINA 7
		LD	BC,PBANKM
		OUT	(C),A
		XOR	A
		LD	(EXCEPCIONES),A
		LD	HL,(buffer_COUNT)
		LD	A,H
		OR	L
		JR	NZ,OK_HAY_UN_BYTE

		LD	A,(CONFRAM)	; REESTABLEZCO ROM/RAM
		OUT	(C),A
		CALL	DO_FILL_BUFFER
		JR	RETRY_RBYTE

OK_HAY_UN_BYTE:	DEC	HL
		LD	(buffer_COUNT),HL
		LD	HL,(buffer_ADDR)
		LD	D,(HL)
		INC	HL
		LD	(buffer_ADDR),HL

		LD	A,(CONFRAM)	; REESTABLEZCO ROM/RAM
		LD	BC,PBANKM
		OUT	(C),A

		LD	A,D
		SCF			; retorno CY si se leyeron bytes

RET_READ_BYTE:	EX	AF,AF'
		POP	AF
		EX	AF,AF'
		POP	IX
		POP	BC
		POP	DE
		POP	HL

		RET

DO_FILL_BUFFER:	LD	A,$07
		OUT	($FE),A
		LD	A,$17		; PONGO PAGINA 7
		LD	BC,PBANKM
		OUT	(C),A

ALT_ENTRY:	LD	HL,tmp_workmem	; GUARDO LO QUE ESTA EN $8000 + 1K
		LD	DE,save_buffer
		LD	BC,1024
		LDIR

		LD	A,(BANKM)	; GUARDO ESTAS VARS. XQ TENGO QUE ALTERARLAS
		LD	(tmp_BANKM),A	; PA' QUE +3DOS NO SE PONGA TRISTE LA PUTA QUE LO PARIO
		LD	A,(BANK678)
		LD	(tmp_BANK678),A
		LD	(save_STACK),SP	; TAMBIEN GUARDO STACK EN PAGINA 7

		LD	HL,SWITCH_ROM2	; copio a $8000 la rutina de lectura del buffer
		LD	DE,tmp_workmem+200
		LD	BC,512
		LDIR

		JP	tmp_workmem+200

SWITCH_ROM2:	LD	SP,tmp_workmem+199	; NUEVO STACK

		LD	A,$04
		LD	(BANK678),A
		LD	BC,PBANK678
		OUT	(C),A

		LD	A,$07
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; AHORA ESTOY ARRIBA CON ROM2 PAGINADA

		LD	A,$82		; establezco mis int. en MODO 2, que solo tiene un RETI
		LD	I,A		; así me evito que +3DOS me pisotee la zona de variables
		LD	HL,$8301	; cosa que no pasar?a si la carga fuera desde cinta real
		LD	($82FF),HL
		LD	(HL),$ED	; ---> RETI - o sea que no hago nada
		INC	HL
		LD	(HL),$4D
		IM	2

		LD	A,(EXCEPCIONES)
		CP	$01
		JR	Z,RESET_FILE
		CP	$02
		JR	Z,SET_POS_FILE

		LD	BC,$0607	; ARCHIVO #6 ABIERTO EN ROM1 POR RUTINA MODIFICADA
					; QUE INTERPRETA EL COMANDO SPECTRUM "filename.tap"

		LD	DE,buff_tapsize
		LD	HL,tmp_buffertap

		CALL	DOS_READ	; LEO UN "sector"
		DI			; +3DOS *SIEMPRE* HABILITA LAS INT CUANDO TERMINA SU
					; SERVICIO

		LD	HL,buff_tapsize	; hago de cuenta que lei todos los bytes
		JR	C,SIN_ERROR

		CP	25		; 25 = End of file
		JR	Z,EOF

		LD	B,$06		; OTRO error que no sea EOF abandono el archivo y desactivo la carga
		CALL	DOS_ABANDON
		XOR	A
		LD	(tapl_stat1),A
		INC	A
		LD	(end_LOAD),A
		LD	HL,$0001	; le hago creer que entro por lo menos un byte para que retorne bien
		JR	SIN_ERROR	; y como desactive la carga desde "cinta" no volvermos aqui

SET_POS_FILE:	LD	B,$06
		LD	DE,buff_tapsize
		PUSH	DE
		LD	DE,(S_PFILE+0)
		LD	HL,(S_PFILE+2)
		JR	CONT_SET_POS

RESET_FILE:	LD	DE,buff_tapsize

EOF:		PUSH	DE		; reestablezco el puntero de archivo a 0. es como si hubiese rebobinado
		LD	H,$00		; la cinta y comienza de nuevo
		LD	L,H
		LD	D,H
		LD	E,H
		LD	(S_PFILE+0),HL
		LD	(S_PFILE+2),HL
CONT_SET_POS:	LD	B,$06

		CALL	DOS_SET_POSITION
		POP	DE
		AND	A		; segun el manual si hay un error en DE me queda la cantidad remante
		LD	HL,buff_tapsize	; por leer, entonces calculo cuantos bytes se leyeron
		SBC	HL,DE

SIN_ERROR:	LD	BC,PBANKM
		LD	A,$17
		OUT	(C),A
		LD	BC,PBANK678
		LD	A,$04
		OUT	(C),A		; PARO EL MOTOR
		JP	RETPAG7

RETPAG7:	LD	SP,(save_STACK)

		LD	DE,tmp_buffertap
		LD	(buffer_ADDR),DE
		LD	(buffer_COUNT),HL

RETPAG7_3:	LD	HL,save_buffer	; RECUPERO LO QUE ESTABA EN $8000 + 1K
		LD	DE,tmp_workmem
		LD	BC,1024
		LDIR

		CALL	REST_RAM_ROM	; SE RETORNA EN HL LA CANTIDAD DE BYTES
		RET			; LEIDOS

		;...
		;...

R3_FREE_1:	EQU	319-($-FREE_ROM3_1)
ROM3_SPARE1:	DS	R3_FREE_1

		DW	$FFFF		; para los juegos que usen IM2 con I=3F

o3C01:		DB	$13,$00,"19"	; testcard message
		DB	$13,$01,"87"	; why is it here???
					; (yo me pregunto lo mismo... pero
					; supongo que estos 8 bytes pueden
					; ser usados aunque no sirvan para
					; nada en la práctica)

FREE_ROM3_2:	EQU	$

END_LDTAPE:	LD	A,$01		; EN EL CASO DE APRETAR SPACE MIENTRAS
		CALL	CALL_OTRAS	; SE CARGA no importa conservar
		INC	SP		; el valor de 'A'
		INC	SP
		EX	AF,AF'
		AND	A
		RET

CALL_OTRAS:	PUSH	HL
		PUSH	DE
		PUSH	BC
		PUSH	IX
		EX	AF,AF'
		PUSH	AF
		EX	AF,AF'
		LD	HL,RET_CALL2
		PUSH	HL
		LD	L,A

		LD	A,$17
		LD	BC,PBANKM
		OUT	(C),A
		LD	A,L
		LD	(EXCEPCIONES),A

		JP	ALT_ENTRY

RET_CALL2:	EX	AF,AF'
		POP	AF
		EX	AF,AF'
		POP	IX
		POP	BC
		POP	DE
		POP	HL

		RET

REST_RAM_ROM:	EX	(SP),HL
		IM	1
		LD	A,(save_INTERR)
		CP	$FF
		JR	Z,OK_IM1
		IM	2
OK_IM1:		LD	A,(save_REGI)
		LD	I,A
		LD	A,(tmp_BANKM)
		LD	(BANKM),A
		LD	A,(tmp_BANK678)
		LD	(BANK678),A
		LD	A,(CONFRAM)	; REESTABLEZCO ROM/RAM
		LD	BC,PBANKM
		OUT	(C),A
		EX	(SP),HL
		RET

DUMMY_IM2:	XOR	A
		LD	(SWAP),A
		RETI


SET_POS_SIG:	PUSH	DE
		PUSH	BC
		PUSH	HL		; ESTA RUTINA SE LLAMA CADA vez
		LD	A,$17		; que se empieza a cargar un bloque
		LD	BC,PBANKM	; va guardando en (S_PFILE) la
		OUT	(C),A		; posicion de archivo en donde comienza
		PUSH	HL		; el siguiente bloque. para ser
		POP	DE		; usado en el manejo de errores
		INC	DE
		INC	DE
		LD	HL,(S_PFILE+2)
		AND	A
		ADD	HL,DE
		LD	(S_PFILE+2),HL
		LD	HL,S_PFILE+0
		JR	NC,NODESB
		INC	(HL)
NODESB:		LD	A,(CONFRAM)	; REESTABLEZCO ROM/RAM
		OUT	(C),A
		POP	HL
		POP	BC
		POP	DE
		RET

MOVE_NEXT_BLQ:	LD	A,$02		; mover al bloque siguiente en caso
		CALL	CALL_OTRAS	; de "error"

ERROR_EN_CARGA:	CALL	SWP_RTALERT	; fuerzo retorno por posible error +3DOS
		INC	SP
		INC	SP
		EX	AF,AF'
		AND	A
		RET

SWP_RTALERT:	PUSH	DE
		PUSH	BC
		PUSH	HL
		LD	A,$17
		LD	BC,PBANKM
		OUT	(C),A
		LD	HL,(rt_alert)
		LD	DE,(save_rt_alert)
		LD	(rt_alert),DE
		LD	(save_rt_alert),HL
		JR	NODESB

CHECK_TERMINAR:	PUSH	AF
		PUSH	BC
		LD	A,$17
		LD	BC,PBANKM
		OUT	(C),A
		LD	A,(end_LOAD)
		CP	$01
		LD	A,(CONFRAM)
		OUT	(C),A
		POP	BC
		JR	NZ,NO_ERROR_P3DOS
		POP	AF
		INC	SP
		INC	SP
		INC	SP
		INC	SP

		JR	ERROR_EN_CARGA

NO_ERROR_P3DOS:	POP	AF
		RET

STANDAR_RAM:	LD	A,$10
		LD	(BANKM),A
		LD	A,$04
		LD	(BANK678),A
		RET

		;...
		;...

R3_FREE_2:	EQU	247-($-FREE_ROM3_2)
ROM3_SPARE2:	DS	R3_FREE_2

;===============================================================================

		INCLUDE	"plus3FONT.asm"

;===============================================================================

; Acknowledgements
; -----------------
; Sean Irvine               for default list of section headings
; Dr. Ian Logan             for labels and functional disassembly.
; Dr. Frank O'Hara          for labels and functional disassembly.
;
; Credits
; -------
; Alex Pallero Gonzales     for corrections.
; Mike Dailly               for comments.
; Alvin Albrecht            for comments.
; Andy Styles               for full relocatability implementation and testing.
; Andrew Owen               for ZASM compatibility and format improvements.

;   For other assemblers you may have to add directives like these near the
;   beginning - see accompanying documentation.
;   ZASM (MacOs) cross-assembler directives. (uncomment by removing $3B )
;   #target rom           ; declare target file format as binary.
;   #code   0,$4000       ; declare code segment.
;   Also see notes at Address Labels 0609 and 1CA5 if your assembler has
;   trouble with expressions.
;
;   Note. The Sinclair Interface 1 ROM written by Dr. Ian Logan and Martin
;   Brennan calls numerous routines in this ROM.
;   Non-standard entry points have a label beginning with X.
