		OUTPUT	"p3t_rom1.rom"

		ORG	$0000

; **************************************************
; *** SPECTRUM +3 ROM 1 DISASSEMBLY (SYNTAX ROM) ***
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

; This file can be assembled to produce a binary image of the ROM
; with Interlogic's Z80ASM assembler (available for Z88, QL, DOS and Linux).
; Note that the defs directive is used and this causes a block of $00 bytes to be created.

;**************************************************

; +3DOS routine addresses

		DEFINE	DOS_INITIALISE $0100
		DEFINE	DOS_VERSION $0103
		DEFINE	DOS_OPEN $0106
		DEFINE	DOS_CLOSE $0109
		DEFINE	DOS_ABANDON $010C
		DEFINE	DOS_REF_HEAD $010F
		DEFINE	DOS_READ $0112
		DEFINE	DOS_WRITE $0115
		DEFINE	DOS_BYTE_READ $0118
		DEFINE	DOS_BYTE_WRITE $011B
		DEFINE	DOS_CATALOG $011E
		DEFINE	DOS_FREE_SPACE $0121
		DEFINE	DOS_DELETE $0124
		DEFINE	DOS_RENAME $0127
		DEFINE	DOS_BOOT $012A
		DEFINE	DOS_SET_DRIVE $012D
		DEFINE	DOS_SET_USER $0130
		DEFINE	DOS_GET_POSITION $0133
		DEFINE	DOS_SET_POSITION $0136
		DEFINE	DOS_GET_EOF $0139
		DEFINE	DOS_GET_1346 $013C
		DEFINE	DOS_SET_1346 $013F
		DEFINE	DOS_FLUSH $0142
		DEFINE	DOS_SET_ACCESS $0145
		DEFINE	DOS_SET_ATTRIBUTES $0148
		DEFINE	DOS_OPENDRIVE $014B
		DEFINE	DOS_SET_MESSAGE $014E
		DEFINE	DOS_REF_XDPB $0151
		DEFINE	DOS_MAP_B $0154

		DEFINE	DD_INTERFACE $0157
		DEFINE	DD_INIT $015A
		DEFINE	DD_SETUP $015D
		DEFINE	DD_SET_RETRY $0160
		DEFINE	DD_READ_SECTOR $0163
		DEFINE	DD_WRITE_SECTOR $0166
		DEFINE	DD_CHECK_SECTOR $0169
		DEFINE	DD_FORMAT $016C
		DEFINE	DD_READ_ID $016F
		DEFINE	DD_TEST_UNSUITABLE $0172
		DEFINE	DD_LOGIN $0175
		DEFINE	DD_SEL_FORMAT $0178
		DEFINE	DD_ASK_1 $017B
		DEFINE	DD_DRIVE_STATUS $017E
		DEFINE	DD_EQUIPMENT $0181
		DEFINE	DD_ENCODE $0184
		DEFINE	DD_L_XDPB $0187
		DEFINE	DD_L_DPB $018A
		DEFINE	DD_L_SEEK $018D
		DEFINE	DD_L_READ $0190
		DEFINE	DD_L_WRITE $0193
		DEFINE	DD_L_ON_MOTOR $0196
		DEFINE	DD_L_T_OFF_MOTOR $0199
		DEFINE	DD_L_OFF_MOTOR $019C

; +3DOS Error codes

		DEFINE	rc_ready $00
		DEFINE	rc_wp $01
		DEFINE	rc_seek $02
		DEFINE	rc_crc $03
		DEFINE	rc_nodata $04
		DEFINE	rc_mark $05
		DEFINE	rc_unrecog $06
		DEFINE	rc_unknown $07
		DEFINE	rc_diskchg $08
		DEFINE	rc_unsuit $09

		DEFINE	rc_badname $14
		DEFINE	rc_badparam $15
		DEFINE	rc_nodrive $16
		DEFINE	rc_nofile $17
		DEFINE	rc_exists $18
		DEFINE	rc_eof $19
		DEFINE	rc_diskfull $1A
		DEFINE	rc_dirfull $1B
		DEFINE	rc_ro $1C
		DEFINE	rc_number $1D
		DEFINE	rc_denied $1E
		DEFINE	rc_norename $1F
		DEFINE	rc_extent $20
		DEFINE	rc_uncached $21
		DEFINE	rc_toobig $22
		DEFINE	rc_notboot $23
		DEFINE	rc_inuse $24
;--------------------------------------------------

; The floating-point calculator commands

;        include "fpcalc.def"
		DEFINE	jump_true $00
		DEFINE	EXchange $01
		DEFINE	delete $02
		DEFINE	SUBtract $03
		DEFINE	multiply $04
		DEFINE	DIvision $05
		DEFINE	to_power $06
		DEFINE	logic_or $07
		DEFINE	no_and_no $08
		DEFINE	no_l_eql $09
		DEFINE	no_gr_eq $0A
		DEFINE	nos_neql $0B
		DEFINE	no_grtr $0C
		DEFINE	no_less $0D
		DEFINE	nos_eql $0E
		DEFINE	ADDition $0F
		DEFINE	strandno $10
		DEFINE	str_l_eql $11
		DEFINE	str_gr_eq $12
		DEFINE	strs_neql $13
		DEFINE	str_grtr $14
		DEFINE	str_less $15
		DEFINE	strs_eql $16
		DEFINE	strs_add $17
		DEFINE	val_str $18
		DEFINE	usr_str $19
		DEFINE	read_in $1A
		DEFINE	NEGate $1B
		DEFINE	code $1C
		DEFINE	val $1D
		DEFINE	len $1E
		DEFINE	sin $1F
		DEFINE	cos $20
		DEFINE	tan $21
		DEFINE	asn $22
		DEFINE	acs $23
		DEFINE	atn $24
		DEFINE	ln $25
		DEFINE	EXp $26
		DEFINE	INt $27
		DEFINE	sqr $28
		DEFINE	sgn $29
		DEFINE	abs $2A
		DEFINE	peek $2B
		DEFINE	IN_port $2C
		DEFINE	usr_no $2D
		DEFINE	str_str $2E
		DEFINE	chr_str $2F
		DEFINE	not $30
		DEFINE	duplicate $31
		DEFINE	n_mod_m $32
		DEFINE	jump $33
		DEFINE	stk_data $34
		DEFINE	DEC_jr_nz $35
		DEFINE	less_0 $36
		DEFINE	greater_0 $37
		DEFINE	end_calc $38
		DEFINE	get_argt $39
		DEFINE	truncate $3A
		DEFINE	fp_calc_2 $3B
		DEFINE	e_to_fp $3C
		DEFINE	re_stack $3D
		DEFINE	series_06 $86
		DEFINE	series_08 $88
		DEFINE	series_0c $8C
		DEFINE	stk_zero $A0
		DEFINE	stk_one $A1
		DEFINE	stk_half $A2
		DEFINE	stk_pi_2 $A3
		DEFINE	stk_ten $A4
		DEFINE	st_mem_0 $C0
		DEFINE	st_mem_1 $C1
		DEFINE	st_mem_2 $C2
		DEFINE	st_mem_3 $C3
		DEFINE	st_mem_4 $C4
		DEFINE	st_mem_5 $C5
		DEFINE	get_mem_0 $E0
		DEFINE	get_mem_1 $E1
		DEFINE	get_mem_2 $E2
		DEFINE	get_mem_3 $E3
		DEFINE	get_mem_4 $E4
		DEFINE	get_mem_5 $E5
;--------------------------------------------------

		.org	$0000

; ROM 1 Header

m0000:		DM	"Syntax"
		DS	2

; RST $08 - The "Error" restart

m0008:		JP	m2ada		; jump to error handler

		DS	5

; RST $10 - The "Print a character restart"

m0010:		RST	28H
		DW	$0010		; call RST $10 in ROM 3
		RET

		DS	4

; RST $18 - The "Collect character" restart

m0018:		RST	28H
		DW	$0018		; call RST $18 in ROM 3
		RET

		DS	4

; RST $20 - The "Collect next character" restart

m0020:		RST	28H
		DW	$0020		; call RST $20 in ROM 3
		RET

		DS	4

; RST $28 : Call a routine in ROM 3, then return to ROM 1
; The address following the RST 28 instruction is called, then control
; is returned to the instruction following the address

m0028:		EX	(SP),HL		; save HL, get return address
		PUSH	AF		; save AF
		LD	A,(HL)		; A=low byte of address to call
		INC	HL
		INC	HL		; HL=address of instruction to return to
		LD	(RETADDR),HL	; save
m0030:		DEC	HL
		LD	H,(HL)
		LD	L,A		; HL=address to call in ROM 3
		POP	AF		; restore AF
		JP	m00aa		; jump on

		NOP

; The maskable interrupt routine

m0038:		PUSH	AF		; save registers
		PUSH	HL
		LD	HL,(FRAMES)	; increment FRAMES
		INC	HL
		LD	(FRAMES),HL
		LD	A,H
		OR	L
		JR	NZ,m0048
		INC	(IY+$40)
m0048:		PUSH	BC
		PUSH	DE
		CALL	m0176		; scan keyboard
		CALL	m0074		; call disk motor timeout routine
		POP	DE		; restore registers
		POP	BC
		POP	HL
		POP	AF
		EI			; re-enable interrupts & exit
		RET

m0056:		DM	"Start: ", 0
m005e:		DM	"system", 0, 0

; The NMI routine

m0066:		PUSH	AF		; save registers
		PUSH	HL
		LD	HL,(NMIADD)	; get routine address
		LD	A,H
		OR	L
		JR	Z,m0070
		JP	(HL)		; execute if non-zero address
m0070:		POP	HL		; restore registers & exit
		POP	AF
		RETN

; The disk motor timeout routine

m0074:		LD	BC,PBANKM
		LD	A,(BANKM)
		OR	$07
		OUT	(C),A		; page in page 7
		LD	A,(timeout)
		OR	A
		JR	Z,m00a1		; move on if already off
		LD	A,(FRAMES)
		BIT	0,A
		JR	NZ,m00a1	; only decrement counter every other frame
		LD	A,(timeout)
		DEC	A		; decrement timeout counter
		LD	(timeout),A
		JR	NZ,m00a1	; move on if non-zero
		LD	BC,PBANK678
		LD	A,(BANK678)
		AND	$F7
		LD	(BANK678),A
		OUT	(C),A		; turn motor off
m00a1:		LD	BC,PBANKM
		LD	A,(BANKM)
		OUT	(C),A		; page in last memory configuration
		RET

; Continuation of RST 28: call a routine in ROM 3

m00aa:		LD	(TARGET),HL	; save ROM 3 address in TARGET
		LD	HL,REGNUOY
		EX	(SP),HL		; stack REGNUOY address beneath TOS
		PUSH	HL
		LD	HL,(TARGET)	; get HL=target address in ROM 3
		EX	(SP),HL		; restore HL & save target address on stack
		PUSH	AF		; stack AF & BC
		PUSH	BC
		DI			; disable interrupts
		JP	STOO		; jump to STOO - pages in ROM 3, returns to
					; target routine which returns to REGNUOY
					; where ROM 1 is paged back and jump made
					; back to RETADDR

; These are copies of the key tables from ROM 3

; The L-mode keytable with CAPS-SHIFT

m00bc:		DM	"BHY65TGV"
		DM	"NJU74RFC"
		DM	"MKI83EDX"
		DM	$0E, "LO92WSZ"
		DM	" ", $0D, "P01QA"

; The extended-mode keytable (unshifted letters)

m00e3:		DB	$E3,$C4,$E0,$E4
		DB	$B4,$BC,$BD,$BB
		DB	$AF,$B0,$B1,$C0
		DB	$A7,$A6,$BE,$AD
		DB	$B2,$BA,$E5,$A5
		DB	$C2,$E1,$B3,$B9
		DB	$C1,$B8

; The extended mode keytable (shifted letters)

m00fd:		DB	$7E,$DC,$DA,$5C
		DB	$B7,$7B,$7D,$D8
		DB	$BF,$AE,$AA,$AB
		DB	$DD,$DE,$DF,$7F
		DB	$B5,$D6,$7C,$D5
		DB	$5D,$DB,$B6,$D9
		DB	$5B,$D7

; The control code keytable (CAPS-SHIFTed digits)

m0117:		DB	$0C,$07,$06,$04
		DB	$05,$08,$0A,$0B
		DB	$09,$0F

; The symbol code keytable (letters with symbol shift)

m0121:		DB	$E2,$2A,$3F,$CD
		DB	$C8,$CC,$CB,$5E
		DB	$AC,$2D,$2B,$3D
		DB	$2E,$2C,$3B,$22
		DB	$C7,$3C,$C3,$3E
		DB	$C5,$2F,$C9,$60
		DB	$C6,$3A

; The extended mode keytable (SYM-SHIFTed digits)

m013b:		DB	$D0,$CE,$A8,$CA
		DB	$D3,$D4,$D1,$D2
		DB	$A9,$CF

; This is a copy of the "keyboard scanning" subroutine from
; o028E in ROM 3

m0145:		LD	L,$2F
		LD	DE,$FFFF
		LD	BC,$FEFE
m014d:		IN	A,(C)
		CPL
		AND	$1F
		JR	Z,m0162
		LD	H,A
		LD	A,L
m0156:		INC	D
		RET	NZ
m0158:		SUB	$08
		SRL	H
		JR	NC,m0158
		LD	D,E
		LD	E,A
		JR	NZ,m0156
m0162:		DEC	L
		RLC	B
		JR	C,m014d
		LD	A,D
		INC	A
		RET	Z
		CP	$28
		RET	Z
		CP	$19
		RET	Z
		LD	A,E
		LD	E,D
		LD	D,A
		CP	$18
		RET

; This is a copy of the "keyboard" subroutines from o02BF in ROM 3

m0176:		CALL	m0145
		RET	NZ
		LD	HL,KSTATE
m017d:		BIT	7,(HL)
		JR	NZ,m0188
		INC	HL
		DEC	(HL)
		DEC	HL
		JR	NZ,m0188
		LD	(HL),$FF
m0188:		LD	A,L
		LD	HL,KSTATE+$04
		CP	L
		JR	NZ,m017d
		CALL	m01d5
		RET	NC
		LD	HL,KSTATE
		CP	(HL)
		JR	Z,m01c7
		EX	DE,HL
		LD	HL,KSTATE+$04
		CP	(HL)
		JR	Z,m01c7
		BIT	7,(HL)
		JR	NZ,m01a8
		EX	DE,HL
		BIT	7,(HL)
		RET	Z
m01a8:		LD	E,A
		LD	(HL),A
		INC	HL
		LD	(HL),$05
		INC	HL
		LD	A,(REPDEL)
		LD	(HL),A
		INC	HL
		LD	C,(IY+$07)
		LD	D,(IY+$01)
		PUSH	HL
		CALL	m01ea
		POP	HL
		LD	(HL),A
m01bf:		LD	(LAST_K),A
		SET	5,(IY+$01)
		RET
m01c7:		INC	HL
		LD	(HL),$05
		INC	HL
		DEC	(HL)
		RET	NZ
		LD	A,(REPPER)
		LD	(HL),A
		INC	HL
		LD	A,(HL)
		JR	m01bf

; This is a copy of the "K-Test" subroutine from o031E in ROM 3

m01d5:		LD	B,D
		LD	D,$00
		LD	A,E
		CP	$27
		RET	NC
		CP	$18
		JR	NZ,m01e3
		BIT	7,B
		RET	NZ
m01e3:		LD	HL,m00bc	; the main keytable
		ADD	HL,DE
		LD	A,(HL)
		SCF
		RET

; This is a copy of the "Keyboard decoding" subroutine from o0333 in
; ROM 3

m01ea:		LD	A,E
		CP	$3A
		JR	C,m021e
		DEC	C
		JP	m,m0206
		JR	Z,m01f8
		ADD	A,$4F
		RET
m01f8:		LD	HL,m00e3-"A"
		INC	B
		JR	Z,m0201
		LD	HL,m00fd-"A"
m0201:		LD	D,$00
		ADD	HL,DE
		LD	A,(HL)
		RET
m0206:		LD	HL,m0121-"A"
		BIT	0,B
		JR	Z,m0201
		BIT	3,D
		JR	Z,m021b
		BIT	3,(IY+$30)
		RET	NZ
		INC	B
		RET	NZ
		ADD	A,$20
		RET
m021b:		ADD	A,$A5
		RET
m021e:		CP	$30
		RET	C
		DEC	C
		JP	m,m0254
		JR	NZ,m0240
		LD	HL,m013b-"0"
		BIT	5,B
		JR	Z,m0201
		CP	$38
		JR	NC,m0239
		SUB	$20
		INC	B
		RET	Z
		ADD	A,$08
		RET
m0239:		SUB	$36
		INC	B
		RET	Z
		ADD	A,$FE
		RET
m0240:		LD	HL,m0117-"0"
		CP	$39
		JR	Z,m0201
		CP	$30
		JR	Z,m0201
		AND	$07
		ADD	A,$80
		INC	B
		RET	Z
		XOR	$0F
		RET
m0254:		INC	B
		RET	Z
		BIT	5,B
		LD	HL,m0117-"0"
		JR	NZ,m0201
		SUB	$10
		CP	$22
		JR	Z,m0269
		CP	$20
		RET	NZ
		LD	A,$5F
		RET
m0269:		LD	A,$40
		RET

; The FORMAT command

m026c:		RST	28H
		DW	$0018		; get character after FORMAT
m026f:		CP	$E0
		JP	Z,m03e3		; move on if LPRINT
		CP	$CA
		JP	Z,m1e02		; move on if not LINE
		CP	$CC
		JP	Z,m1dd9
m027e:		RST	28H
		DW	o1C8C		; get a string expression
		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2BF1		; get string from stack
		LD	A,C
		DEC	A
		DEC	A
		OR	B
		JR	Z,m0291		; move on if length is 2
m028d:		CALL	m2ada
		DB	$4E		; else error "Invalid drive"
m0291:		INC	DE
		LD	A,(DE)		; check 2nd char
		DEC	DE
		CP	":"
		JR	Z,m029c
		CALL	m2ada
		DB	$4E		; error "Invalid drive" if not colon
m029c:		LD	A,(DE)
		AND	$DF		; get capitalised drive letter
		CP	"A"
		JR	Z,m02ab		; move on if A:
		CP	"B"
		JR	Z,m02ab		; or B:
		CALL	m2ada
		DB	$4E		; else error "Invalid drive"
m02ab:		CALL	m2b89		; page in DOS workspace
		SUB	"A"
		PUSH	AF		; save unit number to format
		LD	HL,FLAGS3
		BIT	4,(HL)
		JR	NZ,m02bf	; move on if disk interface present
		CALL	m2b64		; page in normal memory
		CALL	m2ada
		DB	$4C		; else error "Format not supported on +2A"
m02bf:		POP	AF
		OR	A
		JR	Z,m02d3		; move on for unit 0
		PUSH	AF
		LD	HL,FLAGS3
		BIT	5,(HL)
		JR	NZ,m02d2	; move on if drive B: present
		CALL	m2b64		; page in normal memory
		CALL	m2ada
		DB	$4B		; else error "Drive B: not present"
m02d2:		POP	AF		; get unit
m02d3:		PUSH	AF
		LD	C,A
		PUSH	BC
		ADD	A,"A"
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_REF_XDPB	; point IX at XDPB
		CALL	m32ee		; restore TSTACK
		JR	C,m02ec		; move on if no error
		CALL	m2b64		; page in DOS memory
		CALL	m0e9a		; cause DOS error
		DB	$FF
m02ec:		POP	BC
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DD_LOGIN	; login disk
		CALL	m32ee		; restore TSTACK
		JR	NC,m0306	; move on if error
		OR	A
		JR	NZ,m0315	; move on if disk isn't +3 format
		CALL	m0381		; ask if wish to abandon
		JR	NZ,m0315	; move on if not
		CALL	m2b64		; page in normal memory
		RET			; exit
m0306:		CP	$05
		JR	Z,m0315		; move on if error was "missing address mark"
		CP	$09
		JR	Z,m0315		; or "unsuitable media"
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause DOS error
		DB	$FF
m0315:		POP	AF		; get unit number
		PUSH	AF
		ADD	A,"A"
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_REF_XDPB	; point IX to XDPB
		CALL	m32ee		; restore TSTACK
		JR	C,m032d
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause any DOS error
		DB	$FF
m032d:		XOR	A
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DD_SEL_FORMAT	; select +3 format
		CALL	m32ee		; restore TSTACK
		JR	C,m0342
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause any DOS error
		DB	$FF
m0342:		POP	AF
		LD	C,A		; C=unit number
		XOR	A		; start at track 0
m0345:		LD	D,A
		CALL	m036f		; fill format buffer
		LD	E,$E5		; filler byte
		LD	B,$07		; page 7
		LD	HL,tmp_buff	; buffer address
		PUSH	AF
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DD_FORMAT	; format a track
		CALL	m32ee		; restore TSTACK
		JR	C,m0365
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause any DOS error
		DB	$FF
m0365:		POP	AF
		INC	A		; increment track
		CP	$28
		JR	NZ,m0345	; back if more to do
		CALL	m2b64		; page in normal memory
		RET			; done

; Subroutine to fill scratch area with format buffer details

m036f:		LD	B,$09		; 9 sectors
		LD	HL,tmp_buff+$23	; end of scratch area
m0374:		LD	(HL),$02	; 512-byte sectors
		DEC	HL
		LD	(HL),B		; sector number
		DEC	HL
		LD	(HL),$00	; head 0
		DEC	HL
		LD	(HL),D		; track number
		DEC	HL
		DJNZ	m0374
		RET

; Subroutine to display "disk already formatted message",
; and get a key, exiting with Z set if user wishes to abandon

m0381:		LD	HL,m03a7
m0384:		LD	A,(HL)		; get next char
		OR	A
		JR	Z,m038e		; move on if null
		RST	28H
		DW	$0010		; output char
m038b:		INC	HL
		JR	m0384		; loop back
m038e:		RES	5,(IY+$01)	; signal "no key"
m0392:		BIT	5,(IY+$01)
		JR	Z,m0392		; wait for key
		LD	A,(LAST_K)	; get key
		AND	$DF		; capitalise
		CP	"A"		; is it "A"?
		PUSH	AF
		PUSH	HL
		RST	28H
		DW	o0D6E		; clear lower screen
		POP	HL
		POP	AF
		RET			; exit with Z set if abandon requested

; Formatting message

m03a7:		DM	"Disk is already formatted.", $0D
		DM	"A to abandon, other key continue", 0

; The FORMAT LPRINT command

m03e3:		RST	28H
		DW	$0020		; get next char
m03e6:		RST	28H
		DW	o1C8C		; get string expression
		RST	28H
		DW	$0018		; get next char
m03ec:		CP	$3B
		CALL	NZ,m10b1	; check for end-of-statement if not $3B
		JR	NZ,m041c	; move on if not $3B
		RST	28H
		DW	$0020		; get next char
m03f6:		RST	28H
		DW	o1C8C		; get string expression
		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2BF1		; get 2nd string from stack
		LD	A,C
		DEC	A
		OR	B		; check length
		JR	Z,m0407
		JP	m028d		; "Invalid drive" error if not 1
m0407:		LD	A,(DE)
		AND	$DF		; capitalise 2nd string character
		LD	HL,FLAGS3	; prepare to change FLAGS3
		CP	"E"
		JR	NZ,m0415
m0411:		SET	2,(HL)		; if 2nd string "E", set "expand tokens" flag
		JR	m041c
m0415:		CP	"U"
		JP	NZ,m028d	; if 2nd string not "U", error
		RES	2,(HL)		; if "U", reset "expand tokens" flag
m041c:		RST	28H
		DW	o2BF1		; get first string from stack
		LD	A,C
		DEC	A
		OR	B		; check length
		JR	Z,m0427
		JP	m028d		; "Invalid drive" error if not 1

m0427:		LD	A,(DE)
		AND	$DF		; capitalise 1st string character
		LD	HL,FLAGS3	; prepare to change FLAGS3
		CP	"R"
		JR	NZ,m0434
		SET	3,(HL)		; if "R", set print to RS232 flag
		RET
m0434:		CP	"C"
		JR	NZ,m043b
		RES	3,(HL)		; if "C", reset print to RS232 flag
		RET
m043b:		CP	"E"
		JR	NZ,m0442
		SET	2,(HL)		; if "E", set "expand tokens" flag
		RET
m0442:		CP	"U"
		JP	NZ,m028d	; if not "U", error
		RES	2,(HL)		; if "U", reset "expand tokens" flag
		RET

; The ERASE command
; *BUG* No channel is opened before outputting the "Erase (Y/N)?" message,
;       so this is output to the last used stream.
; *BUG* The lower screen is not cleared if "N" is pressed

m044a:		RST	28H
		DW	o2BF1		; get string from stack
		LD	A,B
		OR	C		; check length
		JR	NZ,m0455
		CALL	m2ada
		DB	$2C		; bad filename error if zero
m0455:		PUSH	BC		; save addresses
		PUSH	DE
		PUSH	DE
		POP	HL		; HL=address of filename
		PUSH	BC
		LD	A,"*"
		CPIR
		POP	BC
		JR	Z,m046d		; move on if * wildcard present
		PUSH	DE
		POP	HL
		PUSH	BC
		LD	A,"?"
		CPIR
		POP	BC
		JR	Z,m046d		; move on if ? wildcard present
		JR	m0499		; move on for a single file

m046d:		LD	HL,merase
		CALL	m04c1		; output "Erase "
		CALL	m04ca		; output filespec
		LD	HL,myn
		CALL	m04c1		; output "? (Y/N"
m047c:		LD	HL,FLAGS
		RES	5,(HL)		; signal "no key available"
m0481:		BIT	5,(HL)
		JR	Z,m0481		; loop until keypress
		RES	5,(HL)		; signal "no key available"
		LD	A,(LAST_K)	; get key
		AND	$DF		; make uppercase
		CP	"N"
		JR	NZ,m0493	; move on if not "N"
		POP	DE		; exit without doing anything
		POP	BC		; (lower screen should have been cleared)
		RET
m0493:		CP	"Y"
		JR	Z,m0499
		JR	m047c		; loop back for another key if not "Y"
m0499:		RST	28H
		DW	o0D6E		; clear lower screen
		POP	DE
		POP	BC
		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy filespec into page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
		LD	HL,tmp_fspec
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_DELETE	; delete filespec
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		RET	C		; exit if ok
		CALL	m0e9a		; cause DOS error
		DB	$FF

; Subroutine to output a null-terminated string

m04c1:		LD	A,(HL)		; get next char
		OR	A
		RET	Z		; exit if null
		INC	HL
		RST	28H
		DW	$0010		; output char
m04c8:		JR	m04c1		; loop back

; Subroutine to output a filespec at DE, length BC

m04ca:		LD	A,(DE)		; get next char
		RST	28H
		DW	$0010		; output char
m04ce:		INC	DE
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,m04ca	; back for more
		RET

; Erase messages
m04d5:		RST	28H
		DW	$C101
		LD	(BC),A
		INC	(HL)
		LD	B,B
		LD	B,C
		NOP
		NOP
		LD	($38E1),A
		RET
		DB	0,0,0

; The MOVE command

m04e5:		RST	28H
		DW	o2BF1		; get 2nd string
		LD	A,B
		OR	C		; check length
		JR	NZ,m04f0
		CALL	m2ada
		DB	$2C		; bad filename error if zero
m04f0:		LD	A,(DE)
		CP	'+'
		JP	Z,m0541		; move on if changing attributes
		CP	'-'
		JP	Z,m0541		; move on if changing attributes
		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy filename to page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
m0508:		INC	DE
		CALL	m2b64		; page in normal memory
		PUSH	DE		; save pointer for source filename
		RST	28H
		DW	o2BF1		; get 1st string
		LD	A,B
		OR	C		; check length
		JR	NZ,m0518
		CALL	m2ada
		DB	$2C		; bad filename error if zero
m0518:		POP	HL		; HL=address to place source filename
		PUSH	HL
		EX	DE,HL
		CALL	m3f63		; copy source filename to page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
		CALL	m2b64		; page in normal memory
		POP	HL
		LD	DE,tmp_fspec
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_RENAME	; do rename
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		RET	C		; exit if done ok
		CALL	m0e9a		; cause DOS error
		DB	$FF

; Here we use MOVE to alter attributes of a file

m0541:		LD	A,C
		DEC	A
		DEC	A
		OR	B
		JR	Z,m054b		; move on if 2nd string length=2
		CALL	m2ada
		DB	$47		; invalid attribute error
m054b:		LD	A,(DE)
		LD	B,A		; B='+' or '-'
		INC	DE
		LD	A,(DE)
		AND	$DF		; A=uppercase attribute
		CP	"P"		; check attribute letter
		JR	Z,m0561
		CP	"S"
		JR	Z,m0561
		CP	"A"
		JR	Z,m0561
		CALL	m2ada
		DB	$47		; invalid attribute error
m0561:		PUSH	BC		; save attribute flags
		PUSH	AF
		RST	28H
		DW	o2BF1		; get 1st string
		LD	A,B
		OR	C		; check length
		JR	NZ,m056e
		CALL	m2ada
		DB	$2C		; bad filename error if zero
m056e:		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy to page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
		CALL	m2b64		; page in normal memory
		LD	DE,$0000	; don't set or clear anything yet
		LD	C,$00		; attribute byte to set/clear
		POP	AF		; get attribute letter
		CP	"P"
		JR	NZ,m058c
		SET	2,C		; bit 2 for P
		JR	m0596
m058c:		CP	"S"
		JR	NZ,m0594
		SET	1,C		; bit 1 for S
		JR	m0596
m0594:		SET	0,C		; bit 0 for A
m0596:		POP	AF		; get '+' or '-'
		CP	'+'
		JR	NZ,m059e
		LD	D,C		; if +, we're setting attributes
		JR	m059f
m059e:		LD	E,C		; if -, we're clearing attributes
m059f:		LD	HL,tmp_fspec
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_ATTRIBUTES	; set the attributes
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		RET	C		; exit if done ok
		CALL	m0e9a		; else cause DOS error
		DB	$FF

; The CAT command
; *BUG* Only one buffer of entries is ever considered (64 entries), as a
;       SUB $40 is used (should be SUB $3F)

m05b8:		LD	HL,FLAGS3
		RES	6,(HL)		; signal "standard catalog"
		RST	28H
		DW	o2070		; consider stream information
		JR	C,m05dd		; move on if default stream to be used
		LD	HL,(CH_ADD)
		LD	A,(HL)		; get next char
		CP	","
		JR	Z,m05da		; move on if comma to get filespec
		CP	$0D
		JR	Z,m062b		; move on if end-of-line
		CP	":"
		JR	Z,m062b		; or if end-of-statement
		CP	$B9
		JR	Z,m062b		; or if EXP
		JP	m3b48
		NOP
m05da:		RST	20H		; get next char
		JR	m05f8
m05dd:		LD	A,$02		; use stream 2
		BIT	7,(IY+$01)
		JR	Z,m05e8		; move on if only syntax-checking
		RST	28H
		DW	o1601		; else open channel to stream
m05e8:		LD	HL,(CH_ADD)
		LD	A,(HL)		; check next char
		CP	$0D
		JR	Z,m062b		; move on if end-of-line
		CP	":"
		JR	Z,m062b		; or if end-of-statement
		JP	n3b4d
		NOP
m05f8:		RST	28H
		DW	o1C8C		; get string expression
		RST	28H
		DW	$0018		; get next char
m05fe:		CP	$B9
		JR	NZ,m060a	; move on if not EXP
		LD	HL,FLAGS3
		SET	6,(HL)		; signal "expanded catalog"
		RST	28H
		DW	$0020		; get next char
m060a:		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2BF1		; get string value from stack
		PUSH	BC
		PUSH	DE
		POP	HL		; HL=string address
		LD	A,":"		; check for drive specification
		CPIR
		JR	NZ,m0623	; move on if not found
		DEC	HL
		DEC	HL
		LD	A,(HL)
		AND	$DF
		LD	(DEFADD),A	; else save capitalised drive letter
		JR	m0628		; move on
m0623:		LD	A,$00
		LD	(DEFADD),A	; signal "use default drive"
m0628:		POP	BC
		JR	m0645		; move on
m062b:		RST	28H
		DW	$0018		; get next char
m062e:		CP	$B9
		JR	NZ,m063a	; move on if not EXP
		LD	HL,FLAGS3
		SET	6,(HL)		; signal "expanded catalog"
		RST	28H
		DW	$0020		; get next char
m063a:		CALL	m10b1		; check for end-of-statement
		LD	BC,$0000	; filespec length=0
		LD	A,$00
		LD	(DEFADD),A	; signal "use default drive"
m0645:		LD	A,C
		DEC	A
		DEC	A
		OR	B
		JR	NZ,m065c	; move on unless just 2 chars specified
		INC	DE
		LD	A,(DE)
		DEC	DE
		CP	":"
		JR	NZ,m065c	; move on if not drive specifier
		LD	A,(DE)
		AND	$DF		; get drive letter capitalised
		CP	"T"
		JR	NZ,m065c
		JP	m34c6		; move on to catalog tape
m065c:		LD	HL,tmp_fspec
		EX	DE,HL
		PUSH	BC
		LD	A,B
		OR	C
		JR	Z,m0668		; move on if no filespec
		CALL	m3f63		; copy to page 7 (entry 0)
m0668:		POP	BC
		LD	HL,tmp_fspec
		ADD	HL,BC
		CALL	m2b89
		LD	(HL),$FF	; add terminator
		LD	HL,tmp_buff
		LD	DE,tmp_buff+1
		LD	BC,$000B
		LD	(HL),$00
		LDIR			; zero entry 0
m067f:		LD	B,$40		; 64 entries in buffer
		LD	C,$00		; C=0 for standard catalog
		LD	HL,FLAGS3
		BIT	6,(HL)
		JR	Z,m068c
		LD	C,$01		; C=1 for expanded catalog (inc system files)
m068c:		LD	DE,tmp_buff
		LD	HL,tmp_fspec
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CATALOG	; get next lot of entries
		CALL	m32ee		; restore TSTACK
		JP	NC,m06ab	; move on if error
		LD	HL,tmp_buff+$0D	; address of first returned entry
		DEC	B		; B=# entries found (discard preloaded one)
		LD	A,B
		OR	A
		JR	NZ,m06b7	; move on if entries to display
		JP	m07ba		; move on if catalog finished
m06ab:		CP	$17
		JP	Z,m07ba		; move on if error "file not found"
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause DOS error
		DB	$FF
m06b7:		PUSH	BC		; save number of entries to do
m06b8:		PUSH	AF
		LD	B,$08		; 8 bytes in first half of filename
m06bb:		LD	A,(HL)
		AND	$7F		; get byte and mask bit
		CALL	m2b64		; page in normal memory
		RST	28H
		DW	$0010		; output char
m06c4:		CALL	m2b89		; page in DOS workspace
		INC	HL
		DJNZ	m06bb		; loop back for rest of filename
		CALL	m2b64		; page in normal memory
		LD	A,'.'
		RST	28H
		DW	$0010		; output "."
m06d2:		XOR	A
		LD	(RAMERR),A	; zeroise attributes
		LD	B,$03
m06d8:		CALL	m2b89		; page in DOS workspace
		LD	A,(HL)		; get next byte
		BIT	7,A
		JR	Z,m06fc		; move on if bit 7 not set
		PUSH	AF
		PUSH	HL
		LD	HL,RAMERR
		LD	A,B
		CP	$03
		JR	NZ,m06ee
		SET	3,(HL)		; set bit 3 if first extension byte
		JR	m06f8
m06ee:		CP	$02
		JR	NZ,m06f6
		SET	2,(HL)		; set bit 2 if second extension byte
		JR	m06f8
m06f6:		SET	1,(HL)		; set bit 1 if third extension byte
m06f8:		POP	HL		; restore values
		POP	AF
		AND	$7F		; mask bit 7
m06fc:		CALL	m2b64		; page in normal memory
		RST	28H
		DW	$0010		; output char
m0702:		INC	HL
		DJNZ	m06d8		; loop back for more extension
		PUSH	HL
		LD	HL,FLAGS3
		BIT	6,(HL)		; test if want expanded catalog
		POP	HL
		JR	Z,m073e		; if not, move on
m070e:		LD	A,(RAMERR)	; get attributes
		PUSH	HL
		LD	HL,m0812	; blank message
		BIT	3,A
		JR	Z,m071c
		LD	HL,m0818	; if bit 3 set, PROT message
m071c:		PUSH	AF
		CALL	m07e2		; output message
		POP	AF
		LD	HL,m0813	; blank message
		BIT	2,A
		JR	Z,m072b
		LD	HL,m081e	; if bit 2 set, SYS message
m072b:		PUSH	AF
		CALL	m07e2		; output message
		POP	AF
		LD	HL,m0813	; blank message
		BIT	1,A
		JR	Z,m073a
		LD	HL,m0823	; if bit 1 set, ARC message
m073a:		CALL	m07e2		; output message
		POP	HL
m073e:		LD	A," "
		RST	28H
		DW	$0010		; output space
m0743:		PUSH	HL
		CALL	m2b89		; page in DOS workspace
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		INC	HL		; HA=filesize in K
		CALL	m2b64		; page in normal memory
		LD	L,A
		LD	E," "
		CALL	m07f1
		POP	HL
		INC	HL
		INC	HL		; move to next file entry
		LD	A,"K"
		RST	28H
		DW	$0010		; output "K"
m075c:		CALL	m07eb		; output CR
		CALL	m2b89		; page in DOS workspace
		POP	AF
		DEC	A
		JP	NZ,m06b8	; move back for more files in buffer
		POP	BC
		LD	A,B
		SUB	$3F
		JR	C,m077b		; if not, move on
		LD	HL,$F044
		LD	DE,tmp_buff
		LD	BC,11
		LDIR			; if so, copy last entry to first
		EX	DE,HL
		LD	(HL),D
		JP	m067f		; and back for more

m077b:		CALL	m2b64		; page in normal memory

m077e:		CALL	m07eb		; output CR
		CALL	m2b89		; page in DOS workspace
m0784:		LD	A,(DEFADD)	; get drive letter
		OR	A
		JR	NZ,m079a	; move on if not default
		LD	A,$FF
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_DRIVE	; get default drive
		CALL	m32ee		; restore TSTACK
		JP	NC,m06ab	; go if error
m079a:		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_FREE_SPACE	; get free space on drive
		CALL	m32ee		; restore TSTACK
		JP	NC,m06ab	; go if error
		CALL	m2b64		; page in normal memory
		LD	E,$FF
		CALL	m07f1
		LD	HL,m07c9
		CALL	m07e2		; output "K free" message
		CALL	m07eb		; output CR
		RET			; done

m07ba:		LD	A,($ED1C)
		AND	A
		CALL	m2b64		; page in normal memory
		LD	HL,m07d1
		CALL	Z,m07e2
		JP	m077e
		DB	0,0,0
m07cf:		CALL	m2b64
		RST	10H
m07d3:		CALL	m2b89
		RET
m07d7:		CALL	m2b64
		CALL	m07e2
		JR	m07d3
m07df:		CALL	m2b64
		CALL	m07f1
		JR	m07d3

; Subroutine to output a null-terminated message

m07e2:		LD	A,(HL)		; get next char
		OR	A
		RET	Z		; exit if null
		RST	28H
		DW	$0010		; output char
m07e8:		INC	HL
		JR	m07e2		; loop back

; Subroutine to output a CR char

m07eb:		LD	A,$0D
		RST	28H
		DW	$0010		; output CR
m07f0:		RET


; Subroutine to output a number up to 65535 (in HL)

m07f1:		PUSH	HL
		LD	BC,$D8F0	; -10000
		RST	28H
		DW	o192A		; output 10000s
		LD	BC,$FC18	; -1000
		RST	28H
		DW	o192A		; output 1000s
		JR	m0801

; Subroutine to output a number up to 999 (in HL)

m0800:		PUSH	HL
m0801:		LD	BC,$FF9C	; -100
		RST	28H
		DW	o192A		; output 100s
		LD	C,$F6		; -10
		RST	28H
		DW	o192A		; output 10s
		LD	A,L		; units
		RST	28H
		DW	o15EF		; output units
		POP	HL		; restore number
		RET

; Catalog attribute messages

		DB	0
m0812:
m0813:		DM	"    ", 0
		DB	0
m0818:		DM	" PRT", 0
m081e:		DM	" SYS", 0
m0823:		DM	" ARC", 0

; Subroutine to save a block to tape

m0828:		LD	HL,m0830
		PUSH	HL		; stack SA-RET routine address (why??)
		RST	28H
		DW	$04C6		; save bytes
		RET
m0830:		RST	28H
		DW	o053F		; SA-RET
		RET

; Subroutine to LOAD/VERIFY a block of data, from tape or disk
; On entry, IX=start, DE=length, A=type (usually $FF), carry set for LOAD
; or reset for VERIFY
; File 0 will be open for disk operations, which should be closed before exit
; On exit, carry is set if okay, reset if error

m0834:		PUSH	AF
		LD	A,(RAMERR)
		CP	"T"
		JP	Z,m0883		; move on for tape operations
		POP	AF
		JR	NC,m087a	; go to exit for disk verify (won't get here)
		PUSH	HL		; save registers
		PUSH	DE
		PUSH	BC
		LD	B,$00		; file 0
		LD	C,$00		; page 0
		PUSH	IX
		POP	HL		; HL=address
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_READ	; read the block
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		JR	C,m0865		; move on to exit if okay
		CP	$19
		JR	NZ,m087f	; move on if error not end-of-file
		CALL	m0e9a		; cause error
		DB	$31
m0865:		LD	B,$00
		CALL	m2b89		; page in DOS workspace
m086a:		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		JR	NC,m087f	; move on if error
m087a:		SCF
		POP	BC		; restore registers
		POP	DE
		POP	HL
		RET
m087f:		CALL	m0e9a		; cause DOS error
		DB	$FF
m0883:		POP	AF		; if tape,restore flags and enter next routine

; Subroutine to call LD-BYTES subroutine in ROM 3

m0884:		RST	28H
		DW	o0556		; call it
		RET

; The SAVE/LOAD/VERIFY/MERGE commands

; section 1 - initialisation

m0888:		POP	AF		; discard return address of scan-loop
		LD	A,(T_ADDR)
		SUB	m0f83 and $FF	; store command code (0=SAVE,1=LOAD,
		LD	(T_ADDR),A	; 2=VERIFY,3=MERGE)
		CALL	m1129		; get a string expression
		BIT	7,(IY+$01)
		JP	Z,m09ba		; move on if syntax-checking
		LD	BC,$0011	; 17 bytes required for LOAD
		LD	A,(T_ADDR)
		AND	A
		JR	Z,m08a6
		LD	C,$22		; but 34 for others
m08a6:		RST	28H
		DW	$0030		; make space
		PUSH	DE
		POP	IX		; IX points to space
		LD	B,$0B
		LD	A," "
m08b0:		LD	(DE),A		; fill 11-byte name with spaces
		INC	DE
		DJNZ	m08b0
		LD	(IX+$01),$FF	; place terminator in 2nd byte
		RST	28H
		DW	o2BF1		; get string value from stack
		PUSH	DE
		PUSH	BC

; section 2 - booting a disk

		LD	A,C
		DEC	A
		OR	B		; check length
		JR	NZ,m08e4	; move on if not 1
		LD	A,(DE)
		CP	"*"
		JR	NZ,m08e4	; or if not "*"
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_BOOT	; boot a disk
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		CP	$23
		JR	NZ,m08e0	; if error isn't "disk not bootable", move on
		CALL	m0e9a		; cause error
		DB	$3B
m08e0:		CALL	m0e9a		; cause DOS error
		DB	$FF

; section 3 - setting drive for operation in RAMERR

m08e4:		INC	DE
		LD	A,(DE)
		DEC	DE
		CP	":"
		JR	NZ,m08fe	; move on if no drive specified
		LD	A,(DE)
		AND	$DF		; get capitalised drive letter
		CP	"T"		; check for valid drives
		JR	Z,m090f		; moving on if found
		CP	"A"		; check for valid drives
		JR	C,m08fe		; moving on if found
		CP	"Q"		; check for valid drives
		JR	C,m090f		; moving on if found
m08fe:		LD	A,(T_ADDR)
		OR	A
		LD	A,(SAVDRV)	; use SAVDRV as drive for SAVE
		JR	Z,m090a
		LD	A,(LODDRV)	; or LODDRV otherwise
m090a:		LD	(RAMERR),A	; store drive in RAMERR
		JR	m096c		; move on

; section 4 - changing default drives for LOAD "A:" etc

m090f:		LD	(RAMERR),A
		LD	A,C
		DEC	A
		DEC	A
		OR	B		; check string length
		JR	NZ,m0969	; move on if not 2
		LD	A,(RAMERR)

m0942:		CP	"T"
		JR	Z,m095d		; move on for T:
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_DRIVE	; set default drive
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		JR	C,m095d		; move on if no error
		CALL	m0e9a
		DB	$FF		; cause DOS error

m095d:		LD	A,(T_ADDR)
		OR	A
		LD	A,(RAMERR)
		JR	Z,m0945
		LD	(LODDRV),A
		JR	m0948
m0945:		LD	(SAVDRV),A
m0948:		LD	D,A
		LD	E,$10
		RST	18H
		CP	$B5
		JR	NZ,m0966
		RST	20H
		EX	DE,HL
		XOR	A
		LD	B,A
		LD	C,A
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6
		CALL	m3f00
		DW	$00D6
		CALL	m32ee
		CALL	m2b64
m0966:		POP	BC
		POP	DE
		RET
m0969:		LD	A,(RAMERR)
		CP	$54
		JR	NZ,m096c
m0970:		POP	BC
		POP	DE
		INC	DE
		INC	DE
		DEC	BC
		DEC	BC
		JR	m099a

; section 5 - copying filename to page 7 (disk operations only)

m096c:		CP	"T"
		JR	Z,m0998		; move on for tape operations
		LD	A,(T_ADDR)
		CP	$02
		JR	NZ,m097a	; move on if not VERIFY
		POP	HL		; for VERIFY on disk, just exit
		POP	HL
		RET
m097a:		LD	A,B
		OR	C		; test length of string
		JR	NZ,m0982
		CALL	m0e9a
		DB	$0E		; invalid filename error if zero
m0982:		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy filename to page 7
		POP	BC
		LD	BC,$000A
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
		CALL	m2b64		; page in normal memory
		JR	m0999		; move on with disk operations

; section 6 - copying filename into 1st header
; *BUG* If filename was specified as "T:name", the "T:" is never stripped

m0998:		POP	BC		; restore length & add of filename
m0999:		POP	DE
m099a:		LD	HL,$FFF6
		DEC	BC
		ADD	HL,BC
		INC	BC
		JR	NC,m09b3	; move on if filename 10 letters or less
		LD	A,(T_ADDR)
		AND	A
		JR	NZ,m09ac
		CALL	m0e9a
		DB	$0E		; bad filename error if SAVEing
m09ac:		LD	A,B
		OR	C
		JR	Z,m09ba		; move on if no filename
		LD	BC,$000A	; copy 10 chars
m09b3:		PUSH	IX
		POP	HL
		INC	HL
		EX	DE,HL
		LDIR			; copy filename to header+1 in workspace

; At this point, syntax-checking rejoins the routines
; Each of the following sections fills in the remaining header information
; for their filetype & ensures syntax is correct to end-of-statement.
; At run-time the sections recombine at section 11, with HL=address
; to load/verify/merge/save at

; section 7 - DATA operations
m09ba:		RST	18H
		CP	$E4
		JR	NZ,m0a11	; move on if not DATA
		LD	A,(T_ADDR)
		CP	$03
		JP	Z,m1125		; error if used with MERGE
		RST	20H

m09cc:		RST	28H
		DW	o28B2		; search for variable
		SET	7,C		; set bit 7 of array's name
		JR	NC,m09e0	; jump if handling existing array
		LD	HL,$0000
		LD	A,(T_ADDR)
		DEC	A
		JR	Z,m09f4
		CALL	m0e9a
		DB	$01		; error 2 if trying to SAVE/VERIFY new array
m09e0:		JP	NZ,m1125	; error if just a numeric variable
		BIT	7,(IY+$01)
		JR	Z,m0a01		; move on if checking syntax
		INC	HL
		LD	A,(HL)
		LD	(IX+$0B),A	; copy array length into workspace header
		INC	HL
		LD	A,(HL)
		LD	(IX+$0C),A
		INC	HL
m09f4:		LD	(IX+$0E),C	; copy array name into workspace header
		LD	A,$01		; type 1
		BIT	6,C
		JR	Z,m09fe		; move on if numeric array
		INC	A		; else type 2
m09fe:		LD	(IX+$00),A	; copy type into workspace header
m0a01:		EX	DE,HL
		RST	20H
		CP	")"
		JR	NZ,m09e0	; error if not ")"
		RST	20H
		CALL	m10b1		; check for end-of-statement
m0a0d:		EX	DE,HL
		JP	m0ad5		; jump on

; section 8 - SCREEN$ operations

m0a11:		CP	$AA		; check for SCREEN$
		JR	NZ,m0a36	; move on if not
		LD	A,(T_ADDR)
		CP	$03
		JP	Z,m1125		; error if trying to MERGE
		RST	20H
		CALL	m10b1		; check for end-of-statement
		LD	(IX+$0B),$00	; store screen length
		LD	(IX+$0C),$1B
		LD	HL,$4000
		LD	(IX+$0D),L	; and start
		LD	(IX+$0E),H
		JR	m0a89		; jump on

; section 9 - CODE operations

m0a36:		CP	$AF		; check for CODE
		JR	NZ,m0a8f	; move on if not
		LD	A,(T_ADDR)
		CP	$03
		JP	Z,m1125		; error if trying to MERGE
		RST	20H

m0a45:		CALL	m0e94
		JR	NZ,m0a56	; move on if not end-of-statement
		LD	A,(T_ADDR)
		AND	A
		JP	Z,m1125		; error if trying to SAVE with no parameters
		RST	28H
		DW	o1CE6		; get zero to calculator stack
		JR	m0a67		; move on
m0a56:		CALL	m1121		; get numeric expression
		RST	18H
		CP	","
		JR	Z,m0a6c		; move on if comma
		LD	A,(T_ADDR)
		AND	A
		JP	Z,m1125		; error if trying to SAVE with 1 parameter
m0a67:		RST	28H
		DW	o1CE6		; get zero to calculator stack
		JR	m0a72		; move on
m0a6c:		RST	28H
		DW	$0020		; get next char
m0a6f:		CALL	m1121		; get numeric expression
m0a72:		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o1E99		; get length to BC
		LD	(IX+$0B),C	; store in workspace header
		LD	(IX+$0C),B
		RST	28H
		DW	o1E99		; get address to BC
		LD	(IX+$0D),C	; store in workspace header
		LD	(IX+$0E),B
		LD	H,B		; HL=address
		LD	L,C
m0a89:		LD	(IX+$00),$03	; type 3 to workspace header
		JR	m0ad5		; move on

; section 10 - BASIC operations

m0a8f:		CP	$CA		; check for LINE
		JR	Z,m0a9c		; move on if present
		JP	n24c7		; check for end-of-statement

m0a96:		LD	(IX+$0E),$80	; no auto-run
		JR	m0ab5		; move on
m0a9c:		LD	A,(T_ADDR)
		AND	A
		JP	NZ,m1125	; error unless SAVE with LINE
		RST	28H
		DW	$0020		; get next char
m0aa6:		CALL	m1121		; get numeric expression
		CALL	m10b1		; check for end-of-line
		RST	28H
		DW	o1E99		; get line to BC
		LD	(IX+$0D),C	; store in workspace header
		LD	(IX+$0E),B
m0ab5:		LD	(IX+$00),$00	; type 0
		LD	HL,(E_LINE)
		LD	DE,(PROG)
		SCF
		SBC	HL,DE		; HL=program+vars length
		LD	(IX+$0B),L	; store in workspace header
		LD	(IX+$0C),H
		LD	HL,(VARS)
		SBC	HL,DE		; HL=program only length
		LD	(IX+$0F),L	; store in workspace header
		LD	(IX+$10),H
		EX	DE,HL

; section 11 - LOAD/VERIFY/MERGE tape operations

m0ad5:		LD	A,(T_ADDR)
		AND	A
		JP	Z,m0d6e		; move on if saving
		PUSH	HL
		LD	BC,$0011
		ADD	IX,BC		; IX points to 2nd header
		LD	A,(RAMERR)
		CP	"T"
		JR	NZ,m0b41	; move on if disk operation
m0ae9:		PUSH	IX
		LD	DE,$0011
		XOR	A
		SCF
		CALL	m0884		; load header from tape to 2nd header area
		POP	IX
		JR	NC,m0ae9	; loop back if error
		LD	A,$FE
		RST	28H
		DW	o1601		; open channel to stream -2
		LD	(IY+$52),$03	; set scroll count
		LD	C,$80		; signal "names don't match"
		LD	A,(IX+$00)
		CP	(IX-$11)	; compare types
		JR	NZ,m0b0c	; jump if no match
		LD	C,$F6		; C must be incremented 10 times to match
m0b0c:		CP	$04
		JR	NC,m0ae9	; error for types 4+
		LD	DE,o09C0	; address of message block in ROM 3
		PUSH	BC
		RST	28H
		DW	o0C0A		; print filetype message
		POP	BC
		PUSH	IX
		POP	DE		; DE points to filename to check for
		LD	HL,$FFF0
		ADD	HL,DE		; HL points to loaded filename
		LD	B,$0A		; check 10 chars
		LD	A,(HL)		; get next char
		INC	A
		JR	NZ,m0b28	; move on if name to check not null
		LD	A,C		; if null, signal "10 chars match"
		ADD	A,B
		LD	C,A
m0b28:		INC	DE
		LD	A,(DE)
		CP	(HL)		; compare names
		INC	HL
		JR	NZ,m0b2f
		INC	C		; increment C if chars match
m0b2f:		RST	28H
		DW	$0010		; output char
		DJNZ	m0b28		; loop back
		BIT	7,C
		JR	NZ,m0ae9	; loop back if no match
		LD	A,$0D
		RST	28H
		DW	$0010		; output CR
		POP	HL
		JP	m0ba6		; move on

; section 12 - LOAD/MERGE disk operations

m0b41:		LD	A,(T_ADDR)
		CP	$02
		JR	Z,m0ba6		; move on if VERIFY (can't be here if so!)
		PUSH	IX
		LD	B,$00		; file 0
		LD	C,$01		; exclusive-read
		LD	D,$00
		LD	E,$01
		LD	HL,tmp_fspec
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; open file
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		JR	C,m0b6c
		CALL	m0e9a		; cause any DOS error
		DB	$FF
m0b6c:		LD	B,$00
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_REF_HEAD	; IX to header
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		EX	(SP),IX
		POP	HL
		CALL	m2b89		; page in DOS workspace
		LD	A,(HL)
		CALL	m2b64		; page in normal memory
		CP	(IX-$11)	; compare types
		JR	Z,m0b92
		CALL	m0e9a
		DB	$1D		; error if different
m0b92:		LD	(IX+$00),A	; store in 2nd header area
		PUSH	IX
		POP	DE
		EX	DE,HL
		LD	BC,$000B
		ADD	HL,BC
		EX	DE,HL
		INC	HL
		LD	BC,$0006
		CALL	m3f8a		; copy parameters from page 7 into 2nd header
		POP	HL

; section 13 - Perform tape/disk VERIFY (any type) or LOAD (CODE only)

m0ba6:		LD	A,(IX+$00)
		CP	$03
		JR	Z,m0bb9		; move on for type 3
		LD	A,(T_ADDR)
		DEC	A
		JP	Z,m0c04		; move on if LOADing
		CP	$02
		JP	Z,m0cb2		; move on if MERGEing
m0bb9:		PUSH	HL		; save address to LOAD/VERIFY at
		LD	L,(IX-$06)
		LD	H,(IX-$05)	; HL=length from 1st header
		LD	E,(IX+$0B)
		LD	D,(IX+$0C)	; DE=length from 2nd header
		LD	A,H
		OR	L
		JR	Z,m0bd7		; move on if length not specified (CODE only)
		SBC	HL,DE
		JR	C,m0c00		; error if block larger than requested
		JR	Z,m0bd7
		LD	A,(IX+$00)
		CP	$03
		JR	NZ,m0bfc	; error for uneven lengths except in CODE
m0bd7:		POP	HL		; restore address
		LD	A,H
		OR	L
		JR	NZ,m0be2
		LD	L,(IX+$0D)
		LD	H,(IX+$0E)	; if zero, use start address of 2nd header
m0be2:		PUSH	HL
		POP	IX		; IX=address to load
		LD	A,(T_ADDR)
		CP	$02
		SCF			; set carry for LOAD
		JR	NZ,m0bf6
		AND	A		; reset carry for VERIFY
		LD	A,(RAMERR)
		CP	"T"
		JR	Z,m0bf6
		RET			; exit if VERIFY with disk (won't get here!)
m0bf6:		LD	A,$FF		; data block
m0bf8:		CALL	m0834		; load/verify block from tape/disk
		RET	C		; exit if okay
m0bfc:		CALL	m0e9a		; error R - tape loading error
		DB	$1A
m0c00:		CALL	m0e9a		; error ???
		DB	$4F

; section 14 - Perform tape/disk LOAD (types 0-2)

m0c04:		LD	E,(IX+$0B)
		LD	D,(IX+$0C)	; DE=length from 2nd header
m0c0a:		PUSH	HL
		LD	A,H
		OR	L		; test start=0 (previously undeclared array)
		JR	NZ,m0c15	; move on if not
		INC	DE
		INC	DE
		INC	DE		; add 3 bytes for name (1) & length (2)
		EX	DE,HL
		JR	m0c21		; move on
m0c15:		LD	L,(IX-$06)
		LD	H,(IX-$05)	; HL=size of existing prog+vars or array
		EX	DE,HL
		SCF
		SBC	HL,DE
		JR	C,m0c2a		; move on if no extra space required
m0c21:		LD	DE,$0005	; allow for 5-byte overhead
		ADD	HL,DE
		LD	B,H
		LD	C,L
		RST	28H
		DW	o1F05		; test if space available
m0c2a:		POP	HL		; restore destination address
		LD	A,(IX+$00)
		AND	A
		JR	Z,m0c6f		; move on for BASIC programs
		LD	A,H
		OR	L
		JR	Z,m0c48		; move on if loading new array
		DEC	HL
		LD	B,(HL)
		DEC	HL
		LD	C,(HL)		; get existing array length from vars area
		DEC	HL
		INC	BC
		INC	BC
		INC	BC		; add 3 for name & length
		LD	(X_PTR),IX	; save IX
		RST	28H
		DW	o19E8		; reclaim space
		LD	IX,(X_PTR)	; restore IX
m0c48:		LD	HL,(E_LINE)
		DEC	HL		; HL points to $80 at end of vars
		LD	C,(IX+$0B)
		LD	B,(IX+$0C)	; get length of new array
		PUSH	BC		; save
		INC	BC
		INC	BC
		INC	BC		; add 3 for name & length
		LD	A,(IX-$03)
		PUSH	AF		; save array name (from old header)
		RST	28H
		DW	o1655		; make the room
		DB	$23
		POP	AF
		LD	(HL),A		; store name
		POP	DE
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D		; and length
		INC	HL
		PUSH	HL
		POP	IX		; IX points to array start
		SCF			; set carry for LOAD
		LD	A,$FF		; data block
		JP	m0bf8		; go to load it
m0c6f:		EX	DE,HL		; save DE=destination
		LD	HL,(E_LINE)
		DEC	HL		; end of vars
		LD	(X_PTR),IX	; save IX
		LD	C,(IX+$0B)
		LD	B,(IX+$0C)	; get length of new data block
		PUSH	BC
		RST	28H
		DW	o19E5		; reclaim entire prog+vars
		POP	BC
		PUSH	HL
		PUSH	BC
		RST	28H
		DW	o1655		; make room for new block
		LD	IX,(X_PTR)	; restore IX
		INC	HL
		LD	C,(IX+$0F)
		LD	B,(IX+$10)	; BC=length of program only
		ADD	HL,BC
		LD	(VARS),HL	; set new start of vars
		LD	H,(IX+$0E)
		LD	A,H
		AND	$C0
		JR	NZ,m0ca9	; move on if no auto-run number
		LD	L,(IX+$0D)
		LD	(NEWPPC),HL	; set new line & statement
		LD	(IY+$0A),$00
m0ca9:		POP	DE		; restore length & start
		POP	IX
		SCF			; set carry for LOAD
		LD	A,$FF		; data block
		JP	m0bf8		; go to load it

; section 15 - Perform tape/disk MERGE

m0cb2:		LD	C,(IX+$0B)
		LD	B,(IX+$0C)	; fetch length of new block
		PUSH	BC
		INC	BC
		RST	28H
		DW	$0030		; make length+1 bytes in workspace
m0cbd:		LD	(HL),$80	; terminate with an end-marker
		EX	DE,HL		; HL=start
		POP	DE		; DE=length
		PUSH	HL		; save start
		PUSH	HL
		POP	IX		; IX=start
		SCF			; set carry for LOAD
		LD	A,$FF		; data block
		CALL	m0bf8		; load the block
		POP	HL		; HL=start of new prog
		LD	DE,(PROG)	; DE=start of old prog
m0cd0:		LD	A,(HL)
		AND	$C0
		JR	NZ,m0cee	; move on if all lines done
m0cd5:		LD	A,(DE)
		INC	DE
		CP	(HL)		; compare high bytes of line number
		INC	HL
		JR	NZ,m0cdd	; skip next test if no match
		LD	A,(DE)
		CP	(HL)		; compare low bytes of line number
m0cdd:		DEC	DE
		DEC	HL
		JR	NC,m0ce9	; move on if can place line here
		PUSH	HL
		EX	DE,HL
		RST	28H
		DW	o19B8		; get address of next line in old prog
		POP	HL
		JR	m0cd5		; loop back
m0ce9:		CALL	m0d2a		; enter the new line
		JR	m0cd0		; loop back
m0cee:		LD	A,(HL)		; get var name from workspace
		LD	C,A
		CP	$80
		RET	Z		; exit if all done
		PUSH	HL
		LD	HL,(VARS)	; fetch start of vars
m0cf7:		LD	A,(HL)
		CP	$80
		JR	Z,m0d21		; move on if reached end
		CP	C
		JR	Z,m0d07		; move on if found match
m0cff:		PUSH	BC
		RST	28H
		DW	o19B8		; get to next var
		POP	BC
		EX	DE,HL
		JR	m0cf7		; loop back
m0d07:		AND	$E0
		CP	$A0
		JR	NZ,m0d1f	; move on if not long-named var
		POP	DE
		PUSH	DE
		PUSH	HL
m0d10:		INC	HL
		INC	DE
		LD	A,(DE)
		CP	(HL)		; compare long names
		JR	NZ,m0d1c	; move on if mismatch
		RLA
		JR	NC,m0d10	; loop back
		POP	HL
		JR	m0d1f
m0d1c:		POP	HL
		JR	m0cff		; go back if unsuccessful
m0d1f:		LD	A,$FF		; signal "replace variable"
m0d21:		POP	DE
		EX	DE,HL
		INC	A
		SCF			; signal "variables"
		CALL	m0d2a		; merge in the variable
		JR	m0cee		; loop back

; Subroutine to merge a line or variable (part of section 15)

m0d2a:		JR	NZ,m0d3c	; move on if not replacing a line/variable
		EX	AF,AF'		; save flags
		LD	(X_PTR),HL	; save pointer in new program/vars
		EX	DE,HL
		RST	28H
		DW	o19B8
		RST	28H
		DW	o19E8		; reclaim old line/var
		EX	DE,HL
		LD	HL,(X_PTR)	; restore
		EX	AF,AF'
m0d3c:		EX	AF,AF'		; save flags
		PUSH	DE
		RST	28H
		DW	o19B8		; find length of new line/var
		LD	(X_PTR),HL	; save pointer in new program/vars
		LD	HL,(PROG)
		EX	(SP),HL		; save PROG to avoid corruption
		PUSH	BC
		EX	AF,AF'
		JR	C,m0d53		; move on if adding a variable
		DEC	HL
		RST	28H
		DW	o1655		; make room for new line
		INC	HL
		JR	m0d56
m0d53:		RST	28H
		DW	o1655		; make room for new var
m0d56:		INC	HL		; point to first new location
		POP	BC
		POP	DE
		LD	(PROG),DE	; restore PROG
		LD	DE,(X_PTR)	; retrieve new pointer
		PUSH	BC
		PUSH	DE
		EX	DE,HL
		LDIR			; copy new var/line into space made
		POP	HL
		POP	BC
		PUSH	DE
		RST	28H
		DW	o19E8		; reclaim workspace holding new var/line
		POP	DE
		RET

; section 16 - Perform disk SAVE

m0d6e:		LD	A,(RAMERR)
		CP	"T"
		JP	Z,m0e10		; move on for tape operations
		CALL	m2b89		; page in DOS workspace
		PUSH	HL
		LD	B,$00		; file 0
		LD	C,$03		; exclusive read-write
		LD	D,$01
		LD	E,$03
		LD	HL,tmp_fspec
		PUSH	IX
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; create the file
		CALL	m32ee		; restore TSTACK
		JR	C,m0d9b		; move on unless error
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause DOS error
		DB	$FF
m0d9b:		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_REF_HEAD	; get IX=header data
		CALL	m32ee		; restore TSTACK
		JR	C,m0db1
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause DOS error
		DB	$FF
m0db1:		EX	(SP),IX		; IX=pointer to header in normal memory
		POP	HL		; HL=pointer to header in page 7
		CALL	m2b64		; page in normal memory
		LD	A,(IX+$00)
		CALL	m2b89		; page in DOS workspace
		LD	(HL),A		; transfer type
		INC	HL
		PUSH	IX
		POP	DE
		EX	DE,HL		; DE=DOS header address+1
		LD	BC,$000B
		ADD	HL,BC		; HL=page 0 header parameters
		LD	BC,$0006
		CALL	m2b64		; page in normal memory
		CALL	m3f63		; copy parameters to DOS header
		LD	B,$00		; file 0
		LD	C,$00
		LD	E,(IX+$0B)
		LD	D,(IX+$0C)	; DE=length
		LD	A,D
		OR	E
		CALL	m2b89		; page in DOS workspace
		JR	Z,m0df6		; move on if zero length
		POP	HL		; restore start address
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_WRITE	; write block to file
		CALL	m32ee		; restore TSTACK
		JR	C,m0df6
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause any DOS error
		DB	$FF
m0df6:		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file
		CALL	m32ee		; restore TSTACK
		JR	C,m0e0c
		CALL	m2b64
		CALL	m0e9a		; cause any DOS error
		DB	$FF
m0e0c:		CALL	m2b64		; page in normal memory
		RET			; done

; section 17 - Perform tape SAVE

m0e10:		PUSH	HL
		LD	A,$FD
		RST	28H
		DW	o1601		; open channel to stream -3
		XOR	A
		LD	DE,o09A1
		RST	28H
		DW	o0C0A		; output ROM 3's start tape message
		SET	5,(IY+$02)	; signal "screen needs clearing"
		RST	28H
		DW	o15D4		; wait for a key
		PUSH	IX		; save header address
		LD	DE,$0011
		XOR	A		; header block
		CALL	m0828		; save header
		POP	IX
		LD	B,$32
m0e31:		HALT
		DJNZ	m0e31
		LD	E,(IX+$0B)
		LD	D,(IX+$0C)	; DE=length
		LD	A,$FF		; data block
		POP	IX		; IX=start
		JP	m0828		; save & exit

; Looks like these bits aren't used
msg18:		DB	$16,0,0,$10,0,$11,$07,$13,0
		DM	"Insert tape and press PLAY", 13
		DM	"To cancel - press BREAK twice", $FF
		DB	0,0,0,0,0,0,0,0
		DB	0,0,0,0,0,0,0,0,0

; Subroutine to check if char in A is a statement terminator

m0e94:		CP	$0D
		RET	Z
		CP	":"
		RET

; Subroutine to cause a +3DOS error
; Routine will attempt to close file 0 before causing error
; On entry, A=+3DOS error code and byte following call is $FF
; or, byte following call is +3 BASIC error

m0e9a:		PUSH	AF		; save error code
m0e9b:		LD	A,(RAMERR)
		CP	$54
		JR	Z,m0eca		; move on if SAVE/LOAD was using drive T:
		LD	B,$00
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file 0
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		JR	C,m0eca		; move on if no error
		LD	B,$00
		CALL	m2b89		; page in DOS workspace
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_ABANDON	; else abandon file 0
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
m0eca:		POP	AF		; restore error code
m0ecb:		POP	HL		; get return address
		LD	E,(HL)		; get inline code
		BIT	7,E
		JR	Z,m0edc		; use it as error code if not $FF
		CP	$0A		; convert DOS error code to +3 BASIC error
		JR	NC,m0ed9
m0ed5:		ADD	A,$3D
		JR	m0edb
m0ed9:		ADD	A,$18
m0edb:		LD	E,A
m0edc:		LD	H,E		; place code CALL m2ada, DEFB error
		LD	L,m2ada/$100	; on stack
		PUSH	HL
		LD	L,$CD
		LD	H,m2ada and $FF
		PUSH	HL
		XOR	A
		LD	HL,$0000
		ADD	HL,SP
		JP	(HL)		; jump to execute code on stack & cause error

; The parameter offset table
; This contains offsets from each entry's position into the following
; parameter table

m0eeb:		DB	m0f9c-$		; DEF FN
		DB	m0fb6-$		; CAT
		DB	m0fa9-$		; FORMAT
		DB	m0fac-$		; MOVE
		DB	m0fb2-$		; ERASE
		DB	m0f9f-$		; OPEN#
		DB	m0fa5-$		; CLOSE#
		DB	m0f85-$		; MERGE
		DB	m0f84-$		; VERIFY
		DB	m0f86-$		; BEEP
		DB	m0f8a-$		; CIRCLE
		DB	m0f8e-$		; INK
		DB	m0f8f-$		; PAPER
		DB	m0f90-$		; FLASH
		DB	m0f91-$		; BRIGHT
		DB	m0f92-$		; INVERSE
		DB	m0f93-$		; OVER
		DB	m0f94-$		; OUT
		DB	m0f7c-$		; LPRINT
		DB	m0f7f-$		; LLIST
		DB	m0f2d-$		; STOP
		DB	m0f6c-$		; READ
		DB	m0f6f-$		; DATA
		DB	m0f72-$		; RESTORE
		DB	m0f4b-$		; NEW
		DB	m0f98-$		; BORDER
		DB	m0f5b-$		; CONTINUE
		DB	m0f45-$		; DIM
		DB	m0f48-$		; REM
		DB	m0f33-$		; FOR
		DB	m0f20-$		; GOTO
		DB	m0f29-$		; GOSUB
		DB	m0f42-$		; INPUT
		DB	m0f83-$		; LOAD
		DB	m0f51-$		; LIST
		DB	m0f1d-$		; LET
		DB	m0f68-$		; PAUSE
		DB	m0f3b-$		; NEXT
		DB	m0f54-$		; POKE
		DB	m0f3f-$		; PRINT
		DB	m0f64-$		; PLOT
		DB	m0f4e-$		; RUN
		DB	m0f82-$		; SAVE
		DB	m0f58-$		; RANDOMIZE
		DB	m0f24-$		; IF
		DB	m0f61-$		; CLS
		DB	m0f75-$		; DRAW
		DB	m0f5e-$		; CLEAR
		DB	m0f30-$		; RETURN
		DB	m0f79-$		; COPY

; The parameter table for the BASIC commands
; These comprise command classes ($00-$0E), separators and
; where appropriate, command addresses
; The classes are:
;       00      No further operands
;       01      Used in LET: a variable is required
;       02      Used in LET: an expression (numeric or string) must follow
;       03      A numeric expression may follow (default=0)
;       04      A single character variable must follow
;       05      A set of items may be given
;       06      A numeric expression must follow
;       07      Handles colour items
;       08      Two numeric expressions separated by a comma must follow
;       09      As 08, but colour items may precede the expressions
;       0a      A string expression must follow
;       0b      Handles cassette/disk routines
;       0c      As 00, but handled in ROM 1 not ROM 3
;       0d      As 03, but handled in ROM 1 not ROM 3
;       0e      As 05, but handled in ROM 1 not ROM 3

m0f1d:		DB	$01,"=",$02	; LET
m0f20:		DB	$0E
		DW	$35E1		; GOTO
		DB	$00
m0f24:		DB	$06,$CB,$0E
		DW	m115e		; IF
m0f29:		DB	$06,$0C
		DW	m124a		; GOSUB
m0f2d:		DB	$00
		DW	o1CEE		; STOP
m0f30:		DB	$0E
		DW	m2b09
m0f33:		DB	$04,"=",$06
		DB	$CC,$06,$0E
		DW	m1178		; FOR
m0f3b:		DB	$0E
		DW	$1506		; NEXT
		DB	$00
m0f3f:		DB	$0E
		DW	m217b		; PRINT
m0f42:		DB	$0E
		DW	m218f		; INPUT
m0f45:		DB	$0E
		DW	m14e2		; DIM
m0f48:		DB	$0E
		DW	m1072		; REM
m0f4b:		DB	$0E
		DW	$3877		; NEW
m0f4e:		DB	$0D
		DW	m11f9		; RUN
m0f51:		DB	$0E
		DW	m1539		; LIST
m0f54:		DB	$08,$00
		DW	o1E80		; POKE
m0f58:		DB	$03
		DW	o1E4F		; RANDOMIZE
m0f5b:		DB	$00
		DW	o1E5F		; CONTINUE
m0f5e:		DB	$0D
		DW	m1204		; CLEAR
m0f61:		DB	$00
		DW	o0D6B		; CLS
m0f64:		DB	$09,$00
		DW	o22DC		; PLOT
m0f68:		DB	$06,$00
		DW	o1F3A		; PAUSE
m0f6c:		DB	$0E
		DW	m11a2		; READ
m0f6f:		DB	$0E
		DW	m11e2		; DATA
m0f72:		DB	$03
		DW	o1E42		; RESTORE
m0f75:		DB	$09,$0E
		DW	m2296		; DRAW
m0f79:		DB	$0E
		DW	m21aa		; COPY
m0f7c:		DB	$0E
		DW	m2177		; LPRINT
m0f7f:		DB	$0E
		DW	m1535		; LLIST
m0f82:		DB	$0B		; SAVE
m0f83:		DB	$0B		; LOAD
m0f84:		DB	$0B		; VERIFY
m0f85:		DB	$0B		; MERGE
m0f86:		DB	$08,$00
		DW	o03F8		; BEEP
m0f8a:		DB	$09,$0E
		DW	m2286		; CIRCLE
m0f8e:		DB	$07		; INK
m0f8f:		DB	$07		; PAPER
m0f90:		DB	$07		; FLASH
m0f91:		DB	$07		; BRIGHT
m0f92:		DB	$07		; INVERSE
m0f93:		DB	$07		; OVER
m0f94:		DB	$08,$00
		DW	o1E7A		; OUT
m0f98:		DB	$06,$00
		DW	o2294		; BORDER
m0f9c:		DB	$0E
		DW	m1283		; DEF FN
m0f9f:		DB	$06,",",$0A,$0C
		DW	$35C4		; OPEN#
m0fa5:		DB	$06,$0C
		DW	$35D7		; CLOSE#
m0fa9:		DB	$0E
		DW	m026c		; FORMAT
m0fac:		DB	$0A,$0E
		DW	$39B3		; MOVE
		DB	0,0
m0fb2:		DB	$0A,$0C
		DW	m044a		; ERASE
m0fb6:		DB	$0E
		DW	m05b8		; CAT
m0fb9:		DB	$0E
		DW	cmdspec		; SPECTRUM
m0fbc:		DB	$0E
		DW	m23f1		; PLAY

; The main parser entry point
; Enter here for syntax checking

m0fbf:		RES	7,(IY+$01)	; signal "syntax checking"
		RST	28H
		DW	o19FB		; point to the first code after any line no
		XOR	A
		LD	(SUBPPC),A	; initialise SUBPPC to zero statements
		DEC	A
		LD	(ERR_NR),A	; signal "OK" error code
		JR	m0fd1		; jump to start checking

; The statement loop

m0fd0:		RST	20H		; advance CH_ADD
m0fd1:		RST	28H
		DW	o16BF		; clear workspace
		INC	(IY+$0D)	; increment SUBPPC on each statement
		JP	m,m1125		; error if more than 127 statements on line
		RST	18H		; fetch character
		LD	B,$00
		CP	$0D
		JP	Z,m1073		; move on if end-of-line
		CP	":"
		JR	Z,m0fd0		; loop back if end-of-statement
		LD	HL,m1031
		PUSH	HL		; load stack with return address to STMT-RET
		LD	C,A		; save command code
		RST	20H		; advance CH_ADD
		LD	A,C
		SUB	$CE		; put command code in range $00..$31
		JR	NC,m1004	; move on if valid keyword
		ADD	A,$CE		; else reform character
		LD	HL,m0fb9	; address of SPECTRUM parameter entries
		CP	$A3
		JR	Z,m1010		; move on if SPECTRUM command
		LD	HL,m0fbc	; address of PLAY parameter entries
		CP	$A4
		JR	Z,m1010		; move on if PLAY command
		JP	m1125		; else give Nonsense in BASIC
m1004:		LD	C,A
		LD	HL,m0eeb	; syntax offset table start
		ADD	HL,BC
		LD	C,(HL)
		ADD	HL,BC		; get start of entries in parameter table
		JR	m1010		; move on
m100d:		LD	HL,(T_ADDR)	; get pointer into parameter table
m1010:		LD	A,(HL)		; get next parameter type
		INC	HL
		LD	(T_ADDR),HL	; save pointer
		LD	BC,m100d
		PUSH	BC		; stack return address back into this loop
		LD	C,A
		CP	$20
		JR	NC,m102a	; move on if entry is a separator
		LD	HL,m10c5	; base of command class table
		LD	B,$00
		ADD	HL,BC
		LD	C,(HL)		; get offset
		ADD	HL,BC
		PUSH	HL		; stack computed command class routine address
		RST	18H		; get next char to A
		DEC	B		; B=$FF
		RET			; call command class routine
m102a:		RST	18H		; get next char
		CP	C
		JP	NZ,m1125	; nonsense in BASIC if not required separator
		RST	20H		; get next character
		RET			; back into loop at m100d

; The "STMT-RET" routine. A return is made here after correct interpretation
; of a statement

m1031:		CALL	m2af9		; test BREAK key
		JR	C,m103a		; move on if not pressed
		CALL	m2ada
		DB	$14		; error L - BREAK into program
m103a:		BIT	7,(IY+$0A)
		JP	NZ,m10b8	; move on if a jump is not being made
		LD	HL,(NEWPPC)	; get new line number
		BIT	7,H		; check if running line in edit area
		JR	Z,m105c		; move on if not

; Enter here if running a line in the edit area

m1048:		LD	HL,$FFFE
		LD	(PPC),HL	; this is line "-2"
		LD	HL,(WORKSP)
		DEC	HL		; HL points to end of edit area
		LD	DE,(E_LINE)
		DEC	DE		; DE points to location before edit area
		LD	A,(NSPPC)	; fetch number of next statement to handle
		JR	m1092		; move on

; Perform a jump in the program

m105c:		RST	28H
		DW	o196E		; get start address of line to jump to
		LD	A,(NSPPC)	; get statement number
		JR	Z,m1080		; move on if correct line was found
		AND	A		; else check statement number
		JR	NZ,m10ad	; if not zero, N - statement lost error
		LD	B,A
		LD	A,(HL)
		AND	$C0		; check for end of program
		LD	A,B
		JR	Z,m1080		; move on if not
		CALL	m2ada
		DB	$FF		; else end with 0 - OK error

m1072:		POP	BC		; REM command - drop STMT-RET address to
					; ignore rest of command

; The Line-end routine

m1073:		BIT	7,(IY+$01)
		RET	Z		; exit if syntax-checking
		LD	HL,(NXTLIN)	; get address of NXTLIN
		LD	A,$C0
		AND	(HL)
		RET	NZ		; exit if end of program
		XOR	A		; use statement zero

; The line-use routine

m1080:		CP	$01
		ADC	A,$00		; change statement zero to 1
		LD	D,(HL)
		INC	HL
		LD	E,(HL)
		LD	(PPC),DE	; store line number in PPC
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL		; DE holds start of line
		ADD	HL,DE
		INC	HL		; HL holds start of next line
m1092:		LD	(NXTLIN),HL	; store next line address
		EX	DE,HL
		LD	(CH_ADD),HL	; update CH_ADD to current line start
		LD	D,A
		LD	E,$00
		LD	(IY+$0A),$FF	; signal "no jump"
		DEC	D
		LD	(IY+$0D),D	; statement number-1 to SUBPPC
		JP	Z,m0fd0		; enter loop if want first statement
		INC	D
		RST	28H
		DW	o198B		; else find required statement
		JR	Z,m10b8		; move on if found
m10ad:		CALL	m2ada
		DB	$16		; report N - statement lost


; The "Check-end" subroutine. During syntax-checking, it ensures that
; the end of the statement has been reached, generating an error if not.

m10b1:		BIT	7,(IY+$01)	; check bit 7 of FLAGS
		RET	NZ		; return if run-time
		POP	BC		; drop return address of statement routine
		POP	BC		; drop return address of scan-loop routine
m10b8:		RST	18H		; get next character
		CP	$0D
		JR	Z,m1073		; move back if end-of-line
		CP	$3A
		JP	Z,m0fd0		; move back if end-of-statement
		JP	m1125		; else Nonsense in BASIC error


; The command class offset table
; This contains offsets from the entry in the table to the
; actual command class routines following

m10c5:		DB	m10e9-$
		DB	m110c-$
		DB	m1110-$
		DB	m10e6-$
		DB	m1118-$
		DB	m10ea-$
		DB	m1121-$
		DB	m112d-$
		DB	m111d-$
		DB	m1157-$
		DB	m1129-$
		DB	m115b-$
		DB	m10d7-$
		DB	m10d4-$
		DB	m10d8-$

; Class $0C,$0D,$0E routines
; Enter at m10d4 for $0D, m10d7 for $0C and m10d8 for $0E

m10d4:		RST	28H
		DW	o1CDE		; fetch a number (or 0 if none)
m10d7:		CP	A		; set zero flag for classes $0C & $0D
m10d8:		POP	BC		; drop the scan-loop return address
		CALL	Z,m10b1		; for classes $0C,$0D check for statement end
		EX	DE,HL		; save line pointer in DE
		LD	HL,(T_ADDR)	; get address in parameter table
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=command address
		EX	DE,HL		; restore line pointer in HL
		PUSH	BC		; stack command address
		RET			; and "return" to it

; Class $00,$03,$05 routines
; Enter at m10e6 for $03, m10e9 for $00 and m10ea for $05

m10e6:		RST	28H
		DW	o1CDE		; fetch a number (or 0 if none)
m10e9:		CP	A		; set zero flag for classes $00 & $03
m10ea:		POP	BC		; drop the scan-loop return address
		CALL	Z,m10b1		; for classes $00,$03 check for statement end
		EX	DE,HL		; save line pointer in DE
		LD	HL,(T_ADDR)	; get address in parameter table
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=command address in ROM 3
		EX	DE,HL		; restore line pointer
		PUSH	HL		; and stack it
		LD	HL,m110b	; place ROM 1 return address in RETADDR
		LD	(RETADDR),HL
		LD	HL,REGNUOY
		EX	(SP),HL		; place REGNUOY routine as return address
		PUSH	HL
		LD	H,B
		LD	L,C
		EX	(SP),HL		; stack ROM 3 address, restore line pointer
		PUSH	AF		; stack registers
		PUSH	BC
		DI			; disable interrupts
		JP	STOO		; call ROM 3 routine
m110b:		RET

; Class $01 routine

m110c:		RST	28H
		DW	o1C1F		; use ROM 3 to deal with class $01
		RET

; Class $02 routine

m1110:		POP	BC		; drop scan-loop return address
		RST	28H
		DW	o1C56		; fetch a value
		CALL	m10b1		; check for end of statement
		RET

; Class $04 routine

m1118:		RST	28H
		DW	o1C6C		; use ROM 3 to deal with class $04
		RET

; Class $08 routine

m111c:		RST	20H
m111d:		RST	28H		; use ROM 3 to deal with class $08
		DW	o1C7A
		RET

; Class $06 routine

m1121:		RST	28H
		DW	o1C82		; use ROM 3 to deal with class $06
		RET

; Generate C - Nonsense in BASIC error

m1125:		CALL	m2ada
		DB	$0B		; error C

; Class $0A routine

m1129:		RST	28H
		DW	o1C8C		; use ROM 3 to deal with class $0A
		RET

; Class $07 routine

m112d:		BIT	7,(IY+$01)	; are we running or checking syntax?
		RES	0,(IY+$02)	; signal "main screen"
		JR	Z,m113a
		RST	28H
		DW	o0D4D		; if running, make sure temp colours are used
m113a:		POP	AF		; drop scan-loop return address
		LD	A,(T_ADDR)
		SUB	(m0f8e and $FF)+$28	; form token code INK to OVER
		RST	28H
		DW	o21FC		; change temporary colours as directed
		CALL	m10b1		; check for statement end
		LD	HL,(ATTR_T)
		LD	(ATTR_P),HL	; make temporary colours permanent
		LD	HL,P_FLAG	; now copy even bits of P_FLAG to odd bits
		LD	A,(HL)
		RLCA
		XOR	(HL)
		AND	$AA
		XOR	(HL)
		LD	(HL),A
		RET

; Class $09 routine

m1157:		RST	28H
		DW	o1CBE		; use ROM 3 to handle class $09
		RET

; Class $0B routine

m115b:		JP	m0888		; jump to cassette/disk handling routines


; The IF command

m115e:		POP	BC		; drop return address to STMT-RET
		BIT	7,(IY+$01)
		JR	Z,m1175		; move on if syntax-checking
		LD	HL,(STKEND)	; "delete" item on calculator stack
		LD	DE,$FFFB
		ADD	HL,DE
		LD	(STKEND),HL
		RST	28H
		DW	o34E9		; call "test zero" with HL holding add of value
		JP	C,m1073		; if false, go to next line
m1175:		JP	m0fd1		; if true or syntax-checking, do next statement

; The FOR command

m1178:		CP	$CD
		JR	NZ,m1185	; move on if no STEP
		RST	20H		; advance CH_ADD
		CALL	m1121		; fetch step value
		CALL	m10b1		; check end of statement if syntax-checking
		JR	m119d		; else move on
m1185:		CALL	m10b1		; if no STEP, check end of statement
		LD	HL,(STKEND)	; and stack value "1"
		LD	(HL),$00
		INC	HL
		LD	(HL),$00
		INC	HL
		LD	(HL),$01
		INC	HL
		LD	(HL),$00
		INC	HL
		LD	(HL),$00
		INC	HL
		LD	(STKEND),HL
m119d:		RST	28H
		DW	o1D16		; use ROM 3 to perform command
		RET


; The READ command (enter at m11a2)

m11a1:		RST	20H		; move along statement
m11a2:		CALL	m110c		; check for existing variable
		BIT	7,(IY+$01)
		JR	Z,m11d9		; move on if syntax-checking
		RST	18H		; save current CH_ADD in X_PTR
		LD	(X_PTR),HL
		LD	HL,(DATADD)	; fetch data list pointer
		LD	A,(HL)
		CP	","
		JR	Z,m11c2		; move on unless new statement must be found
		LD	E,$E4
		RST	28H
		DW	o1D86		; search for "DATA" statement
		JR	NC,m11c2
		CALL	m2ada
		DB	$0D		; error E - out of data if not found
m11c2:		INC	HL		; advance pointer
		LD	(CH_ADD),HL
		LD	A,(HL)
		RST	28H
		DW	o1C56		; assign value to variable
		RST	18H
		LD	(DATADD),HL	; store CH_ADD as data pointer
		LD	HL,(X_PTR)	; get pointer to READ statement
		LD	(IY+$26),$00	; clear high byte of X_PTR
		LD	(CH_ADD),HL
		LD	A,(HL)		; get next READ statement character
m11d9:		RST	18H
		CP	","
		JR	Z,m11a1		; loop back if more items
		CALL	m10b1		; check for statement end
		RET

; The DATA command

m11e2:		BIT	7,(IY+$01)
		JR	NZ,m11f3	; move on if not syntax-checking
m11e8:		RST	28H
		DW	o24FB		; scan next expression
		CP	","
		CALL	NZ,m10b1	; if no more items, check for statement end
		RST	20H
		JR	m11e8		; loop back for more
m11f3:		LD	A,$E4		; we're passing by a DATA statement

; Subroutine to pass by a DEF FN or DATA statement during run-time

m11f5:		RST	28H
		DW	o1E39		; use ROM 3 routine
		RET

; The RUN command

m11f9:		RST	28H
		DW	o1E67		; set NEWPPC as required
		LD	BC,$0000
		RST	28H
		DW	o1E45		; do a RESTORE 0
		JR	m1207		; exit via CLEAR command

; The CLEAR command

m1204:		RST	28H
		DW	o1E99		; get operand, use 0 as default
m1207:		LD	A,B
		OR	C
		JR	NZ,m120f	; move on if non-zero
		LD	BC,(RAMTOP)	; use existing RAMTOP if zero
m120f:		PUSH	BC
		LD	DE,(VARS)
		LD	HL,(E_LINE)
		DEC	HL
		RST	28H
		DW	o19E5		; reclaim whole variables area
		RST	28H
		DW	o0D6B		; cls
		LD	HL,(STKEND)
		LD	DE,$0032
		ADD	HL,DE
		POP	DE
		SBC	HL,DE
		JR	NC,m1232	; move on if ramtop value too low
		LD	HL,(P_RAMT)
		AND	A
		SBC	HL,DE
		JR	NC,m1236	; move on if ramtop value not too high
m1232:		CALL	m2ada
		DB	$15		; error M - RAMTOP no good
m1236:		LD	(RAMTOP),DE	; store new RAMTOP
		POP	DE
		POP	HL
		POP	BC
		LD	SP,(RAMTOP)	; reset SP
		INC	SP
		PUSH	BC
		PUSH	HL
		LD	(ERR_SP),SP	; reset ERR_SP
		PUSH	DE
		RET

; The GOSUB command

m124a:		POP	DE		; save STMT_RET address
		LD	H,(IY+$0D)
		INC	H		; increment SUBPPC statement number
		EX	(SP),HL		; exchange error address with statement number
		INC	SP		; reclaim a location
		LD	BC,(PPC)
		PUSH	BC		; save line number
		PUSH	HL		; restack error address
		LD	(ERR_SP),SP	; reset ERR_SP to error address
		PUSH	DE		; restack STMT_RET address
		RST	28H
		DW	o1E67		; set NEWPPC & NSPPC to required values
		LD	BC,$0014
		RST	28H
		DW	o1F05		; test for room before making jump
		RET

; The RETURN command

m1266:		POP	BC		; get STMT_RET address
		POP	HL		; get error address
		POP	DE		; get next stack entry
		LD	A,D
		CP	$3E
		JR	Z,m127d		; move on if end of GOSUB stack
		DEC	SP		; full entry only uses 3 bytes
		EX	(SP),HL		; exchange error address with statement no
		EX	DE,HL
		LD	(ERR_SP),SP	; reset ERR_SP
		PUSH	BC		; restack STMT_RET
		LD	(NEWPPC),HL	; store new line
		LD	(IY+$0A),D	; and statement
		RET
m127d:		PUSH	DE		; reform stack
		PUSH	HL
		CALL	m2ada
		DB	$06		; error 7 - RETURN without GOSUB

; The DEF FN command

m1283:		BIT	7,(IY+$01)
		JR	Z,m128e		; move on if checking syntax
		LD	A,$CE
		JP	m11f5		; else go to skip DEF FN
m128e:		SET	6,(IY+$01)	; signal "numeric variable" in FLAGS
		RST	28H
		DW	o2C8D		; check present code is a letter
		JR	NC,m12ad	; error C if not
		RST	20H		; get next char
		CP	"$"
		JR	NZ,m12a1	; move on if not a string
		RES	6,(IY+$01)	; signal "string variable" in FLAGS
		RST	20H		; get next char
m12a1:		CP	"("
		JR	NZ,m12e1	; error if not (
		RST	20H
		CP	")"
		JR	Z,m12ca		; move on if no parameters
m12aa:		RST	28H
		DW	o2C8D		; check present code is letter
m12ad:		JP	NC,m1125	; error if not
		EX	DE,HL
		RST	20H
		CP	"$"
		JR	NZ,m12b8	; move on if not string
		EX	DE,HL
		RST	20H
m12b8:		EX	DE,HL
		LD	BC,$0006
		RST	28H
		DW	o1655		; make 6 bytes of room after parameter name
		INC	HL
		INC	HL
		LD	(HL),$0E	; store a number marker in first location
		CP	","
		JR	NZ,m12ca	; move on if end of parameters
		RST	20H
		JR	m12aa		; loop back
m12ca:		CP	")"
		JR	NZ,m12e1	; error if no )
		RST	20H
		CP	"="
		JR	NZ,m12e1	; error if no =
		RST	20H
		LD	A,(FLAGS)
		PUSH	AF		; save nature (number/string) of variable
		RST	28H
		DW	o24FB		; scan the expression
		POP	AF
		XOR	(IY+$01)
		AND	$40
m12e1:		JP	NZ,m1125	; error if expression not correct type
		CALL	m10b1		; check for end of statement
		RET


; The Loader routine, called from ROM 0

m12e8:		CALL	m2b89		; page in DOS workspace
		LD	HL,process
		LD	(HL),$FF	; signal "current process is Loader"
		LD	A,(LODDRV)
		CP	$54
		JP	Z,m13c6		; move on if no disk interface present
		XOR	A
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_MESSAGE	; disable ALERT routine
		CALL	m32ee		; restore TSTACK
		PUSH	HL
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_BOOT	; attempt to boot a disk from the boot sector
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		RST	28H
		DW	o16B0		; clear editing workspaces
		LD	HL,(E_LINE)
		LD	BC,$0007
		RST	28H
		DW	o1655		; create 7 bytes of space at E_LINE
		LD	HL,m152e
		LD	DE,(E_LINE)
		LD	BC,$0007
		LDIR			; copy LOAD "disk" into E_LINE
		LD	HL,(E_LINE)
		LD	(CH_ADD),HL	; set CH_ADD
		CALL	m22c7		; clear whole display if necessary
		BIT	6,(IY+$02)
		JR	NZ,m133d	; move on if shouldn't clear lower screen
		RST	28H
		DW	o0D6E		; clear lower screen
m133d:		RES	6,(IY+$02)	; signal "lower screen can be cleared"
		CALL	m2b89		; page in DOS workspace
		LD	HL,ed_flags
		BIT	6,(HL)
		JR	NZ,m1356	; ???
		INC	HL
		LD	A,(HL)
		CP	$00
		JR	NZ,m1356	; ???
		CALL	m3e80
		DW	l1a8e
m1356:		CALL	m2b64		; page in normal memory
		LD	HL,TV_FLAG
		RES	3,(HL)		; signal "mode unchanged"
		LD	A,$19
		SUB	(IY+$4F)
		LD	(SCR_CT),A	; set scroll count according to current line
		SET	7,(IY+$01)	; signal "execution mode"
		LD	(IY+$0A),$01	; statement 1
		LD	HL,n3e00
		PUSH	HL
		LD	HL,ONERR
		PUSH	HL
		LD	(ERR_SP),SP	; set up error stack
		LD	HL,m1383
		LD	(SYNRET),HL	; error return address
		JP	m1048		; execute the edit line, returning here on error
m1383:		CALL	m2b89		; page in DOS workspace
		POP	HL		; retrieve old ALERT address
		LD	A,$FF
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_MESSAGE	; re-enable ALERT routine
		CALL	m32ee		; restore TSTACK
		CALL	m2b64		; page in normal memory
		LD	A,(ERR_NR)	; get error code
		BIT	7,A
		JP	NZ,m25cb	; if OK, exit via standard syntax return
		LD	HL,(PPC)
		LD	DE,$FFFE
		XOR	A
		SBC	HL,DE
		LD	A,H
		OR	L
		JP	NZ,m25cb	; or if error occurred in program
		LD	A,"T"
		LD	(LODDRV),A	; set default load drive to cassette
		LD	HL,TV_FLAG
		SET	0,(HL)		; signal "using lower screen"
		LD	HL,msg18
		CALL	m1524		; output cassette loader message
		LD	HL,TV_FLAG
		RES	0,(HL)		; signal "using main screen"
		SET	6,(HL)		; signal "don't clear lower screen"
		JR	m13c9
m13c6:		CALL	m2b64		; page in normal memory
m13c9:		RST	28H
		DW	o16B0		; clear editing workspaces
		LD	HL,(E_LINE)
		LD	BC,$0003
		RST	28H
		DW	o1655		; make 3 bytes space at E_LINE
		LD	HL,m14df
		LD	DE,(E_LINE)
		LD	BC,$0003
		LDIR			; copy LOAD "" command into E_LINE
		LD	HL,(E_LINE)
		LD	(CH_ADD),HL	; set interpretation address
		CALL	m22c7		; clear whole screen if necessary
		BIT	6,(IY+$02)
		JR	NZ,m13f3
		RST	28H
		DW	o0D6E		; clear lower screen if necessary
m13f3:		RES	6,(IY+$02)	; signal "lower screen can be cleared"
		CALL	m2b89		; page in DOS workspace
		LD	HL,ed_flags
		BIT	6,(HL)
		JR	NZ,m140c	; ???
		INC	HL
		LD	A,(HL)
		CP	$00
		JR	NZ,m140c	; ???
		CALL	m3e80
		DW	l1a8e
m140c:		CALL	m2b64		; page in normal memory
		LD	HL,TV_FLAG
		RES	3,(HL)		; signal "mode unchanged"
		LD	A,$19
		SUB	(IY+$4F)
		LD	(SCR_CT),A	; set scroll count according to current line
		SET	7,(IY+$01)	; signal "execution mode"
		LD	(IY+$0A),$01	; set statement 1
		LD	HL,n3e00
		PUSH	HL
		LD	HL,ONERR
		PUSH	HL
		LD	(ERR_SP),SP	; set up error stack
		LD	HL,m1439
		LD	(SYNRET),HL	; set error return address
		JP	m1048		; execute line in editing area
m1439:		LD	A,(ERR_NR)	; get error code
		BIT	7,A
		JP	NZ,m25cb	; exit via standard error return if OK
		LD	HL,FLAGS3
		BIT	4,(HL)
		JP	Z,m25cb		; or if no disk interface present
		LD	A,"A"
		LD	(LODDRV),A	; else set default load drive to A:
		JP	m25cb		; and exit

; The Print option, called from ROM 0

m1451:		RST	28H
		DW	o16B0		; clear E_LINE, WORKSP, STKBOT etc
		LD	HL,(E_LINE)
		LD	BC,$0001
		RST	28H
		DW	o1655		; make a byte of space at E_LINE
		LD	HL,(E_LINE)
		LD	(HL),$E1	; insert LLIST command
		CALL	m24f0		; execute it

; The SPECTRUM command

m1465:		CALL	m14c4		; set "P" channel to use screen
		LD	SP,(ERR_SP)
		POP	HL		; discard current error return address
		LD	HL,o1303
		PUSH	HL		; address to enter ROM 3 at, in main loop
		LD	HL,$0013	; "AF"
		PUSH	HL
		LD	HL,$0008	; "BC"
		PUSH	HL
		LD	A,$20
		LD	(BANKM),A	; page 0, paging disabled
		PUSH	AF
		PUSH	BC
		DI
		RES	4,(IY+$01)	; signal "48K BASIC" mode
		JP	STOO		; enter 48K ROM

; The routine to enter 48K BASIC, called from ROM 0

m1488:		LD	HL,$6000
		PUSH	HL		; stack address to return to (in RAM)
		LD	DE,$6000
		LD	HL,m14b5
		LD	BC,$000F
		LDIR			; copy following routine into RAM
		LD	A,(BANK678)
		RES	3,A		; turn off disk motor
		LD	BC,PBANK678
		DI
		LD	(BANK678),A
		OUT	(C),A
		EI
		LD	A,$30		; select ROM 3 with paging disabled
		DI
		RES	4,(IY+$01)	; signal "48K BASIC" mode
		LD	(BANKM),A
		PUSH	AF
		PUSH	BC
		JP	STOO		; page in ROM 3 & jump to following routine

m14b5:		LD	A,$30
		LD	BC,PBANKM
		DI
		OUT	(C),A		; page in ROM 3 & disable paging
		EI
		JP	$0000		; reset machine with 48K ROM locked in

; Cliff J Lawson's initials

		DM	"CJL"

; Subroutine to copy screen channel addresses over printer channel addresses

m14c4:		LD	HL,(CHANS)
		LD	DE,$0005
		ADD	HL,DE		; HL=channel info for "S"
		LD	DE,$000A
		EX	DE,HL
		ADD	HL,DE
		EX	DE,HL		; DE=channel info for "P"
		LD	BC,$0004
		LDIR			; overwrite it
		RES	3,(IY+$30)	; set caps lock off
		RES	4,(IY+$01)	; signal "48K BASIC" mode
		RET

; LOAD "" command

m14df:		DB	$EF,$22,$22

; The Loader message

m14e2:		RST	18H
		CP	"#"
		JP	NZ,m22ad
		CALL	m2b35
		LD	B,2
		JP	m2b19
m14f0:		LD	A,B
		CPL
		LD	H,A
		LD	A,C
		CPL
		LD	L,A
		INC	HL
		ADD	HL,SP
		LD	SP,HL
		PUSH	BC
		PUSH	HL
		EX	DE,HL
		LDIR
		POP	HL
		RST	28H
		DW	$250E
		POP	HL
		ADD	HL,SP
		LD	SP,HL
		RET
m1506:		RST	18H
		CP	"#"
		JR	Z,m1515
		RST	28H
		DW	o1C6C
		CALL	m10b1
		RST	28H
		DW	o1DAB
		RET
m1515:		CALL	m2b35
		CALL	m3f00
		DW	$005C
		RST	28H
		DW	o2D28
		RST	28H
		DW	o2AFF
		RET

; Subroutine to output a $FF-terminated message

m1524:		LD	A,(HL)		; get next char
		CP	$FF
		RET	Z		; exit if end
		RST	28H
		DW	$0010		; output
		INC	HL
		JR	m1524		; loop back

; LOAD "disk" command

m152e:		DB	$EF		; LOAD keyword
m152f:		DM	$22, "disk", $22

; The LIST & LLIST commands (enter at m1535 for LLIST, m1539 for LIST)

m1535:		LD	A,$03		; default stream 3 for LLIST
		JR	m153b
m1539:		LD	A,$02		; default stream 2 for LIST
m153b:		LD	(IY+$02),$00	; signal ordinary listing in main screen
m153f:		RST	28H
		DW	o2530		; are we checking syntax?
		JR	Z,m1547
		RST	28H
		DW	o1601		; open channel if not
m1547:		RST	28H
		DW	$0018		; get character
		RST	28H
		DW	o2070		; see if stream must be altered
		JR	C,m1567		; move on if not
		RST	28H
		DW	$0018		; get character
		CP	$3B
		JR	Z,m155a		; move on if ;
		CP	","
		JR	NZ,m1562	; move on if not ,
m155a:		RST	28H
		DW	$0020		; get next character
m155d:		CALL	m1121		; get numeric expression
		JR	m156a		; move on with line number to list from
m1562:		RST	28H
		DW	o1CE6		; else use zero
		JR	m156a
m1567:		RST	28H
		DW	o1CDE		; fetch a numeric expression or use zero
m156a:		CALL	m10b1		; check for end of statement
m156d:		RST	28H
		DW	$1825		; use ROM 3 for actual listing operation
		RET

; PLAY command (enters here after syntax-checking)

m1571:		DI
		PUSH	BC		; save count of strings
		LD	DE,$0037	; $37 bytes required per string
		LD	HL,$003C	; plus $3C bytes overhead
m1579:		ADD	HL,DE
		DJNZ	m1579
		LD	C,L
		LD	B,H		; BC=workspace required
		RST	28H
		DW	$0030		; make space
		DI
		PUSH	DE
m1583:		POP	IY		; IY=start of space
		PUSH	HL		; stack HL=end of space+1
m1586:		POP	IX
		LD	(IY+$10),$FF
m158c:		LD	BC,$FFC9
		ADD	IX,BC		; IX=start of next string parameter space
		LD	(IX+$03),$3C
		LD	(IX+$01),$FF
		LD	(IX+$04),$0F
		LD	(IX+$05),$05
		LD	(IX+$21),$00
		LD	(IX+$0A),$00
		LD	(IX+$0B),$00
		LD	(IX+$16),$FF
		LD	(IX+$17),$00
		LD	(IX+$18),$00
		RST	28H
		DW	o2BF1		; get string from top of stack
		DI
		LD	(IX+$06),E
		LD	(IX+$07),D	; store address of string (twice)
		LD	(IX+$0C),E
		LD	(IX+$0D),D
		EX	DE,HL
		ADD	HL,BC
		LD	(IX+$08),L
		LD	(IX+$09),H	; store address of end-of-string+1
		POP	BC		; restore count of strings
		PUSH	BC
		DEC	B
		LD	C,B
		LD	B,$00		; BC=number of strings left on stack
		SLA	C
		PUSH	IY
		POP	HL
		ADD	HL,BC		; HL=overheadspace+2*(string number-1)
		PUSH	IX
		POP	BC
		LD	(HL),C
		INC	HL
		LD	(HL),B		; store string parameter block address
		OR	A
		RL	(IY+$10)	; shift in 0 bit for each string
		POP	BC
		DEC	B		; decrement string count
		PUSH	BC
		LD	(IX+$02),B	; store string number (0...7) in parameters
		JR	NZ,m158c	; back for another string
		POP	BC		; restore count of strings
		LD	(IY+$27),$1A
		LD	(IY+$28),$0B
		PUSH	IY
		POP	HL
		LD	BC,$002B
		ADD	HL,BC
		EX	DE,HL
		LD	HL,m161d
		LD	BC,$000D
		LDIR			; copy FP routine in
		LD	D,$07
		LD	E,$F8
		CALL	m1a6a		; output $F8 to AY register 7
		LD	D,$0B
		LD	E,$FF
		CALL	m1a6a		; output $FF to AY register 11
		INC	D
		CALL	m1a6a		; output $FF to AY register 12
		JR	m1669		; move on

; FP routine used to calculate tempo values, executed in RAM with ROM 3
; paged in

m161d:		RST	28H		; engage FP-calculator
		DB	stk_ten		; X,10
		DB	EXchange	; 10,X
		DB	DIvision	; 10/X
		DB	stk_data	; 10/X,Y
		DB	$DF,$75,$F4,$38,$75
		DB	DIvision	; 10/(X*Y)
		DB	end_calc
		RET

; Subroutine to check if BREAK is being pressed (exit with carry reset if so)

m162a:		LD	A,$7F
		IN	A,($FE)
		RRA
		RET	C		; exit with carry set if not pressed
		LD	A,$FE
		IN	A,($FE)
		RRA			; test other key & exit
		RET

; Subroutine to initialise string pointers to first string (m163b)

m1636:		LD	BC,$0011
		JR	m163e
m163b:		LD	BC,$0000
m163e:		PUSH	IY
		POP	HL
		ADD	HL,BC		; get address of pointer to string
		LD	(IY+$23),L
		LD	(IY+$24),H	; store address
		LD	A,(IY+$10)
		LD	(IY+$22),A	; copy available strings byte
		LD	(IY+$21),$01	; set string bit marker
		RET

; Subroutine to get address of current string parameter block in IX

m1653:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		PUSH	DE
		POP	IX
		RET

; Subroutine to increment pointer to next string parameter block address

m165a:		LD	L,(IY+$23)
		LD	H,(IY+$24)	; get current parameter block pointer address
		INC	HL
		INC	HL		; move to next
		LD	(IY+$23),L
		LD	(IY+$24),H	; store
		RET

; More PLAY command

m1669:		CALL	m163b		; copy initial info
m166c:		RR	(IY+$22)	; rotate string counter
		JR	C,m1678		; move on if no string
		CALL	m1653		; get IX=address of current string parm block
		CALL	m1748		; interpret string for standard parms
m1678:		SLA	(IY+$21)
		JR	C,m1683		; move on if tried 8 strings
		CALL	m165a		; increment pointer to string parms address
		JR	m166c		; loop back
m1683:		CALL	m1b7f		; find shortest current notelength
		PUSH	DE
		CALL	m1b30		; output next note from each string
		POP	DE
m168b:		LD	A,(IY+$10)
		CP	$FF
		JR	NZ,m1697	; move on unless no strings,or stop encountered
		CALL	m1a81		; close down AY channels for this command
		EI
		RET			; exit
m1697:		DEC	DE
		CALL	m1b64		; pause
		CALL	m1baf		; decrement note lengths & change notes if nec
		CALL	m1b7f		; find shortest current notelength
		JR	m168b		; loop back

; List of PLAY string parameters

m16a3:		DM	"HZYXWUVMT)(NO!"

; Subroutine to get next character from string and increment string
; interpretation address (carry set if no char available)

m16b1:		CALL	m1ad1		; get next character
		RET	C		; exit if end of string
		INC	(IX+$06)	; increment string interpretation address
		RET	NZ
		INC	(IX+$07)
		RET

; Subroutine to get a note from the string, returning semitone value A (or 0)
; If A=$80, a rest was found

m16bd:		PUSH	HL
		LD	C,$00		; C=initial semitone value (natural)
m16c0:		CALL	m16b1		; get next char from string
		JR	C,m16cd		; move on if none
		CP	'&'
		JR	NZ,m16d8	; move on if not a rest
		LD	A,$80		; signal "rest"
m16cb:		POP	HL
		RET
m16cd:		LD	A,(IY+$21)
		OR	(IY+$10)
		LD	(IY+$10),A	; set string to not in use & exit
		JR	m16cb
m16d8:		CP	"#"		; test for sharp sign
		JR	NZ,m16df
		INC	C		; if so, increment semitone value & loop back
		JR	m16c0
m16df:		CP	"$"		; test for flat sign
		JR	NZ,m16e6
		DEC	C		; if so, decrement semitone value & loop back
		JR	m16c0
m16e6:		BIT	5,A
		JR	NZ,m16f0	; move on if lowercase letter
		PUSH	AF
		LD	A,$0C
		ADD	A,C		; for uppercase, add octave to semitone value
		LD	C,A
		POP	AF
m16f0:		AND	$DF		; make uppercase
		SUB	"A"
		JP	C,m1b10		; error k if <A
		CP	$07
		JP	NC,m1b10	; or if >G
		PUSH	BC
m16fd:		LD	B,$00
		LD	C,A
		LD	HL,m19e7
		ADD	HL,BC
		LD	A,(HL)		; get note semitone value
		POP	BC
		ADD	A,C		; add octave/sharp/flat value
		POP	HL
		RET

; Subroutine to get a numeric value from the string into BC (defaults to 0)

m1709:		PUSH	HL
		PUSH	DE
		LD	L,(IX+$06)
		LD	H,(IX+$07)	; get string interpretation address
		LD	DE,$0000	; initial value 0
m1714:		LD	A,(HL)		; get next char
		CP	"0"
		JR	C,m1731
		CP	"9"+1
		JR	NC,m1731	; move on if not a digit
		INC	HL
		PUSH	HL
		CALL	m173c		; multiply current value by 10
		SUB	"0"
		LD	H,$00
		LD	L,A
		ADD	HL,DE		; add in digit
		JR	C,m172e		; jump if overflow
		EX	DE,HL
		POP	HL
		JR	m1714		; back for more digits
m172e:		JP	m1b08		; error l - number too big
m1731:		LD	(IX+$06),L
		LD	(IX+$07),H	; replace updated interpretation address
		PUSH	DE
		POP	BC		; BC=value
		POP	DE		; restore registers
		POP	HL
		RET

; Subroutine to multiply DE by 10

m173c:		LD	HL,$0000	; start with zero
		LD	B,$0A
m1741:		ADD	HL,DE		; add DE 10 times
		JR	C,m172e		; jump if overflow
		DJNZ	m1741
		EX	DE,HL
		RET

; Subroutine to interpret a string

m1748:		CALL	m162a		; check for break
		JR	C,m1755		; move on if not pressed
		CALL	m1a81		; close down channels for this command
		EI			; re-enable interrupts
		CALL	m2ada
		DB	20		; error L - BREAK into program
m1755:		CALL	m16b1		; get next character of string
		JP	C,m1990		; move on if no more available
		CALL	m19de		; search for char in parameter list
		LD	B,$00
		SLA	C
		LD	HL,m19b8
		ADD	HL,BC		; form pointer to routine address
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL		; HL=routine address
		CALL	m1770		; call routine
		JR	m1748		; loop back for more standard params
		RET			; exit when note length changed, or note found
m1770:		JP	(HL)

; Parameter !: comment

m1771:		CALL	m16b1		; get next character from string
		JP	C,m198f		; move on if end of string
		CP	'!'
		RET	Z		; exit if end of comment
		JR	m1771		; loop back

; Parameter O: octave

m177c:		CALL	m1709		; get number from string
		LD	A,C
		CP	$09
		JP	NC,m1b00	; error n if not 1-8
		SLA	A
		SLA	A
		LD	B,A
		SLA	A
		ADD	A,B
		LD	(IX+$03),A	; store base note number (12*octave)
		RET

; Parameter N: number separator

m1791:		RET

; Parameter (: start of repeat section

m1792:		LD	A,(IX+$0B)	; get current bracket depth
		INC	A		; increment
		CP	$05
		JP	Z,m1b18		; if depth now 5, cause error d
		LD	(IX+$0B),A	; store new depth
		LD	DE,$000C	; offset for bracket addresses
		CALL	m1813		; get pointer to next bracket address down
		LD	A,(IX+$06)
		LD	(HL),A
		INC	HL
		LD	A,(IX+$07)
		LD	(HL),A		; store address to repeat from
		RET

; Parameter ): end of repeat section

m17ae:		LD	A,(IX+$16)	; get number of )s encountered so far
		LD	DE,$0017	; offset for close bracket addresses
		OR	A
		JP	m,m17dc		; move on if none so far
		CALL	m1813		; get address of current
		LD	A,(IX+$06)
		CP	(HL)
		JR	NZ,m17dc
		INC	HL
		LD	A,(IX+$07)
		CP	(HL)
		JR	NZ,m17dc	; move on if not the same
		DEC	(IX+$16)	; decrement close bracket depth
		LD	A,(IX+$16)
		OR	A
		RET	p		; exit if still positive
		BIT	0,(IX+$0A)
		RET	Z		; exit if not infinite repeat
		LD	(IX+$16),$00	; set no close brackets
		XOR	A
		JR	m17f7
m17dc:		LD	A,(IX+$16)
		INC	A		; increment close bracket depth
		CP	$05
		JP	Z,m1b18		; error d if depth of 5
		LD	(IX+$16),A	; restore depth
		CALL	m1813		; get pointer to next close bracket address
		LD	A,(IX+$06)
		LD	(HL),A
		INC	HL
		LD	A,(IX+$07)
		LD	(HL),A		; store address to repeat to
		LD	A,(IX+$0B)	; get current open bracket depth
m17f7:		LD	DE,$000C
		CALL	m1813		; get pointer to address (or string start)
		LD	A,(HL)
		LD	(IX+$06),A
		INC	HL
		LD	A,(HL)
		LD	(IX+$07),A	; reset interpretation address to correct point
		DEC	(IX+$0B)	; decrement open bracket depth
		RET	p
		LD	(IX+$0B),$00	; reset to zero if no open brackets
		SET	0,(IX+$0A)	; set "infinite repeat"
		RET

; Subroutine to get HL=Ath word entry after IX+DE
; Used to find address for bracket address entries

m1813:		PUSH	IX
		POP	HL
		ADD	HL,DE		; add offset to string area
		LD	B,$00
		LD	C,A
		SLA	C
		ADD	HL,BC		; add offset to Ath word
		RET

; Parameter T: tempo

m181e:		CALL	m1709		; get number from string
		LD	A,B
		OR	A
		JP	NZ,m1b00	; error n if >255
		LD	A,C
		CP	$3C
		JP	C,m1b00		; error n if <60
		CP	$F1
		JP	NC,m1b00	; error n if >240
		LD	A,(IX+$02)
		OR	A
		RET	NZ		; ignore unless in first string
		LD	B,$00
		PUSH	BC
		POP	HL
		ADD	HL,HL
		ADD	HL,HL
		PUSH	HL
		POP	BC		; BC=tempo*4
		PUSH	IY
		RST	28H
		DW	o2D2B		; stack BC on calculator stack
		DI
		POP	IY
		PUSH	IY
		PUSH	IY
		POP	HL
		LD	BC,$002B	; offset to FP calculation routine
		ADD	HL,BC
		LD	IY,ERR_NR
		PUSH	HL		; stack FP routine address
		LD	HL,m1864
		LD	(RETADDR),HL	; set up return address
		LD	HL,REGNUOY
		EX	(SP),HL
		PUSH	HL
		PUSH	AF
		PUSH	HL
		JP	STOO		; call FP calculator - TOS=10/(tempo*4*val)
m1864:		DI
		RST	28H
		DW	o2DA2		; get value to BC
		DI
		POP	IY
		LD	(IY+$27),C
		LD	(IY+$28),B	; store tempo value
		RET

; Parameter M: channel

m1872:		CALL	m1709		; get number from string
		LD	A,C
		CP	$40
		JP	NC,m1b00	; error n if >63
		CPL
		LD	E,A
		LD	D,$07
		CALL	m1a6a		; output channel complement to AY register 7
		RET

; Parameter V: volume level

m1883:		CALL	m1709		; get number from string
		LD	A,C
		CP	$10
		JP	NC,m1b00	; error n if >15
		LD	(IX+$04),A	; store volume level
		LD	E,(IX+$02)
		LD	A,$08
		ADD	A,E
		LD	D,A		; AY register=channel+8 (channel=0..7)
		LD	E,C
		CALL	m1a6a		; output volume level to register
		RET

; Parameter U: volume effect in a string

m189b:		LD	E,(IX+$02)
		LD	A,$08
		ADD	A,E
		LD	D,A		; AY register=channel+8 (channel=0..7)
		LD	E,$1F
		LD	(IX+$04),E	; store volume effect marker
		RET

; Parameter W: volume effect

m18a8:		CALL	m1709		; get number from string
		LD	A,C
		CP	$08
		JP	NC,m1b00	; error n if >7
		LD	B,$00
		LD	HL,m19d6
		ADD	HL,BC
		LD	A,(HL)		; get envelope byte
		LD	(IY+$29),A	; store it
		RET

; Parameter X: volume duration

m18bc:		CALL	m1709		; get number from string
		LD	D,$0B
		LD	E,C
		CALL	m1a6a		; output duration to AY registers 11
		INC	D
		LD	E,B
		CALL	m1a6a		; and 12
		RET

; Parameter Y: MIDI channel

m18cb:		CALL	m1709		; get number from string
		LD	A,C
		DEC	A		; decrement
		JP	m,m1b00		; error n if was 0
		CP	$10
		JP	NC,m1b00	; error n if >15
		LD	(IX+$01),A	; store channel
		RET

; Parameter Z: MIDI programming code

m18dc:		CALL	m1709		; get number from string
		LD	A,C
		CALL	m1d91		; output code to MIDI
		RET

; Parameter H: stop PLAY command

m18e4:		LD	(IY+$10),$FF	; signal "no strings in use"
		RET

; Notes and other parameters

m18e9:		CALL	m1a07		; is char a digit? (ie note length)
		JP	C,m196f		; move on if not
		CALL	m199a		; get HL=pointer to note lengths for string
		CALL	m19a2		; save in overhead area
		XOR	A
		LD	(IX+$21),A	; zero number of tied notes
		CALL	m1ab6		; decrement interpretation address
		CALL	m1709		; get number from string
		LD	A,C
		OR	A
		JP	Z,m1b00		; error n if <1
		CP	$0D
		JP	NC,m1b00	; or >12
		CP	$0A
		JR	C,m1920		; move on unless dealing with triplets
		CALL	m19ee		; get note length value
		CALL	m1962		; increment number of tied notes
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store note length for first note
m1916:		CALL	m1962		; increment number of tied notes
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store note length for next tied note
		INC	HL
		JR	m1926
m1920:		LD	(IX+$05),C	; save new note length
		CALL	m19ee		; get note length value
m1926:		CALL	m1962		; increment number of tied notes
m1929:		CALL	m1ad1		; test next character
		CP	'_'
		JR	NZ,m195c	; move on unless tieing notes
		CALL	m16b1		; get the character
		CALL	m1709		; get number from string
		LD	A,C
		CP	$0A
		JR	C,m194d		; move on if not triplet
		PUSH	HL
		PUSH	DE
		CALL	m19ee		; get new note length value
		POP	HL
		ADD	HL,DE
		LD	C,E
		LD	B,D		; BC=old note length value+new note length val
		EX	DE,HL		; so does DE
		POP	HL		; restore address to store
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store value
		LD	E,C
		LD	D,B
		JR	m1916		; loop back
m194d:		LD	(IX+$05),C	; store new note length
		PUSH	HL
		PUSH	DE
		CALL	m19ee		; get note length value
		POP	HL
		ADD	HL,DE
		EX	DE,HL		; DE=old note length val+new note length val
		POP	HL
		JP	m1929		; loop back

; Store note length value & move on

m195c:		LD	(HL),E
		INC	HL
		LD	(HL),D		; store note length value
		JP	m198a		; move on

; Subroutine to increment number of tied notes for a string

m1962:		LD	A,(IX+$21)	; get number of tied notes
		INC	A		; increment
m1966:		CP	$0B
		JP	Z,m1b28		; error o - too many tied notes
		LD	(IX+$21),A
		RET

; Notes and other parameters (continued)

m196f:		CALL	m1ab6		; decrement string interpretation pointer
		LD	(IX+$21),$01	; set 1 tied note
		CALL	m199a		; get pointer to note lengths for string
		CALL	m19a2		; save in overhead area
		LD	C,(IX+$05)	; get current note length
		PUSH	HL
		CALL	m19ee		; calc note length value
		POP	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store it
		JP	m198a
m198a:		POP	HL		; retrieve return address
m198b:		INC	HL
		INC	HL		; move on by two
		PUSH	HL		; restack
		RET			; return

; Subroutine to set current string to "finished"

m198f:		POP	HL		; discard return address
m1990:		LD	A,(IY+$21)	; get string mask bit
		OR	(IY+$10)
		LD	(IY+$10),A	; place into strings counter
		RET

; Subroutine to set HL=pointer to note lengths for current string

m199a:		PUSH	IX
		POP	HL
		LD	BC,$0022
		ADD	HL,BC
		RET

; Subroutine to save note lengths pointer of string in overhead area

m19a2:		PUSH	HL		; save note lengths pointer
		PUSH	IY
		POP	HL
		LD	BC,$0011
		ADD	HL,BC		; HL=overhead area+$11
		LD	B,$00
		LD	C,(IX+$02)
m19af:		SLA	C
		ADD	HL,BC		; HL=overhead area+$11+(string*2)
		POP	DE
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store note lengths pointer
		EX	DE,HL		; restore HL=note lengths pointer
		RET

; Table of routine addresses for elements of PLAY strings

m19b8:		DW	m18e9		; note or other parameter
		DW	m1771		; Z
		DW	m177c		; Y
		DW	m1791		; X
		DW	m1792		; W
		DW	m17ae		; U
		DW	m181e		; V
		DW	m1872		; M
		DW	m1883		; T
		DW	m189b		; )
		DW	m18a8		; (
		DW	m18bc		; N
		DW	m18cb		; O
		DW	m18dc		; !
		DW	m18e4		; H

; Table of waveforms for volume effects

m19d6:		DB	$00,$04,$0B,$0D
		DB	$08,$0C,$0E,$0A

; Subroutine to search for string character A in parameter list
; Z set if found

m19de:		LD	BC,$000F
		LD	HL,m16a3
		CPIR
		RET

; Table of note semitone values

m19e7:		DB	$09,$0B,$00,$02,$04,$05,$07

; Subroutine to get note length value (DE) for note length C (1-12)

m19ee:		PUSH	HL
		LD	B,$00
		LD	HL,m19fa	; start of table
		ADD	HL,BC
		LD	D,$00
		LD	E,(HL)		; DE=note length value
		POP	HL
		RET

; Table of note length values

m19fa:		DB	$80
		DB	$06,$09,$0C,$12
		DB	$18,$24,$30,$48
		DB	$60,$04,$08,$10

; Subroutine to test if A is a digit (carry reset if so)

m1a07:		CP	"0"
		RET	C
		CP	"9"+1
		CCF
		RET

; Subroutine to play note A through AY channel for current string

m1a0e:		LD	C,A		; save semitone value
		LD	A,(IX+$03)
		ADD	A,C		; add in base note value
		CP	$80
		JP	NC,m1b20	; error m if out of range
		LD	C,A		; save note value
		LD	A,(IX+$02)
		OR	A
		JR	NZ,m1a2d	; move on unless first string
		LD	A,C
		CPL
		AND	$7F
		SRL	A
m1a25:		SRL	A
		LD	D,$06
		LD	E,A
		CALL	m1a6a		; output value to AY register 6
m1a2d:		LD	(IX+$00),C	; save last note value
		LD	A,(IX+$02)
		CP	$03
		RET	NC		; exit unless outputting AY channel 0-2
		LD	HL,m1c84
		LD	B,$00
		LD	A,C
		SUB	$15
		JR	NC,m1a45
		LD	DE,$0FBF	; lowest note possible
		JR	m1a4c
m1a45:		LD	C,A
		SLA	C		; form offset into semitone table
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
m1a4b:		LD	D,(HL)		; get DE=note value
m1a4c:		EX	DE,HL
		LD	D,(IX+$02)
		SLA	D		; AY register=0,2 or 4
		LD	E,L
		CALL	m1a6a		; output low byte of note value
		INC	D		; AY register=1,3 or 5
		LD	E,H
		CALL	m1a6a		; output high byte of note value
		BIT	4,(IX+$04)
		RET	Z		; exit unless envelope to be used
		LD	D,$0D
		LD	A,(IY+$29)
		LD	E,A
		CALL	m1a6a		; output waveform number to AY register 13
		RET

; Subroutine to output value E to sound register D

m1a6a:		PUSH	BC
		LD	BC,$FFFD
		OUT	(C),D		; select register D
		LD	BC,$BFFD
		OUT	(C),E		; output value E
		POP	BC
		RET

; Subroutine to get value of AY register A

m1a77:		PUSH	BC
		LD	BC,$FFFD
		OUT	(C),A		; select register
		IN	A,(C)		; get value
		POP	BC
		RET

; Subroutine to close down AY channels associated with this PLAY command

m1a81:		LD	D,$07
		LD	E,$FF
		CALL	m1a6a		; output $FF to AY register 7
		LD	D,$08
		LD	E,$00
		CALL	m1a6a		; output 0 to AY register 8
		INC	D
		CALL	m1a6a		; output 0 to AY register 9
		INC	D
		CALL	m1a6a		; output 0 to AY register 10
		CALL	m163b		; initialise string pointer info
m1a9a:		RR	(IY+$22)	; test for string
		JR	C,m1aa6		; move on if none
		CALL	m1653		; get IX=address of string parameter block
		CALL	m1d7b		; output terminator if MIDI channel
m1aa6:		SLA	(IY+$21)
		JR	C,m1ab1		; move on when 8 strings tested for
		CALL	m165a		; increment pointer to next string block
		JR	m1a9a		; loop back
m1ab1:		LD	IY,ERR_NR	; reset IY to system variables
		RET			; done

; Subroutine to decrement string interpretation pointer (skipping white space)

m1ab6:		PUSH	HL
		PUSH	DE
		LD	L,(IX+$06)
		LD	H,(IX+$07)	; get current pointer
m1abe:		DEC	HL		; decrement
		LD	A,(HL)
		CP	$20
		JR	Z,m1abe
		CP	$0D
		JR	Z,m1abe		; loop back while on white space
		LD	(IX+$06),L
		LD	(IX+$07),H	; store updated pointer
		POP	DE
		POP	HL
		RET

; Subroutine to get next character from string in A, skipping any white space
; Carry set on exit if end of string reached

m1ad1:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	L,(IX+$06)
		LD	H,(IX+$07)	; get HL=string interpretation address
m1ada:		LD	A,H
		CP	(IX+$09)	; compare against string end address
		JR	NZ,m1ae9
		LD	A,L
		CP	(IX+$08)
		JR	NZ,m1ae9
		SCF			; set carry if end of string
		JR	m1af3
m1ae9:		LD	A,(HL)
		CP	" "
		JR	Z,m1af7		; move to skip any spaces
		CP	$0D
		JR	Z,m1af7		; or CRs
		OR	A		; reset carry
m1af3:		POP	BC
		POP	DE
		POP	HL
m1af6:		RET
m1af7:		INC	HL		; increment string interpretation address
		LD	(IX+$06),L
		LD	(IX+$07),H
		JR	m1ada		; loop back

; Error routines for PLAY

m1b00:		CALL	m1a81		; close down
		EI
		CALL	m2ada
		DB	41		; error n - Out of range
m1b08:		CALL	m1a81		; close down
		EI
		CALL	m2ada
		DB	39		; error l - Number too big
m1b10:		CALL	m1a81		; close down
		EI
		CALL	m2ada
		DB	38		; error k - Invalid note name
m1b18:		CALL	m1a81		; close down
		EI
		CALL	m2ada
		DB	31		; error d - Too many brackets
m1b20:		CALL	m1a81		; close down
		EI
		CALL	m2ada
		DB	40		; error m - Note out of range
m1b28:		CALL	m1a81		; close down
		EI
		CALL	m2ada
		DB	42		; error o - Too many tied notes

; Subroutine to output next note from each string

m1b30:		CALL	m163b		; initialise string pointer info
m1b33:		RR	(IY+$22)	; test for next string
		JR	C,m1b5a		; move on if not present
		CALL	m1653		; get address of string parameter block to IX
		CALL	m16bd		; get note from string
		CP	$80
		JR	Z,m1b5a		; move on if rest found
		CALL	m1a0e		; calculate semitone & play if string 0-2
		LD	A,(IX+$02)
		CP	$03
		JR	NC,m1b57	; move on if strings 3-7
		LD	D,$08
		ADD	A,D
		LD	D,A
		LD	E,(IX+$04)
		CALL	m1a6a		; output volume level to AY register 8+channel
m1b57:		CALL	m1d5c		; output semitone to MIDI channels
m1b5a:		SLA	(IY+$21)
		RET	C		; exit when 8 strings done
		CALL	m165a		; get to next string parameter block
		JR	m1b33		; loop back

; Subroutine to pause for current notelength (DE)

m1b64:		PUSH	HL
		LD	L,(IY+$27)
		LD	H,(IY+$28)	; HL=tempo value
		LD	BC,$0064
		OR	A
		SBC	HL,BC
		PUSH	HL
		POP	BC
		POP	HL
m1b74:		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,m1b74	; timing delay
		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,m1b64	; loop DE times
		RET

; Subroutine to find shortest cuurent note across all strings

m1b7f:		LD	DE,$FFFF	; largest notelength so far (-1)
		CALL	m1636		; initialise pointers to first string
m1b85:		RR	(IY+$22)	; test for next string
		JR	C,m1b9d		; move on if not present
		PUSH	DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; get notelength pointer
		EX	DE,HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; get note length
		PUSH	DE
		POP	HL
		POP	BC
		OR	A
		SBC	HL,BC
		JR	C,m1b9d		; move on if already found smaller
		PUSH	BC
		POP	DE		; keep current
m1b9d:		SLA	(IY+$21)	; shift string bit marker
		JR	C,m1ba8		; move on if done 8 strings
		CALL	m165a		; increment pointer to next strings pointer
		JR	m1b85		; loop back
m1ba8:		LD	(IY+$25),E
		LD	(IY+$26),D	; store shortest current note length
		RET

; Subroutine to decrement remaining note lengths for each string, changing
; notes if necessary

m1baf:		XOR	A
		LD	(IY+$2A),A	; set no strings have changed notes
		CALL	m163b		; initialise string pointers
m1bb6:		RR	(IY+$22)
		JP	C,m1c48		; move on if string not present
		CALL	m1653		; get address of current string parameter block
		PUSH	IY
		POP	HL
		LD	BC,$0011
		ADD	HL,BC
		LD	B,$00
		LD	C,(IX+$02)
		SLA	C
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		PUSH	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL		; HL=notelength for this string
		LD	E,(IY+$25)
		LD	D,(IY+$26)	; DE=length to play
		OR	A
		SBC	HL,DE
		EX	DE,HL
		POP	HL
		JR	Z,m1bea		; move on if same length
		LD	(HL),E
		INC	HL
		LD	(HL),D		; else store remaining length
		JR	m1c48		; and move on
m1bea:		LD	A,(IX+$02)
		CP	$03
		JR	NC,m1bfa	; move on if MIDI channel
		LD	D,$08
		ADD	A,D
		LD	D,A		; select AY register 8+channel
		LD	E,$00
		CALL	m1a6a		; output 0 to register
m1bfa:		CALL	m1d7b		; output terminator if MIDI channel
		PUSH	IX
		POP	HL
		LD	BC,$0021
		ADD	HL,BC
		DEC	(HL)		; decrement number of tied notes
		JR	NZ,m1c14	; move on if still some left
		CALL	m1748		; interpret string for parameters
		LD	A,(IY+$21)
		AND	(IY+$10)
		JR	NZ,m1c48	; move on if string no longer in use
		JR	m1c2b		; move on
m1c14:		PUSH	IY
		POP	HL
		LD	BC,$0011
		ADD	HL,BC
		LD	B,$00
		LD	C,(IX+$02)
		SLA	C
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	DE
		INC	DE
		LD	(HL),D
		DEC	HL
		LD	(HL),E		; store pointer to next tied note length
m1c2b:		CALL	m16bd		; get note from string
		LD	C,A
		LD	A,(IY+$21)
		AND	(IY+$10)
		JR	NZ,m1c48	; move on if string no longer in use
		LD	A,C
		CP	$80
		JR	Z,m1c48		; move on if rest found
		CALL	m1a0e		; play note through channel
		LD	A,(IY+$21)
		OR	(IY+$2A)
		LD	(IY+$2A),A	; signal "note changed for this string"
m1c48:		SLA	(IY+$21)
		JR	C,m1c54		; move on if no more strings
		CALL	m165a		; get to next string parameter block
		JP	m1bb6		; loop back
m1c54:		LD	DE,$0001
		CALL	m1b64		; pause
		CALL	m163b		; initialise pointers to first string
m1c5d:		RR	(IY+$2A)
		JR	NC,m1c7a	; move on if note didn't change
		CALL	m1653		; get pointer to string parameter block
		LD	A,(IX+$02)
		CP	$03
		JR	NC,m1c77	; move on if MIDI channel
		LD	D,$08
		ADD	A,D
		LD	D,A
		LD	E,(IX+$04)
		CALL	m1a6a		; output volume to AY register 8+channel
m1c77:		CALL	m1d5c		; output semitone to MIDI channel
m1c7a:		SLA	(IY+$21)
		RET	C		; exit if 8 strings done
		CALL	m165a		; move to next string parameter block
		JR	m1c5d		; loop back

; The semitone table of note values

m1c84:		DW	$0FBF,$0EDC,$0E07,$0D3D
		DW	$0C7F,$0BCC,$0B22,$0A82
		DW	$09EB,$095D,$08D6,$0857
		DW	$07DF,$076E,$0703,$069F
		DW	$0640,$05E6,$0591,$0541
		DW	$04F6,$04AE,$046B,$042C
		DW	$03F0,$03B7,o0382,o034F
		DW	$0320,$02F3,$02C8,o02A1
		DW	$027B,$0257,$0236,$0216
		DW	$01F8,$01DC,$01C1,$01A8
		DW	$0190,$0179,$0164,$0150
		DW	$013D,$012C,$011B,$010B
		DW	$00FC,$00EE,$00E0,$00D4
		DW	$00C8,$00BD,$00B2,$00A8
		DW	$009F,$0096,$008D,$0085
		DW	$007E,o0077,o0070,$006A
		DW	$0064,$005E,$0059,$0054
		DW	$004F,$004B,$0047,$0043
		DW	$003F,$003B,o0038,$0035
		DW	$0032,$002F,$002D,$002A
		DW	$0028,$0025,$0023,$0021
		DW	$001F,$001E,o001C,$001A
		DW	$0019,$0018,$0016,$0015
		DW	$0014,$0013,$0012,$0011
		DW	$0010,$000F,$000E,$000D
		DW	$000C,$000C,$000B,$000B
		DW	$000A,$0009,$0009,$0008

; Subroutine to output a semitone if a MIDI channel

m1d5c:		LD	A,(IX+$01)
		OR	A
		RET	m		; exit if not a MIDI channel
		OR	$90
		CALL	m1d91		; output channel selector to MIDI
		LD	A,(IX+$00)
		CALL	m1d91		; output last semitone value
		LD	A,(IX+$04)
		RES	4,A		; ignore waveform flag
		SLA	A
		SLA	A
		SLA	A
		CALL	m1d91		; output volume to MIDI
		RET

; Subroutine to output terminator to MIDI channel

m1d7b:		LD	A,(IX+$01)
		OR	A
		RET	m		; exit if not a MIDI channel
		OR	$80
		CALL	m1d91		; output channel selector to MIDI
		LD	A,(IX+$00)
		CALL	m1d91		; output semitone to MIDI
		LD	A,$40
		CALL	m1d91		; output terminator to MIDI
		RET

; Subroutine to output a value (A) to the MIDI port (uses AUX)

m1d91:		LD	L,A		; save value
		LD	BC,$FFFD
		LD	A,$0E
		OUT	(C),A		; select AY register 14 (RS232/AUX)
		LD	BC,$BFFD
		LD	A,$FA
		OUT	(C),A		; output data low to AUX
		LD	E,$03
m1da2:		DEC	E
		JR	NZ,m1da2	; delay loop
		NOP
		NOP
		NOP
		NOP
		LD	A,L
		LD	D,$08		; 8 bits to output
m1dac:		RRA
		LD	L,A
		JP	NC,m1db7
		LD	A,$FE
		OUT	(C),A		; if set, output data high to AUX
		JR	m1dbd
m1db7:		LD	A,$FA
		OUT	(C),A		; if reset, output data low to AUX
		JR	m1dbd
m1dbd:		LD	E,$02
m1dbf:		DEC	E
		JR	NZ,m1dbf	; delay loop
		NOP
		ADD	A,$00
		LD	A,L
		DEC	D
		JR	NZ,m1dac	; loop back for more bits
		NOP
		NOP
		ADD	A,$00
		NOP
		NOP
		LD	A,$FE
		OUT	(C),A		; output data high to register
		LD	E,$06
m1dd5:		DEC	E
		JR	NZ,m1dd5	; delay loop
		RET

; Unused code for a FORMAT "P";n command, used in same way as FORMAT LINE n

m1dd9:		RST	20H
		RST	28H
		DW	o1C82		; get a string expression
		RST	18H
		CP	","
		JP	NZ,m1125
		RST	20H		; get next char
		RST	28H
		DW	o1C82		; get numeric expression
		RST	18H
		LD	HL,FLAGS3
		RES	6,(HL)
		CP	","
		JR	NZ,m1df7
		SET	6,(HL)
		RST	20H
		RST	28H
		DW	o1C82		; get a string expression
m1df7:		CALL	m10b1
		LD	A,1
		RST	28H
		DW	o1601
		JP	m37fe

m1e02:		RST	28H
		DW	$0020		; get next char & continue into FORMAT LINE

; The FORMAT LINE command

m1e05:		RST	28H
		DW	o1C82		; get numeric expression
		BIT	7,(IY+$01)
		JR	Z,m1e15		; move on if syntax-checking
		RST	28H
		DW	o1E99		; get value to BC
		LD	(BAUD),BC	; set BAUD rate
m1e15:		RST	28H
		DW	$0018		; get next char
		CP	$0D
		JR	Z,m1e21		; move on if end-of-line
		CP	":"
		JP	NZ,m1125	; error if not end-of-statement
m1e21:		CALL	m10b1		; check for end-of-statement
		LD	BC,(BAUD)
		LD	A,B
		OR	C
		JR	NZ,m1e30	; move on if baud rate not zero
		CALL	m2ada
		DB	$25		; else error "invalid baud rate"
m1e30:		LD	HL,m1e50	; baud rate table
m1e33:		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; get next baud rate
		INC	HL
		EX	DE,HL
		LD	A,H
		CP	$25
		JR	NC,m1e47	; move on if end of table
		AND	A
		SBC	HL,BC
		JR	NC,m1e47	; move on if >= required rate
		EX	DE,HL
		INC	HL		; skip timing constant
		INC	HL
		JR	m1e33		; loop back for next
m1e47:		EX	DE,HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; get appropriate timing constant
		LD	(BAUD),DE	; save in BAUD
		RET

; The baud rate table

m1e50:		DW	$0032,$0AA5	; 50
		DW	$006E,$04D4	; 110
		DW	$012C,$01C3	; 300
		DW	$0258,$00E0	; 600
		DW	$04B0,$006E	; 1200
		DW	$0960,$0036	; 2400
		DW	$12C0,$0019	; 4800
		DW	$2580,$000B	; 9600

; Printer input channel routine

m1e70:		LD	HL,FLAGS3
		BIT	3,(HL)
		JP	Z,m1e85		; move on if using Centronics
		LD	HL,SERFL
		LD	A,(HL)
		AND	A
		JR	Z,m1e89		; move on if no RS232 character waiting
		LD	(HL),$00	; reset SERFL flag
		INC	HL
		LD	A,(HL)		; and get character
		SCF
		RET
m1e85:		RST	28H
		DW	o15C4		; invalid I/O device error
		RET
m1e89:		CALL	m2af9		; test for BREAK

m1e8c:		DI
		EXX
		LD	DE,(BAUD)	; DE=BAUD
		LD	HL,(BAUD)
		SRL	H
		RR	L		; HL=BAUD/2
		OR	A
		LD	B,$FA		; B=timing constant
		EXX
		LD	C,$FD
		LD	D,$FF
		LD	E,$BF
		LD	B,D
		LD	A,$0E
		OUT	(C),A		; select AY register 14
		IN	A,(C)		; get RS232/AUX value
		OR	$F0
		AND	$FB		; set CTS low
		LD	B,E
		OUT	(C),A		; output CTS low
		LD	H,A		; save RS232/AUX value with CTS low
m1eb2:		LD	B,D
		IN	A,(C)		; get RS232/AUX value
		AND	$80
		JR	Z,m1ec2		; move on if TXD was low (ie data ready)
m1eb9:		EXX
		DEC	B		; decrement timer
		EXX
		JR	NZ,m1eb2	; loop back
		XOR	A		; carry reset for no data
		PUSH	AF
		JR	m1efb		; move on if no data received
m1ec2:		IN	A,(C)
		AND	$80
		JR	NZ,m1eb9	; back if TXD high
		IN	A,(C)
		AND	$80
		JR	NZ,m1eb9	; back if TXD high
		EXX
		LD	BC,$FFFD
		LD	A,$80		; A'=char to build (carry will be set when
		EX	AF,AF'		; all 8 bits have been read)
m1ed5:		ADD	HL,DE
		NOP
		NOP
		NOP
		NOP
m1eda:		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,m1eda	; baud rate timing loop
		IN	A,(C)		; get RS232/AUX data
		AND	$80		; mask data bit
		JP	Z,m1eef		; move on if zero
		EX	AF,AF'
		SCF
		RRA			; rotate a 1 bit in
		JR	C,m1ef8		; move on if byte complete
		EX	AF,AF'
		JP	m1ed5		; loop back for more bits
m1eef:		EX	AF,AF'
		OR	A
		RRA			; rotate a 0 bit in
		JR	C,m1ef8		; move on if byte complete
		EX	AF,AF'
		JP	m1ed5		; loop back for more bits
m1ef8:		SCF
		PUSH	AF
		EXX
m1efb:		LD	A,H
		OR	$04
		LD	B,E
		OUT	(C),A		; set RS232 CTS high
		EXX
		LD	H,D
		LD	L,E		; HL=BAUD
		LD	BC,$0007
		OR	A
		SBC	HL,BC
m1f0a:		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,m1f0a	; timing loop
		LD	BC,$FFFD
		ADD	HL,DE
		ADD	HL,DE
		ADD	HL,DE
m1f15:		IN	A,(C)
		AND	$80
		JR	Z,m1f23		; move on if TXD low (2nd byte available)
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,m1f15	; timing loop
		POP	AF		; restore value
		EI
		RET			; exit
m1f23:		IN	A,(C)
		AND	$80
		JR	NZ,m1f15	; move back if TXD high
		IN	A,(C)
		AND	$80
		JR	NZ,m1f15	; move back if TXD high
		LD	H,D
		LD	L,E
		LD	BC,$0002
		SRL	H
		RR	L
		OR	A
		SBC	HL,BC
		LD	BC,$FFFD
		LD	A,$80		; prepare 2nd byte in A'
		EX	AF,AF'
m1f41:		NOP
		NOP
		NOP
		NOP
		ADD	HL,DE
m1f46:		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,m1f46	; timing loop
		IN	A,(C)
		AND	$80		; test bit
		JP	Z,m1f5b		; move on if zero
		EX	AF,AF'
		SCF
		RRA			; rotate a 1 bit in
		JR	C,m1f64		; move on if byte complete
		EX	AF,AF'
		JP	m1f41		; back for more bits
m1f5b:		EX	AF,AF'
		OR	A
		RRA			; rotate a 0 bit in
		JR	C,m1f64		; move on if byte complete
		EX	AF,AF'
		JP	m1f41		; back for more bits
m1f64:		LD	HL,SERFL
		LD	(HL),$01	; flag "2nd byte available"
		INC	HL
		LD	(HL),A		; store 2nd byte
		POP	AF		; restore the 1st byte
		EI
		RET			; done

; Printer output channel routine

m1f6e:		PUSH	HL
		LD	HL,FLAGS3
		BIT	2,(HL)
		POP	HL
		JP	Z,m2051		; go to output if pure binary channel
		PUSH	AF
		LD	A,(TVPARS)
		OR	A
		JR	Z,m1f8e		; move on if no inline parameters expected
		DEC	A
		LD	(TVPARS),A	; decrement # parameters
		JR	NZ,m1f89	; move on if more still needed
		POP	AF
		JP	m2020		; move on
m1f89:		POP	AF
		LD	(TVDATA+1),A	; save first parameter & exit
		RET
m1f8e:		POP	AF
		CP	$A3
		JR	C,m1fa0		; move on unless BASIC token
		LD	HL,(RETADDR)
		PUSH	HL		; save RETADDR
		RST	28H
		DW	o0B52		; output tokens using ROM 3
		POP	HL
		LD	(RETADDR),HL	; restore RETADDR
		SCF
		RET
m1fa0:		LD	HL,FLAGS
		RES	0,(HL)		; reset "outputting space" flag
		CP	" "
		JR	NZ,m1fab
		SET	0,(HL)		; set "outputting space" flag
m1fab:		CP	$7F
		JR	C,m1fb1
		LD	A,"?"		; substitute ? for graphics
m1fb1:		CP	" "
		JR	C,m1fcc		; move on for control codes
		PUSH	AF
		LD	HL,COL
		INC	(HL)		; increment column
		LD	A,(WIDTH)
		CP	(HL)
		JR	NC,m1fc8	; if within width, move on to print
		CALL	m1fd0		; output CRLF
		LD	A,$01
		LD	(COL),A		; set first column
m1fc8:		POP	AF
		JP	m2051		; output character
m1fcc:		CP	$0D
		JR	NZ,m1fde	; move on unless CR
m1fd0:		XOR	A
		LD	(COL),A		; reset column counter
		LD	A,$0D
		CALL	m2051		; output CRLF
		LD	A,$0A
		JP	m2051		; & exit
m1fde:		CP	$06
		JR	NZ,m2001	; move on unless PRINT comma
		LD	BC,(COL)	; B=WIDTH, C=COL
		LD	E,$00
m1fe8:		INC	E		; increment COL & E
		INC	C
		LD	A,C
		CP	B
		JR	Z,m1ff6		; if end of line, go to do E spaces
m1fee:		SUB	$08
		JR	Z,m1ff6		; or if at a tab stop
		JR	NC,m1fee
		JR	m1fe8		; loop back until reach a tab stop or eol
m1ff6:		PUSH	DE
		LD	A," "
		CALL	m1f6e		; output a space
		POP	DE
		DEC	E
		RET	Z
		JR	m1ff6		; loop back for more
m2001:		CP	$16
		JR	Z,m200e		; move on for AT (2 inline codes)
		CP	$17
		JR	Z,m2017
		CP	$10
		RET	C		; exit for codes 0-15
		JR	m2017		; move on for colour codes (1 inline code)
m200e:		LD	(TVDATA),A	; store control code
		LD	A,$02
		LD	(TVPARS),A	; & number of codes required
		RET
m2017:		LD	(TVDATA),A	; store control code
		LD	A,$01
		LD	(TVPARS),A	; & number of codes required
		RET

; Here, we deal with inline parameters

m2020:		LD	D,A		; save last parameter
		LD	A,(TVDATA)	; get control code
		CP	$16
		JR	Z,m2030		; move on for AT
		CP	$17
		CCF
		RET	NZ		; ignore other codes except TAB
		LD	A,(TVDATA+1)	; use first parameter as column
		LD	D,A
m2030:		LD	A,(WIDTH)	; get width
		CP	D
		JR	Z,m2038
		JR	NC,m203e
m2038:		LD	B,A
		LD	A,D
		SUB	B		; reduce column by width until in range
		LD	D,A
		JR	m2030
m203e:		LD	A,D
		OR	A
		JP	Z,m1fd0		; for column 0, do CRLF
m2043:		LD	A,(COL)
		CP	D
		RET	Z		; exit if at right column
		PUSH	DE
		LD	A," "
		CALL	m1f6e		; output a space
		POP	DE
		JR	m2043		; loop back

; Subroutine to output a character to the printer (Centronics or RS232)

m2051:		PUSH	HL
		LD	HL,FLAGS3
		BIT	3,(HL)
		POP	HL
		JP	Z,m20a8		; move on if print output is centronics

m205c:		PUSH	AF		; save character
		LD	C,$FD
		LD	D,$FF
		LD	E,$BF
		LD	B,D
		LD	A,$0E
		OUT	(C),A		; select AY register 14
m2067:		CALL	m2af9		; test for BREAK
		IN	A,(C)		; read RS232/AUX
		AND	$40
		JR	NZ,m2067	; loop until DTR low
		LD	HL,(BAUD)
		LD	DE,$0002
		OR	A
		SBC	HL,DE
		EX	DE,HL		; DE=BAUD-2
		POP	AF		; restore character
		CPL			; invert it
		SCF			; set carry for initial bit
		LD	B,$0B		; 11 bits to output
m207f:		DI
m2080:		PUSH	BC		; save registers
		PUSH	AF
		LD	A,$FE
		LD	H,D		; HL=BAUD-2
		LD	L,E
		LD	BC,$BFFD
		JP	NC,m2092	; move on to output a one bit
		AND	$F7		; mask RS232 RXD off
		OUT	(C),A		; output zero bit to RS232
		JR	m2098
m2092:		OR	$08		; set RS232 RXD on
		OUT	(C),A		; output one bit to RS232
		JR	m2098
m2098:		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,m2098	; timing loop for baud rate
		NOP			; more timing values
		NOP
		NOP
		POP	AF		; restore registers
		POP	BC
		OR	A		; clear carry (for stop bits)
		RRA			; rotate next bit
		DJNZ	m2080		; loop back for more
		EI
		RET

m20a8:		PUSH	AF		; save character
m20a9:		CALL	m2af9		; test for BREAK
		LD	BC,$0FFD
		IN	A,(C)
		BIT	0,A
		JR	NZ,m20a9	; wait for printer ready
		POP	AF
		OUT	(C),A		; output character
		DI
m20b9:		LD	BC,PBANK678
		LD	A,(BANK678)
		AND	$EF
		OUT	(C),A		; output strobe
		OR	$10
		LD	(BANK678),A
		OUT	(C),A		; output strobe
		EI
		SCF
		RET
		NOP

; The COPY (to printer) command routine

m20ce:		BIT	7,(IY+$01)
		RET	Z		; exit if syntax-checking
		LD	A,(BANK678)
		LD	BC,PBANK678
		SET	4,A
		DI			; disable interrupts
		LD	(BANK678),A
		OUT	(C),A		; set strobe high
		EI
		LD	HL,YLOC
		LD	(HL),$2B	; set Y location to 43 (4 bits on each row)
m20e7:		LD	HL,m216b
		CALL	m2151		; output 120dpi line command
		CALL	m2107		; output raster image
		LD	HL,m2172
		CALL	m2151		; output linefeed
		LD	HL,YLOC
		XOR	A
		CP	(HL)
		JR	Z,m2100
		DEC	(HL)
		JR	m20e7		; loop back for more lines
m2100:		LD	HL,m2174
		CALL	m2151		; reset linespacing and done
		RET

; Subroutine to output a raster line for a non-expanded copy

m2107:		LD	HL,XLOC
		LD	(HL),$FF	; set XLOC
m210c:		CALL	m2118		; output a pixel's width
		LD	HL,XLOC
		XOR	A
		CP	(HL)
		RET	Z		; exit if done line
		DEC	(HL)
		JR	m210c		; back for next pixel width

; Subroutine to output a pixel's width of a non-expanded copy

m2118:		LD	DE,$C000	; D=pixel position mask
		LD	BC,(XLOC)	; C=x-position, B=y-position
		SCF
		RL	B
		SCF
		RL	B		; B=top row of required bits
		LD	A,C
		CPL
		LD	C,A		; start at left of screen
		XOR	A		; initialise raster byte
		PUSH	AF
		PUSH	DE
		PUSH	BC
m212c:		CALL	m215f		; get pixel state
		POP	BC
		POP	DE
		LD	E,$00
		JR	Z,m2136		; set E=0 for no pixel
		LD	E,D		; or E=mask for pixel
m2136:		POP	AF
		OR	E		; combine pixel into raster byte
		PUSH	AF
		DEC	B		; decrement Y position
		SRL	D
		SRL	D		; shift mask right twice (4 pixels per row)
		PUSH	DE
		PUSH	BC
		JR	NC,m212c	; loop back if more pixels to get
		POP	BC
		POP	DE
		POP	AF
		LD	B,$03
m2147:		PUSH	BC
		PUSH	AF
		CALL	m2051		; output raster byte
		POP	AF
		POP	BC
		DJNZ	m2147		; loop back for 3 passes
		RET

; Subroutine to output a counted string at HL to the printer

m2151:		LD	B,(HL)		; get count
		INC	HL
m2153:		LD	A,(HL)		; get next char
		PUSH	HL
		PUSH	BC
		CALL	m2051		; output char to printer
		POP	BC
		POP	HL
		INC	HL
		DJNZ	m2153		; loop back for more
		RET

; Subroutine to check pixel at B=y,C=x
; On exit, Z is reset if pixel is ink, set if pixel is paper

m215f:		RST	28H
		DW	o22AA		; get address of pixel in HL
		LD	B,A
		INC	B		; B=counter to get required pixel
		XOR	A		; zero A
		SCF			; set carry
m2166:		RRA
m2167:		DJNZ	m2166
		AND	(HL)		; mask against screen byte
		RET

; The line header for a non-expanded copy

m216b:		DB	$06		; 6 bytes
		DB	$1B,"1"		; select 7/72" linespacing
		DB	$1B,"L",$00,$03	; print 768 dots in 120dpi

; The line terminator for a non-expanded copy

m2172:		DB	$01		; 1 byte
		DB	$0A		; linefeed

; The terminator for a non-expanded copy

m2174:		DB	$02		; 2 bytes
		DB	$1B,"2"		; select 1/6" linespacing

; The PRINT & LPRINT commands (enter at m2177 for LPRINT, m217b for PRINT)

m2177:		LD	A,$03		; use stream 3 for LPRINT
		JR	m217d
m217b:		LD	A,$02		; use stream 2 for PRINT
m217d:		RST	28H
		DW	o2530		; are we syntax-checking?
		JR	Z,m2185
		RST	28H
		DW	o1601		; open channel if not
m2185:		RST	28H
		DW	o0D4D		; set temporary colours
		RST	28H
		DW	o1FDF		; use ROM 3 for command routine
		CALL	m10b1		; check for end-of-statement
		RET

; The INPUT command

m218f:		RST	28H
		DW	o2530
		JR	Z,m219c		; move on if syntax-checking
		LD	A,$01
m2196:		RST	28H
		DW	o1601		; open channel to stream 1
		RST	28H
		DW	o0D6E		; clear lower screen
m219c:		LD	(IY+$02),$01	; set DF_SZ to 1
		RST	28H
		DW	o20C1		; deal with the input items
		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	$20A0		; use ROM 3 for actual routine
		RET

; The COPY command

m21aa:		RST	18H
		CP	$0D
		JP	Z,m20ce		; go to do printer copy if end of line
		CP	":"
		JP	Z,m20ce		; or end of statement
		CP	$B9
		JP	Z,m3328		; go to do expanded copy if COPY EXP
		DS	5
		RST	28H
		DW	o1C8C		; get a string expression
		RST	28H
		DW	$0018		; get character
m21c5:		CP	$CC
		JR	Z,m21cd		; move on if found TO
		CALL	m2ada
		DB	$0B		; error C - nonsense in BASIC
m21cd:		RST	28H
		DW	$0020		; get next char
m21d0:		CP	$AA
		JP	Z,m2237		; move on if COPY f$ TO SCREEN$
		CP	$A3
		JP	Z,m2257		; move on if COPY f$ TO SPECTRUM FORMAT
		CP	$E0
		JP	Z,m2237		; move on if COPY f$ TO LPRINT
		RST	28H
		DW	o1C8C		; get a string expression
		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2BF1		; fetch last value from calculator stack
		LD	A,B
		OR	C		; check length of second string
		JR	NZ,m21f0
		CALL	m2ada
		DB	$2C		; error "Bad filename"
m21f0:		INC	DE
		LD	A,(DE)		; check 2nd char of 2nd string
		DEC	DE
		CP	":"
		JR	NZ,m21fb	; move on if not specifying a drive
		LD	A,(DE)
		AND	$DF		; convert drive letter to uppercase
		LD	(DE),A
m21fb:		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy 2nd string to page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; terminate 2nd filename with $FF
		INC	DE		; increment pointer after 2nd filename
		CALL	m2b64		; page in normal memory
		PUSH	DE		; save pointer
		RST	28H
		DW	o2BF1		; fetch value from calculator stack
		LD	A,B
		OR	C
		JR	NZ,m2218	; check length of first string
		CALL	m2ada
		DB	$2C		; error "Bad filename"
m2218:		INC	DE
		LD	A,(DE)		; check 2nd char of first string
		DEC	DE
		CP	":"
		JR	NZ,m2223	; move on if not specifying a drive
		LD	A,(DE)
		AND	$DF		; convert drive letter to uppercase
		LD	(DE),A
m2223:		POP	HL		; restore address in page 7
		EX	DE,HL
		CALL	m3f63		; copy 1st filename to page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; terminate 1st filename with $FF
		CALL	m2b64		; page in normal memory
		XOR	A		; signal "copying to a file"
		SCF
		CALL	m2ba3		; do the copy
		RET

; The COPY...TO SCREEN$/LPRINT commands

m2237:		PUSH	AF		; save keyword
		RST	28H
		DW	$0020		; get next char
m223b:		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2BF1		; get string
		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy into page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
		CALL	m2b64		; page in normal memory
		POP	AF		; restore keyword as destination flag
		AND	A		; reset Z flag
		CALL	m2ba3		; copy the file
		RET

; The COPY....TO SPECTRUM FORMAT command

m2257:		RST	28H
		DW	$0020		; get next char
m225a:		CP	$D0		; check for FORMAT
		JR	Z,m2262
		CALL	m2ada
		DB	$0B		; nonsense in BASIC if not
m2262:		RST	28H
		DW	$0020		; get to next char
m2265:		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2BF1		; get string
		LD	HL,tmp_fspec
		EX	DE,HL
		CALL	m3f63		; copy into page 7
		CALL	m2b89		; page in DOS workspace
		LD	A,$FF
		LD	(DE),A		; add terminator
		CALL	m2b64		; page in normal memory
		XOR	A
		CALL	m2ba3		; copy the file
		RET

; The NEW command

m2280:		DI
		CALL	m3e80
		DW	$01B0		; use routine in ROM 0

; The CIRCLE command

m2286:		RST	18H		; get current char
		CP	","
		JR	NZ,m22c3	; error C if not comma
		RST	20H		; get next char
		RST	28H
		DW	o1C82		; get numeric expression
		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	$232D		; use ROM 3 for actual routine
		RET

; The DRAW command

m2296:		RST	18H		; get current char
		CP	","
		JR	Z,m22a2		; move on if comma
		CALL	m10b1		; check for end-of-statement
		RST	28H
		DW	o2477		; use ROM 3 to draw line
		RET
m22a2:		RST	20H		; get next char
		RST	28H
		DW	o1C82		; get numeric expression
		CALL	m10b1		; check for end of statement
		RST	28H
		DW	$2394		; use ROM 3 to draw curve
		RET

; The DIM command

m22ad:		RST	28H
		DW	o28B2		; search variables area
		JR	NZ,m22c3	; move on if error
		RST	28H
		DW	o2530
		JR	NZ,m22bf	; move on if runtime
		RES	6,C		; test string syntax as if numeric
		RST	28H
		DW	o2996		; check syntax of parenthesised expression
		CALL	m10b1		; check for end-of-statement
m22bf:		RST	28H
		DW	o2C15		; use ROM 3 for actual command
		RET
m22c3:		CALL	m2ada
		DB	$0B		; error C - nonsense in BASIC


; Subroutine to clear whole display unless unnecessary

m22c7:		BIT	0,(IY+$30)	; check FLAGS2
		RET	Z		; exit if not necessay
		RST	28H
		DW	o0DAF		; cls
		RET

; Subroutine to evaluate an expression for the calculator, & set the
; result to be used by the next calculation

m22d0:		LD	HL,$FFFE
		LD	(PPC),HL	; set statement -2
		RES	7,(IY+$01)	; signal "syntax checking"
		CALL	m2368		; set interpreter to start of line
		RST	28H
		DW	o24FB		; evaluate an expression
		BIT	6,(IY+$01)
		JR	Z,m2312		; move on if value not numeric
		RST	18H
		CP	$0D
		JR	NZ,m2312	; or if next character isn't end-of-line
		SET	7,(IY+$01)	; signal "executing"
		CALL	m2368		; set interpreter to start of line
		LD	HL,m25cb
		LD	(SYNRET),HL	; set up error return address
		RST	28H
		DW	o24FB		; evaluate an expression
		BIT	6,(IY+$01)
		JR	Z,m2312		; move on if value not numeric
		LD	DE,LASTV
		LD	HL,(STKEND)
		LD	BC,$0005
		OR	A
		SBC	HL,BC
		LDIR			; copy result into LASTV variable
		JP	m2316
m2312:		CALL	m2ada
		DB	25		; error Q - parameter error
m2316:		LD	A,$0D
		CALL	m2347		; do a newline
		LD	BC,$0001
		RST	28H
		DW	$0030		; make a byte in the workspace
m2321:		LD	(K_CUR),HL	; save address of cursor
		PUSH	HL
		LD	HL,(CURCHL)	; get address of current channel information
		PUSH	HL
		LD	A,$FF
		RST	28H
		DW	o1601		; open channel to stream -1
		RST	28H
		DW	o2DE3		; print the result value
		POP	HL
		RST	28H
		DW	o1615		; restore old channel
		POP	DE
		LD	HL,(K_CUR)	; get new cursor address
		AND	A
		SBC	HL,DE		; HL=# of result chars
m233c:		LD	A,(DE)
		CALL	m2347		; "type" each result character
		INC	DE
		DEC	HL
		LD	A,H
		OR	L
		JR	NZ,m233c	; loop back for more
		RET

; Subroutine to "do" a key (A) using ROM 0's editing keys

m2347:		PUSH	HL
		PUSH	DE
		CALL	m2b89		; page in DOS workspace
		LD	HL,ed_flags
		RES	3,(HL)		; ???
		PUSH	AF
		LD	A,$02
		RST	28H
		DW	o1601		; open channel to stream 2
		POP	AF
		CALL	m3e80
		DW	o0716		; "do" the key
		LD	HL,ed_flags
		RES	3,(HL)		; ???
		CALL	m2b64		; page in normal memory
		POP	DE
		POP	HL
		RET

; Subroutine to set interpreter to entered line, with A=first char

m2368:		LD	HL,(E_LINE)
		DEC	HL
		LD	(CH_ADD),HL	; CH_ADD=E_LINE-1
		RST	20H		; get next char
		RET

; Subroutine to determine if line is a single LET statement (Z set if so)

m2371:		CALL	m2368		; get first char in E_LINE
		CP	$F1
		RET	NZ		; exit unless LET
		LD	HL,(CH_ADD)
m237a:		LD	A,(HL)
		INC	HL
		CP	$0D
		RET	Z		; exit when end of line found (with Z set)
		CP	":"
		JR	NZ,m237a
		OR	A
		RET			; or when end of statement found (with Z reset)

; Subroutine to check if character is a binary operator (Z set if so)

m2385:		LD	B,A		; save char
		LD	HL,m2397	; list of operators
m2389:		LD	A,(HL)		; get next
		INC	HL
		OR	A
		JR	Z,m2393		; if end of list, exit with Z reset
		CP	B
		JR	NZ,m2389	; loop back if no match
		LD	A,B		; restore char
		RET			; exit with Z set
m2393:		OR	$FF		; reset Z
		LD	A,B		; restore character
		RET

; List of valid binary operators for numeric calculations

m2397:		DM	"+-*/^=><"
		DB	$C7,$C8,$C9	; <=,>=,<>
		DB	$C5,$C6		; OR,AND
		DB	0

; Subroutine to check if a character is a valid function (Z set if so)

m23a5:		CP	$A5
		JR	C,m23b7		; move on if before RND
		CP	$C4
		JR	NC,m23b7	; or after NOT
		CP	$AC
		JR	Z,m23b7		; or if AT
		CP	$AD
		JR	Z,m23b7		; or if TAB
		CP	A		; set Z for valid functions
		RET
m23b7:		CP	$A5		; reset Z
		RET

; Subroutine to check if character is start of a value

m23ba:		LD	B,A		; save character
		OR	$20		; make lowercase
		CP	"a"
		JR	C,m23c7
		CP	"z"+1
		JR	NC,m23c7
		CP	A		; set Z if character is a letter
		RET
m23c7:		LD	A,B
		CP	'.'
		RET	Z		; exit with Z set if "."
		CALL	m23e4		; check for digits
		JR	NZ,m23e1	; if not, junk character & exit
m23d0:		RST	20H		; get next char
		CALL	m23e4
		JR	Z,m23d0		; loop back while still digits
		CP	'.'
		RET	Z		; exit with Z set if "."
		CP	"E"
		RET	Z		; or "E"
		CP	"e"
		RET	Z		; or "e"
		JR	m2385		; else check for a binary operator
m23e1:		OR	$FF		; junk character & reset Z
		RET

; Subroutine to check if A is a digit (Z set if so)

m23e4:		CP	"0"
		JR	C,m23ee
		CP	"9"+1
		JR	NC,m23ee
		CP	A		; set Z if char is a digit
		RET
m23ee:		CP	"0"		; reset Z otherwise
		RET

; The PLAY command

m23f1:		LD	B,$00		; string counter
		RST	18H		; get char
m23f4:		PUSH	BC
		RST	28H
		DW	o1C8C		; get a string
		POP	BC
		INC	B		; increment counter
		CP	","
		JR	NZ,m2401	; move on if no more strings
		RST	20H		; get next char
		JR	m23f4		; loop back
m2401:		LD	A,B
		CP	$09
		JR	C,m240a		; up to 9 strings allowed
		CALL	m2ada
		DB	$2B		; error "Too many tied notes"
m240a:		CALL	m10b1		; check for end-of-statement
		JP	m1571		; go to execute command

; Subroutine called from ROM 0 to initialise DOS & check the status
; of drives on the system, displaying information to the user.

; aca inicializa +3dos
m2410:		CALL	m2b89		; page in DOS workspace (page 7)
		CALL	m32b6
		LD	HL,FLAGS3
		BIT	7,(HL)
		JR	NZ,m2427	; move on if DOS already initialised
		SET	7,(HL)		; signal "DOS initialised"
		CALL	m3f00
		DW	$00A3
		LD	($DF9D),A
m2427:		CALL	m3f00
		DW	$0100
		CALL	m32ee

		IF alternative

			LD	A,16		; colores linea 2 de la barra de
			RST	10H		; estado, no imprimo ningún msg
			LD	A,definklin_lw	; tipo "physical drives", no
			RST	10H		; tiene sentido para mi por que
						; es obvio lo que significan
						; las letras

		ELSE

			LD	HL,m368c
			CALL	m24b5

		ENDIF

m242a:		LD	A,$FF
		LD	HL,$0002	; standard ALERT routine in ROM 2
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_MESSAGE
		CALL	m32ee		; restore TSTACK
		LD	HL,FLAGS3
		RES	4,(HL)		; signal "disk interface not present"

		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DD_INTERFACE
		CALL	m32ee		; restore TSTACK
		LD	A,$30
		JR	NC,m2488
		LD	HL,FLAGS3
		SET	4,(HL)		; signal "disk interface present"
		RES	5,(HL)		; signal "no drive B:"
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DD_ASK_1
		CALL	m32ee		; restore TSTACK
		JR	C,m24a3		; move on if drive B exists
		LD	C,$00
		LD	HL,5
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_MAP_B	; map drive B: to unit 0
		CALL	m32ee		; restore TSTACK
		LD	A,$31
		JR	m2488

m24a3:		LD	HL,FLAGS3
		SET	5,(HL)		; signal "drive B: present"

		LD	A,$32
m2488:		RST	10H
		LD	HL,m369e
		CALL	m24b5
		LD	A,($DF9D)
		ADD	A,$30
		RST	10H
		LD	HL,m36a8
		CALL	m24b5
		LD	HL,$E2A0
		LD	C,$41
		LD	B,$10
m24a2:		LD	A,(HL)
		INC	HL
		OR	(HL)
		INC	HL
		JR	Z,m24b9
		LD	A,(LODDRV)
		CP	C
		LD	E,0
		JR	NZ,m24b2
		LD	E,1

m24b2:
	IF alternative
		LD	A,$14		; prefiero video inverso para
					; mostrar la letra de la unidad
					; por defecto por que en un +3
					; real se ve mejor por composite
	ELSE
		LD	A,$13		; video bright
	ENDIF

		RST	10H

		LD	A,E
		RST	10H
		LD	A,C
		RST	10H

m24b9:		INC	C
		DJNZ	m24a2

	IF alternative
		CALL	rest_ink
	ELSE
		CALL	m2b64		; page in normal memory
	ENDIF

		RET

; Subroutine to print a zero-terminated string

m24b5:		LD	A,(HL)		; get next char
		OR	A
		RET	Z		; exit if zero
		RST	10H
		INC	HL
		JR	m24b5		; back for more

; Drives present messages

n24c7:		CP	$B5
		JR	NZ,m24cc
		RST	20H
m24cc:		CALL	m10b1
		JP	m0a96
		DS	30

; Subroutine used to execute a command line or evaluate an expression

m24f0:		LD	(IY+$00),$FF	; clear error
		LD	(IY+$31),$02	; set lower screen to 2 lines
		LD	HL,ONERR
		PUSH	HL
		LD	(ERR_SP),SP	; set up error stack
		LD	HL,m2560
		LD	(SYNRET),HL	; set error return address
		CALL	m2368		; set interpretation address & get first char
		CALL	m23a5		; test for function
m250c:		JP	Z,m22d0		; if so, evaluate the expression
		CP	"("
		JP	Z,m22d0		; or if bracketed expression
		CP	"-"
		JP	Z,m22d0		; or a unary operator (+ or -)
		CP	"+"
		JP	Z,m22d0
		CALL	m23ba		; check for start of a value (var or number)
		JP	Z,m22d0		; if so, evaluate the expression
		CALL	m2b89		; page in DOS workspace
		LD	A,(process)	; get current process
		CALL	m2b64		; page in normal memory
		CP	$04		; is it the calculator?
m252f:		JP	NZ,m0fbf	; if not, execute it
		CALL	m2371		; is line a single LET statement?
		JP	Z,m0fbf		; if so, execute it
		POP	HL		; unstack ONERR address
		RET			; exit

; The +3-specific error-handling routine
; ONERR jumps here

m253a:		CALL	m2b89		; page in DOS workspace
		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file 0
		CALL	m32ee		; restore TSTACK
		JR	C,m2559		; move on if no error
		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_ABANDON	; abandon file 0
		CALL	m32ee		; restore TSTACK
m2559:		CALL	m2b64		; page back normal memory
		LD	HL,(SYNRET)
		JP	(HL)		; return to syntax return address

; This is one of the syntax return addresses, used when entering a line
; as a direct command

m2560:		BIT	7,(IY+$00)
		JR	NZ,m2567
		RET			; exit if error in line
m2567:		LD	HL,(E_LINE)
		LD	(CH_ADD),HL	; reset CH_ADD to editing line
		RST	28H
		DW	o19FB		; get line number of editing line
		LD	A,B
		OR	C
		JP	NZ,m268e	; move on if it exists, to add to program
		RST	18H		; get character
		CP	$0D
		RET	Z		; exit if empty line
		CALL	m22c7		; clear display if necessary
		BIT	6,(IY+$02)
		JR	NZ,m2585
		RST	28H
		DW	o0D6E		; clear lower screen if necessary
m2585:		RES	6,(IY+$02)	; signal "lower screen clear"
		CALL	m2b89		; page in DOS workspace
		LD	HL,ed_flags
		BIT	6,(HL)		; ???
		JR	NZ,m259e
		INC	HL
		LD	A,(HL)		; ???
		CP	$00
		JR	NZ,m259e
		CALL	m3e80
		DW	l1a8e
m259e:		CALL	m2b64		; page in normal memory
		LD	HL,TV_FLAG
		RES	3,(HL)		; signal "mode hasn't changed"
		LD	A,$19
		SUB	(IY+$4F)
		LD	(SCR_CT),A	; set appropriate scroll count
		SET	7,(IY+$01)	; signal "execution"
		LD	(IY+$0A),$01	; jump to statement 1
		LD	HL,n3e00
		PUSH	HL		; stack GOSUB stack end marker
		LD	HL,ONERR
		PUSH	HL		; stack error address
		LD	(ERR_SP),SP	; reset ERR_SP
		LD	HL,m25cb
		LD	(SYNRET),HL	; store execution error handler address
		JP	m1048		; execute immediate command

; This is one of the syntax return addresses, used during execution

m25cb:		LD	SP,(RAMTOP)
		INC	SP		; clear return stack
		LD	HL,TSTACK
		LD	(OLDSP),HL	; set OLDSP to temporary stack area
		NOP
		RES	5,(IY+$01)	; signal no key available
		LD	A,(ERR_NR)
		INC	A		; A=error code
m25df:		PUSH	AF		; save error code
		LD	HL,$0000
		LD	(IY+$37),H	; clear FLAGX
		LD	(IY+$26),H	; clear high byte of X_PTR
		LD	(DEFADD),HL	; clear DEFADD
		LD	HL,$0001
		LD	(STRMS+6),HL	; reset stream 0
		RST	28H
		DW	o16B0		; clear editing areas and calculator etc
		RES	5,(IY+$37)	; ???
		RST	28H
		DW	o0D6E		; clear lower screen
		SET	5,(IY+$02)	; signal "clear lower screen after keystroke"
		POP	AF		; get back error code
		LD	B,A		; save it
		CP	$0A
		JR	C,m2614		; move on if 0-9
		CP	$1D
		JR	C,m2612		; move on if A-R
		CP	$2C
		JR	NC,m261a	; move on if +3DOS error
		ADD	A,$14		; else convert for errors a-o
		JR	m2614
m2612:		ADD	A,$07		; convert to code to letter
m2614:		RST	28H
		DW	o15EF		; output error character (0-9 or A-R or a-o)
		LD	A," "
		RST	10H		; output space
m261a:		LD	A,B		; get back error code
		CP	$1D
		JR	C,m2631		; move on if old 48K Spectrum error
		SUB	$1D
		LD	B,$00
		LD	C,A
		LD	HL,m2705
		ADD	HL,BC
		ADD	HL,BC		; HL points to address of error message
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; DE=error message address
		CALL	m2ace		; output it
		JR	m2637
m2631:		LD	DE,o1391	; base address of ROM 3 message table
		RST	28H
		DW	o0C0A		; output 48K Spectrum error message
m2637:		XOR	A
		LD	DE,o1536
		RST	28H
		DW	o0C0A		; output "comma" message
		LD	BC,(PPC)	; get error line number
		RST	28H
		DW	o1A1B		; output it
		LD	A,":"
		RST	10H		; output ":"
		LD	C,(IY+$0D)	; get error statement number
		LD	B,$00
		RST	28H
		DW	o1A1B		; output it
		RST	28H
		DW	o1097		; clear editing area/workspace
		LD	A,(ERR_NR)
		INC	A
		JR	Z,m2674		; move on if error "OK"
		CP	$09
		JR	Z,m2661		; move on if error "9 - STOP statement"
		CP	$15
		JR	NZ,m2664	; move on if not "L - BREAK into program"
m2661:		INC	(IY+$0D)	; increment statement for CONTINUE
m2664:		LD	BC,$0003
		LD	DE,OSPCC
		LD	HL,NSPPC
		BIT	7,(HL)
		JR	Z,m2672
		ADD	HL,BC
m2672:		LDDR
m2674:		LD	(IY+$0A),$FF	; clear NSPPC
		RES	3,(IY+$01)	; signal "K" mode
		LD	HL,FLAGS3
		RES	0,(HL)		; ???
		CALL	m3e80
		DW	l067b
m2686:		LD	A,$10		; error G - no room for line
		LD	BC,$0000
		JP	m25df		; loop back

; Routine to ???

m268e:		LD	(E_PPC),BC	; ???
		CALL	m2b89		; page in DOS workspace
		LD	A,B
		OR	C
		JR	Z,m26a1
		LD	(E_PPC),BC	; ???
		LD	($EC08),BC
m26a1:		CALL	m2b64		; page in normal memory
		LD	HL,(CH_ADD)
		EX	DE,HL
		LD	HL,m2686	; error return address (no room)
		PUSH	HL
		LD	HL,(WORKSP)
		SCF
		SBC	HL,DE
		PUSH	HL		; HL=line length
		LD	H,B
		LD	L,C
		RST	28H
		DW	o196E		; get address of line in program
		JR	NZ,m26c0	; if line not in program yet, move on
		RST	28H
		DW	o19B8		; get address of next line
		RST	28H
		DW	o19E8		; delete the existing line
m26c0:		POP	BC		; restore line length
		LD	A,C
		DEC	A
		OR	B
		JR	NZ,m26db	; move on if no line body (just deleting)
		CALL	m2b89		; page in DOS workspace
		PUSH	HL
		LD	HL,(E_PPC)
		CALL	m3e80
		DW	l1418
		LD	(E_PPC),HL
		POP	HL
		CALL	m2b64		; page in normal memory
		JR	m2703
m26db:		PUSH	BC
		INC	BC
		INC	BC
		INC	BC
		INC	BC
		DEC	HL
		LD	DE,(PROG)
		PUSH	DE
m26e6:		RST	28H
		DW	o1655		; make space for ???
		POP	HL
m26ea:		LD	(PROG),HL
		POP	BC
		PUSH	BC
		INC	DE
		LD	HL,(WORKSP)
		DEC	HL
		DEC	HL
		LDDR
		LD	HL,(E_PPC)
		EX	DE,HL
		POP	BC
		LD	(HL),B
		DEC	HL
		LD	(HL),C
		DEC	HL
		LD	(HL),E
		DEC	HL
		LD	(HL),D
m2703:		POP	AF		; ???
		RET

; Table of error message addresses

m2705:		DW	m276d
		DW	m2778
		DW	m2787
		DW	m2791
		DW	m27a2
		DW	m27b5
		DW	m27c1
		DW	m27c1
		DW	m27d4
		DW	m27e2
		DW	m27f3
		DW	m2804
		DW	m2812
		DW	m2823
		DW	m282f
		DW	m2842
		DW	m2842
		DW	m284e
		DW	m285c
		DW	m286b
		DW	m2879
		DW	m288c
		DW	m289d
		DW	m28a6
		DW	m28b4
		DW	m28c5
		DW	m28d2
		DW	m28e5
		DW	m28fd
		DW	m290b
		DW	m2913
		DW	m291f
		DW	m2933
		DW	m293f
		DW	m294e
		DW	m2965
		DW	m296e
		DW	m297c
		DW	m2983
		DW	m2997
		DW	m29af
		DW	m29c1
		DW	m29d6
		DW	m29e6
		DW	m29f7
		DW	m2a0f
		DW	m2a29
		DW	m2a42
		DW	m2a59
		DW	m2a74
		DW	m2a8a
		DW	m2a97
		DW	m36fb
		DW	m370c
		DW	m3724
		DW	m3733
		DW	m3741
		DW	m3759
		DW	m376d
		DW	m3781
		DW	m378d
		DW	m379e
		DS	18

; The +3 BASIC and +3DOS error messages

m276d:		DM	"MERGE erro", "r"+$80
m2778:		DM	"Wrong file typ", "e"+$80
m2787:		DM	"CODE erro", "r"+$80
m2791:		DM	"Too many bracket", "s"+$80
m27a2:		DM	"File already exist", "s"+$80
m27b5:		DM	"Invalid nam", "e"+$80
m27c1:		DM	"File does not exis", "t"+$80
m27d4:		DM	"Invalid devic", "e"+$80
m27e2:		DM	"Invalid baud rat", "e"+$80
m27f3:		DM	"Invalid note nam", "e"+$80
m2804:		DM	"Number too bi", "g"+$80
m2812:		DM	"Note out of rang", "e"+$80
m2823:		DM	"Out of rang", "e"+$80
m282f:		DM	"Too many tied note", "s"+$80
m2842:		DM	"Bad filenam", "e"+$80
m284e:		DM	"Bad parameter", "s"+$80
m285c:		DM	"Drive not foun", "d"+$80
m286b:		DM	"File not foun", "d"+$80
m2879:		DM	"File already exist", "s"+$80
m288c:		DM	"End of file foun", "d"+$80
m289d:		DM	"Disk ful", "l"+$80
m28a6:		DM	"Directory ful", "l"+$80
m28b4:		DM	"File is read onl", "y"+$80
m28c5:		DM	"File not ope", "n"+$80
m28d2:		DM	"File already in us", "e"+$80
m28e5:		DM	"No rename between drive", "s"+$80
m28fd:		DM	"Missing exten", "t"+$80
m290b:		DM	"Uncache", "d"+$80
m2913:		DM	"File too bi", "g"+$80
m291f:		DM	"Disk is not bootabl", "e"+$80
m2933:		DM	"Drive in us", "e"+$80
m293f:		DM	"Drive not read", "y"+$80
m294e:		DM	"Disk is write protecte", "d"+$80
m2965:		DM	"Seek fai", "l"+$80
m296e:		DM	"CRC data erro", "r"+$80
m297c:		DM	"No dat", "a"+$80
m2983:		DM	"Missing address mar", "k"+$80
m2997:		DM	"Unrecognised disk forma", "t"+$80
m29af:		DM	"Unknown disk erro", "r"+$80
m29c1:		DM	"Disk has been change", "d"+$80
m29d6:		DM	"Unsuitable medi", "a"+$80
m29e6:		DM	"Invalid attribut", "e"+$80
m29f7:		DM	"Cannot copy to/from tap", "e"+$80
m2a0f:		DM	"Destination cannot be wil", "d"+$80
m2a29:		DM	"Destination must be driv", "e"+$80
m2a42:		DM	"Drive B: is not presen", "t"+$80
m2a59:		DM	"+2A does not support forma", "t"+$80
m2a74:		DM	"Drive must be A: or B", ":"+$80
m2a8a:		DM	"Invalid driv", "e"+$80
m2a97:		DM	"Code length erro", "r"+$80

; Subroutine to output an error message (terminated by
; a byte with bit 7 set)
; Enter with DE=message address

m2ace:		LD	A,(DE)		; get next char
		AND	$7F		; mask bit 7
m2ad1:		PUSH	DE
		RST	10H		; output
		POP	DE
		LD	A,(DE)
m2ad5:		INC	DE
		ADD	A,A		; check bit 7
		JR	NC,m2ace	; loop back if not set
		RET

; The Error Handling routine
; Enter here with inline error code-1

m2ada:		POP	HL		; get address of error code
		LD	SP,(ERR_SP)	; reset SP
		LD	A,(HL)
		LD	(RAMERR),A	; store error number-1
		INC	A		; get error code
		CP	$1E
		JR	NC,m2aeb	; move on if a +3-specific error
m2ae8:		RST	28H
		DW	RAMRST		; else call ROM 3 error handler
m2aeb:		DEC	A
		LD	(IY+$00),A	; save code in ERR_NR
m2aef:		LD	HL,(CH_ADD)	; get address at which error occurred
		LD	(X_PTR),HL	; save it
		RST	28H
		DW	o16C5		; clear calculator stack
		RET			; exit to error address

; Subroutine to test the BREAK key
; Terminates with error L - BREAK into program if so

m2af9:		LD	A,$7F
		IN	A,($FE)
		RRA
		RET	C		; exit if SPACE not pressed
		LD	A,$FE
		IN	A,($FE)
		RRA
		RET	C		; or if CAPS not pressed
		CALL	m2ada
		DB	$14		; error L

; Subroutine to execute routine at HL, returning to m3a1b in order to
; return control to ROM 3
; It is provided to allow serial input into 48K BASIC, but looks buggy

m2b09:		RST	18H
		CP	$23
		JR	Z,m2b14
		CALL	m10b1
		JP	m1266
m2b14:		CALL	m2b35
		LD	B,0
m2b19:		CALL	m3f00
		DW	$0062
		PUSH	DE
		PUSH	HL
		POP	BC
		RST	28H
		DW	o2D2B
		POP	BC
		RST	28H
		DW	o2D2B
		LD	DE,m2b56
		LD	BC,10
		CALL	m14f0
		RST	28H
		DW	o2AFF
		RET
m2b35:		RST	20H
		RST	28H
		DW	o1C82
		RST	18H
		CP	","
		JP	NZ,m22c3
		RST	20H
		RST	28H
		DW	o1C1F
		BIT	6, (IY+$01)
		JP	Z,m22c3
		POP	HL
		CALL	m10b1
		PUSH	HL
		RST	28H
		DW	o1E94
		RST	28H
		DW	o1601
		RET
m2b56:		RST	28H
		DW	$4034
		LD	B,C
		NOP
		NOP
		INC	B
		RRCA
		DB	$38
		RET
		DS	4

; Subroutine to page in normal memory (page 0) and swap SP with OLDSP

m2b64:		EX	AF,AF'		; save AF
		LD	A,$10
		DI
		CALL	m2b7e		; page in page 0
		POP	AF		; AF=return address
		LD	(TARGET),HL	; save HL
		LD	HL,(OLDSP)
		LD	(OLDSP),SP
		LD	SP,HL		; SP now swapped with OLDSP
		EI
		LD	HL,(TARGET)	; restore HL
		PUSH	AF		; restack return address
		EX	AF,AF'		; restore AF
		RET

; Subroutine to page in a RAM/ROM/screen combination (in A)

m2b7e:		PUSH	BC
		LD	BC,PBANKM
		OUT	(C),A		; page it in
		LD	(BANKM),A
		POP	BC
		RET

; Subroutine to page in DOS workspace (page 7) and swap SP with OLDSP

m2b89:		EX	AF,AF'		; save AF
		DI
		POP	AF		; AF=return address
		LD	(TARGET),HL	; save HL
		LD	HL,(OLDSP)
		LD	(OLDSP),SP
		LD	SP,HL		; SP swapped with OLDSP
		LD	HL,(TARGET)	; restore HL
		PUSH	AF		; push back return address
		LD	A,$17
		CALL	m2b7e		; page in page 7
		EI
		EX	AF,AF'		; restore AF
		RET

; Subroutine to copy a file
; Enter with A=destination flag
; A=$00 - file
;  =$E0 - printer
;  =$AA - screen
; Z flag set if A=$00
; C flag reset if copying TO SPECTRUM FORMAT

m2ba3:		CALL	m2b89		; page in DOS workspace
		LD	(dst_dev),A	; save destination flag
		PUSH	AF
		JR	Z,m2bb2		; move on if copying to a file
		CP	$E0		; copy to LPRINT?
		LD	A,$03		; use stream 3
		JR	Z,m2bb4
m2bb2:		LD	A,$02		; use stream 2
m2bb4:		CALL	m2b64		; page in normal memory
		RST	28H
		DW	o1601		; open channel to stream
		CALL	m2b89		; page in DOS workspace
		POP	AF		; restore destination flag
		JR	Z,m2c1e		; move on if copying to another file
		LD	HL,tmp_fspec	; stored filename address
		LD	BC,$0001	; use file number 0,exclusive-read
		LD	DE,$0001	; open action - error if doesn't exist
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; open file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error opening
m2bd7:		LD	B,$00		; file 0
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_BYTE_READ	; read a byte
		CALL	m32ee		; restore TSTACK
		JR	C,m2bed		; move on if no error
		CP	$19
		JP	NZ,m3219	; move on if not end-of-file error
		JR	m2c0a		; end of file, so move on to close file
m2bed:		LD	A,(dst_dev)	; check destination flag
		CP	$AA
		LD	A,C		; A=byte from file
		JR	NZ,m2bff	; move on unless copying to screen
		CP	$0D
		JR	Z,m2bff		; okay to output CRs
		CP	$20
		JR	NC,m2bff
		LD	A,$20		; but replace other control chars with spaces
m2bff:		CALL	m2b64		; page in normal memory
		RST	28H
		DW	$0010		; output byte
m2c05:		CALL	m2b89		; page in DOS workspace
		JR	m2bd7		; back for more
m2c0a:		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error closing
		CALL	m2b64		; page in normal memory
		RET			; done

; This part of the copy routine copies a file to another file

m2c1e:		PUSH	AF		; save destination flag
		LD	HL,tmp_fspec
		LD	(dst_add),HL	; store address of destination filespec
		LD	DE,dst_file
		CALL	m3109		; copy filespec, error if too long
		PUSH	HL		; save address of source filespec
		LD	A,$FF
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_DRIVE	; set default drive
		CALL	m32ee		; restore TSTACK
		LD	(dst_drv),A	; use default drive for destination
		LD	(src_drv),A	; use default drive for source
		LD	HL,dst_file
		CALL	m30f0		; is destination drive specified?
		JR	NZ,m2c4d	; move on if not
		LD	DE,dst_drv
		CALL	m30e3		; place drive letter at dst_drv
m2c4d:		POP	HL		; restore address of source filespec
		POP	AF		; restore destination flag
		JP	NC,m3123	; move on if copying TO SPECTRUM FORMAT
		LD	(src_add),HL	; save address of source filespec
		LD	DE,src_file
		CALL	m3109		; copy filespec, error if too long
		LD	HL,src_file
		CALL	m30f0		; is source drive specified?
		JR	NZ,m2c69	; move on if not
		LD	DE,src_drv
		CALL	m30e3		; place drive letter at src_drv
m2c69:		LD	(SCR_CT),A	; zeroise scroll count
		LD	A,$0D
		RST	10H		; output CR
		XOR	A
		LD	(wild),A	; clear "wild" flag
		LD	(copied),A	; zero # files copied
		LD	(dst_dev),A	; destination is a file
		LD	HL,dst_file
		CALL	m30b6		; check if destination wild
		LD	A,(wild)
		OR	A
		JR	Z,m2c8c
		CALL	m2b64		; if so, page in normal memory
		CALL	m2ada		; and cause error "destination cannot be wild"
		DB	$49
m2c8c:		LD	HL,m3283
		LD	DE,tmp_file
		LD	BC,$000E
		LDIR			; copy temporary filename
		LD	HL,src_file
		CALL	m30b6		; check if source wild
		LD	A,(wild)
		OR	A
		JR	NZ,m2ca9	; move on if so
		CALL	m2d58		; copy a single file
		JP	m2d26		; finish up
m2ca9:		LD	HL,(dst_add)	; get address of dest filespec
		CALL	m30f0		; get past drive
		LD	A,$FF
		CP	(HL)
		JR	Z,m2cbb		; move on if just drive
		CALL	m2b64		; page in normal memory
		CALL	m2ada
		DB	$4A		; else error "destination must be drive"
m2cbb:		LD	HL,wld_next
		XOR	A
		LD	B,$0D
m2cc1:		LD	(HL),A		; zeroise directory entry 1
		INC	HL
		DJNZ	m2cc1
m2cc5:		LD	HL,wld_next
		LD	DE,cat_spec
		LD	BC,$000D
		LDIR			; and zeroise directory entry 0
		LD	HL,(src_add)
		LD	BC,$0200	; 1 entry required, include system files
		LD	DE,cat_spec
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CATALOG	; get next filename
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	HL,dst_dev
		LD	A,(HL)
		OR	A
		JR	NZ,m2cf7	; move on if not copying first file
		INC	A
		LD	(HL),A		; set "first file copied" flag
		LD	A,$17
		DEC	B
		JP	Z,m3219		; cause error "File not found" if none
		INC	B
m2cf7:		DEC	B		; B=0 if no more matches
		JR	Z,m2d26		; move to finish if done
		LD	HL,src_file
		CALL	m30f0		; get past drive of source
		EX	DE,HL
		LD	HL,wld_next	; address of found entry
		LD	B,$08
		CALL	m30d9		; copy filename part into source
		LD	HL,wld_next+8	; get to extension of found entry
		LD	A,'.'
		LD	(DE),A		; insert "."
		INC	DE
		LD	B,$03
		CALL	m30d9		; copy extension part into source
		LD	A,$FF
		LD	(DE),A		; insert terminator
		LD	HL,dst_file
		CALL	m30f0		; get past drive name in dest
		LD	(HL),$FF	; insert terminator
		CALL	m2d58		; copy a file
		JP	m2cc5		; loop back for more

; Copy file routines - end part

m2d26:		LD	HL,tmp_file
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_DELETE	; delete temp file
		CALL	m32ee		; restore TSTACK
		LD	A,(copied)
		DEC	A		; A=# files copied-1
		LD	HL,m3291	; "1 file copied" message
		JR	Z,m2d4c		; move on if 1
		INC	A
		LD	L,A
		LD	H,$00		; HL=# files copied
		LD	A,$0D
		RST	10H		; output CR
		LD	E," "
		CALL	m0800		; output #
		LD	HL,m32a5	; "files copied" message
m2d4c:		CALL	m3268		; output message
		LD	A,$17
		LD	(SCR_CT),A	; set scroll count
		CALL	m2b64		; page in normal memory
		RET			; done!

; Subroutine to copy a single file

m2d58:		LD	HL,dst_file	; dest filespec
		LD	DE,src_file	; source filespec
m2d5e:		LD	A,(DE)
		CP	(HL)		; compare filespecs
		JR	NZ,m2d72	; move on if different
		LD	A,$FF
		CP	(HL)
		JR	NZ,m2d6e
		CALL	m2b64		; page in normal memory
		CALL	m2ada
		DB	$30		; error if filespecs the same
m2d6e:		INC	HL		; increment pointers
		INC	DE
		JR	m2d5e		; loop back
m2d72:		LD	HL,dst_file
		CALL	m30f0		; move past drive specifier in dest filespec
		LD	A,(HL)
		CP	$FF
		JR	NZ,m2d94	; move on if a destination filename specified
		LD	HL,dst_file
		CALL	m30f0
		PUSH	HL		; store address after drive specifier
		LD	HL,src_file
		CALL	m30f0		; get to filename of source
		POP	DE
m2d8b:		LD	A,(HL)
		LD	(DE),A		; copy filename to destination
		INC	A
		JR	Z,m2d94		; move on when done
		INC	DE
		INC	HL
		JR	m2d8b		; loop back
m2d94:		XOR	A
		LD	(copy_ram),A	; signal "copy via M:"
		LD	A,"M"
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_FREE_SPACE	; find free space on drive M:
		CALL	m32ee		; restore TSTACK
		JP	NC,m2dc7
		LD	A,H
		OR	A
		JR	Z,m2daf
		LD	HL,$003F	; use max 63K on drive M:
m2daf:		LD	A,L
		CP	$40
		JR	C,m2db7
		LD	HL,$003F	; use max 63K on drive M:
m2db7:		LD	H,L
		LD	L,$00
		ADD	HL,HL
		ADD	HL,HL
		LD	(free_m),HL	; store free bytes on drive M:
		LD	DE,o0800
		OR	A
		SBC	HL,DE
		JR	NC,m2dd1	; move on if >=2K free
m2dc7:		LD	A,$FF
		LD	(SCR_CT),A	; set scroll count
		LD	A,$01
		LD	(copy_ram),A	; signal "copy via RAM"
m2dd1:		XOR	A
		LD	(dst_open),A	; signal no temporary file open
		LD	(eof),A		; signal not EOF
		LD	HL,dst_file
		CALL	m30f0		; get past drive of dest
		LD	A,(HL)
		CP	$FF
		JP	NZ,m2e5d	; if dest filename specified, jump on
		LD	HL,src_file
		CALL	m30f0		; get past drive of source
		LD	A,(HL)
m2deb:		CP	$FF
		JP	NZ,m2e5d	; if source filename specified, jump on
		LD	A,(dst_drv)	; check destination drive
		CP	"M"
		JP	Z,m2e5d		; move on if M: (will fail on attempted copy)
		LD	A,(src_drv)	; check source drive
		CP	"M"
		JP	Z,m2e5d		; move on if M: (will fail on attempted copy)
		LD	A,"A"		; by this stage, we must be copy A:<-->B:
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_REF_XDPB	; get XDPB for drive A:
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	C,0
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DD_LOGIN	; login disk in drive A:
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		OR	A
		LD	A,$06
		JP	NZ,m3219	; cause error 6 if not a standard +3 disk
		LD	A,(src_drv)	; get source drive letter
		LD	BC,$0001
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPENDRIVE	; open source drive as exclusive-read file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,(dst_drv)	; get dest drive letter
		LD	BC,$0102
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPENDRIVE	; open dest drive as exclusive-write file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,$01
		LD	(dst_open),A	; signal temporary file open
		LD	A,(copy_ram)
		OR	A
		JP	Z,m2f44		; copy via M: if >=2K free on drive M:
		JP	m2ecd		; else copy via RAM
m2e5d:		LD	HL,src_file	; source name
		LD	A,$FF
		LD	(SCR_CT),A	; set max scroll count
		PUSH	HL
		PUSH	HL
		CALL	m3268		; display filespec
		POP	DE
		EX	DE,HL
		OR	A
		SBC	HL,DE
		LD	DE,$0011
		ADD	HL,DE
		LD	B,L		; B=# spaces required
m2e74:		PUSH	BC
		LD	A," "
		RST	10H		; output a space
		POP	BC
		DJNZ	m2e74		; loop back
		POP	HL
		LD	A,(dst_drv)	; get dest drive letter
		OR	$20		; make lowercase
		CP	"m"
		JR	Z,m2e95		; move on if copying to M:
		LD	A,(copy_ram)
		OR	A
		JR	NZ,m2e95	; or if >=2K free on M:
		LD	A,(src_drv)	; get source drive letter
		OR	$20
		CP	"m"
		JP	NZ,m2f2d	; if not copying from M:, move on
m2e95:		LD	HL,src_file	; source filename
		LD	BC,$0001	; file 0,excl read
		LD	DE,$0002	; must be openable
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	HL,dst_file	; dest filename
		PUSH	HL
		CALL	m3268		; display filename
		POP	HL
		LD	BC,$0102	; file 1, exc write
		LD	DE,$0204	; create new file
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,$01
		LD	(dst_open),A	; signal temporary file open

; Subroutine to copy everything from file 0 to file 1, via a 2K area
; in page 0 (bug: this should be page 7!)
m2ecd:		LD	BC,$0007	; file 0, page 0 (oops, should be page 7!)
		LD	DE,o0800	; 2K to read
		LD	HL,tmp_buff	; address to read
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_READ	; read bytes
		CALL	m32ee		; restore TSTACK
		JR	C,m2f08		; move on if no error
		CP	$19
		JP	NZ,m3219	; if error not end-of-file, cause error
		LD	A,$01
		LD	(eof),A		; signal end-of-file reached
m2eed:		PUSH	DE		; save # unread bytes
		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file 0
		CALL	m32ee		; restore TSTACK
		POP	DE
		JP	NC,m3219	; move on if error
		LD	HL,o0800
		OR	A
		SBC	HL,DE
		EX	DE,HL		; DE=number of bytes read
		JR	m2f0b		; move on
m2f08:		LD	DE,o0800	; DE=2048 bytes read
m2f0b:		LD	A,E
		OR	D
		JR	Z,m2f23		; if no bytes read, move on
		LD	HL,tmp_buff
		LD	BC,$0107
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_WRITE	; write bytes to file 1 from page 0 (oops)
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
m2f23:		LD	A,(eof)
		OR	A
		JP	Z,m2ecd		; loop back if not end-of-file
		JP	m309e		; close file 1 and exit

; Continuation of copy command, where M: is involved

m2f2d:		LD	HL,src_file	; source filename
		LD	BC,$0001	; file 0, excl read
		LD	DE,$0002	; must be openable
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error

; Subroutine to copy everything from file 0 to file 1, via a temporary
; file in drive M:
; Each 2K is read via RAM in page 0 - this should be page 7 (oops!)

m2f44:		LD	HL,tmp_file	; temporary filename
		LD	BC,$0203	; file 2, exclusive read-write mode
		LD	DE,$0204	; open & create actions
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; open temporary file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	HL,$0000
		LD	(tmp_bytes),HL	; zero # bytes copied to temp file
m2f61:		LD	BC,$0007
		LD	DE,o0800
		LD	HL,tmp_buff
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_READ	; read 2K into RAM
		CALL	m32ee		; restore TSTACK
		JR	C,m2f9c		; move on if no error
		CP	$19
		JP	NZ,m3219	; cause error if it wasn't end-of-file
		LD	A,$01
		LD	(eof),A		; signal end-of-file reached
		PUSH	DE		; save # unread bytes
		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file 0
		CALL	m32ee		; restore TSTACK
		POP	DE
		JP	NC,m3219	; move on if error
		LD	HL,o0800
		OR	A
		SBC	HL,DE
		EX	DE,HL		; DE=# bytes read
		JR	m2f9f
m2f9c:		LD	DE,o0800	; DE=2048 bytes read
m2f9f:		LD	A,E
		OR	D
		JR	Z,m2fb9		; move on if no bytes read
		PUSH	DE
		LD	HL,tmp_buff
		LD	BC,$0207
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_WRITE	; write bytes to temporary file
		CALL	m32ee		; restore TSTACK
		POP	DE
		JP	NC,m3219	; move on if error
m2fb9:		LD	HL,(tmp_bytes)
		ADD	HL,DE
		LD	(tmp_bytes),HL	; update number of bytes copied to temp file
		LD	DE,o0800
		ADD	HL,DE
		EX	DE,HL
		LD	HL,(free_m)
		LD	A,(eof)
		OR	A
		JR	NZ,m2fd2	; move on if end-of-file reached
		SBC	HL,DE
		JR	NC,m2f61	; loop back if temp file can take 2K more
m2fd2:		LD	A,(src_drv)
		AND	$DF		; get source drive (capitalised)
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_FLUSH	; flush for source drive
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	B,$02
		LD	HL,$0000
		LD	E,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_SET_POSITION	; get to start of temp file
		CALL	m32ee		; restore TSTACK
		LD	A,(dst_open)
		OR	A
		JR	NZ,m301e	; move on if dst_file contains spec of temp file
		LD	HL,dst_file
		PUSH	HL
		CALL	m3268		; else display filespec
		POP	HL
		LD	BC,$0102
		LD	DE,$0204
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; open file 1 in exclusive-write mode
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,$01
		LD	(dst_open),A	; signal dest file is open
m301e:		LD	HL,tmp_buff
		LD	DE,o0800
		LD	BC,$0207
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_READ	; read 2K from temp file
		CALL	m32ee		; restore TSTACK
		LD	HL,o0800	; HL=o0800 bytes read
		JR	C,m3042		; move on if no error
		CP	$19
		JP	NZ,m3219	; cause non-EOF errors
		LD	HL,o0800
		OR	A
		SBC	HL,DE		; HL=# bytes read
m3042:		EX	DE,HL
		LD	BC,$0107
		LD	HL,tmp_buff
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_WRITE	; write bytes to file 1
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	HL,(tmp_bytes)
		LD	DE,o0800
		OR	A
		SBC	HL,DE
		JR	C,m3069		; move on if temp file empty
		LD	A,H
		OR	L
		LD	(tmp_bytes),HL	; update bytes left in temp file
		JR	NZ,m301e	; loop back to copy more
m3069:		LD	B,$02
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close temp file
		CALL	m32ee		; restore TSTACK
		LD	A,(dst_drv)
		AND	$DF		; get dest drive (capitalised)
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_FLUSH	; flush dest drive
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,(eof)
		OR	A
		JP	Z,m2f44		; loop back if not EOF
		LD	HL,tmp_file
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_DELETE	; delete temp file
		CALL	m32ee		; restore TSTACK

; Enter here if copying via 2K area in RAM

m309e:		LD	B,$01
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close file 1
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,$0D
		RST	10H		; output CR
		LD	HL,copied
		INC	(HL)		; increment # files copied
		RET

; Subroutine to check whether filespec at HL is wild
; Causes error if filespec longer than $11 (inc terminator)

m30b6:		LD	B,$11
		LD	A,(HL)
		CP	"?"
		JR	NZ,m30c4	; move on if not ? wildcard
		PUSH	AF
		LD	A,$01
		LD	(wild),A	; set wildcard flag
		POP	AF
m30c4:		CP	"*"
		JR	NZ,m30cf	; move on if not * wildcard
		PUSH	AF
		LD	A,$01
		LD	(wild),A	; set wildcard flag
		POP	AF
m30cf:		INC	HL		; increment pointer
		INC	A
		RET	Z		; exit if done
		DJNZ	m30b6		; loop back
		LD	A,$14
		JP	m3219		; cause bad filename error if too long

; Subroutine to copy up to B chars from HL to DE, stopping at first space

m30d9:		LD	A,(HL)		; get next char
		CP	" "
		RET	Z		; exit if space
		LD	(DE),A		; copy char
		INC	HL		; increment pointers
		INC	DE
		DJNZ	m30d9		; loop back
		RET

; Subroutine to get a drive letter from a filespec & place it
; in the address at DE. HL points past the colon of the filespec

m30e3:		DEC	HL
		DEC	HL
		LD	A,(HL)		; get character before colon
		OR	$20		; make lowercase
		CP	"a"
		RET	C		; exit if < 'a'
		CP	'{'
		RET	NC		; or if > 'z'
		LD	(DE),A		; store drive letter
		RET

; Subroutine to check if filespec includes drive specification
; On entry, HL=address of filespec
; On exit, Z flag set if drive specified, and HL points to
; start of filename after colon.

m30f0:		PUSH	HL
		POP	DE		; copy address of filename to DE
		LD	A,(HL)		; get first char
		INC	A
		JR	Z,m3103		; move to exit if end of filename
		LD	B,$03		; check first 3 chars
m30f8:		LD	A,(HL)
		CP	":"		; is char a ":" ?
		JR	Z,m3107		; move on if so
		INC	A
		JR	Z,m3103		; exit if end of filename
		INC	HL
		DJNZ	m30f8		; back for more
m3103:		OR	$FF		; reset Z flag - no drive specified
		EX	DE,HL		; HL=start of filename
		RET
m3107:		INC	HL		; HL=start of filename after drive spec
		RET			; exit with Z set

; Subroutine to copy a $FF-terminated filename from HL to DE
; If max length of $11 (inc terminator) exceeded, cause error

m3109:		LD	B,$11		; allow 17 characters in a filename
		LD	A,(HL)
		LD	(DE),A		; copy filename
		INC	HL		; increment pointers
		INC	DE
		INC	A		; test for end of filename
		JR	Z,m3119		; exit if found
		DJNZ	m3109		; loop back
		LD	A,$14
		JP	m3219		; cause +3DOS error $14, "Bad filename"
m3119:		RET


; Subroutine to clear screen and open channel to stream 2

m311a:		RST	28H
		DW	o0D6B		; cls
		LD	A,$02
		RST	28H
		DW	o1601		; open channel to stream 2
		RET

; Routine to copy files to spectrum format

m3123:		XOR	A
		LD	(wild),A	; no wildcard
		LD	(dst_open),A	; dest not open file
		LD	HL,dst_file
		CALL	m30b6		; is dest filespec wild?
		LD	A,(wild)
		OR	A
		JR	Z,m313d		; move on if not
		CALL	m2b64		; page in normal memory
		CALL	m2ada
		DB	$49		; else error "destination cannot be wild"
m313d:		LD	HL,dst_file
		LD	B,$12
m3142:		LD	A,(HL)
		CP	'.'		; has file got extension?
		INC	HL
		JR	Z,m314b		; move on if so
		INC	A
		JR	NZ,m3142
m314b:		DEC	HL
		EX	DE,HL
		LD	HL,m3214
		LD	BC,$0004	; length 4 misses terminator (oops!)
		LDIR			; copy ".HED" extension
		LD	HL,(dst_add)
		LD	BC,$0001	; file 0, exclusive read
		LD	DE,$0001
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; open source file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	HL,dst_file	; dest filename
		LD	BC,$0102	; file 1, exclusive write
		LD	DE,$0104
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_OPEN	; open dest file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,$01
		LD	(dst_open),A	; signal dest open
		LD	HL,$0000
		LD	(tmp_bytes),HL	; signal 0 bytes copied
m318e:		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_BYTE_READ	; read a byte
		CALL	m32ee		; restore TSTACK
		JR	C,m31a4		; move on if no error
		CP	$19
		JP	NZ,m3219	; cause non-EOF error
		JR	Z,m31bd		; move on
m31a4:		LD	B,$01
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_BYTE_WRITE	; write byte
		CALL	m32ee		; restore TSTACK
		LD	HL,(tmp_bytes)
		INC	HL		; update bytes copied
		LD	(tmp_bytes),HL
		JR	C,m318e		; loop back if no error
		JP	m3219		; cause error
m31bd:		LD	B,$00
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close source file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		LD	A,(dst_drv)
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_FLUSH	; flush dest drive
		CALL	m32ee		; restore TSTACK
		LD	B,$01
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_REF_HEAD	; point at header data for dest file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219
		LD	A,$03
		LD	(IX+$00),A	; set CODE type
		LD	HL,(tmp_bytes)
		LD	(IX+$01),L
		LD	(IX+$02),H	; set length
		XOR	A
		LD	(IX+$03),A	; set load address to zero
		LD	(IX+$04),A
		LD	B,$01
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; close dest file
		CALL	m32ee		; restore TSTACK
		JP	NC,m3219	; move on if error
		CALL	m2b64		; page in normal memory
		RET			; done

m3214:		DM	".HED", $FF

; Routine to close files 0-2, delete temporary files and
; generate the +3DOS error held in A

m3219:		PUSH	AF		; save +3DOS error code
		LD	B,$03		; three files
m321c:		PUSH	BC		; stack counter
		DEC	B		; decrement counter
		PUSH	BC		; stack file number
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_CLOSE	; try to close file B
		CALL	m32ee		; restore TSTACK
		POP	BC		; restore file number
		JR	C,m3238		; move on if closed okay
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_ABANDON	; else abandon it
		CALL	m32ee		; restore TSTACK
m3238:		POP	BC
		DJNZ	m321c		; back for other files
		LD	A,$0D
		RST	10H		; new line on screen
		LD	A,(dst_open)
		OR	A
		JR	Z,m3252		; move on if no temporary file created
		LD	HL,dst_file	; HL=address of temporary filename
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_DELETE	; delete temporary file
		CALL	m32ee		; restore TSTACK
m3252:		LD	HL,tmp_file
		CALL	m32b6		; save TSTACK in page 7
		CALL	m3f00
		DW	DOS_DELETE	; delete other temporary file
		CALL	m32ee		; restore TSTACK
		POP	AF		; restore +3DOS error code
		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause +3DOS error
		DB	$FF

; Subroutine to display filename/message at HL

m3268:		LD	A,(HL)		; get next char
		INC	HL
		OR	A
		RET	Z		; exit if null
		CP	$FF
		RET	Z		; or $FF
		AND	$7F
		RST	10H		; display character
		JR	m3268		; loop back


; Subroutine to get a key (apparently unused)

m3274:		LD	HL,FLAGS
		RES	5,(HL)		; set "no key"
m3279:		BIT	5,(HL)
		JR	Z,m3279		; loop until key available
		RES	5,(HL)		; set "no key"
		LD	A,(LAST_K)	; get it
		RET

; Temporary filespec, used in COPY

m3283:		DM	"M:VAXNSUZ.$$$", $FF

; Files copied messages

		DS	37

; Subroutine to copy TSTACK to a temporary area in page 7, and
; reset SP to use whole of TSTACK again

m32b6:		DI
		LD	(tmp_hl),HL	; save HL
		PUSH	AF
		POP	HL
		LD	(tmp_af),HL	; save AF
		LD	(tmp_de),DE	; save DE
		LD	(tmp_bc),BC	; save BC
		LD	HL,TSTACK
		LD	DE,tmp_stack
		LD	BC,$0084
		LDDR			; copy TSTACK area into page 7
		POP	BC		; BC=return address
		LD	(tmp_sp),SP	; save SP
		LD	HL,TSTACK
		LD	SP,HL		; set SP back to top of TSTACK
		PUSH	BC		; restack return address
		LD	BC,(tmp_bc)	; restore BC
		LD	DE,(tmp_de)	; restore DE
		LD	HL,(tmp_af)
		PUSH	HL
		POP	AF		; restore AF
		LD	HL,(tmp_hl)	; restore HL
		EI
		RET

; Subroutine to restore TSTACK from where it's been saved in a temporary
; area in page 7

m32ee:		DI
		LD	(tmp_hl),HL	; save HL
		PUSH	AF
		POP	HL
		LD	(tmp_af),HL	; save AF
		LD	(tmp_de),DE	; save DE
		LD	(tmp_bc),BC	; save BC
		POP	HL
		LD	(tmp_ret),HL	; save return address
		LD	HL,tmp_stack
		LD	DE,TSTACK
		LD	BC,$0084
		LDDR			; restore TSTACK from saved location
		LD	HL,(tmp_sp)
		LD	SP,HL		; restore SP
		LD	HL,(tmp_ret)
		PUSH	HL		; restack return address
		LD	BC,(tmp_bc)	; restore BC
		LD	DE,(tmp_de)	; restore DE
		LD	HL,(tmp_af)
		PUSH	HL
		POP	AF		; restore AF
		LD	HL,(tmp_hl)	; restore HL
		EI
		RET

; The COPY EXP command

m3328:		XOR	A
		CALL	m2b89		; page in DOS workspace
		LD	(tmp_buff+5),A	; flag "normal copy exp"
		CALL	m2b64		; page in normal memory
		RST	28H
		DW	$0020		; get next character
		CP	$DD
		JR	NZ,x34be	; move on if not INVERSE
		LD	A,$FC
		CALL	m2b89		; page in DOS workspace
		LD	(tmp_buff+5),A	; flag "inverse copy exp"
		CALL	m2b64		; page in normal memory
		RST	28H
		DW	$0020		; get to next char
x34be:		EQU	$
m3347:		CALL	m10b1		; check for end-of-statement
		LD	A,(BANK678)
		LD	BC,PBANK678
		SET	4,A		; set strobe high
		DI
		LD	(BANK678),A
		OUT	(C),A		; output strobe
		EI
		CALL	m2b89		; page in DOS workspace
		DI
		LD	A,$1B		; set DUMPLF/216" linespacing
		CALL	m33b9
		LD	A,"3"
		CALL	m33b9
		LD	HL,DUMPLF
		LD	A,(HL)
		CALL	m33b9
		LD	HL,$401F	; address of top right corner of display
		LD	E,$20		; number of chars per line
m3373:		PUSH	HL
		LD	D,$01		; start with bit 0 pixel
m3376:		PUSH	DE
		PUSH	HL
		LD	HL,m34bf
		CALL	m33c5		; output raster line header
		POP	HL
		POP	DE
		PUSH	HL
m3381:		CALL	m33d1		; output raster data for next two pixels
		LD	A,H
		AND	$07
		INC	H		; get to next pixel line down
		CP	$07
		JR	NZ,m3381	; loop back if still in same character line
		LD	A,H
		SUB	$08		; back to top line of a character
		LD	H,A
		LD	A,L
		ADD	A,$20		; move to next character line down
		LD	L,A
		JR	NC,m3381	; loop back if same screen third
		LD	A,H
		ADD	A,$08		; increment screen third
		LD	H,A
		CP	$58
		JR	NZ,m3381	; loop back if all thirds not done
		POP	HL		; restore top of screen address
		SLA	D
		SLA	D		; shift left two pixels
		JR	NC,m3376	; loop back if within same char
		POP	HL		; restore top of screen address
		DEC	HL		; previous character
		DEC	E
		JR	NZ,m3373	; loop back if not finished
		LD	A,$1B		; reset printer
		CALL	m33b9
		LD	A,'@'
		CALL	m33b9
		EI
		CALL	m2b64		; page in normal memory
		RET			; done

; Subroutine to page in normal memory, output a character to the
; printer, and page back DOS workspace

m33b9:		EI
		CALL	m2b64		; page in normal memory
		CALL	m2051		; output char to printer
		CALL	m2b89		; page in DOS workspace
		DI
		RET

; Subroutine to output a $FF-terminated string to the printer

m33c5:		LD	A,(HL)		; get next char
		CP	$FF
		RET	Z		; exit if $FF
		PUSH	HL
		CALL	m33b9		; output char
		POP	HL
		INC	HL
		JR	m33c5		; loop back

; Subroutine to output 4 raster bytes for the next 2 pixels

m33d1:		PUSH	AF		; save registers
		PUSH	HL
		PUSH	DE
		PUSH	HL
		CALL	m3402		; E=attribute for address in HL
		POP	HL
		CALL	m3412		; clear 4-byte buffer
		CALL	m343f		; copy appropriate pattern for pixel to buffer
		CALL	m341f		; shift pattern left 3 bits
		SLA	D		; shift pixel number
		CALL	m343f		; merge in pattern for next pixel
		CALL	m3432		; shift patterns left 2 bits
		LD	B,$04		; get ready to output 4 raster bytes
		LD	HL,tmp_buff
m33ef:		LD	A,(HL)		; get pattern
		PUSH	BC
		PUSH	HL
		LD	HL,tmp_buff+5
		XOR	(HL)		; invert if required
		CALL	m33b9		; output byte
		POP	HL
		POP	BC
		INC	HL
		DJNZ	m33ef		; loop back
		POP	DE
		POP	HL
		POP	AF
		RET

; Subroutine to get attribute byte in E for screen address in HL

m3402:		PUSH	AF
		LD	A,H
		AND	$18
		SRL	A
		SRL	A
		SRL	A
		OR	$58		; address attribs
		LD	H,A
		LD	E,(HL)		; get attrib
		POP	AF
		RET

; Subroutine to clear a 4-byte area at tmp_buff

m3412:		PUSH	HL
		LD	HL,tmp_buff
		LD	B,$04
m3418:		LD	(HL),$00	; clear the buffer
		INC	HL
		DJNZ	m3418
		POP	HL
		RET

; Subroutine to shift patterns in buffer left 3 bits

m341f:		PUSH	HL
		PUSH	BC
		LD	HL,tmp_buff
		LD	B,$04
m3426:		SLA	(HL)		; shift left
		SLA	(HL)
		SLA	(HL)
		INC	HL
		DJNZ	m3426
		POP	BC
		POP	HL
		RET

; Subroutine to shift patterns in buffer left 2 bits

m3432:		LD	HL,tmp_buff
		LD	B,$04
m3437:		SLA	(HL)		; shift left
		SLA	(HL)
		INC	HL
		DJNZ	m3437
		RET

; Subroutine to merge required pattern for pixel into buffer at tmp_buff

m343f:		PUSH	DE		; save registers
		PUSH	HL
		LD	A,D
		AND	(HL)		; mask required pixel
		LD	A,E		; A=attribute
		JR	NZ,m344c	; move on if need ink
		SRL	A		; shift paper colour to ink position
		SRL	A
		SRL	A
m344c:		AND	$07		; mask off ink/paper colour as required
		BIT	6,E		; check BRIGHT
		JR	Z,m3454
		OR	$08		; add 8 if bright
m3454:		LD	HL,m346f	; address of colour offsets table
		LD	D,$00
		LD	E,A
		ADD	HL,DE
		LD	E,(HL)		; DE=offset into pattern table
		LD	HL,m347f
		ADD	HL,DE		; HL=required pattern address
		LD	B,$04
		LD	DE,tmp_buff
m3465:		LD	A,(DE)
		OR	(HL)
		LD	(DE),A		; merge pattern into buffer
		INC	HL
		INC	DE
		DJNZ	m3465
		POP	HL		; restore registers
		POP	DE
		RET

; Table of offsets into following pattern table

m346f:		DB	$00,$04,$08,$0C
		DB	$10,$14,$18,$1C
		DB	$20,$24,$28,$2C
		DB	$30,$34,$38,$3C

; Pattern table for expanded copy

m347f:		DB	$07,$07,$07,$07	; black
		DB	$07,$05,$07,$07	; blue
		DB	$03,$07,$06,$07	; red
		DB	$07,$03,$06,$03	; magenta
		DB	$06,$03,$06,$03	; green
		DB	$06,$05,$02,$05	; cyan
		DB	$02,$05,$02,$05	; yellow
		DB	$01,$06,$03,$04	; white
		DB	$07,$07,$07,$07	; black
		DB	$05,$02,$03,$04	; bright blue
		DB	$06,$01,$02,$01	; bright red
		DB	$01,$04,$02,$04	; bright magenta
		DB	$04,$00,$04,$01	; bright green
		DB	$01,$00,$04,$00	; bright cyan
		DB	$00,$02,$00,$00	; bright yellow
		DB	$00,$00,$00,$00	; bright white

; Raster line header for expanded copy

m34bf:		DB	$0D,$0A		; CRLF
		DB	$1B,"L",$00,$03	; 768 bytes in 120dpi mode
		DB	$FF

; CAT "T:" routine

m34c6:		LD	BC,$0011
		RST	28H
		DW	$0030		; make space for tape header
		PUSH	DE
		POP	IX		; IX=address of space
m34cf:		LD	A,$0D
		RST	28H
		DW	$0010		; output CR
m34d4:		LD	A,$7F
		IN	A,($FE)
		RRA
		JR	C,m34e3		; move on if BREAK not pressed
		LD	A,$FE
		IN	A,($FE)
		RRA
		JR	C,m34e3		; move on if BREAK not pressed
		RET			; done
m34e3:		LD	A,$00
		LD	DE,$0011
		SCF
		PUSH	IX
		RST	28H
		DW	o0556		; read a header
		POP	IX
		JR	NC,m34d4	; loop back if failed
		PUSH	IX
		LD	A,$22
		RST	28H
		DW	$0010		; output quote
		LD	B,$0A		; name length 10
m34fb:		LD	A,(IX+$01)
		RST	28H
		DW	$0010		; output next byte
		INC	IX
		DJNZ	m34fb		; loop back
		POP	IX
		LD	HL,m35a1
		CALL	m3591		; output quote and space
		LD	A,(IX+$00)	; get file type
		CP	$00
		JR	NZ,m3537	; move on if not program
		LD	A,(IX+$0E)
		CP	$80
		JR	Z,m352f		; move on if no auto-run line number
		LD	HL,m35be
		CALL	m3591		; display "LINE" message
		LD	C,(IX+$0D)
		LD	B,(IX+$0E)
		CALL	m359a		; output line number
		LD	A," "
		RST	28H
		DW	$0010		; output space
m352f:		LD	HL,m35a4
		CALL	m3591		; output "BASIC" message
		JR	m34cf		; loop back
m3537:		CP	$01
		JR	NZ,m3554	; move on if not number array
		LD	HL,m35ad
		CALL	m3591		; output "DATA" message
		LD	A,(IX+$0E)
		AND	$7F
		OR	$40
		RST	28H
		DW	$0010		; output variable name
		LD	HL,m35b9+1
		CALL	m3591		; output "()" message
		JP	m34cf		; loop back
m3554:		CP	$02
		JR	NZ,m3571	; move on if not character array
		LD	HL,m35ad
		CALL	m3591		; output "DATA" message
		LD	A,(IX+$0E)
		AND	$7F
		OR	$40
		RST	28H
		DW	$0010		; output variable name
		LD	HL,m35b9
		CALL	m3591		; output "$()" message
		JP	m34cf		; loop back
m3571:		LD	HL,m35b3
		CALL	m3591		; output "CODE" message
		LD	C,(IX+$0D)
		LD	B,(IX+$0E)
		CALL	m359a		; output load address
		LD	A,","
		RST	28H
		DW	$0010		; output comma
		LD	C,(IX+$0B)
		LD	B,(IX+$0C)
		CALL	m359a		; output length
		JP	m34cf		; loop back

; Subroutine to output a null-terminated string

m3591:		LD	A,(HL)		; get next char
		OR	A
		RET	Z		; exit if null
		RST	28H
		DW	$0010		; output char
		INC	HL
		JR	m3591		; loop back

; Subroutine to output number in BC

m359a:		RST	28H
		DW	o2D2B		; stack number on calculator
		RST	28H
		DW	o2DE3		; output number
		RET

; Messages for tape catalogs

m35a1:		DM	$22, " ", $00
m35a4:		DM	"(BASIC) ", 0
m35ad:		DM	"DATA ", 0
m35b3:		DM	"CODE ", 0
m35b9:		DM	"$() ", 0
m35be:		DM	"LINE ", 0

		RST	28H
		DW	o2BF1
		PUSH	BC
		PUSH	DE
		RST	28H
		DW	o1E94
		POP	DE
		POP	BC
		CALL	m3f00
		DW	$0056
m35d3:		JP	NC,m0edb
		RET
m35d7:		RST	28H
		DW	o1E94
		CALL	l3f00
		DW	$0059
		JR	m35d3
		RST	18H
		CP	"#"
		JR	Z,m35f0
		RST	28H
		DW	o1C82
		CALL	m10b1
		RST	28H
		DW	o1E67
		RET
m35f0:		CALL	m111c
		CALL	m10b1
		LD	DE,m04d5
		LD	BC,13
		CALL	m14f0
		RST	28H
		DW	o1E94
		RST	28H
		DW	o1601
		RST	28H
		DW	o2DA2
		PUSH	BC
		RST	28H
		DW	o2DA2
		PUSH	BC
		POP	HL
		POP	DE
		LD	B,1
		CALL	m3f00
		DW	$0062
		RET

; ####################################################################################################################

SPARE_ROM1_0:	EQU	$

is_tap:		XOR	A
		LD	(tapl_stat1),A
		DEC	DE		; veo si termina en .tap
		LD	A,(DE)
		AND	$DF
		CP	"P"		; me baso en que termine solo con la letra "P" por ".TAP"
		JR	NZ,notap	; si no es otra cosa no lo considero como ".TAP" solo
					; espero que el usuario sea prolijo y haya proporcionado
		LD	BC,$0000	; un nombre de archivo en el argumento del tipo "*.TAP"
		LD	(S_PFILE+0),BC
		LD	(S_PFILE+2),BC

		LD	B,$06
		CALL	m3f00
		DW	DOS_ABANDON

		LD	BC,$0601	; abro el archivo lectura exc. con handle: 06
		LD	A,C
		LD	(tapl_stat1),A	; 1=en pag 7 me indicará que estoy cargando desde cinta
		LD	DE,$0002	; error si no existe + puntero en 0
		LD	HL,tmp_file
		CALL	m3f00
		DW	DOS_OPEN	; open file
		JR	C,ok_opentap	; salto adelante si todo Ok

		CALL	m2b64		; page in normal memory
		CALL	m0e9a		; cause DOS error
		DB	$FF

ok_opentap:	LD	A,(LODDRV)
		LD	(tapl_stat2),A
		LD	A,"T"
		LD	(LODDRV),A	; cambio a T: el "drive" por defecto

		CALL	m32ee		; esta es la forma de retornar
		CALL	m2b64		; al basic
		RET

notap:		LD	HL,tmp_file	; trato de continuar como si que...
		RET			; "aquí no pasó nada"

; ##############################################################################

FREE_ROM1_0:	EQU	$

		;...
		;...

R1_FREE_0:	EQU	103-($-SPARE_ROM1_0)
ROM1_SPARE0:	DS	R1_FREE_0

		IF alternative

rest_ink:		LD	HL,strver
			CALL	m24b5
			LD	A,$10		; *2
			RST	10H		; *1
			LD	A,defink	; *2
			RST	10H		; *1
			JP	m2b64		; *3 - page in normal memory
			DS	3		; *9

		ELSE

m368c:			DM	"Physical drives: ", 0	; *18

		ENDIF

m369e:		DM	" floppy, ", 0
m36a8:		DM	" IDE"

		IF alternative

			DM	", ",0,0

strver:			DM	", v"
			DB	($30+VMAYOR)
			DB	"."
			DB	($30+VMINOR)
			DB	($30+VPATCH)
			DM	VRPATCH
			DS	5

		ELSE

			DM	"Logical drives: ", 0

		ENDIF

m36bd:		DM	"Really format hard disk (Y/N)?", 0
m36dc:		DM	"Really delete partition (Y/N)?", 0
m36fb:		DM	"Invalid partitio", "n"+$80
m370c:		DM	"Partition already exist", "s"+$80
m3724:		DM	"Not implemente", "d"+$80
m3733:		DM	"Partition ope", "n"+$80
m3741:		DM	"Out of partition handle", "s"+$80
m3759:		DM	"Not a swap partitio", "n"+$80
m376d:		DM	"Drive already mappe", "d"+$80
m3781:		DM	"Out of XDPB", "s"+$80
m378d:		DM	"No swap partitio", "n"+$80
m379e:		DM	"Invalid devic", "e"+$80

		DS	$31

;CORREGIDO MANUALMENTE DE ACA PARA ABAJO

m37dd:		CALL	m24b5
		LD	HL,FLAGS
		RES	5,(HL)
m37e5:		BIT	5,(HL)
		JR	Z,m37e5
		RES	5,(HL)
		LD	A,(LAST_K)
		AND	$DF
		CP	"N"
		JR	Z,m37f8
		CP	"Y"
		JR	NZ,m37e5
m37f8:		PUSH	AF
		RST	28H
		DW	o0D6E
		POP	AF
		RET

m37fe:		RST	28H
		DW	o0D6E
		LD	HL,m36bd
		CALL	m37dd
		PUSH	AF
		LD	BC,0
		LD	A,(FLAGS3)
		BIT	6,A
		JR	Z,m3815
		RST	28H
		DW	o1E99
m3815:		PUSH	BC
		RST	28H
		DW	o1E99
		PUSH	BC
		RST	28H
		DW	o1E94
		LD	C,A
		POP	HL
		POP	DE
		POP	AF
		CP	"N"
		RET	Z
m3824:		PUSH	HL
		PUSH	BC
		PUSH	DE
		LD	B,7
		LD	HL,$ED11
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$01A2
		CP	$42
		SCF
		CCF
		JP	NZ,m395a
		LD	IX,$ED11
		LD	H,(IX+$03)
		LD	L,(IX+$06)
		LD	E,(IX+$01)
		INC	IXH
		LD	D,(IX+$00)
		CALL	m32ee
		CALL	m2b64
		EX	(SP),HL
		AND	A
		SBC	HL,DE
		JP	NC,m38c7
		ADD	HL,DE
		LD	A,H
		OR	L
		JR	Z,m3862
		EX	DE,HL
		POP	HL
		SET	7,H
		JR	m3863
m3862:		POP	HL
m3863:		POP	BC
		LD	A,C
		POP	BC
		PUSH	DE
		POP	IX
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00B2
		CALL	m32ee
		CALL	m2b64
		RET	C
m387b:		CALL	m0ecb
		RST	38H
		RST	18H
		CP	$E4
		JR	Z,m388e
		CP	$B9
		JR	Z,m388e
		CALL	m10b1
		JP	m2280
m388e:		PUSH	AF
		RST	20H
		RST	28H
		DW	o1C8C
		RST	18H
		CP	","
		JP	NZ,m1125
m3899:		RST	20H
		RST	28H
		DW	o1C82
		CALL	m10b1
		RST	28H
		DW	o1E99
		PUSH	BC
		CALL	m3965
		JP	NZ,m398a
		POP	HL
		POP	AF
		CP	$B9
		LD	A,2
		LD	BC,17
		JR	Z,m38ba
		LD	A,3
		LD	BC,17
m38ba:		CALL	m2b89
		LD	($EFA8),A
		PUSH	HL
		AND	A
		SBC	HL,BC
		POP	HL
		JR	C,m38ce
m38c7:		CALL	m2b64
		CALL	m2ada
		LD	A,(BC)
m38ce:		LD	A,H
		OR	L
		JR	Z,m38c7
		PUSH	DE
		PUSH	HL
		LD	HL,m3d8b
		LD	DE,$EFB8
		LD	BC,$1D
		LDIR
		POP	HL
		PUSH	HL
		LD	D,L
		LD	E,0
		LD	A,L
		CP	5
		JR	C,m3903
		CP	9
		JR	C,m38fa
		SRL	D
		RR	E
		LD	BC,$3F06
		LD	H,3
		LD	A,$C0
		JR	m390c
m38fa:		LD	BC,o1F05
		LD	H,1
		LD	A,$F0
		JR	m390c
m3903:		SLA	D
		LD	BC,$0F04
		LD	H,0
		LD	A,$FF
m390c:		LD	($EFBA),BC
		LD	($EFC1),A
		LD	A,H
		LD	($EFBC),A
		DEC	DE
		LD	B,L
		XOR	A
m391a:		ADD	A,$10
		DJNZ	m391a
		AND	A
		JR	NZ,m3925
		DEC	A
		LD	DE,$07F7
m3925:		LD	($EFCA),A
		LD	($EFBD),DE
		POP	HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		DEC	HL
		LD	($EFB0),HL
		LD	A,$FF
		LD	($EFAF),A
		POP	AF
		PUSH	AF
		LD	HL,$EF98
		CALL	m32b6
		CALL	m3f00
		DW	$00B8
		CALL	m32ee
		JR	NC,m395d
		POP	AF
		LD	L,$E5
		LD	IX,$0020
		CALL	m32b6
		CALL	m3f00
		DW	$00BB
m395a:		CALL	m32ee
m395d:		CALL	m2b64
		RET	C
		CALL	m0ecb
		RST	38H
m3965:		RST	28H
		DW	o2BF1
		LD	A,B
		OR	C
		JR	NZ,m3970
		CALL	m2ada
		INC	L
m3970:		INC	DE
		LD	A,(DE)
		DEC	DE
		CP	">"
		LD	A,0
		JR	NZ,m398e
m3979:		LD	A,(DE)
		INC	DE
		INC	DE
		DEC	BC
		DEC	BC
		SUB	"0"
		JR	Z,m398e
		CP	1
		JR	Z,m398e
		LD	D,A
		CP	5
		RET	C
m398a:		CALL	m2ada
		LD	E,C
m398e:		PUSH	AF
		LD	B,0
		LD	A,C
		CP	$11
		JR	C,m3998
		LD	A,$10
m3998:		EX	DE,HL
		LD	DE,$EF98
		CALL	m3f63
		DI
		LD	A,(BANKM)
		OR	7
		LD	BC,$7FFD
		OUT	(C),A
		EX	DE,HL
		LD	D,$10
m39ad:		LD	(HL),$20
		INC	HL
		DEC	D
		JR	NZ,m39ad
		LD	A,(BANKM)
		OUT	(C),A
		EI
		POP	DE
		RET
m39bb:		RST	18H
		CP	$EB
		JP	Z,m3df2
		CP	$BF
		JR	Z,m39dc
		CP	$DF
		JR	Z,m3a1f
		CP	$C4
		JR	Z,m3a3d
		CP	$CC
		JP	NZ,m1125
		RST	20H
		RST	28H
		DW	o1C8C
		CALL	m10b1
		JP	m04e5
m39dc:		RST	20H
		RST	28H
		DW	o1C8C
		CALL	m3a81
		CALL	m10b1
		CALL	m3965
		LD	A,D
		PUSH	AF
		JR	NZ,m3a03
		LD	HL,$EF98
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00B5
		CALL	m32ee
		CALL	m2b64
		JR	NC,m3a1b
m3a03:		PUSH	BC
		CALL	m3a8e
		POP	BC
		POP	AF
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00F1
		CALL	m32ee
		CALL	m2b64
		RET	C
m3a1b:		CALL	m0ecb
		RST	38H
m3a1f:		RST	20H
		CALL	m3a81
		CALL	m10b1
		CALL	m3a8e
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00F4
		CALL	m32ee
		CALL	m2b64
		JR	NC,m3a1b
		RET
m3a3d:		RST	20H
		CALL	m10b1
		LD	HL,m36dc
		CALL	m37dd
		PUSH	AF
		CALL	m3965
		JP	NZ,m398a
		POP	AF
		CP	"N"
		RET	Z
		LD	A,D
		PUSH	AF
		LD	HL,$EF98
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00B5
		CALL	m32ee
		CALL	m2b64
		JR	NC,m3a7d
		POP	AF
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00BE
		CALL	m32ee
		CALL	m2b64
		RET	C
m3a7d:		CALL	m0ecb
		RST	38H
m3a81:		RST	18H
		LD	HL,FLAGS3
		RES	6,(HL)
		CP	$B5
		RET	NZ
		SET	6,(HL)
		RST	20H
		RET
m3a8e:		RST	28H
		POP	AF
		DEC	HL
		DEC	BC
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,m3aab
		INC	DE
		LD	A,(DE)
		CP	":"
		JR	NZ,m3aab
		DEC	DE
		LD	A,(DE)
		AND	$DF
		CP	"A"
		JR	C,m3aab
		CP	"Q"
		JR	NC,m3aab
		LD	L,A
		RET
m3aab:		CALL	m2ada
		LD	C,(HL)
cmdspec:	RST	18H
		CP	$AB		; ATTR
		LD	BC,$1Ff
		JR	Z,m3b08
		CP	$D9		; INK
		LD	BC,$0107
		JR	Z,m3b08
		CP	$DA		; PAPER
		LD	BC,$0407
		JR	Z,m3b08
		CP	$DB		; FLASH
		LD	BC,$0801
		JR	Z,m3b08
		CP	$DC		; BRIGHT
		LD	BC,$0701
		JR	Z,m3b08
		CP	$0D		; ENTER
		JR	Z,m3adb
		CP	":"
		JR	NZ,m3ae1
m3adb:		CALL	m10b1
		JP	m1465
m3ae1:		RST	28H
		DW	o1C8C
		CALL	m10b1
		RST	28H
		DW	o2BF1
		EX	DE,HL
		LD	DE,tmp_file
		CALL	m3f63
		CALL	m2b89
		LD	A,$FF
		LD	(DE),A

; #############################################################################
		;ld      hl, tmp_file
		CALL	is_tap
; #############################################################################

		CALL	m32b6
		CALL	m3f00
		DW	$00FD
		CALL	m32ee
		JP	m3d21

;CAMBIA/STORE ATRIBUTOS
m3b08:		PUSH	BC
		RST	20H
		RST	28H
		DW	o1C82
		CALL	m3a81
		CALL	m10b1
		RST	28H
		DW	o1E94
		POP	BC
		LD	D,A
		LD	A,C
		CP	D
		JR	NC,m3b20
		CALL	m2ada
		INC	DE
m3b20:		DEC	B
		JR	Z,m3b29
		RLC	D
		RLC	C
		JR	m3b20
m3b29:		CALL	m2b89
		LD	A,C
		CPL
		LD	C,A
		LD	A,(ed_ATTR_T)
		AND	C
		OR	D
		LD	(ed_ATTR_T),A
		LD	(ed_ATTR_P),A
		CALL	m2b64
		LD	HL,FLAGS3
		BIT	6,(HL)
		RET	Z
		LD	H,A
		XOR	A
		JP	m3e5c

m3b48:		CP	$AD		; TAB
		JR	Z,m3b64
		CP	$B5		; ASN
		JP	Z,m3da8
		CALL	m2ada
		DEC	BC
n3b4d:		CP	$B9		; EXP
		JP	Z,m062b
		CP	$B5		; ASN
		JP	Z,m3da8
		CP	$AD		; TAB
		JP	NZ,m05f8
m3b64:		RST	20H
		CP	$B9		; EXP
		JR	NZ,m3b6f
		RST	20H
		LD	HL,FLAGS3
		SET	6,(HL)
m3b6f:		CALL	m10b1
		CALL	m2b89
		LD	B,2
m3b77:		PUSH	BC
		LD	A,2
		SUB	B
		LD	DE,0
		PUSH	DE
		PUSH	AF
		LD	HL,m3d48
		CALL	m07d7
		POP	AF
		PUSH	AF
		LD	C,A
		ADD	A,$30
		CALL	m07cf
		LD	B,7
		LD	HL,$ED11
		CALL	m32b6
		CALL	m3f00
		DW	$01A2
		CALL	m32ee
		CP	$42
		SCF
		CCF
		JR	NZ,m3c06
		LD	IX,$ED11
		LD	HL,m3d77
		CALL	m07d7
		LD	L,(IX+$01)
		INC	IXH
		LD	H,(IX+$00)
		DEC	IXH
		LD	E,$FF
		CALL	m07df
		LD	A, "/"
		CALL	m07cf
		LD	H,0
		LD	L,(IX+$03)
		LD	E,$FF
		CALL	m07df
		LD	A, "/"
		CALL	m07cf
		LD	H,0
		LD	L,(IX+$06)
		LD	E,$FF
		CALL	m07df
		LD	A, ")"
		CALL	m07cf
		LD	A,13
		CALL	m07cf
m3bdd:		LD	BC,0
m3be0:		POP	AF
		PUSH	AF
		LD	HL,$EF98
		CALL	m32b6
		CALL	m3f00
		DW	$00C4
		CALL	m32ee
		JP	NC,m3d15
		LD	IX,$EF98
		LD	A,(IX+$10)
		CP	0
		JR	NZ,m3c0e
		POP	AF
		POP	DE
		INC	DE
		PUSH	DE
		PUSH	AF
		JP	m3cf9
m3c06:		LD	HL,m3d7a
		CALL	m07d7
		JR	m3bdd
m3c0e:		PUSH	BC
		LD	HL,$EF98
		LD	E,$10
m3c14:		LD	A,(HL)
		INC	HL
		AND	A
		JR	NZ,m3c1b
		LD	A,$7E
m3c1b:		CALL	m07cf
		DEC	E
		JR	NZ,m3c14
		LD	A,$20
		CALL	m07cf
		LD	L,(IX+$18)
		LD	H,(IX+$19)
		LD	E,(IX+$1A)
		LD	A,E
		OR	H
		JR	NZ,m3c46
		LD	H,L
		LD	L,(IX+$17)
		INC	HL
		SRL	H
		RR	L
		LD	E,$20
		CALL	m07df
		LD	HL,m3d2b
		JR	m3c63
m3c46:		XOR	A
		SRL	E
		RR	H
		RR	L
		RRA
		SRL	E
		RR	H
		RR	L
		RRA
		SRL	E
		RR	H
		RR	L
		LD	E,$20
		CALL	m07df
		LD	HL,m3d27
m3c63:		CALL	m07d7
		LD	A,(IX+$10)
		CP	1
		LD	HL,m005e
		JR	Z,m3c8f
		CP	2
		LD	HL,m3d52
		JR	Z,m3c8f
		CP	3
		LD	HL,m3d57
		JR	Z,m3c8f
		CP	$FE
		LD	HL,m3d5d
		JR	Z,m3c8f
		CP	$FF
		LD	HL,m3d63
		JR	Z,m3c8f
		LD	HL,m3d68
m3c8f:		CALL	m07d7
		LD	A,(IX+$10)
		CP	3
		JR	NZ,m3ca7
		LD	A,(IX+$3C)
		AND	A
		JR	Z,m3ca7
		CALL	m07cf
		LD	A, ":"
		CALL	m07cf
m3ca7:		LD	A,13
		CALL	m07cf
		LD	HL,FLAGS3
		BIT	6,(HL)
		JR	Z,m3cf8
		LD	HL,m0056
		CALL	m07d7
		LD	L,(IX+$11)
		LD	H,(IX+$12)
		LD	E,$FF
		CALL	m07df
		LD	A, ","
		CALL	m07cf
		LD	L,(IX+$13)
		LD	H,0
		LD	E,$FF
		CALL	m07df
		LD	HL,m3d70
		CALL	m07d7
		LD	L,(IX+$14)
		LD	H,(IX+$15)
		LD	E,$FF
		CALL	m07df
		LD	A, ","
		CALL	m07cf
		LD	L,(IX+$16)
		LD	H,0
		LD	E,$FF
		CALL	m07df
		LD	A,13
		CALL	m07cf
m3cf8:		POP	BC
m3cf9:		INC	BC
		LD	A,B
		OR	C
		JP	NZ,m3be0
m3cff:		POP	AF
		POP	HL
		LD	E,$FF
		CALL	m07df
		LD	HL,m3d2e
		CALL	m07d7
m3d0c:		POP	BC
		DEC	B
		JP	NZ,m3b77
		CALL	m2b64
		RET
m3d15:		CP	$38
		JR	Z,m3cff
		CP	$16
		JR	NZ,m3d21
		POP	AF
		POP	HL
		JR	m3d0c
m3d21:		CALL	m2b64
		JP	m387b
m3d27:		DM	"Mb ", 0
m3d2b:		DM	"K ", 0
m3d2e:		DM	" free partition entries", 13, 13, 0
m3d48:		DM	"IDE unit ", 0
m3d52:		DM	"swap", 0
m3d57:		DM	"data ", 0
m3d5d:		DM	"*BAD*", 0
m3d63:		DM	"FREE", 0
m3d68:		DM	"unknown", 0
m3d70:		DM	" End: ", 0
m3d77:		DM	" (", 0
m3d7a:		DM	" (not detected)", 13, 0
m3d8b:		DB	0,2,0,0,0,0,0,$FF,1,0,0,0,$80,0
		DB	0,2,3,0,0,$80,0,0,2,0,0,0,0,0,0
m3da8:		RST	20H
		CALL	m10b1
		CALL	m2b89
		LD	B,$10
		LD	L, "A"
m3db3:		PUSH	BC
		PUSH	HL
		LD	BC,$EF98
		CALL	m32b6
		CALL	m3f00
		DW	$00F7
		CALL	m32ee
		JP	NC,m3d21
		JR	Z,m3de9
		POP	HL
		PUSH	HL
		LD	A,L
		CALL	m07cf
		LD	A, ":"
		CALL	m07cf
		LD	A, " "
		CALL	m07cf
		LD	HL,$EF98
		LD	B,$12
m3ddd:		LD	A,(HL)
		CALL	m07cf
		INC	HL
		DJNZ	m3ddd
		LD	A,13
		CALL	m07cf
m3de9:		POP	HL
		POP	BC
		INC	L
		DJNZ	m3db3
		CALL	m2b64
		RET
m3df2:		RST	20H
		RST	28H
		DW	o1C8C
		CALL	m10b1
		RST	28H
		DW	o2BF1
		LD	B,0
		LD	A,C
		AND	A
		JR	NZ,m3e06
		CALL	m2ada
		INC	L
m3e06:		CP	$10
		JR	C,m3e0c
		LD	C,$10
m3e0c:		PUSH	BC
		EX	DE,HL
		LD	DE,tmp_file
		CALL	m3f63
		CALL	m3965
		LD	A,D
		JP	NZ,m398a
		PUSH	AF
		LD	HL,$EF98
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00B5
		CALL	m32ee
		CALL	m2b64
		JR	NC,m3e58
		POP	AF
		POP	DE
		CALL	m2b89
		LD	HL,tmp_file
		ADD	HL,DE
		LD	D,A
		LD	A,$11
		SUB	E
m3e3f:		LD	(HL),$20
		INC	HL
		DEC	A
		JR	NZ,m3e3f
		LD	A,D
		LD	HL,tmp_file
		CALL	m32b6
		CALL	m3f00
		DW	$00C1
		CALL	m32ee
		CALL	m2b64
		RET	C
m3e58:		CALL	m0ecb
		RST	38H

m3e5c:		LD	B,A
		LD	C,A
		LD	L,8
		CALL	m2b89
		CALL	m32b6
		CALL	m3f00
		DW	$00D6
		LD	A,(ATTR_P)
		LD	H,A
		LD	L,9
		XOR	A
		LD	B,A
		LD	C,A
		CALL	m3f00
		DW	$00D6
		CALL	m32ee
		CALL	m2b64
		RET

; Subroutine to call a subroutine in ROM 0
; The subroutine address is inline after the call to this routine

m3e80:		LD	(OLDHL),HL	; save HL in OLDHL
		LD	(OLDBC),BC	; save BC in OLDBC
		PUSH	AF
		POP	HL
		LD	(OLDAF),HL	; save AF in OLDAF
		EX	(SP),HL		; HL=address of inline address
		LD	C,(HL)
		INC	HL
		LD	B,(HL)		; BC=inline ROM 0 address
		INC	HL
		EX	(SP),HL		; stack return address
		PUSH	BC
		POP	HL		; HL=routine address in ROM 0
		LD	A,(BANKM)
		AND	$EF
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; page in ROM 0

; The rest of the routine continues at $3EA2 in ROM 0
; The following is a continuation of a mirrored routine in ROM 0 for
; calling this ROM

m3ea2:		EI
		LD	BC,$3EB5
		PUSH	BC		; stack return add to swap back ROMs
		PUSH	HL		; stack routine address
		LD	HL,(OLDAF)
		PUSH	HL
		POP	AF		; restore AF
		LD	BC,(OLDBC)	; restore BC
		LD	HL,(OLDHL)	; restore HL
		RET			; execute routine in this ROM

; This part is the routine which returns control to ROM 0

m3eb5:		PUSH	AF		; save AF & BC
		PUSH	BC
		LD	A,(BANKM)
		AND	$EF
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; page back ROM 0

; The rest of the routine continues at $3EC5 in ROM 0
; The following is a continuation of a mirrored routine in ROM 0 for
; returning to this ROM

m3ec5:		EI
		POP	BC		; restore registers
		POP	AF
		RET			; return

m3291:		DM	$0D,"  1 file copied.",$0D,$0D,$00
m32a5:		DM	" files copied.",$0D,$0D,$00
		DS	$12

; Subroutine to call a subroutine in ROM 2
; The subroutine address is inline after the call to this routine
; This routine is duplicated in ROMs 0 & 2, so that when we start switching
; (first to ROM 0, then to ROM 2) there is no problem.

m3f00:		LD	(OLDHL),HL	; save HL,BC and AF
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
		LD	HL,m3f42
		PUSH	HL
		PUSH	BC
		POP	HL		; HL=address to call in ROM
		LD	A,(BANKM)
		AND	$EF
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A		; page in ROM 0
		LD	A,(BANK678)
		OR	$04
		LD	(BANK678),A
m3f2a:		LD	BC,PBANK678
		OUT	(C),A		; page in ROM 2
		EI
		PUSH	HL		; stack routine address to call in ROM 2
		LD	HL,(OLDAF)	; restore registers
		PUSH	HL
		POP	AF
		LD	BC,(OLDBC)
		LD	HL,(OLDHL)
		RET			; exit to routine

; This part of the routine returns control to ROM 1

m3f42:		PUSH	BC		; save registers
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

; Subroutine to copy a block of memory from HL in page 0 to
; DE in page 7 (length BC bytes)

m3f63:		DI
		EXX
		LD	BC,PBANKM	; BC'=paging port
		EXX
m3f69:		EXX
		LD	A,$10
		OUT	(C),A		; page in page 0
		EXX
		LD	A,(HL)
		EX	AF,AF'		; get A'=byte from page 0
		EXX
		LD	A,$17
		OUT	(C),A		; page in page 7
		EXX
		EX	AF,AF'
		LD	(DE),A		; store byte from page 0 in page 7
		INC	HL		; increment addresses
		INC	DE
		DEC	BC		; decrement counter
		LD	A,B
		OR	C
		JR	NZ,m3f69	; loop back for more
		LD	A,(BANKM)
		LD	BC,PBANKM
		OUT	(C),A		; page in previous memory
		EI			; enable interrupts
		RET

; Subroutine to copy a block of memory from HL in page 7 to
; DE in page 0 (length BC bytes)

m3f8a:		DI
		EXX
		LD	BC,PBANKM	; BC'=paging port
		EXX
m3f90:		EXX
		LD	A,$17
		OUT	(C),A		; page in page 7
		EXX
		LD	A,(HL)
		EX	AF,AF'		; A'=byte from page 7
		EXX
		LD	A,$10
		OUT	(C),A		; page in page 0
		EXX
		EX	AF,AF'
		LD	(DE),A		; store byte in page 0
		INC	HL		; increment addresses
		INC	DE
		DEC	BC		; decrement pointers
		LD	A,B
		OR	C
		JR	NZ,m3f90	; loop back for more
		LD	A,(BANKM)
		LD	BC,PBANKM
		OUT	(C),A		; page in previous memory
		EI			; enable interrupts
		RET

m07c9:		DM	"K free",13,0
m07d1:		DM	"No files found",13,0
merase:		DM	"Erase ",0
myn:		DM	" ? (Y/N)",0

FREE_ROM1_1:	EQU	$

		;...
		;...

R1_FREE_1:	EQU	$0027-($-FREE_ROM1_1)
ROM1_SPARE1:	DS	R1_FREE_1

; ------------------------------------------------------------------------------

; ============================
; PLAY command data structures
; ============================
;
; During execution of the PLAY command, an area of memory $3C bytes long plus
; $37 bytes per string is reserved. IY is used to point to the "overhead" area,
; and IX points to the data for the string currently being considered.
; A maximum of 8 strings are allowed.
;
;
; Overhead Area: IY+n
; -------------------
;
; Offset  Length  Description
; ------  ------  -----------
; +00     10      Address of data area for each string
; +10     1       String presence flags (bit reset if string in use)
; +11     10      Note length of current note for each string
; +21     1       String counter
; +22     1       String presence flags, shifted with string counter
; +23     2       Address of pointer to string data area
; +25     2       Shortest current note length (=length to play)
; +27     2       Tempo value
; +29     1       Waveform number for volume effects
; +2a     1       Notes changed flag
; +2b     0d      FP routine used to calculate tempo value
; +38     4       Unused
;
;
; String Data Areas: IX+n
; -----------------------
;
; Offset  Length  Description
; ------  ------  -----------
; +00     1       Current semitone number
; +01     1       $FF, or MIDI channel 0-15
; +02     1       String number 0-7
; +03     1       Base semitone number (12*octave)
; +04     1       Volume level (bit 4 set=use volume effect)
; +05     1       Note length
; +06     2       String interpretation pointer
; +08     2       String end+1
; +0a     1       Infinite repeat flag
; +0b     1       Open bracket depth
; +0c     2       String start
; +0e     8       Opening bracket addresses (max depth 4)
; +16     1       Close bracket depth
; +17     0a      Close bracket addresses (max depth 5)
; +21     1       # tied notes
; +22     16      Note lengths for tied notes (max 11)
