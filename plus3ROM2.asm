		OUTPUT	"p3t_rom2.rom"

		ORG	$0000

; *************************************************
; *** SPECTRUM +3 ROM 2 DISASSEMBLY (+3DOS ROM) ***
; *************************************************

; ROM2 del PROYECTO +3e de la VERSION IDE 8bits
; Desensamblado por DJr, con la valiosa ayuda de DZ80 V2.0

; este desensamble no sabe nada de las constantes spanish, garry, y V41
; aca todo es IDE8 v4.0 e ingles . a.k.a no me la compliquesn

n0000:		JR	$0000	; que es esto??? no parece tener uso he probado
		JP	l22c2	; a sustituirlo pos "DS 8" como en la ROM
		JP	l196a	; original y aparentemente todo funciona bien

n0008:		DB	"PLUS3DOS"

n0010:		DB	$2F,$58,$32,$9A	; ¿¿¿que es todo esto??? son como datos
		DB	$2F,$9E,$34,$E3	; al azar si se examina su desensamble
		DB	$80,$01,$C0,$05	; y a primera vista no parecen ser
		DB	$C4,$09,$C8,$0D	; necesarios, pero he comprobado que si
		DB	$61,$0F,$CC,$14	; quito esto y pongo "DS 40" como en la
		DB	$51,$00,$13,$16 ; ROM original muchos Z80s no cargan
		DB	$24,$17,$56,$19 ; con el comando SPECTRUM
		DB	$10,$13,$14,$ED
		DB	$79,$01,$06,$7F
		DB	$3E,$C3,$FB,$C9

/*
		n0000:	DS 8
		n0008:	DM "PLUS3DOS"
		n0010:	DS 40
*/

/*
;===============================================================================

POR OTRO LADO: NO HAY MUCHO ESPACIO LIBRE PARA RASCAR, PERO CUALQUIER COSA BAJO
LA DIRECTIVA "DS" ES TODO USABLE PARA PONER ALGUN CAMBIO QUE QUIERAS HACER

;===============================================================================
*/

; The maskable interrupt routine

		PUSH	AF
		PUSH	HL
		LD	HL,(FRAMES)
		INC	HL		; increment FRAMES
		LD	(FRAMES),HL
		LD	A,H
		OR	L
		JR	NZ,l0048
		INC	(IY+$40)	; increment high byte of FRAMES
l0048:		PUSH	BC
		PUSH	DE
		CALL	l3e72		; scan the keyboard
		CALL	l0068		; test for disk motor timeout
		POP	DE
		POP	BC
		POP	HL
		POP	AF
		EI
		RET

		JP	l3a24
		JP	l3cc9
		JP	l3d5f
		JP	l3d55
		JP	l3d64
		NOP

; The Non-maskable interrupt

x0066:		RETN

l0068:		LD	BC,PBANKM
		LD	A,(BANKM)
		OR	$07
		OUT	(C),A
		LD	A,(timeout)
		OR	A
		JR	Z,l0095		; (+$1D)
		LD	A,(FRAMES)
		BIT	0,A
		JR	NZ,l0095	; (+$16)
		LD	A,(timeout)
		DEC	A
		LD	(timeout),A
		JR	NZ,l0095	; (+$0D)
		LD	BC,PBANK678
		LD	A,(BANK678)
		AND	$F7
		LD	(BANK678),A
		OUT	(C),A
l0095:		LD	BC,PBANKM
		LD	A,(BANKM)
		OUT	(C),A
		RET

		DW	$0000

		JP	l2808
		JP	l249a
		JP	l2828
		JP	l280d
		JP	l2558
		JP	l25fb
		JP	l2a01
		JP	l2bc8
		JP	l2de2
		JP	l2d0b
		JP	l2f93
		JP	l2c0f
		JP	l2b69
		JP	l2b92
		JP	l2c3b
		JP	l2c94
		JP	l2cdb
		JP	l306c
		JP	l3083
		JP	l318b
		JP	l2cdb
		JP	l3131
		JP	l3106
		JP	l314e
		JP	l30af
		JP	l30bb
		JP	l31e1
		JP	l3227
		JP	l3370
		JP	l33cd
		JP	l34ac
		JP	l3531

l0100:		JP	l01a8		; DOS_INITIALISE
l0103:		JP	l01dc		; DOS_VERSION
l0106:		JP	l0635		; DOS_OPEN
l0109:		JP	l074a		; DOS_CLOSE
l010c:		JP	l076b		; DOS_ABANDON
x010f:		JP	l08bb		; DOS_REF_HEAD
l0112:		JP	l19c0		; DOS_READ
l0115:		JP	l117f		; DOS_WRITE
l0118:		JP	l1129		; DOS_BYTE_READ
l011b:		JP	l1219		; DOS_BYTE_WRITE
l011e:		JP	l0a23		; DOS_CATALOG
l0121:		JP	l08fc		; DOS_FREE_SPACE
l0124:		JP	l092e		; DOS_DELETE
l0127:		JP	l0979		; DOS_RENAME
l012a:		JP	l1b4d		; DOS_BOOT
l012d:		JP	l0919		; DOS_SET_DRIVE
x0130:		JP	l0906		; DOS_SET_USER
l0133:		JP	l105f		; DOS_GET_POSITION
x0136:		JP	l107b		; DOS_SET_POSITION
l0139:		JP	l1068		; DOS_GET_EOF
l013c:		JP	l01ec		; DOS_GET_1346
l013f:		JP	l01f2		; DOS_SET_1346
l0142:		JP	l05cf		; DOS_FLUSH
l0145:		JP	l08cd		; DOS_SET_ACCESS
l0148:		JP	l0963		; DOS_SET_ATTRIBUTES
l014b:		JP	l070e		; DOS_OPEN_DRIVE
l014e:		JP	l02f0		; DOS_SET_MESSAGE
l0151:		JP	l17c5		; DOS_REF_XDPB
l0154:		JP	l1935		; DOS_MAP_B
l0157:		JP	l1f86		; DD_INTERFACE
l015a:		JP	l1f91		; DD_INIT
l015d:		JP	x1fa6		; DD_SETUP
l0160:		JP	l1966		; DD_SET_RETRY
l0163:		JP	l1c75		; DD_READ_SECTOR
l0166:		JP	x1c83		; DD_WRITE_SECTOR
l0169:		JP	x1c8c		; DD_CHECK_SECTOR
x016c:		JP	l1c9a		; DD_FORMAT
l016f:		JP	l1cac		; DD_READ_ID
l0172:		JP	l1edb		; DD_TEST_UNSUITABLE
l0175:		JP	l1cf6		; DD_LOGIN
l0178:		JP	l1d51		; DD_SEL_FORMAT
l017b:		JP	l1f4f		; DD_ASK_1
l017e:		JP	l1f5b		; DD_DRIVE_STATUS
l0181:		JP	l1eeb		; DD_EQUIPMENT
l0184:		JP	l1c50		; DD_ENCODE
l0187:		JP	l1d64		; DD_L_XDPB
l018a:		JP	l1da6		; DD_L_DPB
l018d:		JP	l1fd5		; DD_L_SEEK
l0190:		JP	l3ef5		; DD_L_READ
l0193:		JP	l2122		; DD_L_WRITE
l0196:		JP	l2181		; DD_L_ON_MOTOR
l0199:		JP	l21a6		; DD_L_T_OFF_MOTOR
l019c:		JP	l3ee6		; DD_L_OFF_MOTOR

		JP	l39bd
		JP	l24bf
		JP	l29ca

l01a8:		LD	A,($DF9D)
		PUSH	AF
		LD	HL,pg_buffer
		LD	DE,$DB01
		LD	BC,$09FF
		LD	(HL),$00
		LDIR
		CALL	l1f86
		JR	NC,l01c4	; (+$06)
		CALL	l1f91
		CALL	l1737
l01c4:		LD	HL,$0878
		LD	DE,$0008
		PUSH	DE
		CALL	x1798
		POP	DE
		CALL	l14a5
		CALL	l0508
		POP	AF
		LD	($DF9D),A
		JP	l2828
l01dc:		XOR	A
		LD	B,$01
		SUB	B
		LD	A,$00
		LD	B,A
		LD	C,A
		LD	DE,$0101
		LD	HL,$0069
		SCF
		RET

l01ec:		CALL	l1ab1
		JP	l149c
l01f2:		PUSH	DE
		EX	DE,HL
		CALL	l1ab1
		OR	A
		SBC	HL,DE
		EX	DE,HL
		SCF
		CALL	NZ,l1abb
		POP	DE
		RET	NC

		EX	DE,HL
		CALL	l149c
		EX	DE,HL
		OR	A
		SBC	HL,DE
		ADD	HL,DE
		SCF
		CALL	NZ,l14a1
		RET

l020f:		INC	C
		DEC	C
		JR	NZ,l0216	; (+$03)
		INC	B
		DEC	B
		RET	Z

l0216:		CALL	l021b
		LDIR
l021b:		PUSH	HL
		PUSH	BC
		LD	B,A
		LD	HL,BANKM
		LD	A,(HL)
		AND	$07
		PUSH	AF
		LD	A,(HL)
		AND	$F8
		OR	B
		LD	BC,PBANKM
		LD	(HL),A
		OUT	(C),A
		POP	AF
		POP	BC
		POP	HL
		RET

l0233:		ADD	A,A
		LD	L,A
		OR	$C0
		LD	H,A
		LD	A,L
		LD	L,$00
		RLCA
		RLCA
		RLCA
		AND	$06
		CP	$04
		RET	NC

		INC	A
		RET

l0245:		LD	A,C
		CP	B
		JR	NZ,l024f	; (+$06)
		PUSH	IX
		POP	BC
		JP	l020f
l024f:		PUSH	BC
		CALL	l02cc
		CALL	l020f
		POP	BC
		PUSH	BC
		LD	A,B
		EX	DE,HL
		CALL	l02cc
		EX	DE,HL
		CALL	l020f
		PUSH	IX
		POP	BC
		LD	A,B
		OR	C
		POP	BC
		RET	Z

		LD	A,R
		DI
		PUSH	AF
		OR	A
		CALL	l027b
		CALL	l0290
		SCF
		CALL	l027b
		POP	AF
		RET	po

		EI
		RET

l027b:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	BC,$0020
		LD	DE,pg_buffer
		LD	HL,$BFE0
		JR	NC,l028a	; (+$01)
		EX	DE,HL
l028a:		LDIR
		POP	BC
		POP	DE
		POP	HL
		RET

l0290:		PUSH	IX
		EX	(SP),HL
		LD	A,H
		OR	A
		JR	NZ,l029c	; (+$05)
		LD	A,L
		CP	$20
		JR	C,x029e		; (+$02)
l029c:		LD	A,$20
x029e:		PUSH	BC
		LD	C,A
		LD	B,$00
		OR	A
		SBC	HL,BC
		POP	BC
		EX	(SP),HL
		POP	IX
		OR	A
		RET	Z

		PUSH	DE
		PUSH	BC
		PUSH	AF
		LD	B,A
		LD	A,C
		LD	C,B
		LD	B,$00
		LD	DE,$BFE0
		CALL	l020f
		POP	AF
		POP	BC
		POP	DE
		PUSH	HL
		PUSH	BC
		LD	C,A
		LD	A,B
		LD	B,$00
		LD	HL,$BFE0
		CALL	l020f
		POP	BC
		POP	HL
		JR	l0290		; (-$3C)
l02cc:		PUSH	HL
		LD	BC,$0000
		LD	HL,src_add
		OR	A
		SBC	HL,DE
		JR	C,l02ee		; (+$16)
		JR	Z,l02ee		; (+$14)
		PUSH	IX
		POP	BC
		OR	A
		SBC	HL,BC
		ADD	HL,BC
		JR	NC,l02e5	; (+$02)
		LD	B,H
		LD	C,L
l02e5:		PUSH	IX
		POP	HL
		OR	A
		SBC	HL,BC
		PUSH	HL
		POP	IX
l02ee:		POP	HL
		RET

l02f0:		OR	A
		JR	NZ,l02f6	; (+$03)
		LD	HL,$0000
l02f6:		LD	DE,(rt_alert)
		LD	(rt_alert),HL
		EX	DE,HL
		RET

l02ff:		LD	B,A
		LD	HL,(rt_alert)
		LD	A,H
		OR	L
		LD	A,B
		JR	NZ,x030a	; (+$02)
		INC	L
		RET

x030a:		PUSH	BC
		LD	HL,x03cd
		LD	(al_resp),HL
		CALL	l032d
		POP	BC
		PUSH	BC
		CALL	l031f
		POP	BC
		SUB	$01
		CCF
		LD	A,B
		RET

l031f:		PUSH	HL
		LD	HL,(rt_alert)
		EX	(SP),HL
		RET

l0325:		LD	A,$0A
		LD	HL,x03cd
		LD	(al_resp),HL
l032d:		LD	IX,al_mess
		PUSH	IX
		CALL	l033c
		LD	(IX+$00),$FF
		POP	HL
		RET

l033c:		AND	$7F
		LD	HL,(al_resp)
		LD	B,A
		INC	B
		JR	l034a		; (+$05)
l0345:		LD	A,(HL)
		INC	HL
		INC	A
		JR	NZ,l0345	; (-$05)
l034a:		DJNZ	l0345		; (-$07)
l034c:		LD	A,(HL)
		INC	HL
		CP	$FF
		RET	Z

		PUSH	HL
		CALL	l0358
		POP	HL
		JR	l034c		; (-$0C)
l0358:		OR	A
		JP	p,l03c7
		CP	$FE
		JR	Z,l03c6		; (+$66)
		CP	$FD
		JR	Z,l0397		; (+$33)
		CP	$FC
		JR	Z,l0394		; (+$2C)
		CP	$FB
		JR	Z,l037d		; (+$11)
		CP	$FA
		JR	NZ,l033c	; (-$34)
		LD	(IX+$00),$10
		INC	IX
		LD	A,(BORDCR)
		AND	$07
		JR	l03c7		; (+$4A)
l037d:		LD	(IX+$00),$10
		INC	IX
		LD	A,(BORDCR)
		AND	$38
		CP	$20
		JR	NC,l0390	; (+$04)
		LD	A,$05
		JR	l0392		; (+$02)
l0390:		LD	A,$02
l0392:		JR	l03c7		; (+$33)
l0394:		LD	A,E
		JR	l0398		; (+$01)
l0397:		LD	A,D
l0398:		PUSH	DE
		PUSH	BC
		LD	L,A
		LD	H,$00
		LD	D,H
		LD	BC,$FF9C
		CALL	l03b1
		LD	BC,$FFF6
		CALL	l03b1
		LD	A,L
		ADD	A,$30
		POP	BC
		POP	DE
		JR	l03c7		; (+$16)
l03b1:		LD	A,$FF
l03b3:		PUSH	HL
		INC	A
		ADD	HL,BC
		JR	NC,l03bc	; (+$04)
		EX	(SP),HL
		POP	HL
		JR	l03b3		; (-$09)
l03bc:		POP	HL
		OR	A
		JR	Z,l03c2		; (+$02)
		LD	D,$30
l03c2:		ADD	A,D
		RET	Z

		JR	l03c7		; (+$01)
l03c6:		LD	A,C
l03c7:		LD	(IX+$00),A
		INC	IX
		RET

x03cd:		DB	$8B, "not ready", $8F, $FF
		DB	$8C,"write protected", $8F, $FF
		DB	$8D, "seek fail", $8F, $FF
		DB	$8E, "data error", $8F, $FF
		DB	$8E, "no data", $8F, $FF
		DB	$8E, "missing address mark", $8F, $FF
		DB	$8B, "bad format", $8F, $FF
		DB	$8E, "unknown error", $8F, $FF
		DB	$8C,"changed, please replace", $8F, $FF
		DB	$8C,"unsuitable", $8F, $FF
		DB	"Please put the disk for ", $FE, ": into the drive then press "
		DB	"any key", $FF
		DB	"Drive ", $FE, ": ", $FF
		DB	$8B, "disk ", $FF
		DB	$8B, "track ", $FD, ", ", $FF
		DB	$8D, "sector ", $FC, ", ", $FF
		DB	" - Retry, Ignore or Cancel? ", $FF

l04eb:		OR	A
		RET	Z

l04ed:		SRL	D
		RR	E
		DEC	A
		JR	NZ,l04ed	; (-$07)
		RET

l04f5:		OR	A
		RET	Z

		EX	DE,HL
l04f8:		ADD	HL,HL
		DEC	A
		JR	NZ,l04f8	; (-$04)
		EX	DE,HL
		RET

l04fe:		JP	(HL)
l04ff:		CP	$61
		RET	C

		CP	$7B
		RET	NC

		ADD	A,$E0
		RET

l0508:		LD	BC,$1041
l050b:		LD	A,C
		CALL	l17cb
		LD	A,C
		JR	C,l0517		; (+$05)
		INC	C
		DJNZ	l050b		; (-$0A)
		LD	A,$41
l0517:		LD	(def_drv),A
		LD	(LODDRV),A
		LD	(SAVDRV),A
		RET

x0521:		CALL	l0532
		RET	NC

		RRA
		LD	A,$1D
		RET

l0529:		CALL	l0532
		RET	NC

		RRA
		RRA
		LD	A,$1D
		RET

l0532:		CALL	l0558
		RET	NC

		RLCA
		RRA
		RET	C

		LD	A,$1D
		RET

l053c:		CALL	l0558
		RET	NC

		RLA
		CCF
		LD	A,$1D
		RET	NC

		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	H,B
		LD	L,C
		LD	(HL),$00
		LD	D,B
		LD	E,C
		INC	DE
		LD	BC,$0037
		LDIR
		POP	BC
		POP	DE
		POP	HL
		RET

l0558:		PUSH	HL
		PUSH	DE
		LD	A,B
		CP	$10
		LD	A,$15
		JR	NC,l0573	; (+$12)
		LD	HL,$DB68
		LD	DE,o0038
		INC	B
l0568:		ADD	HL,DE
		DJNZ	l0568		; (-$03)
		LD	B,H
		LD	C,L
		LD	HL,$0020
		ADD	HL,BC
		LD	A,(HL)
		SCF
l0573:		POP	DE
		POP	HL
		RET

l0576:		LD	HL,$0020
		ADD	HL,BC
		LD	E,(HL)
		LD	(HL),$00
		PUSH	HL
		PUSH	DE
		CALL	l0586
		POP	DE
		POP	HL
		LD	(HL),E
		RET

l0586:		LD	HL,fcbs
		LD	E,$12
l058b:		PUSH	HL
		PUSH	BC
		LD	BC,$0020
		ADD	HL,BC
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		POP	BC
		POP	HL
		BIT	7,D
		JR	Z,l05c4		; (+$2A)
		PUSH	HL
		LD	HL,$0021
		ADD	HL,BC
		CP	(HL)
		POP	HL
		JR	NZ,l05c4	; (+$21)
		LD	A,(BC)
		CP	$22
		JR	Z,l05b0		; (+$08)
		LD	A,(HL)
		CP	$22
		CALL	NZ,l0d7a
		JR	NZ,l05c4	; (+$14)
l05b0:		PUSH	HL
		LD	HL,$0020
		ADD	HL,BC
		LD	A,(HL)
		RRCA
		RRCA
		AND	$03
		LD	H,A
		LD	A,D
		AND	$03
		OR	H
		XOR	H
		LD	A,$1E
		POP	HL
		RET	NZ

l05c4:		PUSH	DE
		LD	DE,o0038
		ADD	HL,DE
		POP	DE
		DEC	E
		JR	NZ,l058b	; (-$42)
		SCF
		RET

l05cf:		CALL	l04ff
		CALL	l0c13
		RET	NC

l05d6:		PUSH	BC
		LD	BC,fcbs
		LD	E,$12
l05dc:		LD	HL,$0020
		ADD	HL,BC
		BIT	7,(HL)
		JR	Z,l05f1		; (+$0D)
		INC	HL
		LD	A,(HL)
		CP	(IX+$1C)
		SCF
		PUSH	DE
		CALL	Z,l0756
		POP	DE
		JR	NC,l05fb	; (+$0A)
l05f1:		LD	HL,o0038
		ADD	HL,BC
		LD	B,H
		LD	C,L
		DEC	E
		JR	NZ,l05dc	; (-$1E)
		SCF
l05fb:		POP	BC
		RET

l05fd:		LD	BC,fcbs
		LD	E,$12
l0602:		PUSH	HL
		PUSH	DE
		LD	A,D
		EX	DE,HL
		LD	HL,$0021
		ADD	HL,BC
		CP	(HL)
		JR	NZ,l0627	; (+$1A)
		INC	HL
		BIT	3,(HL)
		JR	Z,l0627		; (+$15)
		LD	HL,$002B
		ADD	HL,BC
		LD	A,E
		CP	(HL)
		JR	NZ,l0627	; (+$0D)
		INC	HL
		LD	A,D
		CP	(HL)
		JR	NZ,l0627	; (+$08)
		CALL	l0c0c
		CALL	C,l12ab
		JR	NC,l062e	; (+$07)
l0627:		LD	HL,o0038
		ADD	HL,BC
		LD	B,H
		LD	C,L
		SCF
l062e:		POP	DE
		POP	HL
		RET	NC

		DEC	E
		JR	NZ,l0602	; (-$32)
		RET

l0635:		PUSH	DE
		PUSH	BC
		CALL	l053c
		CALL	C,l0ae9
		CALL	C,l0c0c
		POP	HL
		POP	DE
		RET	NC

		PUSH	DE
		LD	A,L
		LD	HL,$0020
		ADD	HL,BC
		LD	(HL),A
		CALL	l0586
		LD	HL,l0d7a
		CALL	C,l0d9e
		POP	DE
		RET	NC

		JR	NZ,l0687	; (+$30)
		LD	A,E
		OR	A
		LD	A,$18
		RET	Z

		DEC	E
		JR	NZ,l0667	; (+$08)
		CALL	l06cc
		CALL	C,l080b
		JR	l06a3		; (+$3C)
l0667:		DEC	E
		JR	NZ,l0672	; (+$08)
		CALL	l06cc
		CALL	C,l0863
		JR	l06a3		; (+$31)
l0672:		PUSH	DE
		DEC	E
		JR	NZ,l067e	; (+$08)
		CALL	l06e8
		CALL	C,l098d
		JR	l0685		; (+$07)
l067e:		OR	A
		LD	A,$15
		DEC	E
		CALL	Z,l0938
l0685:		POP	DE
		RET	NC

l0687:		LD	A,D
		OR	A
		LD	A,$17
		RET	Z

		DEC	D
		JR	NZ,l0697	; (+$08)
		CALL	x06b1
		CALL	C,l07e6
		JR	l069e		; (+$07)
l0697:		OR	A
		LD	A,$15
		DEC	D
		CALL	Z,x06b1
l069e:		RET	NC

		XOR	A
		SCF
		JR	l06a5		; (+$02)
l06a3:		RET	NC

		SBC	A,A
l06a5:		PUSH	AF
		LD	HL,$0020
		ADD	HL,BC
		SET	7,(HL)
		INC	(IX+$21)
		POP	AF
		RET

x06b1:		LD	HL,$0020
		ADD	HL,BC
		LD	A,(HL)
		RRA
		RRA
		LD	A,$1E
		CALL	C,x1871
		LD	HL,$0000
		CALL	C,x0caa
		RET	NC

		LD	HL,$0022
		ADD	HL,BC
		SET	0,(HL)
		SCF
		RET

l06cc:		CALL	l0d3a
		JR	NC,l06e0	; (+$0F)
		LD	HL,$0020
		ADD	HL,BC
		BIT	1,(HL)
		SCF
		RET	Z

		CALL	l0eb3
		CALL	C,x1871
		RET

l06e0:		CP	$19
		SCF
		CCF
		RET	NZ

		LD	A,$17
		RET

l06e8:		PUSH	BC
		LD	H,B
		LD	L,C
		LD	DE,sysfcb0
		LD	BC,o0038
		LDIR
		POP	BC
		LD	A,$42
		LD	($DF29),A
		LD	HL,$4B41
		LD	($DF2A),HL
		PUSH	BC
		LD	BC,sysfcb0
		CALL	l0938
		POP	BC
		RET	C

		CP	$17
		SCF
		RET	Z

		OR	A
		RET

l070e:		CALL	l04ff
		LD	D,A
		LD	E,C
		CALL	l053c
		RET	NC

		LD	A,$22
		LD	(BC),A
		LD	HL,$0020
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D
		LD	A,D
		CALL	l17cb
		CALL	C,l0586
		RET	NC

		LD	E,(IX+$05)
		LD	D,(IX+$06)
		INC	DE
		LD	A,(IX+$02)
		SUB	$02
		CALL	NZ,l04f5
		CALL	x1896
		SLA	E
		RL	D
		LD	HL,$0024
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D
		SCF
		JP	l06a5
l074a:		CALL	l0532
		CALL	C,l0c0c
		CALL	C,l0756
		RET	NC

		JR	l0789		; (+$33)
l0756:		LD	HL,$0020
		ADD	HL,BC
		BIT	1,(HL)
		SCF
		RET	Z

		CALL	x07a1
		CALL	C,l12ab
		CALL	C,l1685
		CALL	C,l0cb6
		RET

l076b:		CALL	l0532
		CALL	C,l0c0c
		RET	NC

		LD	HL,$0020
		ADD	HL,BC
		BIT	1,(HL)
		JR	Z,l0789		; (+$0F)
		INC	HL
		INC	HL
		BIT	1,(HL)
		JR	Z,x0786		; (+$06)
		CALL	l102d
		CALL	l0f35
x0786:		CALL	l1632
l0789:		LD	HL,$0020
		ADD	HL,BC
		LD	(HL),$00
		DEC	(IX+$21)
		CALL	Z,l181b
		SCF
		RET

l0797:		DB	"PLUS3DOS", $1A, $01

x07a1:		LD	HL,$0022
		ADD	HL,BC
		BIT	6,(HL)
		SCF
		RET	Z

		CALL	l1063
		PUSH	HL
		PUSH	DE
		CALL	l07b9
		POP	DE
		POP	HL
		PUSH	AF
		CALL	l107f
		POP	AF
		RET

l07b9:		LD	HL,$000B
		LD	E,H
		CALL	l107f
		LD	HL,$0023
		ADD	HL,BC
		LD	E,$03
		CALL	x07dc
		RET	NC

		XOR	A
		CALL	l1226
		RET	NC

		LD	HL,$0030
		ADD	HL,BC
		LD	E,$08
		CALL	x07dc
		RET	NC

		JP	l1267
x07dc:		LD	A,(HL)
		INC	HL
		CALL	l1226
		RET	NC

		DEC	E
		JR	NZ,x07dc	; (-$09)
		RET

l07e6:		LD	E,$0A
		LD	HL,l0797
		CALL	x07dc
		RET	NC

		LD	A,$00
		CALL	l1226
		RET	NC

		LD	E,$74
l07f7:		XOR	A
		CALL	l1226
		RET	NC

		DEC	E
		JR	NZ,l07f7	; (-$08)
		CALL	l1267
		RET	NC

l0803:		LD	HL,$0022
		ADD	HL,BC
		SET	6,(HL)
		SCF
		RET

l080b:		CALL	l125f
		JR	NC,l0857	; (+$47)
		JR	NZ,l085c	; (+$4A)
		LD	E,$0A
		LD	HL,l0797
x0817:		CALL	l114c
		JR	NC,l0857	; (+$3B)
		CP	(HL)
		INC	HL
		JR	NZ,l085c	; (+$3C)
		DEC	E
		JR	NZ,x0817	; (-$0C)
		CALL	l114c
		JR	NC,l0857	; (+$2F)
		CP	$01
		JR	NC,l085c	; (+$30)
		LD	HL,$0023
		ADD	HL,BC
		LD	E,$03
l0832:		CALL	l114c
		RET	NC

		LD	(HL),A
		INC	HL
		DEC	E
		JR	NZ,l0832	; (-$09)
		CALL	l114c
		RET	NC

		LD	HL,$0030
		ADD	HL,BC
		LD	E,$08
l0845:		CALL	l114c
		RET	NC

		LD	(HL),A
		INC	HL
		DEC	E
		JR	NZ,l0845	; (-$09)
		LD	HL,$0080
		LD	E,H
		CALL	l107f
		JR	l0803		; (-$54)
l0857:		CP	$19
		SCF
		CCF
		RET	NZ

l085c:		LD	HL,$0000
		LD	E,L
		CALL	l107f
l0863:		LD	HL,$0000
		LD	(filerecs),HL
		XOR	A
		LD	($DF92),A
		LD	HL,$0897
		CALL	l0d9e
		RET	NC

		LD	DE,($DF91)
		LD	HL,($DF8F)
		LD	L,$00
		SRL	D
		RR	E
		RR	H
		RR	L
		LD	A,D
		OR	A
		LD	A,$22
		RET	NZ

		PUSH	HL
		LD	HL,$0025
		ADD	HL,BC
		LD	(HL),E
		POP	DE
		DEC	HL
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		SCF
		RET

		CALL	l0d7a
		RET	NZ

		PUSH	BC
		LD	B,H
		LD	C,L
		CALL	l1445
		LD	B,A
		EX	DE,HL
		LD	HL,(filerecs)
		OR	A
		SBC	HL,DE
		LD	A,($DF92)
		SBC	A,B
		JR	NC,l08b7	; (+$08)
		LD	(filerecs),DE
		LD	A,B
		LD	($DF92),A
l08b7:		POP	BC
		SCF
		SBC	A,A
		RET

l08bb:		CALL	l0532
		RET	NC

		LD	IX,$0030
		ADD	IX,BC
		LD	HL,$0022
		ADD	HL,BC
		BIT	6,(HL)
		SCF
		RET

l08cd:		LD	E,C
		PUSH	DE
		CALL	l0532
		CALL	C,l0c0c
		CALL	C,l0756
		POP	DE
		RET	NC

		LD	HL,$0020
		ADD	HL,BC
		LD	D,(HL)
		LD	(HL),E
		PUSH	HL
		PUSH	DE
		CALL	l0586
		POP	DE
		POP	HL
		JR	NC,l08f9	; (+$10)
		BIT	1,E
		JR	Z,l08f5		; (+$08)
		CALL	l0eb3
		CALL	C,x1871
		JR	NC,l08f9	; (+$04)
l08f5:		SET	7,(HL)
		SCF
		RET

l08f9:		LD	(HL),D
		OR	A
		RET

l08fc:		CALL	l04ff
		CALL	l0c13
		RET	NC

		JP	l0f9e
l0906:		CP	$FF
		JR	Z,l0914		; (+$0A)
		CP	$10
		LD	B,A
		LD	A,$15
		RET	NC

		LD	A,B
		LD	(def_user),A
l0914:		LD	A,(def_user)
		SCF
		RET

l0919:		CALL	l04ff
		CP	$FF
		JR	Z,l0929		; (+$09)
		LD	B,A
		CALL	l17cb
		RET	NC

		LD	A,B
		LD	(def_drv),A
l0929:		LD	A,(def_drv)
		SCF
		RET

l092e:		LD	BC,sysfcb0
		CALL	l0aff
		CALL	C,l0c0c
		RET	NC

l0938:		CALL	l0576
		CALL	C,x1871
		RET	NC

		LD	HL,$0945
		JP	l09b7
		CALL	l0d74
		RET	NZ

		CALL	l0eb3
		RET	NC

		PUSH	HL
		PUSH	DE
		XOR	A
		CALL	l0f38
		POP	DE
		POP	HL
		LD	(HL),$E5
		CALL	l0e24
		RET	NC

		CALL	l1035
		SBC	A,A
		LD	(extchg),A
		RET

l0963:		LD	(att_clr),DE
		LD	BC,sysfcb0
		CALL	l0aff
		CALL	C,l0c0c
		CALL	C,x1871
		RET	NC

		LD	HL,$09C9
		JR	l09b1		; (+$38)
l0979:		PUSH	DE
		LD	BC,sysfcb1
		CALL	l0ae9
		CALL	C,l0c0c
		POP	HL
		PUSH	BC
		LD	BC,sysfcb0
		CALL	C,l0ae9
		POP	BC
		RET	NC

l098d:		LD	HL,$0021
		ADD	HL,BC
		LD	A,($DF41)
		XOR	(HL)
		LD	A,$1F
		RET	NZ

		CALL	x1871
		PUSH	BC
		LD	BC,sysfcb0
		CALL	C,l0576
		LD	HL,l0d74
		CALL	C,l0d9e
		POP	BC
		RET	NC

		CCF
		LD	A,$18
		RET	Z

		LD	HL,$09FE
l09b1:		PUSH	HL
		CALL	l0576
		POP	HL
		RET	NC

l09b7:		XOR	A
		LD	(extchg),A
		CALL	l0d9e
		RET	NC

		LD	A,(extchg)
		OR	A
		LD	A,$17
		CALL	NZ,l1685
		RET

		CALL	l0d74
		RET	NZ

		PUSH	BC
		LD	A,(att_set)
		LD	C,$FF
		PUSH	HL
		CALL	l09e2
		POP	HL
		LD	A,(att_clr)
		INC	C
		CALL	l09e2
		POP	BC
		JR	l0a1a		; (+$38)
l09e2:		RLA
		LD	B,$04
		INC	HL
		CALL	l09ef
		INC	HL
		INC	HL
		INC	HL
		INC	HL
		LD	B,$03
l09ef:		RLA
		JR	NC,l09fa	; (+$08)
		res	7,(HL)
		INC	C
		DEC	C
		JR	Z,l09fa		; (+$02)
		SET	7,(HL)
l09fa:		INC	HL
		DJNZ	l09ef		; (-$0E)
		RET

		CALL	l0d74
		RET	NZ

		CALL	l0eb3
		RET	NC

		PUSH	DE
		EX	DE,HL
		LD	HL,sysfcb0
		LD	A,(DE)
		AND	$10
		OR	(HL)
		LD	(DE),A
		INC	DE
		INC	HL
		PUSH	BC
		LD	BC,$000B
		LDIR
		POP	BC
		POP	DE
l0a1a:		CALL	l0e24
		RET	NC

		SBC	A,A
		LD	(extchg),A
		RET

l0a23:		LD	(cat_buff),DE
		LD	(cat_filt),BC
		LD	A,$01
		LD	(cat_ents),A
		LD	BC,sysfcb0
		CALL	l0aff
		CALL	C,l0c0c
		CALL	C,l05d6
		LD	HL,$0A47
		CALL	C,l0d9e
		LD	BC,(cat_size)
		RET

		PUSH	BC
		CALL	l0a4f
		POP	BC
		SCF
		SBC	A,A
		RET

l0a4f:		CALL	l0d7a
		RET	NZ

		LD	A,(cat_filt)
		RRA
		JR	C,l0a62		; (+$09)
		PUSH	HL
		LD	BC,$000A
		ADD	HL,BC
		BIT	7,(HL)
		POP	HL
		RET	NZ

l0a62:		LD	DE,(cat_buff)
		CALL	l0ad3
		RET	NC

		LD	BC,(cat_size)
l0a6e:		PUSH	HL
		LD	HL,$000D
		ADD	HL,DE
		EX	DE,HL
		POP	HL
		DEC	C
		DJNZ	l0a7b		; (+$03)
		RET	Z

		JR	l0aab		; (+$30)
l0a7b:		CALL	l0ad3
		JR	C,l0a6e		; (-$12)
		JR	Z,x0ac3		; (+$41)
		PUSH	HL
		PUSH	DE
		LD	HL,(cat_size)
		LD	H,$00
		DEC	HL
		LD	B,H
		LD	C,L
		ADD	HL,HL
		ADD	HL,BC
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,BC
		LD	BC,(cat_buff)
		ADD	HL,BC
		LD	A,L
		SUB	E
		LD	C,A
		LD	A,H
		SBC	A,D
		LD	B,A
		DEC	HL
		LD	DE,$000D
		EX	DE,HL
		ADD	HL,DE
		EX	DE,HL
		LD	A,B
		OR	C
		JR	Z,l0aa9		; (+$02)
		LDDR
l0aa9:		POP	DE
		POP	HL
l0aab:		PUSH	HL
		PUSH	DE
		INC	HL
		LD	BC,$000B
		LDIR
		XOR	A
		LD	(DE),A
		INC	DE
		LD	(DE),A
		LD	HL,(cat_size)
		LD	A,H
		CP	L
		ADC	A,$00
		LD	(cat_ents),A
		POP	DE
		POP	HL
x0ac3:		CALL	l0f6a
		EX	DE,HL
		LD	BC,$000B
		ADD	HL,BC
		LD	A,(HL)
		ADD	A,E
		LD	(HL),A
		INC	HL
		LD	A,(HL)
		ADC	A,D
		LD	(HL),A
		RET

l0ad3:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	B,$0B
		INC	HL
l0ad9:		LD	A,(HL)
		ADD	A,A
		LD	C,A
		LD	A,(DE)
		ADD	A,A
		CP	C
		JR	NZ,x0ae5	; (+$04)
		INC	DE
		INC	HL
		DJNZ	l0ad9		; (-$0C)
x0ae5:		POP	BC
		POP	DE
		POP	HL
		RET

l0ae9:		CALL	l0b49
		RET	NC

		LD	HL,$0001
		ADD	HL,BC
		LD	E,$0B
l0af3:		LD	A,(HL)
		INC	HL
		CP	$3F
		LD	A,$14
		RET	Z

		DEC	E
		JR	NZ,l0af3	; (-$0A)
		SCF
		RET

l0aff:		JP	l0b49
l0b02:		CALL	l0b0c
		JR	NC,l0b2b	; (+$24)
		CALL	l0b2b
		SCF
		RET

l0b0c:		CALL	l0b42
		RET	NC

		LD	E,A
		CALL	l0bdd
		CALL	C,l0b42
		JR	NC,l0b25	; (+$0C)
		LD	D,A
		LD	A,E
		ADD	A,A
		LD	E,A
		ADD	A,A
		ADD	A,A
		ADD	A,E
		ADD	A,D
		LD	E,A
		CALL	l0bdd
l0b25:		LD	A,E
		CP	$10
		RET	NC

		LD	(BC),A
		RET

l0b2b:		CALL	l0bd3
		RET	NC

		CP	$41
		CCF
		RET	NC

		CP	$51
		RET	NC

		PUSH	HL
		LD	HL,$0021
		ADD	HL,BC
		LD	(HL),A
		POP	HL
		CALL	l0bdd
		SCF
		RET

l0b42:		SUB	$30
		CCF
		RET	NC

		CP	$0A
		RET

l0b49:		PUSH	BC
		CALL	l0b51
		POP	BC
		LD	A,$14
		RET

l0b51:		PUSH	HL
		LD	HL,$0021
		ADD	HL,BC
		LD	A,(def_drv)
		LD	(HL),A
		POP	HL
		LD	A,(def_user)
		LD	(BC),A
		CALL	l0bd3
		JR	NC,l0b84	; (+$20)
		LD	E,A
		PUSH	HL
l0b66:		CP	$3A
		SCF
		JR	Z,l0b70		; (+$05)
		CALL	l0bdd
		JR	C,l0b66		; (-$0A)
l0b70:		POP	HL
		LD	A,E
		JR	NC,l0b8a	; (+$16)
		CALL	l0b02
		RET	NC

		CALL	l0bd3
		RET	NC

		XOR	$3A
		RET	NZ

		CALL	l0bcf
		JR	C,l0b8a		; (+$06)
l0b84:		INC	BC
		LD	E,$0B
		SCF
		JR	l0bc6		; (+$3C)
l0b8a:		INC	BC
		CP	$2E
		RET	Z

		LD	E,$08
		CALL	l0ba0
		CCF
		LD	E,$03
		JR	NC,l0bbd	; (+$25)
		XOR	$2E
		RET	NZ

		CALL	l0bcf
		JR	NC,l0bbd	; (+$1D)
l0ba0:		PUSH	HL
		CP	$20
		LD	HL,l0bf9
		CALL	NC,l0bef
		POP	HL
		JR	C,l0bbd		; (+$11)
		DEC	E
		RET	m
		CP	$2A
		CALL	Z,l0bc6
		LD	(BC),A
		INC	BC
		CALL	l0bdd
		JR	NZ,l0ba0	; (-$1A)
		CALL	C,l0bcf
l0bbd:		PUSH	AF
		LD	A,$20
		CALL	l0bc8
		POP	AF
		CCF
		RET

l0bc6:		LD	A,$3F
l0bc8:		INC	E
l0bc9:		DEC	E
		RET	Z

		LD	(BC),A
		INC	BC
		JR	l0bc9		; (-$06)
l0bcf:		CALL	l0bdd
		RET	NC

l0bd3:		CALL	l0be2
x0bd6:		RET	NZ

		CALL	l0bdd
		JR	C,x0bd6		; (-$06)
		RET

l0bdd:		LD	A,(HL)
		CP	$FF
		RET	Z

		INC	HL
l0be2:		LD	A,(HL)
		CP	$FF
		RET	Z

		AND	$7F
		CALL	l04ff
		CP	$20
		SCF
		RET

l0bef:		CP	(HL)
		SCF
		RET	Z
		INC	HL
		BIT	7,(HL)
		JR	Z,l0bef		; (-$08)
		OR	A
		RET

l0bf9:		DB	"!&()+,-./:"
		DB	$3B
		DB	"<=>[\\]|"
		DB	$80

l0c0c:		PUSH	HL
		LD	HL,$0021
		ADD	HL,BC
		LD	A,(HL)
		POP	HL
l0c13:		CALL	l17ef
		RET	NC

		BIT	0,(IX+$1B)
		SCF
		RET	NZ

		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l0ec1
		SET	1,(IX+$1B)
		XOR	A
		LD	(IX+$22),A
		LD	(IX+$23),A
		CALL	l0c60
		LD	BC,$0000
		LD	HL,$0C4D
		CALL	l0d9e
		LD	(IX+$24),C
		LD	(IX+$25),B
		POP	BC
		POP	DE
		POP	HL
		RET	NC

		SET	0,(IX+$1B)
		res	1,(IX+$1B)
		RET

		CALL	l0c53
		SCF
		SBC	A,A
		RET

l0c53:		LD	A,(HL)
		CP	$E5
		JP	Z,l1035
		LD	B,D
		LD	C,E
		LD	A,$FF
		JP	l0f38
l0c60:		LD	A,(IX+$07)
		LD	(IX+$24),A
		LD	A,(IX+$08)
		LD	(IX+$25),A
		RET

l0c6d:		CALL	l0d1e
		RET	C

		PUSH	HL
		CALL	l0cb6
		POP	HL
		RET	NC

l0c77:		EX	DE,HL
		LD	HL,$000C
		ADD	HL,BC
		LD	(HL),D
		INC	HL
		INC	HL
		LD	(HL),E
		LD	E,$11
		XOR	A
l0c83:		INC	HL
		LD	(HL),A
		DEC	E
		JR	NZ,l0c83	; (-$05)
		CALL	l0d3a
		RET	C

		LD	HL,$0022
		ADD	HL,BC
		SET	2,(HL)
		OR	A
		RET

l0c94:		CALL	l0d1e
		JR	NC,l0ca4	; (+$0B)
		LD	HL,$0022
		ADD	HL,BC
		BIT	2,(HL)
		JP	NZ,l0fcb
		SCF
		RET

l0ca4:		PUSH	HL
		CALL	l0cb6
		POP	HL
		RET	NC

x0caa:		CALL	l0c77
		RET	C

		CP	$19
		SCF
		CCF
		RET	NZ

		JP	l0fcb
l0cb6:		LD	HL,$0022
		ADD	HL,BC
		LD	A,(HL)
		AND	$03
		SCF
		RET	Z

		CP	$02
		JP	Z,l102d
		AND	$02
		JR	NZ,l0cd5	; (+$0D)
		LD	HL,$0D50
		CALL	l0d9e
		RET	NC

		LD	A,$20
		CCF
		RET	NZ

		JR	l0ceb		; (+$16)
l0cd5:		CALL	l0feb
		RET	NC

		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l103e
		JR	NZ,l0ce8	; (+$07)
		LD	B,$0A
l0ce3:		LD	(HL),$00
		INC	HL
		DJNZ	l0ce3		; (-$05)
l0ce8:		POP	BC
		POP	DE
		POP	HL
l0ceb:		PUSH	DE
		PUSH	BC
		EX	DE,HL
		LD	H,B
		LD	L,C
		LD	BC,$0020
		LDIR
		POP	BC
		POP	DE
		CALL	l0e24
		CALL	C,l1685
		LD	HL,$0022
		ADD	HL,BC
		res	0,(HL)
		SCF
		RET

l0d05:		PUSH	BC
		LD	A,(IX+$04)
		CPL
		AND	$1F
		LD	B,A
		LD	A,D
		RRA
		RRA
		RRA
		RRA
		AND	$0F
		LD	L,A
		LD	A,E
		ADD	A,A
		LD	A,D
		ADC	A,A
		AND	B
		LD	H,A
		LD	A,B
		POP	BC
		RET

l0d1e:		CALL	l0d05
		PUSH	HL
		PUSH	DE
		PUSH	BC
		EX	DE,HL
		LD	HL,$000E
		ADD	HL,BC
		LD	B,A
		LD	A,(HL)
		XOR	E
		JR	NZ,l0d36	; (+$08)
		DEC	HL
		DEC	HL
		LD	A,(HL)
		XOR	D
		AND	B
		JR	NZ,l0d36	; (+$01)
		SCF
l0d36:		POP	BC
		POP	DE
		POP	HL
		RET

l0d3a:		LD	HL,$0D50
		CALL	l0d9e
		RET	NC

		CCF
		LD	A,$19
		RET	NZ

		PUSH	BC
		LD	D,B
		LD	E,C
		LD	BC,$0020
		LDIR
		POP	BC
		SCF
		RET

		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,(BC)
		XOR	(HL)
		CALL	Z,l0d87
		JR	NZ,l0d72	; (+$18)
		LD	A,(DE)
		INC	A
		JR	Z,l0d68		; (+$0A)
		LD	A,(IX+$04)
		CPL
		LD	B,A
		LD	A,(DE)
		XOR	(HL)
		AND	B
		JR	NZ,l0d72	; (+$0A)
l0d68:		INC	DE
		INC	HL
		INC	DE
		INC	HL
		LD	A,(DE)
		CP	$FF
		JR	Z,l0d72		; (+$01)
		XOR	(HL)
l0d72:		JR	l0d82		; (+$0E)
l0d74:		LD	A,(BC)
		XOR	(HL)
		AND	$EF
		JR	l0d7c		; (+$02)
l0d7a:		LD	A,(BC)
		XOR	(HL)
l0d7c:		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	Z,l0d87
l0d82:		POP	BC
		POP	DE
		POP	HL
		SCF
		RET

l0d87:		PUSH	BC
		LD	D,B
		LD	E,C
		INC	DE
		INC	HL
		LD	B,$0B
l0d8e:		LD	A,(DE)
		CP	$3F
		JR	Z,l0d98		; (+$05)
		XOR	(HL)
		AND	$7F
		JR	NZ,l0d9c	; (+$04)
l0d98:		INC	DE
		INC	HL
		DJNZ	l0d8e		; (-$0E)
l0d9c:		POP	BC
		RET

l0d9e:		LD	(rt_dirent),HL
		CALL	l15bf
		LD	DE,$0000
		PUSH	AF
l0da8:		LD	A,E
		AND	$0F
		JR	NZ,l0db3	; (+$06)
		POP	AF
		CALL	l0def
		RET	NC

		PUSH	AF
l0db3:		POP	AF
		PUSH	AF
		PUSH	HL
		PUSH	IX
		PUSH	DE
		PUSH	BC
		LD	C,A
		LD	B,$07
		LD	A,E
		CALL	l0e5a
		CALL	l0245
		POP	BC
		POP	DE
		POP	IX
		PUSH	DE
		CALL	l0ddd
		POP	DE
		POP	HL
		JR	NC,l0dd7	; (+$07)
		JR	Z,l0dd7		; (+$05)
		CALL	l0de5
		JR	NC,l0da8	; (-$2F)
l0dd7:		LD	HL,direntry
		INC	SP
		INC	SP
		RET

l0ddd:		LD	HL,(rt_dirent)
		PUSH	HL
		LD	HL,direntry
		RET

l0de5:		INC	DE
		LD	A,(IX+$24)
		SUB	E
		LD	A,(IX+$25)
		SBC	A,D
		RET

l0def:		PUSH	BC
		PUSH	DE
		LD	A,$04
		CALL	l04eb
		CALL	x1896
l0df9:		CALL	l1560
		JR	NC,l0e21	; (+$23)
		LD	B,A
		PUSH	HL
		CALL	l0e6d
		JR	C,l0e1d		; (+$18)
		BIT	1,(IX+$1B)
		JR	Z,l0e0c		; (+$01)
		LD	(HL),A
l0e0c:		CP	(HL)
		SCF
		JR	Z,l0e1d		; (+$0D)
		CALL	l15d8
		LD	A,$08
		CALL	l1a9d
		JR	NZ,l0e1d	; (+$03)
		POP	HL
		JR	l0df9		; (-$24)
l0e1d:		POP	HL
		JR	NC,l0e21	; (+$01)
		LD	A,B
l0e21:		POP	DE
		POP	BC
		RET

l0e24:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	C,E
		LD	A,$04
		CALL	l04eb
		CALL	x1896
		PUSH	BC
		LD	BC,$0001
		CALL	x1590
		POP	BC
		JR	NC,l0e56	; (+$1C)
		LD	B,A
		PUSH	IX
		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,C
		CALL	l0e5a
		EX	DE,HL
		LD	C,$07
		CALL	l0245
		POP	BC
		POP	DE
		POP	HL
		POP	IX
		CALL	l0e6d
		JR	C,l0e56		; (+$02)
		LD	(HL),A
		SCF
l0e56:		POP	BC
		POP	DE
		POP	HL
		RET

l0e5a:		AND	$0F
		JR	Z,l0e65		; (+$07)
		LD	DE,$0020
l0e61:		ADD	HL,DE
		DEC	A
		JR	NZ,l0e61	; (-$04)
l0e65:		LD	DE,direntry
		LD	IX,$0020
		RET

l0e6d:		PUSH	HL
		PUSH	DE
		EX	DE,HL
		LD	E,(IX+$0B)
		LD	A,(IX+$0C)
		AND	$7F
		LD	D,A
		LD	A,$02
		CALL	l04eb
		CALL	x1896
		SBC	HL,DE
		CCF
		POP	DE
		POP	HL
		RET	C

		PUSH	BC
		LD	A,B
		CALL	l021b
		PUSH	AF
		XOR	A
		LD	BC,$0002
l0e91:		ADD	A,(HL)
		INC	HL
		DJNZ	l0e91		; (-$04)
		DEC	C
		JR	NZ,l0e91	; (-$07)
		LD	B,A
		POP	AF
		CALL	l021b
		LD	L,(IX+$26)
		LD	H,(IX+$27)
		ADD	HL,DE
		PUSH	DE
		LD	DE,$0000
		CALL	x1896
		OR	A
		SBC	HL,DE
		POP	DE
		LD	A,B
		OR	A
		POP	BC
		RET

l0eb3:		PUSH	DE
		EX	DE,HL
		LD	HL,$0009
		ADD	HL,DE
		LD	A,(HL)
		ADD	A,A
		EX	DE,HL
		POP	DE
		CCF
		LD	A,$1C
		RET

l0ec1:		CALL	l0fbe
		LD	A,$03
		CALL	l04eb
		INC	DE
		PUSH	HL
l0ecb:		LD	(HL),$00
		INC	HL
		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,l0ecb	; (-$08)
		POP	HL
		LD	A,(IX+$09)
		LD	(HL),A
		INC	HL
		LD	A,(IX+$0A)
		LD	(HL),A
		RET

l0ede:		PUSH	BC
		PUSH	HL
		PUSH	DE
		LD	A,$03
		CALL	l04eb
		PUSH	DE
		CALL	l0fbe
		POP	DE
		ADD	HL,DE
		POP	DE
		LD	A,E
		AND	$07
		LD	B,A
		LD	A,$01
		INC	B
l0ef4:		RRCA
		DJNZ	l0ef4		; (-$03)
		LD	B,A
		AND	C
		LD	C,A
		LD	A,B
		CPL
		AND	(HL)
		OR	C
		LD	(HL),A
		POP	HL
		POP	BC
		RET

l0f02:		PUSH	HL
		PUSH	BC
		CALL	l0fbe
l0f07:		LD	BC,$0880
l0f0a:		LD	A,(HL)
		AND	C
		JR	Z,l0f1c		; (+$0E)
		RRC	C
		LD	A,D
		OR	E
		LD	A,$1A
		JR	Z,l0f32		; (+$1C)
		DEC	DE
		DJNZ	l0f0a		; (-$0F)
		INC	HL
		JR	l0f07		; (-$15)
l0f1c:		LD	A,(HL)
		OR	C
		LD	(HL),A
		LD	A,(IX+$05)
		SUB	E
		LD	E,A
		LD	A,(IX+$06)
		SBC	A,D
		LD	D,A
		POP	BC
		PUSH	BC
		LD	HL,$0022
		ADD	HL,BC
		SET	0,(HL)
		SCF
l0f32:		POP	BC
		POP	HL
		RET

l0f35:		LD	H,B
		LD	L,C
		XOR	A
l0f38:		PUSH	BC
		LD	C,A
		LD	A,$0F
		CP	(HL)
		JR	C,l0f67		; (+$28)
		LD	DE,$0010
		ADD	HL,DE
		LD	B,$10
		INC	B
		JR	l0f65		; (+$1D)
l0f48:		LD	E,(HL)
		INC	HL
		LD	A,(IX+$06)
		OR	A
		LD	D,A
		JR	Z,l0f54		; (+$03)
		DEC	B
		LD	D,(HL)
		INC	HL
l0f54:		LD	A,D
		OR	E
		JR	Z,l0f65		; (+$0D)
		PUSH	HL
		LD	A,(IX+$05)
		SUB	E
		LD	A,(IX+$06)
		SBC	A,D
		CALL	NC,l0ede
		POP	HL
l0f65:		DJNZ	l0f48		; (-$1F)
l0f67:		POP	BC
		SCF
		RET

l0f6a:		PUSH	DE
		EX	DE,HL
		LD	A,(DE)
		CP	$10
		LD	HL,$0000
		JR	NC,l0f8e	; (+$1A)
		LD	HL,$0010
		ADD	HL,DE
		LD	DE,$1000
l0f7b:		LD	A,(IX+$06)
		OR	A
		LD	A,(HL)
		INC	HL
		JR	Z,l0f86		; (+$03)
		OR	(HL)
		DEC	D
		INC	HL
l0f86:		OR	A
		JR	Z,l0f8a		; (+$01)
		INC	E
l0f8a:		DEC	D
		JR	NZ,l0f7b	; (-$12)
		EX	DE,HL
l0f8e:		POP	DE
l0f8f:		LD	A,(IX+$02)
		DEC	A
		DEC	A
l0f94:		DEC	A
		JR	Z,x0f9a		; (+$03)
		ADD	HL,HL
		JR	l0f94		; (-$06)
x0f9a:		LD	A,H
		OR	L
		SCF
		RET

l0f9e:		LD	HL,$0000
		PUSH	HL
		CALL	l0fbe
l0fa5:		LD	BC,$0880
l0fa8:		LD	A,(HL)
		AND	C
		JR	NZ,l0faf	; (+$03)
		EX	(SP),HL
		INC	HL
		EX	(SP),HL
l0faf:		RRC	C
		LD	A,D
		OR	E
		JR	Z,l0fbb		; (+$06)
		DEC	DE
		DJNZ	l0fa8		; (-$10)
		INC	HL
		JR	l0fa5		; (-$16)
l0fbb:		POP	HL
		JR	l0f8f		; (-$2F)
l0fbe:		LD	L,(IX+$28)
		LD	H,(IX+$29)
		LD	E,(IX+$05)
		LD	D,(IX+$06)
		RET

l0fcb:		LD	A,(IX+$22)
		OR	(IX+$23)
		LD	A,$1B
		RET	Z

		LD	HL,$0022
		ADD	HL,BC
		SET	1,(HL)
		res	2,(HL)
		LD	A,(IX+$22)
		SUB	$01
		LD	(IX+$22),A
		JR	NC,l0fe9	; (+$03)
		DEC	(IX+$23)
l0fe9:		SCF
		RET

l0feb:		PUSH	BC
		LD	C,(IX+$24)
		LD	B,(IX+$25)
		CALL	l0c60
		LD	HL,$1028
		CALL	l0d9e
		JR	NC,l1026	; (+$29)
		EX	(SP),HL
		PUSH	HL
		PUSH	DE
		PUSH	AF
		LD	DE,$0022
		ADD	HL,DE
		res	1,(HL)
		POP	AF
		POP	DE
		POP	HL
		EX	(SP),HL
		JR	NZ,l1020	; (+$13)
		EX	DE,HL
		OR	A
		SBC	HL,BC
		ADD	HL,BC
		EX	DE,HL
		JR	C,l1017		; (+$02)
		LD	B,D
		LD	C,E
l1017:		LD	(IX+$24),C
		LD	(IX+$25),B
		SCF
		JR	l1026		; (+$06)
l1020:		CALL	l1035
		LD	A,$20
		OR	A
l1026:		POP	BC
		RET

		LD	A,(HL)
		XOR	$E5
		SCF
		RET

l102d:		PUSH	HL
		LD	HL,$0022
		ADD	HL,BC
		res	1,(HL)
		POP	HL
l1035:		SCF
		INC	(IX+$22)
		RET	NZ

		INC	(IX+$23)
		RET

l103e:		LD	A,E
		AND	$03
		CPL
		ADD	A,$04
		JR	Z,l104d		; (+$07)
		LD	BC,$0020
l1049:		ADD	HL,BC
		DEC	A
		JR	NZ,l1049	; (-$04)
l104d:		LD	A,(HL)
		CP	$21
		RET	NZ

		LD	A,E
		AND	$03
		JR	Z,l105d		; (+$07)
		LD	BC,$000A
l1059:		ADD	HL,BC
		DEC	A
		JR	NZ,l1059	; (-$04)
l105d:		INC	HL
		RET

l105f:		CALL	l0532
		RET	NC

l1063:		LD	HL,$0026
		JR	l106f		; (+$07)
l1068:		CALL	l0532
		RET	NC

		LD	HL,$0023
l106f:		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		EX	DE,HL
		LD	E,A
		LD	D,$00
		SCF
		RET

l107b:		CALL	l0532
		RET	NC

l107f:		LD	A,L
		LD	D,E
		LD	E,H
		LD	HL,$0026
		ADD	HL,BC
		JR	l109c		; (+$14)
l1088:		LD	A,$80
		JR	l108e		; (+$02)
l108c:		LD	A,$01
l108e:		LD	HL,$0026
		ADD	HL,BC
		ADD	A,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		JR	NC,l109a	; (+$01)
		INC	DE
l109a:		DEC	HL
		DEC	HL
l109c:		PUSH	HL
		PUSH	AF
		XOR	(HL)
		JP	m,l10ac
		INC	HL
		LD	A,(HL)
		CP	E
		JR	NZ,l10ac	; (+$05)
		INC	HL
		LD	A,(HL)
		CP	D
		JR	Z,l10b2		; (+$06)
l10ac:		LD	HL,$0022
		ADD	HL,BC
		res	5,(HL)
l10b2:		POP	AF
		POP	HL
		LD	(HL),A
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		SCF
		RET

l10bb:		PUSH	BC
		PUSH	AF
		LD	HL,$0022
		ADD	HL,BC
		EX	DE,HL
		LD	HL,$0025
		ADD	HL,BC
		LD	B,$03
		OR	A
l10c9:		INC	DE
		INC	HL
		LD	A,(DE)
		SBC	A,(HL)
		DJNZ	l10c9		; (-$06)
		JR	NC,l10d6	; (+$05)
		LD	BC,$0003
		LDDR
l10d6:		POP	AF
		POP	BC
		RET

l10d9:		CALL	l114c
		RET	NC

		PUSH	DE
		LD	E,A
		LD	A,(rw_page)
		LD	HL,(rw_add)
		CALL	l021b
		LD	(HL),E
		CALL	l021b
		INC	HL
		LD	(rw_add),HL
		POP	DE
		LD	A,D
		OR	E
		DEC	DE
		SCF
		RET

l10f6:		PUSH	DE
		CALL	l12fe
		POP	DE
		RET	NC

		PUSH	DE
		CALL	l1088
		LD	HL,$002D
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		PUSH	BC
		LD	C,(HL)
		LD	A,(rw_page)
		LD	B,A
		LD	HL,(rw_add)
		EX	DE,HL
		LD	IX,$0080
		CALL	l0245
		POP	BC
		LD	(rw_add),DE
		POP	DE
		LD	HL,$FF81
		ADD	HL,DE
		EX	DE,HL
		LD	A,D
		OR	E
		DEC	DE
		SCF
		RET

; DOS_BYTE_READ

l1129:		CALL	x0521		; get FCB & test if open for reading
		RET	NC		; exit if error
		LD	HL,$0026
		ADD	HL,BC
		EX	DE,HL		; DE=filepointer address
		LD	HL,$0023
		ADD	HL,BC		; HL=filelength address
		PUSH	BC
		LD	B,$03
		OR	A
l113a:		LD	A,(DE)		; test filepointer (carry must be set
		SBC	A,(HL)		; if pointer within file)
		INC	DE
		INC	HL
		DJNZ	l113a
		POP	BC
		LD	A,$19		; error "end of file"
		CALL	C,l114c		; get a byte if within file
		RET	NC		; exit if error
		LD	C,A		; C=byte read
		CP	$1A		; set Z if soft-EOF
		SCF			; success
		RET

; Subroutine to read a byte (A) from the file

l114c:		PUSH	HL
		PUSH	DE
		LD	HL,$0022
		ADD	HL,BC
		BIT	5,(HL)		; has record been changed?
		JR	NZ,l115b
		CALL	l12fe		; if so, get new record details into FCB
		JR	NC,x117c	; exit if error
l115b:		LD	HL,$0026
		ADD	HL,BC
		LD	A,(HL)		; low byte of filepointer
		AND	$7F		; offset into record
		LD	HL,$002D
		ADD	HL,BC
		ADD	A,(HL)
		LD	E,A
		INC	HL
		ADC	A,(HL)
		SUB	E
		LD	D,A		; DE=address of byte in buffer
		INC	HL
		LD	A,(HL)		; A=bank of buffer  *** de DONDE mierda sale este dato quien le manda decir que = 1????
		EX	DE,HL
		CALL	l021b		; page in buffer bank
		LD	D,(HL)		; get byte
		CALL	l021b		; page back original bank
		PUSH	DE
		CALL	l108c		; increment filepointer
		POP	AF		; A=byte
		SCF			; success
x117c:		POP	DE
		POP	HL
		RET

l117f:		LD	A,C
		LD	(rw_page),A
		LD	(rw_add),HL
		CALL	l0529
		RET	NC

		ADD	HL,DE
		PUSH	HL
		CALL	l119f
		CALL	l10bb
		POP	HL
		RET	C

		PUSH	AF
		LD	DE,(rw_add)
		OR	A
		SBC	HL,DE
		EX	DE,HL
		POP	AF
		RET

l119f:		DEC	DE
l11a0:		LD	HL,$0026
		ADD	HL,BC
		LD	A,(HL)
		AND	$7F
		JR	Z,l11b0		; (+$07)
		CALL	l11c4
		RET	NC

		RET	Z

		JR	l11a0		; (-$10)
l11b0:		LD	HL,$FF81
		ADD	HL,DE
		JR	NC,l11bd	; (+$07)
		CALL	l11e2
		RET	NC

		RET	Z

		JR	l11b0		; (-$0D)
l11bd:		CALL	l11c4
		RET	NC

		RET	Z

		JR	l11bd		; (-$07)
l11c4:		LD	A,(rw_page)
		LD	HL,(rw_add)
		CALL	l021b
		LD	L,(HL)
		CALL	l021b
		LD	A,L
		CALL	l1226
		RET	NC

		LD	HL,(rw_add)
		INC	HL
		LD	(rw_add),HL
		LD	A,D
		OR	E
		DEC	DE
		SCF
		RET

l11e2:		PUSH	DE
		CALL	l12cf
		POP	DE
		RET	NC

		LD	HL,$0022
		ADD	HL,BC
		SET	4,(HL)
		PUSH	DE
		CALL	l1088
		LD	HL,$002D
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		PUSH	BC
		LD	B,(HL)
		LD	A,(rw_page)
		LD	C,A
		LD	HL,(rw_add)
		LD	IX,$0080
		CALL	l0245
		POP	BC
		LD	(rw_add),HL
		POP	DE
		LD	HL,$FF81
		ADD	HL,DE
		EX	DE,HL
		LD	A,D
		OR	E
		DEC	DE
		SCF
		RET

l1219:		LD	E,C
		CALL	l0529
		RET	NC

		LD	A,E
		CALL	l1226
		CALL	C,l10bb
		RET

l1226:		PUSH	HL
		PUSH	DE
		LD	E,A
		LD	HL,$0022
		ADD	HL,BC
		BIT	5,(HL)
		JR	NZ,l123a	; (+$09)
		PUSH	HL
		PUSH	DE
		CALL	l12cf
		POP	DE
		POP	HL
		JR	NC,l125c	; (+$22)
l123a:		SET	4,(HL)
		LD	HL,$0026
		ADD	HL,BC
		LD	A,(HL)
		AND	$7F
		LD	HL,$002D
		ADD	HL,BC
		PUSH	DE
		ADD	A,(HL)
		LD	E,A
		INC	HL
		ADC	A,(HL)
		SUB	E
		LD	D,A
		INC	HL
		LD	A,(HL)
		EX	DE,HL
		POP	DE
		CALL	l021b
		LD	(HL),E
		CALL	l021b
		CALL	l108c
l125c:		POP	DE
		POP	HL
		RET

l125f:		CALL	l1278
		RET	NC

		LD	A,D
		CP	E
		SCF
		RET

l1267:		CALL	l1278
		RET	NC

		CALL	l021b
		LD	(HL),E
		CALL	l021b
		CALL	l1088
		JP	l10bb
l1278:		LD	HL,$0000
		LD	E,H
		CALL	l107f
		LD	HL,$0022
		ADD	HL,BC
		BIT	5,(HL)
		JR	NZ,l128b	; (+$04)
		CALL	l12fe
		RET	NC

l128b:		LD	HL,$002D
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		EX	DE,HL
		PUSH	AF
		CALL	l021b
		PUSH	AF
		XOR	A
		LD	E,$7F
l129d:		ADD	A,(HL)
		INC	HL
		DEC	E
		JR	NZ,l129d	; (-$05)
		LD	E,A
		LD	D,(HL)
		POP	AF
		CALL	l021b
		POP	AF
		SCF
		RET

l12ab:		PUSH	HL
		LD	HL,$0022
		ADD	HL,BC
		BIT	3,(HL)
		JR	Z,l12c8		; (+$14)
		BIT	4,(HL)
		JR	Z,l12c8		; (+$10)
		PUSH	HL
		PUSH	DE
		LD	HL,$002B
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		CALL	x1590
		POP	DE
		POP	HL
		JR	NC,l12cd	; (+$05)
l12c8:		LD	A,(HL)
		AND	$C7
		LD	(HL),A
		SCF
l12cd:		POP	HL
		RET

l12cf:		LD	A,(BC)
		CP	$22
		LD	HL,$12DC
		JR	NZ,l1309	; (+$32)
		LD	HL,$146E
		JR	l1309		; (+$2D)
		PUSH	DE
		CALL	l0c94
		POP	DE
		CALL	C,l1371
		RET	NC

		PUSH	HL
		PUSH	DE
		PUSH	AF
		CALL	l1353
		CALL	l1433
		CALL	NC,l1416
		POP	AF
		POP	DE
		POP	HL
		RET

; Subroutine to get the abs log sector (DE) and address (AHL) of
; record DE in the current file

		PUSH	DE
		CALL	l0c6d		; find extent HL for file
		POP	DE
		CALL	C,x13a4		; if found, get address of record
		RET

; Subroutine to get the current record into a buffer and update the FCB
; with its details

l12fe:		LD	A,(BC)
		CP	$22		; test for "drive open as file"
		LD	HL,$12F5	; routine to use for normal file
		JR	NZ,l1309
		LD	HL,$146E	; routine to use for drive
l1309:		CALL	l135d		; DE=record number for filepointer
		RET	NC		; exit if file too big
		PUSH	HL
		LD	HL,$0022
		ADD	HL,BC
		BIT	3,(HL)
		POP	HL
		JR	Z,l1323		; move on if no sector currently in buffer
		PUSH	HL
		EX	DE,HL
		CALL	l1353		; get record number from FCB
		EX	DE,HL
		OR	A
		SBC	HL,DE
		POP	HL
		JR	Z,l1349		; move on if record numbers match
l1323:		CALL	l0c0c		; ensure correct disk logged in
		CALL	C,l12ab		; get sector in FCB to buffer
		RET	NC		; exit if error
		PUSH	HL
		LD	HL,$0029
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	HL
		CALL	l1353		; DE=record number required
		CALL	l04fe		; call routine in HL
		RET	NC		; exit if error
		PUSH	HL
		LD	HL,$002B
		ADD	HL,BC
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store abs logical sector number
		POP	DE
		INC	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D		; store add of record
		INC	HL
		LD	(HL),A		; store bank for buffer
l1349:		LD	HL,$0022
		ADD	HL,BC
		LD	A,(HL)
		OR	$28		; set bit 3 (valid sector) & bit 5
		LD	(HL),A		; (valid filepointer)
		SCF
		RET

l1353:		PUSH	HL
		LD	HL,$0029
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		POP	HL
		RET

l135d:		PUSH	HL
		LD	HL,$0026
		ADD	HL,BC
		LD	A,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		ADD	A,A
		ADC	HL,HL
		EX	DE,HL
		CCF
		LD	A,$22
		POP	HL
		RET

l1371:		PUSH	DE
		CALL	l13dc
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL
		JR	C,x138b		; (+$11)
		CALL	l0f02
		JR	NC,l1394	; (+$15)
		LD	(HL),E
		LD	A,(IX+$06)
		OR	A
		JR	Z,l1388		; (+$02)
		INC	HL
		LD	(HL),D
l1388:		EX	DE,HL
		JR	l1392		; (+$07)
x138b:		LD	A,E
		AND	$03
		SCF
		CALL	Z,l1433
l1392:		SBC	A,A
		SCF
l1394:		POP	DE
		CALL	C,l13bd
		PUSH	HL
		CALL	C,l139e
		JR	l13b6		; (+$18)
l139e:		JP	Z,l1578
		JP	l1560

; Subroutine to find abs log sector (DE) and record address (AHL)
; for record DE in current file

x13a4:		PUSH	DE
		CALL	l13dc		; get block & offset
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL
		CALL	C,l1433		; check record is in this extent
		POP	DE
		CALL	C,l13bd		; calculate sector & offset
x13b2:		PUSH	HL		; stack offset into sector
		CALL	C,l1560		; get AHL=address of sector DE
l13b6:		EX	DE,HL
		EX	(SP),HL		; stack sector, restore offset
		PUSH	AF
		ADD	HL,DE		; now AHL=address of record
		POP	AF
		POP	DE		; restore sector
		RET

l13bd:		PUSH	BC
		PUSH	AF
		EX	DE,HL
		LD	A,(IX+$02)
		SUB	$02
		CALL	NZ,l04f5
		CALL	x1896
		EX	DE,HL
		LD	B,D
		LD	A,D
		AND	$01
		LD	D,A
		EX	DE,HL
		XOR	B
		RRCA
		ADD	A,E
		LD	E,A
		ADC	A,D
		SUB	E
		LD	D,A
		POP	AF
		POP	BC
		RET

l13dc:		PUSH	BC
		LD	H,B
		LD	L,C
		LD	A,(IX+$03)
		AND	E
		RRA
		LD	B,A
		LD	A,$00
		RRA
		LD	C,A
		LD	A,(IX+$02)
		CALL	l04eb
		LD	D,$00
		LD	A,(IX+$06)
		OR	A
		LD	A,E
		JR	Z,l1403		; (+$0B)
		AND	$07
		ADD	A,A
		ADD	A,$11
		LD	E,A
		ADD	HL,DE
		LD	D,(HL)
		DEC	HL
		JR	l1409		; (+$06)
l1403:		AND	$0F
		ADD	A,$10
		LD	E,A
		ADD	HL,DE
l1409:		LD	E,(HL)
		LD	A,D
		OR	E
		LD	A,$19
		JR	Z,l1412		; (+$02)
		EX	DE,HL
		SCF
l1412:		LD	D,B
		LD	E,C
		POP	BC
		RET

l1416:		PUSH	HL
		LD	A,E
		AND	$7F
		INC	A
		LD	HL,$000F
		ADD	HL,BC
		LD	(HL),A
		LD	A,E
		RLA
		LD	A,D
		RLA
		AND	$1F
		DEC	HL
		DEC	HL
		DEC	HL
		LD	(HL),A
		LD	HL,$0022
		ADD	HL,BC
		SET	0,(HL)
		SCF
		POP	HL
		RET

l1433:		PUSH	DE
		PUSH	HL
		CALL	l1445
		OR	A
		LD	A,$22
		JR	NZ,x1442	; (+$05)
		EX	DE,HL
		SBC	HL,DE
		LD	A,$19
x1442:		POP	HL
		POP	DE
		RET

l1445:		PUSH	DE
		LD	HL,$000C
		ADD	HL,BC
		LD	D,(HL)
		LD	E,$00
		SRL	D
		RR	E
		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		OR	A
		JP	p,l145b
		LD	A,$80
l145b:		ADD	A,E
		LD	E,A
		ADC	A,D
		SUB	E
		DEC	HL
		LD	L,(HL)
		LD	H,$00
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	A,L
		LD	D,A
		ADC	A,H
		SUB	D
		EX	DE,HL
		POP	DE
		RET

		LD	A,E
		AND	$03
		LD	H,A
		LD	L,$00
		SRL	H
		RR	L
		LD	A,$02
		CALL	l04eb
		PUSH	HL
		PUSH	DE
		EX	DE,HL
		LD	A,(IX+$02)
		LD	E,(IX+$05)
		LD	D,(IX+$06)
		INC	DE
		SUB	$02
		CALL	NZ,l04f5
		CALL	x1896
		OR	A
		SBC	HL,DE
		POP	DE
		POP	HL
		LD	A,$19
		JP	x13b2
l149c:		LD	DE,(cachenum)
		RET

l14a1:		CALL	l1672
		RET	NC

l14a5:		LD	HL,$0000
		LD	(bcb_inuse),HL
		LD	(bcb_free),HL
		LD	H,D
		LD	(cachenum),HL
		LD	IX,bcbs
		LD	B,$10
		LD	A,$07
		LD	HL,cache7
		JR	l14cd		; (+$0E)
l14bf:		LD	A,E
		OR	A
		SCF
		RET	Z

		LD	HL,cachenum
		INC	(HL)
		LD	A,D
		INC	D
		DEC	E
		CALL	l0233
l14cd:		LD	(IX+$08),L
		LD	(IX+$09),H
		LD	(IX+$0A),A
		LD	HL,(bcb_free)
		LD	(IX+$00),L
		LD	(IX+$01),H
		LD	(bcb_free),IX
		EX	DE,HL
		LD	DE,$000B
		ADD	IX,DE
		EX	DE,HL
		DJNZ	l14bf		; (-$2D)
		SCF
		RET

l14ee:		LD	DE,bcb_inuse
l14f1:		EX	DE,HL
		CALL	l1707
		CCF
		LD	A,$21
		RET	Z

		PUSH	HL
		LD	HL,$0005
		ADD	HL,DE
		LD	A,(HL)
		CP	(IX+$1C)
		JR	NZ,x150c	; (+$08)
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	A
		SBC	HL,BC
x150c:		POP	HL
		JR	NZ,l14f1	; (-$1E)
		SCF
		RET

l1511:		CALL	l1704
		JR	NZ,l1530	; (+$1A)
		LD	DE,bcb_inuse
l1519:		EX	DE,HL
		CALL	l1707
		CCF
		LD	A,$21
		RET	Z

		CALL	l170f
		JR	NZ,l1519	; (-$0D)
		CALL	x1547
		CALL	l1658
		RET	NC

		CALL	l1716
l1530:		PUSH	HL
		LD	HL,$0002
		ADD	HL,DE
		XOR	A
		LD	(HL),A
		INC	HL
		LD	(HL),A
		INC	HL
		LD	(HL),A
		INC	HL
		LD	A,(IX+$1C)
		LD	(HL),A
		INC	HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		POP	HL
		SCF
		RET

x1547:		PUSH	IX
		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	HL,$0005
		ADD	HL,DE
		LD	A,(HL)
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		EX	DE,HL
		LD	D,A
		CALL	l05fd
		POP	BC
		POP	DE
		POP	HL
		POP	IX
		RET

l1560:		BIT	3,(IX+$1B)
		JP	NZ,l1b35
		PUSH	DE
		PUSH	BC
		LD	B,D
		LD	C,E
		CALL	l14ee
		JR	C,l1576		; (+$06)
		CALL	l1511
		CALL	C,l16e4
l1576:		JR	l1589		; (+$11)
l1578:		BIT	3,(IX+$1B)
		JP	NZ,l1b35
		PUSH	DE
		PUSH	BC
		LD	B,D
		LD	C,E
		CALL	l14ee
		CALL	NC,l1511
l1589:		PUSH	AF
		CALL	C,l171e
		POP	AF
		JR	l15af		; (+$1F)
x1590:		BIT	3,(IX+$1B)
		JP	NZ,l1b35
		PUSH	DE
		PUSH	BC
		PUSH	BC
		LD	B,D
		LD	C,E
		CALL	l14ee
		POP	BC
		JR	NC,l15af	; (+$0D)
		PUSH	HL
		LD	HL,$0002
		ADD	HL,DE
		SET	0,(HL)
		INC	HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		POP	HL
		SCF
l15af:		JR	NC,l15bc	; (+$0B)
		LD	HL,$0008
		ADD	HL,DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	A,(HL)
		EX	DE,HL
		SCF
l15bc:		POP	BC
		POP	DE
		RET

l15bf:		CALL	l1829
		RET	C

		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	E,(IX+$07)
		LD	D,(IX+$08)
		INC	DE
		LD	A,$04
		CALL	l04eb
		CALL	x1896
		DEC	DE
		JR	l15de		; (+$06)
l15d8:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	DE,$FFFF
l15de:		LD	B,D
		LD	C,E
		LD	DE,bcb_inuse
l15e3:		EX	DE,HL
x15e4:		CALL	l1707
		JR	Z,l1654		; (+$6B)
		PUSH	HL
		LD	HL,$0005
		ADD	HL,DE
		LD	A,(HL)
		CP	(IX+$1C)
		JR	NZ,l1610	; (+$1C)
		INC	HL
		LD	A,C
		SUB	(HL)
		INC	HL
		LD	A,B
		SBC	A,(HL)
		JR	C,l1610		; (+$14)
		CALL	x1547
		DEC	HL
		DEC	HL
		DEC	HL
		DEC	HL
		DEC	HL
		BIT	0,(HL)
		JR	NZ,l1610	; (+$08)
		POP	HL
		PUSH	HL
		CALL	l1716
		POP	HL
		JR	x15e4		; (-$2C)
l1610:		POP	HL
		JR	l15e3		; (-$30)
l1613:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	DE,bcb_inuse
l1619:		EX	DE,HL
l161a:		CALL	l1707
		JR	Z,l1654		; (+$35)
		PUSH	HL
		LD	HL,$0005
		ADD	HL,DE
		LD	A,(HL)
		POP	HL
		CP	(IX+$1C)
		JR	NZ,l1619	; (-$12)
		PUSH	HL
		CALL	l1716
		POP	HL
		JR	l161a		; (-$18)
l1632:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	DE,bcb_inuse
l1638:		EX	DE,HL
l1639:		CALL	l1707
		JR	Z,l1654		; (+$16)
		PUSH	HL
		LD	HL,$0003
		ADD	HL,DE
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	A
		SBC	HL,BC
		POP	HL
		JR	NZ,l1638	; (-$15)
		PUSH	HL
		CALL	l1716
		POP	HL
		JR	l1639		; (-$1B)
l1654:		POP	BC
		POP	DE
		POP	HL
		RET

l1658:		PUSH	IX
		PUSH	HL
		LD	HL,$0002
		ADD	HL,DE
		BIT	0,(HL)
		SCF
		JR	Z,l166e		; (+$0A)
		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		CALL	l17cb
		CALL	l1685
l166e:		POP	HL
		POP	IX
		RET

l1672:		PUSH	HL
		PUSH	DE
		LD	DE,bcb_inuse
l1677:		EX	DE,HL
		CALL	l1707
		JR	Z,l1682		; (+$05)
		CALL	l1658
		JR	C,l1677		; (-$0B)
l1682:		POP	DE
		POP	HL
		RET

l1685:		PUSH	HL
		PUSH	DE
		PUSH	BC
l1688:		LD	DE,bcb_inuse
		LD	BC,$FFFF
		CALL	l16a6
		JR	Z,l16a2		; (+$0F)
l1693:		PUSH	DE
		CALL	l16a6
		JR	Z,l169c		; (+$03)
		POP	AF
		JR	l1693		; (-$09)
l169c:		POP	DE
		CALL	l16ce
		JR	C,l1688		; (-$1A)
l16a2:		POP	BC
		POP	DE
		POP	HL
		RET

l16a6:		EX	DE,HL
		CALL	l1707
		RET	Z

		LD	HL,$0002
		ADD	HL,DE
		BIT	0,(HL)
		JR	Z,l16a6		; (-$0D)
		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		CP	(IX+$1C)
		JR	NZ,l16a6	; (-$16)
		INC	HL
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		OR	A
		SBC	HL,BC
		ADD	HL,BC
		JR	Z,l16c9		; (+$02)
		JR	NC,l16a6	; (-$23)
l16c9:		LD	B,H
		LD	C,L
		SCF
		SBC	A,A
		RET

l16ce:		PUSH	DE
		CALL	l16f1
		CALL	l18e8
		POP	DE
		RET	NC

		LD	HL,$0002
		ADD	HL,DE
		res	0,(HL)
		INC	HL
		XOR	A
		LD	(HL),A
		INC	HL
		LD	(HL),A
		SCF
		RET

; Subroutine to read buffer (DE=BCB) from disk

l16e4:		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l16f1		; get buffer & sector details
		CALL	l18df		; read the sector
		POP	BC
		POP	DE
		POP	HL
		RET

l16f1:		LD	HL,$000A
		ADD	HL,DE
		LD	B,(HL)
		DEC	HL
		LD	D,(HL)
		DEC	HL
		LD	E,(HL)
		PUSH	DE
		DEC	HL
		LD	D,(HL)
		DEC	HL
		LD	E,(HL)
		CALL	l18b6
		POP	HL
		RET

l1704:		LD	HL,bcb_free
l1707:		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		DEC	HL
		LD	A,D
		OR	E
		SCF
		RET

l170f:		EX	DE,HL
		LD	A,(HL)
		INC	HL
		OR	(HL)
		DEC	HL
		EX	DE,HL
		RET

l1716:		CALL	l172f
		LD	HL,bcb_free
		JR	l1724		; (+$06)
l171e:		CALL	l172f
		LD	HL,bcb_inuse
l1724:		LD	A,(HL)
		LD	(DE),A
		INC	HL
		INC	DE
		LD	A,(HL)
		LD	(DE),A
		DEC	DE
		LD	(HL),D
		DEC	HL
		LD	(HL),E
		RET

l172f:		LD	A,(DE)
		LD	(HL),A
		INC	HL
		INC	DE
		LD	A,(DE)
		LD	(HL),A
		DEC	DE
		RET

l1737:		LD	A,"A"
		LD	(unit0),A
		LD	HL,l176e
		LD	DE,xdpb_a+$1B
		LD	BC,$0015
		LDIR
		LD	HL,xdpb_a
		LD	(xdpb_ptrs),HL
		LD	HL,l1783
		LD	DE,xdpb_b+$1B
		LD	BC,$0015
		LDIR
		LD	HL,xdpb_b
		LD	($E2A2),HL
		LD	C,$01
		CALL	l1935
		CALL	l1f4f
		RET	NC

		LD	A,$03
		LD	L,$42
		JP	l3227

; The extended XDPB info for drive A:

l176e:		DB	$04,"A",$00	; flags,drive,unit
		DB	$00,$00,$00,$00	; last access,filesopen
		DW	$0000,$0000	; #free direntries,last used
		DW	chksm_a,alloc_a	; checksum vector,alloc bitmap
		DW	l1988		; login disk
		DW	l197c		; read sector
		DW	l1982		; write sector

; The extended XDPB info for drive B:

l1783:		DB	$04,"B",$01	; flags,drive,unit
		DB	$00,$00,$00,$00	; last access,filesopen
		DW	$0000,$0000	; #free direntries,lastused
		DW	chksm_b,alloc_b	; checksum vector,alloc bitmap
		DW	l1988		; login disk
		DW	l197c		; read sector
		DW	l1982		; write sector

x1798:		PUSH	HL
		LD	HL,l17ae
		LD	DE,xdpb_m+$1B
		LD	BC,$0015
		LDIR
		LD	HL,xdpb_m
		LD	($E2B8),HL
		POP	HL
		JP	l1ac9

; The extended XDPB info for drive M:

l17ae:		DB	$08,"M",$FF	; flags,drive,unit
		DB	$00,$00,$00,$00	; last access,filesopen
		DW	$0000,$0000	; #free direntries,lastused
		DW	$0000,alloc_m	; no checksum;alloc bitmap
		DW	l1845		; login disk
		DW	l1845		; read sector
		DW	l1845		; write sector

l1845:		SCF
		RET

l17c5:		CALL	l04ff
		LD	HL,xdpb_ptrs
l17cb:		SUB	$41
		JR	C,l17eb		; (+$1C)
		CP	$10
		JR	NC,l17eb	; (+$18)
		PUSH	HL
		ADD	A,A
		ADD	A,$A0
		LD	L,A
		ADC	A,$E2
		SUB	L
		LD	H,A
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		PUSH	HL
		POP	IX
		LD	A,H
		OR	L
		ADD	A,$FF
		POP	HL
		LD	A,$16
		RET

l17eb:		LD	A,$15
		OR	A
		RET

l17ef:		CALL	l17cb
		RET	NC

		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l180d
		BIT	0,(IX+$1B)
		SCF
		CALL	Z,l1805
		POP	BC
		POP	DE
		POP	HL
		RET

l1805:		LD	A,(IX+$1A)
		RLA
		RET	C

		JP	l18d3
l180d:		BIT	0,(IX+$1B)
		RET	Z

		LD	A,(IX+$21)
		OR	A
		RET	NZ

		CALL	l1829
		RET	C

l181b:		LD	A,(IX+$21)
		OR	A
		LD	A,$24
		RET	NZ

		res	0,(IX+$1B)
		JP	l1613
l1829:		BIT	7,(IX+$0C)
		SCF
		RET	NZ

		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,R
		DI
		LD	A,(FRAMES)
		LD	HL,($5C79)
		JP	po,l183f
		EI
l183f:		LD	B,A
		LD	A,(IX+$1E)
		LD	E,(IX+$1F)
		LD	D,(IX+$20)
		ADD	A,$64
		JR	NC,l184e	; (+$01)
		INC	DE
l184e:		LD	C,A
		LD	A,B
		SUB	C
		SBC	HL,DE
		PUSH	AF
		LD	HL,FRAMES
		LD	A,R
		DI
		LD	A,(HL)
		LD	(IX+$1E),A
		INC	HL
		LD	A,(HL)
		LD	(IX+$1F),A
		INC	HL
		LD	A,(HL)
		LD	(IX+$20),A
		JP	po,l186c
		EI
l186c:		POP	AF
		POP	BC
		POP	DE
		POP	HL
		RET

x1871:		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l187b
		POP	BC
		POP	DE
		POP	HL
		RET

l187b:		BIT	2,(IX+$1B)
		SCF
		RET	Z

		CALL	l190a
		CALL	l1edb
		RET	NC

		CALL	l1f5b
		LD	C,A
		AND	$20
		RET	Z

		BIT	6,C
		LD	A,$01
		RET	NZ

		SCF
		RET

x1896:		PUSH	HL
		PUSH	BC
		LD	C,(IX+$0D)
		LD	B,(IX+$0E)
		EX	DE,HL
		LD	E,(IX+$00)
		LD	D,(IX+$01)
		LD	A,$02
		CALL	l04eb
		JR	l18ae		; (+$02)
l18ac:		ADD	HL,DE
		DEC	BC
l18ae:		LD	A,B
		OR	C
		JR	NZ,l18ac	; (-$06)
		EX	DE,HL
		POP	BC
		POP	HL
		RET

l18b6:		PUSH	HL
		PUSH	BC
		EX	DE,HL
		LD	E,(IX+$00)
		LD	D,(IX+$01)
		LD	A,$02
		CALL	l04eb
		LD	BC,$FFFF
		OR	A
l18c8:		INC	BC
		SBC	HL,DE
		JR	NC,l18c8	; (-$05)
		ADD	HL,DE
		EX	DE,HL
		LD	D,C
		POP	BC
		POP	HL
		RET

l18d3:		PUSH	HL
		LD	L,(IX+$2A)
		LD	H,(IX+$2B)
		LD	DE,$0000
		JR	l18ef		; (+$10)
l18df:		PUSH	HL
		LD	L,(IX+$2C)
		LD	H,(IX+$2D)
		JR	l18ef		; (+$07)
l18e8:		PUSH	HL
		LD	L,(IX+$2E)
		LD	H,(IX+$2F)
l18ef:		LD	(rt_temp),HL
		POP	HL
l18f3:		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l1903
		POP	BC
		POP	DE
		POP	HL
		RET	C

		CALL	l1a9d
		JR	Z,l18f3		; (-$0F)
		RET

l1903:		PUSH	HL
		LD	HL,(rt_temp)
		EX	(SP),HL
		RET

l1909:		JP	(HL)
l190a:		PUSH	HL
		LD	C,(IX+$1D)
		LD	A,C
		OR	A
		JR	NZ,l1927	; (+$15)
		LD	HL,unit0
		LD	A,(IX+$1C)
		CP	(HL)
		JR	Z,l1927		; (+$0C)
		LD	(HL),A
		PUSH	IX
		PUSH	DE
		PUSH	BC
		CALL	l1929
		POP	BC
		POP	DE
		POP	IX
l1927:		POP	HL
		RET

l1929:		PUSH	AF
		LD	C,A
		CALL	l0325
		POP	AF
		PUSH	HL
		LD	HL,(rt_chgdsk)
		EX	(SP),HL
		RET

l1935:		LD	IX,xdpb_b
		CALL	l181b
		RET	NC

		LD	A,C
		OR	A
		JR	Z,l1944		; (+$03)
		LD	HL,$0000
l1944:		LD	DE,(rt_chgdsk)
		LD	(rt_chgdsk),HL
		LD	IX,xdpb_b
		LD	(IX+$1D),C
		CALL	l1f86
		JR	NC,l1963	; (+$0C)
		LD	A,C
		OR	A
		SCF
		CALL	NZ,l1f4f
		JR	C,l1963		; (+$04)
		LD	(IX+$1D),$00
l1963:		SCF
		EX	DE,HL
		RET

l1966:		LD	(retry_cnt),A
		RET

l196a:		LD	A,$0A
		OR	A
		JP	l22c4

		DS	12

; Low-level read sector subroutine for drives A: & B:

l197c:		CALL	l190a		; check if disk change required
		JP	l1c75		; DD_READ_SECTOR

; Low-level write sector subroutine for drives A: & B:

l1982:		CALL	l190a		; check if disk change required
		JP	x1c83		; DD_WRITE_SECTOR

; Low-level login disk subroutine for drives A: & B:

l1988:		CALL	l190a		; check if disk change required
		CALL	l1cf6		; DD_LOGIN
		RET	NC		; exit if error
		LD	A,(IX+$0F)
		XOR	$02
		LD	A,$06		; "unrecognised disk format"
		RET	NZ		; error if sectorsize <> 512
		RR	D
		RR	E
		LD	HL,$FFD2
		ADD	HL,DE
		CCF
		RET	NC		; error if alloc vector size/2 >$2D
		LD	E,(IX+$0B)
		LD	A,(IX+$0C)
		AND	$7F
		LD	D,A
		LD	HL,$FFBF
		ADD	HL,DE
		CCF
		RET	C		; success if chksum size <= $40
		LD	(IX+$0B),$40
		LD	A,(IX+$0C)
		AND	$80
		OR	$00
		LD	(IX+$0C),A	; else set chksum size to $40
		SCF
		RET

l19c0:		LD	A,C
		LD	(rw_page),A
		LD	(rw_add),HL
		CALL	x0521
		RET	NC

		ADD	HL,DE
		PUSH	HL
		CALL	l19dd
		POP	HL
		RET	C

		PUSH	AF
		LD	DE,(rw_add)
		OR	A
		SBC	HL,DE
		EX	DE,HL
		POP	AF
		RET

l19dd:		PUSH	BC
		PUSH	DE
		LD	HL,$0023
		ADD	HL,BC
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		LD	A,E
		SCF
		SBC	A,(HL)
		LD	E,A
		INC	HL
		LD	A,D
		SBC	A,(HL)
		LD	D,A
		INC	HL
		LD	A,B
		SBC	A,(HL)
		EX	DE,HL
		POP	DE
		POP	BC
		JR	C,l1a06		; (+$0D)
		DEC	DE
		SBC	HL,DE
		ADD	HL,DE
		SBC	A,$00
		JR	NC,l1a0a	; (+$09)
		EX	DE,HL
		CALL	l1a0a
		RET	NC

l1a06:		LD	A,$19
		OR	A
		RET

l1a0a:		INC	DE
		LD	A,D
		AND	$FE
		JR	Z,l1a65		; (+$55)
		PUSH	BC
		PUSH	DE
		XOR	A
		LD	HL,$F516
		CALL	l3886
		POP	DE
		POP	BC
		JR	NC,l1a65	; (+$48)
		LD	IX,$F518
		PUSH	DE
		LD	HL,$0026
		ADD	HL,BC
		LD	A,(HL)
		ADD	A,E
		LD	(HL),A
		INC	HL
		LD	A,(HL)
		ADC	A,D
		LD	(HL),A
		INC	HL
		LD	A,(HL)
		ADC	A,$00
		LD	(HL),A
		LD	HL,$0022
		ADD	HL,BC
		res	5,(HL)
		LD	L,(IX-$02)
		LD	H,(IX-$01)
		CALL	l276d
		CALL	x27c2
		LD	HL,(rw_add)
		JR	l1a4c		; (+$03)
l1a49:		CALL	l276d
l1a4c:		LD	A,(rw_page)
		CALL	l021b
		EX	(SP),HL
		AND	A
		SBC	HL,DE
		JR	C,l1a8b		; (+$33)
		EX	(SP),HL
		CALL	l25b0
		EX	(SP),HL
		LD	A,H
		OR	L
		EX	(SP),HL
		JR	NZ,l1a49	; (-$19)
		POP	DE
		JR	l1a98		; (+$33)
l1a65:		DEC	DE
		LD	HL,$0026
		ADD	HL,BC
		LD	A,(HL)
		AND	$7F
		JR	Z,l1a77		; (+$08)
		CALL	l10d9
		RET	NC

		RET	Z

		JP	l1a0a
l1a77:		LD	HL,$FF81
		ADD	HL,DE
		JR	NC,l1a84	; (+$07)
		CALL	l10f6
		RET	NC

		RET	Z

		JR	l1a77		; (-$0D)
l1a84:		CALL	l10d9
		RET	NC

		RET	Z

		JR	l1a84		; (-$07)
l1a8b:		ADD	HL,DE
		EX	DE,HL
		AND	A
		SBC	HL,DE
		EX	(SP),HL
		CALL	l25b0
		POP	DE
		CALL	l27ce
l1a98:		LD	(rw_add),HL
		SCF
		RET

l1a9d:		PUSH	IX
		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l3ee6
		LD	C,(IX+$1C)
		CALL	l02ff
		POP	BC
		POP	DE
		POP	HL
		POP	IX
		RET

l1ab1:		LD	A,($E3F4)
		LD	H,A
		LD	A,($E3F1)
		SUB	H
		LD	L,A
		RET

l1abb:		LD	IX,xdpb_m
		LD	A,(IX+$1C)
		AND	A
		JR	Z,l1ac9		; (+$04)
		CALL	l181b
		RET	NC

l1ac9:		PUSH	HL
		LD	HL,l1b2d
		LD	DE,spec_m
		LD	BC,$0008
		LDIR
		POP	DE
		LD	A,E
		CP	$04
		JR	C,l1b11		; (+$36)
		ADD	A,D
		LD	($E3F1),A
		LD	A,D
		LD	($E3F4),A
l1ae3:		LD	A,D
		PUSH	DE
		CALL	l0233
		CALL	l021b
		LD	D,H
		LD	E,L
		INC	DE
		LD	(HL),$E5
		LD	BC,$01FF
		LDIR
		CALL	l021b
		POP	DE
		INC	D
		DEC	E
		JR	NZ,l1ae3	; (-$1A)
		LD	IX,xdpb_m
		LD	HL,spec_m
		CALL	l1da6
		LD	(IX+$0B),$00
		LD	(IX+$0C),$80
		SCF
		RET

l1b11:		LD	A,(IX+$1C)
		LD	(IX+$1C),$00
		CALL	l04ff
		SUB	$41
		RET	C

		LD	E,A
		LD	D,$00
		LD	HL,xdpb_ptrs
		ADD	HL,DE
		ADD	HL,DE
		LD	(HL),$00
		INC	HL
		LD	(HL),$00
		SCF
		RET

l1b2d:		DB	$00,$00,$00,$01
		DB	$02,$00,$03,$00

l1b35:		PUSH	DE
		CALL	l18b6
		LD	A,E
		OR	A
		JR	NZ,x1b48	; (+$0B)
		LD	A,D
		LD	HL,$E3F1
		CP	(HL)
		JR	NC,x1b48	; (+$04)
		CALL	l0233
		SCF
x1b48:		POP	DE
		RET	C

		LD	A,$02
		RET

l1b4d:		LD	A,$41
		CALL	l17ef
		CALL	C,l181b
		LD	DE,$0000
		CALL	C,l1560
		RET	NC

		LD	C,A
		PUSH	HL
		CALL	l021b
		PUSH	AF
		XOR	A
		LD	B,A
		LD	E,$02
l1b66:		ADD	A,(HL)
		INC	HL
		DJNZ	l1b66		; (-$04)
		DEC	E
		JR	NZ,l1b66	; (-$07)
		LD	E,A
		POP	AF
		CALL	l021b
		POP	HL
		LD	A,E
		XOR	$03
		LD	A,$23
		RET	NZ

		DI
		LD	B,$03
		LD	DE,$FE00
		LD	IX,$0200
		CALL	l0245
		LD	A,$03
		CALL	l021b
		LD	HL,$1BA1
		LD	DE,$FDFB
		LD	BC,$0005
		LDIR
		LD	BC,PBANK678
		LD	A,$07
		LD	SP,$FE00
		JP	$FDFB
		OUT	(C),A
		JP	$FE10
l1ba6:		LD	A,(IX+$19)
		AND	$40
		OR	$0D
		CALL	x1c12
		LD	L,(IX+$0F)
		LD	H,(IX+$13)
		LD	($E408),HL
		LD	H,E
		LD	L,(IX+$18)
		LD	($E40A),HL
		LD	A,$06
		LD	($E405),A
		RET

l1bc6:		LD	A,(IX+$19)
		OR	$11
		CALL	l1bdf
		LD	(HL),$01
		RET

l1bd1:		LD	A,(IX+$19)
		OR	$06
		JR	l1bdf		; (+$07)
l1bd8:		LD	A,(IX+$19)
		AND	$C0
		OR	$05
l1bdf:		CALL	x1c12
		LD	A,E
		ADD	A,(IX+$14)
		LD	E,A
		PUSH	DE
		LD	HL,(rt_encode)
		LD	A,H
		OR	L
		CALL	NZ,l1909
		LD	A,E
		LD	($E40A),A
		LD	L,(IX+$0F)
		LD	H,E
		LD	($E40B),HL
		LD	A,(IX+$17)
		LD	($E40D),A
		LD	H,B
		LD	L,D
		LD	($E408),HL
		LD	A,$09
		LD	($E405),A
		LD	HL,$E40E
		LD	(HL),$FF
		POP	DE
		RET

x1c12:		LD	($E401),HL
		LD	L,A
		LD	A,B
		LD	(ddl_parms),A
		CALL	l1c2b
		LD	H,C
		LD	($E406),HL
		LD	L,(IX+$15)
		LD	H,(IX+$16)
		LD	($E403),HL
		RET

l1c2b:		LD	A,(IX+$11)
		AND	$7F
		LD	B,$00
		RET	Z

		DEC	A
		JR	NZ,x1c3e	; (+$08)
		LD	A,D
		RRA
		LD	D,A
		LD	A,B
		RLA
		LD	B,A
		JR	l1c4a		; (+$0C)
x1c3e:		LD	A,D
		SUB	(IX+$12)
		JR	C,l1c4a		; (+$06)
		SUB	(IX+$12)
		CPL
		LD	D,A
		INC	B
l1c4a:		LD	A,B
		ADD	A,A
		ADD	A,A
		OR	C
		LD	C,A
		RET

l1c50:		OR	A
		JR	NZ,l1c56	; (+$03)
		LD	HL,$0000
l1c56:		LD	DE,(rt_encode)
		LD	(rt_encode),HL
		EX	DE,HL
		RET

l1c5f:		PUSH	AF
		CALL	l1bd1
		POP	AF
		LD	L,A
		LD	H,$00
		LD	($E403),HL
		LD	HL,$1C6F
		JR	l1cc5		; (+$56)
		LD	HL,ddl_parms
		JP	l2119
l1c75:		CALL	l1bd1
		LD	HL,$1C7D
		JR	l1cc5		; (+$48)
		LD	HL,ddl_parms
		JP	l3ef5
x1c83:		CALL	l1edb
		RET	NC

		CALL	l1bd8
		JR	l1ca1		; (+$15)
x1c8c:		CALL	l1bc6
		CALL	l1ca1
		RET	NC

		LD	A,($E433)
		CP	$08
		SCF
		RET

l1c9a:		CALL	l1edb
		RET	NC

		CALL	l1ba6
l1ca1:		LD	HL,$1CA6
		JR	l1cc5		; (+$1F)
		LD	HL,ddl_parms
		JP	l2122
l1cac:		CALL	l1cb7
		LD	HL,fdc_res
		RET	NC

		LD	A,($E436)
		RET

l1cb7:		CALL	l1c2b
		LD	HL,$1CBF
		JR	l1cc5		; (+$06)
		LD	A,(IX+$19)
		JP	l2159

; Routine to turn on motor, try an operation in HL on track D multiple
; times & then start the motor off timeout

l1cc5:		CALL	l2181		; turn on motor
		CALL	l1ef2		; try the operation multiple times
		JP	l21a6		; start motor off timeout & exit

; Tables of specifications for disk types 0-3
; Format as on p215 of +3 Manual

; Type 0 - Spectrum +3 format

l1cce:		DB	$00,$00,$28,$09
		DB	$02,$01,$03,$02
		DB	$2A,$52

; Type 1 - CPC system format

		DB	$01,$00,$28,$09
		DB	$02,$02,$03,$02
		DB	$2A,$52

; Type 2 - CPC data format

		DB	$02,$00,$28,$09
		DB	$02,$00,$03,$02
		DB	$2A,$52

; Type 3 - PCW format

		DB	$03,$81,$50,$09
		DB	$02,$01,$04,$04
		DB	$2A,$52

l1cf6:		XOR	A
		CALL	l1d51
		LD	D,$00
		PUSH	BC
		CALL	C,l1cac
		POP	BC
		RET	NC

		AND	$C0
		LD	E,$01
		CP	$40
		JR	Z,l1d0f		; (+$05)
		INC	E
		CP	$C0
		JR	NZ,l1d15	; (+$06)
l1d0f:		LD	A,E
		CALL	l1d51
		JR	x1d49		; (+$34)
l1d15:		PUSH	BC
		LD	HL,$E40F
		LD	DE,$0000
		LD	B,$07
		LD	A,$0A
		PUSH	HL
		CALL	l1c5f
		POP	HL
		POP	BC
		JR	C,l1d30		; (+$08)
		CP	$08
		SCF
		CCF
		RET	NZ

		LD	A,$06
		RET

l1d30:		PUSH	BC
		LD	D,H
		LD	E,L
		LD	C,(HL)
		LD	B,$0A
l1d36:		LD	A,(DE)
		INC	DE
		CP	C
		JR	NZ,l1d40	; (+$05)
		DJNZ	l1d36		; (-$07)
		LD	HL,l1cce
l1d40:		POP	BC
		LD	A,(HL)
		CP	$04
		LD	A,$06
		CALL	C,l1d64
x1d49:		PUSH	HL
		PUSH	BC
		CALL	C,l1e64
		POP	BC
		POP	HL
		RET

l1d51:		LD	E,A
		CP	$04
		LD	A,$06
		RET	NC

		LD	A,E
		ADD	A,A
		LD	E,A
		ADD	A,A
		ADD	A,A
		ADD	A,E
		ADC	A,l1cce AND $FF
		LD	L,A
		ADC	A,l1cce/$100
		SUB	L
		LD	H,A
l1d64:		PUSH	HL
		PUSH	BC
		LD	A,(HL)
		LD	B,$41
		DEC	A
		JR	Z,l1d73		; (+$07)
		LD	B,$C1
		DEC	A
		JR	Z,l1d73		; (+$02)
		LD	B,$01
l1d73:		LD	(IX+$14),B
		INC	HL
		LD	A,(HL)
		LD	(IX+$11),A
		INC	HL
		LD	A,(HL)
		LD	(IX+$12),A
		INC	HL
		LD	A,(HL)
		LD	(IX+$13),A
		INC	HL
		LD	B,(HL)
		INC	HL
		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		LD	(IX+$17),A
		INC	HL
		LD	A,(HL)
		LD	(IX+$18),A
		LD	HL,$0080
		CALL	l1f6e
		LD	(IX+$15),L
		LD	(IX+$16),H
		LD	(IX+$19),$60
		POP	BC
		POP	HL
l1da6:		PUSH	BC
		PUSH	HL
		EX	DE,HL
		LD	HL,$0004
		ADD	HL,DE
		LD	A,(HL)
		LD	(IX+$0F),A
		PUSH	AF
		CALL	l1f64
		LD	(IX+$10),A
		DEC	HL
		LD	L,(HL)
		LD	H,$00
		POP	BC
		CALL	l1f6e
		LD	(IX+$00),L
		LD	(IX+$01),H
		LD	HL,$0006
		ADD	HL,DE
		LD	A,(HL)
		LD	(IX+$02),A
		LD	C,A
		PUSH	HL
		CALL	l1f64
		LD	(IX+$03),A
		DEC	HL
		LD	E,(HL)
		LD	(IX+$0D),E
		LD	(IX+$0E),$00
		DEC	HL
		DEC	HL
		LD	B,(HL)
		DEC	HL
		LD	D,(HL)
		DEC	HL
		LD	A,(HL)
		LD	L,D
		LD	H,$00
		LD	D,H
		AND	$7F
		JR	Z,l1def		; (+$01)
		ADD	HL,HL
l1def:		SBC	HL,DE
		EX	DE,HL
		LD	HL,$0000
l1df5:		ADD	HL,DE
		DJNZ	l1df5		; (-$03)
		LD	A,C
		SUB	(IX+$0F)
		LD	B,A
		CALL	l1f75
		DEC	HL
		LD	(IX+$05),L
		LD	(IX+$06),H
		LD	B,$03
		LD	A,H
		OR	A
		JR	Z,l1e0e		; (+$01)
		INC	B
l1e0e:		LD	A,C
		SUB	B
		CALL	l1f64
		LD	(IX+$04),A
		POP	DE
		PUSH	HL
		LD	B,$02
		CALL	l1f75
		INC	HL
		INC	HL
		EX	(SP),HL
		INC	DE
		LD	A,(DE)
		OR	A
		JR	NZ,l1e2d	; (+$08)
		ADD	HL,HL
		LD	A,H
		INC	A
		CP	$02
		JR	NC,l1e2d	; (+$01)
		INC	A
l1e2d:		LD	B,A
		LD	HL,$0000
l1e31:		SCF
		RR	H
		RR	L
		DJNZ	l1e31		; (-$07)
		LD	(IX+$09),H
		LD	(IX+$0A),L
		LD	H,$00
		LD	L,A
		LD	B,C
		INC	B
		INC	B
		CALL	l1f6e
		PUSH	HL
		DEC	HL
		LD	(IX+$07),L
		LD	(IX+$08),H
		LD	B,$02
		CALL	l1f75
		INC	HL
		LD	(IX+$0B),L
		LD	(IX+$0C),H
		POP	HL
		ADD	HL,HL
		ADD	HL,HL
		POP	DE
		POP	BC
		LD	A,(BC)
		SCF
		POP	BC
		RET

l1e64:		LD	B,A
		PUSH	DE
		PUSH	BC
		CALL	l1e86
		POP	BC
		POP	DE
		RET	NC

		LD	A,(IX+$11)
		AND	$03
		JR	Z,l1e78		; (+$04)
		BIT	1,(HL)
		JR	Z,l1e82		; (+$0A)
l1e78:		LD	A,B
		SCF
		BIT	7,(IX+$11)
		RET	Z

		BIT	3,(HL)
		RET	NZ

l1e82:		LD	A,$09
		OR	A
		RET

l1e86:		CALL	l1fc9
		LD	A,(HL)
		AND	$0C
		JR	Z,l1e9a		; (+$0C)
		LD	A,(HL)
		AND	$03
		SCF
		RET	NZ

		LD	A,(IX+$11)
		AND	$03
		SCF
		RET	Z

l1e9a:		LD	A,(IX+$11)
		AND	$03
		LD	D,$02
		JR	Z,l1eaf		; (+$0C)
		DEC	A
		LD	D,$05
		JR	Z,l1eaf		; (+$07)
		LD	A,(IX+$12)
		ADD	A,A
		SUB	$03
		LD	D,A
l1eaf:		PUSH	HL
		CALL	l1cac
		POP	HL
		RET	NC

		LD	DE,($E434)
		LD	A,(IX+$11)
		AND	$03
		JR	Z,l1ec9		; (+$09)
		DEC	D
		JR	Z,l1ec7		; (+$04)
		SET	0,(HL)
		JR	l1ec9		; (+$02)
l1ec7:		SET	1,(HL)
l1ec9:		LD	A,(IX+$11)
		DEC	E
		DEC	E
		JR	Z,l1ed1		; (+$01)
		CPL
l1ed1:		RLA
		JR	NC,l1ed7	; (+$03)
		SET	3,(HL)
		RET

l1ed7:		SET	2,(HL)
		SCF
		RET

l1edb:		PUSH	HL
		CALL	l1fc9
		BIT	3,(HL)
		POP	HL
		SCF
		RET	Z

		LD	A,(IX+$11)
		RLA
		LD	A,$09
		RET

l1eeb:		CALL	l1fc9
		LD	A,(HL)
		AND	$0F
		RET

l1ef2:		LD	A,(retry_cnt)
		LD	B,A
l1ef6:		PUSH	BC
		CALL	x1f22
		POP	BC
		RET	Z

		CP	$04
		JR	NZ,l1f1e	; (+$1E)
		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,(IX+$19)
		CALL	l2159
		CALL	l20a9
		POP	BC
		POP	DE
		POP	HL
		JR	NZ,l1f1e	; (+$0D)
		RET	NC

		LD	A,($E436)
		XOR	(IX+$14)
		AND	$C0
		LD	A,$08
		RET	NZ

		RRA
l1f1e:		DJNZ	l1ef6		; (-$2A)
		OR	A
		RET

x1f22:		LD	A,B
		AND	$07
		JR	Z,l1f34		; (+$0D)
		AND	$03
		JR	NZ,l1f3e	; (+$13)
		PUSH	HL
		CALL	l1fc9
		res	6,(HL)
		POP	HL
		JR	l1f3e		; (+$0A)
l1f34:		PUSH	DE
		LD	D,(IX+$12)
		DEC	D
		CALL	l1fd5
		POP	DE
		RET	NC

l1f3e:		CALL	l1fd5
		RET	NC

		PUSH	HL
		PUSH	DE
		PUSH	BC
		CALL	l1909
		POP	BC
		POP	DE
		CALL	l20a9
		POP	HL
		RET

l1f4f:		PUSH	BC
		LD	C,$01
		CALL	l1f5b
		POP	BC
		AND	$60
		RET	Z

		SCF
		RET

l1f5b:		CALL	l2181
		CALL	l20e6
		JP	l21a6
l1f64:		OR	A
		RET	Z

		LD	B,A
		LD	A,$01
l1f69:		ADD	A,A
		DJNZ	l1f69		; (-$03)
		DEC	A
		RET

l1f6e:		LD	A,B
		OR	A
		RET	Z

l1f71:		ADD	HL,HL
		DJNZ	l1f71		; (-$03)
		RET

l1f75:		LD	A,B
		OR	A
		RET	Z

l1f78:		SRL	H
		RR	L
		DJNZ	l1f78		; (-$06)
		RET

l1f7f:		DB	$0A		; motor on time
		DB	$32		; motor off time
		DB	$AF		; write off time
		DB	$1E		; head settle time
		DB	$0C		; step rate
		DB	$0F		; head unload time
		DB	$03		; head load time x2+1

l1f86:		PUSH	BC
		LD	BC,$2FFD
		IN	A,(C)
		ADD	A,$01
		CCF
		POP	BC
		RET

l1f91:		LD	HL,equipment
		LD	B,$10
x1f96:		LD	(HL),$00
		INC	HL
		DJNZ	x1f96		; (-$05)
		LD	A,$0F
		LD	(retry_cnt),A
		CALL	l3ee6
		LD	HL,l1f7f
x1fa6:		LD	DE,tm_mtron
		LD	BC,$0005
		LDIR
		LD	A,(tm_step)
		DEC	A
		RLCA
		RLCA
		RLCA
		CPL
		AND	$F0
		OR	(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		LD	A,$03
		CALL	x216a
		LD	A,L
		CALL	x216a
		LD	A,H
		JP	x216a
l1fc9:		LD	A,C
		AND	$03
		ADD	A,A
		ADD	A,$20
		LD	L,A
		ADC	A,$E4
		SUB	L
		LD	H,A
		RET

l1fd5:		PUSH	HL
		CALL	l1fc9
		CALL	l1fde
		POP	HL
		RET

l1fde:		LD	A,(retry_cnt)
		LD	B,A
l1fe2:		BIT	6,(HL)
		JR	NZ,l1ff1	; (+$0B)
		INC	HL
		LD	(HL),$00
		DEC	HL
		CALL	l2016
		JR	NC,x2007	; (+$18)
		SET	6,(HL)
l1ff1:		LD	A,D
		INC	HL
		CP	(HL)
		DEC	HL
		SCF
		RET	Z

		OR	A
		JR	NZ,l1fff	; (+$05)
		CALL	l2016
		JR	l2002		; (+$03)
l1fff:		CALL	l203a
l2002:		JR	NC,l200c	; (+$08)
		INC	HL
		LD	(HL),D
		RET

x2007:		PUSH	DE
		CALL	NZ,l2036
		POP	DE
l200c:		res	6,(HL)
		RET	Z

		CALL	l20ce
		DJNZ	l1fe2		; (-$32)
		CP	A
		RET

l2016:		CALL	l201a
		RET	Z

l201a:		PUSH	BC
		LD	B,(IX+$12)
		DEC	B
		BIT	7,(IX+$11)
		JR	NZ,x202c	; (+$07)
		BIT	3,(HL)
		JR	Z,x202c		; (+$03)
		LD	A,B
		ADD	A,A
		LD	B,A
x202c:		LD	A,$07
		CALL	x216a
		LD	A,C
		AND	$03
		JR	l205d		; (+$27)
l2036:		LD	D,(IX+$12)
		DEC	D
l203a:		PUSH	BC
		LD	A,D
		INC	HL
		SUB	(HL)
		DEC	HL
		JR	NC,l2043	; (+$02)
		CPL
		INC	A
l2043:		LD	B,A
		LD	A,$0F
		CALL	x216a
		LD	A,C
		CALL	x216a
		LD	A,D
		BIT	7,(IX+$11)
		JR	NZ,l205d	; (+$09)
		BIT	3,(HL)
		JR	Z,l205d		; (+$05)
		LD	A,B
		ADD	A,A
		LD	B,A
		LD	A,D
		ADD	A,A
l205d:		PUSH	HL
		CALL	x216a
l2061:		LD	A,(tm_step)
		CALL	l207b
		DJNZ	l2061		; (-$08)
		LD	A,(tm_hdset)
		CALL	l207b
		LD	HL,fdc_res
		CALL	l20df
		CALL	l2084
		POP	HL
		POP	BC
		RET

l207b:		LD	L,$DC
l207d:		DEC	L
		JR	NZ,l207d	; (-$03)
		DEC	A
		JR	NZ,l207b	; (-$08)
		RET

l2084:		LD	A,C
		OR	$20
		INC	HL
		XOR	(HL)
		AND	$FB
		SCF
		RET	Z

		LD	A,(HL)
		AND	$C0
		XOR	$80
		JR	Z,l20a5		; (+$11)
		LD	A,(HL)
		XOR	C
		AND	$03
		JR	Z,l209f		; (+$05)
		CALL	l20df
		JR	l2084		; (-$1B)
l209f:		LD	A,(HL)
		AND	$08
		XOR	$08
		RET	Z

l20a5:		LD	A,$02
		OR	A
		RET

l20a9:		INC	HL
		LD	A,(HL)
		XOR	C
		SCF
		RET	Z

		AND	$08
		XOR	$08
		RET	Z

		INC	HL
		LD	A,(HL)
		CP	$80
		SCF
		RET	Z

		XOR	$02
		LD	A,$01
		RET	Z

		LD	A,$03
		BIT	5,(HL)
		RET	NZ

		INC	A
		BIT	2,(HL)
		RET	NZ

		INC	A
		BIT	0,(HL)
		RET	NZ

		INC	A
		INC	A
		RET

l20ce:		PUSH	HL
		PUSH	AF
		LD	HL,fdc_res
l20d3:		CALL	l20df
		AND	$C0
		CP	$80
		JR	NZ,l20d3	; (-$09)
		POP	AF
		POP	HL
		RET

l20df:		LD	A,$08
		CALL	x216a
		JR	l20f2		; (+$0C)
l20e6:		LD	A,$04
		CALL	x216a
		LD	A,C
		CALL	x216a
l20ef:		LD	HL,fdc_res
l20f2:		PUSH	DE
		PUSH	BC
		LD	BC,$2FFD
		LD	D,$00
		INC	HL
		PUSH	HL
l20fb:		IN	A,(C)
		ADD	A,A
		JR	NC,l20fb	; (-$05)
		JP	p,l2112
		LD	B,$3F
		IN	A,(C)
		LD	B,$2F
		LD	(HL),A
		INC	HL
		INC	D
		EX	(SP),HL
		EX	(SP),HL
		EX	(SP),HL
		EX	(SP),HL
		JR	l20fb		; (-$17)
l2112:		POP	HL
		LD	A,(HL)
		DEC	HL
		LD	(HL),D
		POP	BC
		POP	DE
		RET

l2119:		CALL	l2134
		CALL	l21c5
		JP	l20ef
l2122:		CALL	l2134
		CALL	l2214
		LD	A,(tm_wroff)
l212b:		DEC	A
		INC	BC
		INC	BC
		INC	BC
		JR	NZ,l212b	; (-$06)
		JP	l20ef

; Subroutine to get page (D) and address (HL) of buffer for low-level
; command, and output all command bytes except last (in A)
; On entry, HL=address of parameter block

l2134:		CALL	l20ce		; wait until ready for new command
		LD	A,(BANKM)	; get old BANKM
		AND	$F8
		OR	(HL)		; set page required
		LD	B,A
		INC	HL
		LD	E,(HL)
		INC	HL
		LD	D,(HL)		; DE=buffer address
		INC	HL
		LD	C,(HL)		; C=# bytes to transfer (low)
		PUSH	BC
		INC	HL
		INC	HL
		LD	B,(HL)		; B=# command bytes
		INC	HL
		DEC	B
l214a:		LD	A,(HL)		; get next command byte
		INC	HL
		CALL	x216a		; send it
		DJNZ	l214a		; back for all except last
		LD	A,(HL)
		EX	DE,HL
		POP	DE		; D=page required, E=#bytes to transfer (low)
		LD	BC,PBANKM
		DI			; turn off interrupts
		RET

l2159:		CALL	l20ce
		AND	$40
		OR	$0A
		CALL	x216a
		LD	A,C
		CALL	x216a
		JP	l20ef
x216a:		PUSH	DE
		PUSH	BC
		LD	D,A
		LD	BC,$2FFD
l2170:		IN	A,(C)
		ADD	A,A
		JR	NC,l2170	; (-$05)
		ADD	A,A
		JR	C,l217e		; (+$06)
		LD	B,$3F
		OUT	(C),D
		EX	(SP),HL
		EX	(SP),HL
l217e:		POP	BC
		POP	DE
		RET

l2181:		PUSH	BC
		PUSH	AF
		XOR	A
		LD	(timeout),A
		LD	A,(BANK678)
		BIT	3,A
		JR	NZ,l21a3	; (+$15)
		OR	$08
		CALL	l21ba
		LD	A,(tm_mtron)
l2196:		PUSH	AF
		LD	BC,$3548
l219a:		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,l219a	; (-$05)
		POP	AF
		DEC	A
		JR	NZ,l2196	; (-$0D)
l21a3:		POP	AF
		POP	BC
		RET

l21a6:		PUSH	AF
		XOR	A
		LD	(timeout),A
		LD	A,(BANK678)
		AND	$08
		JR	Z,l21b8		; (+$06)
		LD	A,(tm_mtroff)
		LD	(timeout),A
l21b8:		POP	AF
		RET

l21ba:		PUSH	BC
		LD	BC,PBANK678
		LD	(BANK678),A
		OUT	(C),A
		POP	BC
		RET

l21c5:		CALL	x216a
		OUT	(C),D
		LD	BC,$2FFD
		LD	D,$20
		JR	l21db		; (+$0A)

l21d1:		LD	B,$3F
		INI
		LD	B,$2F
		DEC	E
		JP	Z,l21ec
l21db:		IN	A,(C)
		JP	p,l21db
		AND	D
		JP	NZ,l21d1
		JR	l222f		; (+$49)

l21e6:		LD	B,$3F
		IN	A,(C)
		LD	B,$2F
l21ec:		IN	A,(C)
		JP	p,l21ec
		AND	D
		JP	NZ,l21e6
		JR	l222f		; (+$38)

; Subroutine to output last byte of command to FDC and read bytes to buffer

l21f7:		CALL	x216a		; send command
		OUT	(C),D		; page in required bank  **** AVERIGUAR DE DONDE SALE "D" ****
		LD	BC,$2FFD
		LD	D,$20
		JR	l2209
l2203:		LD	B,$3F
		INI			; read a byte
		LD	B,$2F
l2209:		IN	A,(C)
		JP	p,l2209		; wait until FDC ready
		AND	D
		JP	NZ,l2203	; loop back if more bytes to read
		JR	l222f		; go to repage bank 7 & exit

l2214:		CALL	x216a
		OUT	(C),D
		LD	BC,$2FFD
		LD	D,$20
		JR	l2226		; (+$06)

l2220:		LD	B,$40
		OUTI
		LD	B,$2F
l2226:		IN	A,(C)
		JP	p,l2226
		AND	D
		JP	NZ,l2220
l222f:		LD	A,(BANKM)
		LD	BC,PBANKM
		OUT	(C),A
		EI
		RET

; ******************** KEYBOARD SCANNING ROUTINES *****************

; These are copies of the keytables from ROM 3

; The L-mode keytable with CAPS-SHIFT

l2239:		DB	"BHY65TGV"
		DB	"NJU74RFC"
		DB	"MKI83EDX"
		DB	$0E, "LO92WSZ"
		DB	" ", $0D, "P01QA"

; The extended-mode keytable (unshifted letters)

n22c5:		DB	$E3,$C4,$E0,$E4
		DB	$B4,$BC,$BD,$BB
		DB	$AF,$B0,$B1,$C0
		DB	$A7,$A6,$BE,$AD
		DB	$B2,$BA,$E5,$A5
		DB	$C2,$E1,$B3,$B9
		DB	$C1,$B8

; The extended mode keytable (shifted letters)

n22df:		DB	$7E,$DC,$DA,$5C
		DB	$B7,$7B,$7D,$D8
		DB	$BF,$AE,$AA,$AB
		DB	$DD,$DE,$DF,$7F
		DB	$B5,$D6,$7C,$D5
		DB	$5D,$DB,$B6,$D9
		DB	$5B,$D7

; The control code keytable (CAPS-SHIFTed digits)

n22f9:		DB	$0C,$07,$06,$04
		DB	$05,$08,$0A,$0B
		DB	$09,$0F

; The symbol code keytable (letters with symbol shift)

n2303:		DB	$E2,$2A,$3F,$CD
		DB	$C8,$CC,$CB,$5E
		DB	$AC,$2D,$2B,$3D
		DB	$2E,$2C,$3B,$22
		DB	$C7,$3C,$C3,$3E
		DB	$C5,$2F,$C9,$60
		DB	$C6,$3A

; The extended mode keytable (SYM-SHIFTed digits)

n231d:		DB	$D0,$CE,$A8,$CA
		DB	$D3,$D4,$D1,$D2
		DB	$A9,$CF

l22c2:		XOR	A
		LD	A,B
l22c4:		PUSH	HL
		LD	HL,(CURCHL)
		EX	(SP),HL
		PUSH	HL
		LD	HL,(FLAGS)
		EX	(SP),HL
		PUSH	HL
		LD	HL,(FLAGS2)
		EX	(SP),HL
		LD	HL,o22F0
		LD	(al_resp),HL
		PUSH	AF
		CALL	l032d
		POP	AF
		CALL	n3e00
		RST	30H
		CCF
		POP	HL
		LD	(FLAGS2),HL
		POP	HL
		LD	(FLAGS),HL
		POP	HL
		LD	(CURCHL),HL
		RET

		DB	$8B,13,$FB
		DB	"Not ready", 13

		DB	$8E,$FF,$8B,13,$FB
		DB	"Write protected", 13

		DB	$8E,$FF,$8C,13,$FB
		DB	"Seek fail", 13

		DB	$8E,$FF,$8D
		DB	"Data error", 13

		DB	$8E,$FF,$8D
		DB	"No data", 13

		DB	$8E,$FF,$8D
		DB	"Missing address mark", 13

		DB	$8E,$FF,$8B,13,$FB
		DB	"Bad format", 13

		DB	$8E,$FF,$8D
		DB	"Unknown error", 13

		DB	$8E,$FF,$8B,13,$FB
		DB	"Disk changed, please replace", 13

		DB	$8E,$FF,$8B,13,$FB
		DB	"Disk unsuitable", 13

		; EJ:
		; Please put the disk for A: into
		; the drive then press any key

		DB	$8E,$FF
		DB	"Please put the disk for ", $FE
		DB	": into the drive then press any key", $FF

		; EJ:
		; Drive A: track t sector s
		; Retry, Ignore or Cancel?

		DB	"Drive ",$FE,":",$FF,$8B," track ",$FD,$FF,$8C
		DB	" sector ",$FC,$0D,$FB,$FF,$FA
		DB	"Retry, Ignore or Cancel?",$FF

FREE_ROM2_0:	EQU	$

		;...
		;...

R2_FREE_0:	EQU	119-($-FREE_ROM2_0)
ROM2_SPARE0:	DS	R2_FREE_0

l249a:		LD	BC,$0200
l249d:		PUSH	BC
		LD	A,$02
		SUB	B
		LD	C,A
		LD	B,$00
		LD	HL,tmp_buff
		CALL	l24bf
		POP	BC
		CP	$42
		JR	NZ,l24b0	; (+$01)
		INC	C
l24b0:		DJNZ	l249d		; (-$15)
		LD	A,C
		LD	($DF9D),A
		RET

l24b7:		LD	BC,$FEEF
		LD	A,$A0
		OUT	(C),A
		RET

l24bf:		PUSH	BC
		PUSH	DE
		LD	DE,$07D0
		LD	($DF9E),DE
		LD	A,C
		AND	A
		LD	A,$A0
		JR	Z,l24d0		; (+$02)
		LD	A,$B0
l24d0:		CALL	l2738
		JR	NC,l24ec	; (+$17)
		LD	E,$00
l24d7:		LD	BC,$DEEF
		OUT	(C),E
		IN	A,(C)
		CP	E
		JR	NZ,l2553	; (+$72)
		INC	E
		JR	NZ,l24d7	; (-$0D)
		LD	A,$EC
		LD	BC,$FFEF
		OUT	(C),A
		SCF
l24ec:		POP	DE
		POP	BC
		CALL	C,l257e
		JR	NC,l254a	; (+$57)
		PUSH	DE
		PUSH	BC
		PUSH	HL
		LD	D,$00
l24f8:		XOR	A
		LD	BC,$EEEF
		OUT	(C),A
		LD	BC,$DFEF
		INC	A
		OUT	(C),A
		LD	BC,$DEEF
		OUT	(C),A
		INC	D
		LD	BC,$EFEF
		OUT	(C),D
		JR	Z,l2538		; (+$27)
		LD	BC,$FFEF
		LD	A,$20
		OUT	(C),A
		NOP
l2519:		IN	A,(C)
		RLCA
		JR	C,l2519		; (-$05)
		AND	$12
		CP	$10
		JR	NZ,l2538	; (+$14)
		LD	B,$CE
		PUSH	DE
		LD	DE,$0100
		LD	H,E
		LD	L,E
		CALL	l25b0
		LD	HL,$8900
		CALL	l2678
		POP	DE
		JR	C,l24f8		; (-$40)
l2538:		DEC	D
		POP	HL
		POP	BC
		LD	A,B
		CALL	l021b
		LD	(HL),D
		INC	H
		POP	DE
		LD	A,$07
		CALL	l021b
		LD	A,$42
		AND	A
l254a:		PUSH	HL
		LD	HL,$0000
		LD	($DF9E),HL
		POP	HL
		RET

l2553:		LD	A,$00
		AND	A
		JR	l24ec		; (-$6C)
l2558:		BIT	7,C
		LD	A,$02
		RET	NZ

		PUSH	BC
		PUSH	DE
		SLA	E
		RL	D
		RL	C
		CALL	l2571
		JR	NC,l256e	; (+$04)
		INC	E
		CALL	l2571
l256e:		POP	DE
		POP	BC
		RET

l2571:		CALL	l26a8
		RET	NC

		LD	A,$20
		PUSH	BC
		LD	BC,$FFEF
		OUT	(C),A
		POP	BC
l257e:		PUSH	HL
		LD	HL,$8908
		CALL	l2678
		POP	HL
		RET	NC

		PUSH	BC
		PUSH	DE
		LD	A,B
		CALL	l021b
		LD	BC,$CEEF
		LD	DE,$0100
		CALL	l25b0
		POP	DE
		POP	BC
		PUSH	HL
		LD	HL,$8900
		CALL	l2678
		POP	HL
		RET	NC

		PUSH	BC
		LD	BC,$FFEF
		IN	A,(C)
		POP	BC
		AND	$01
		SCF
		RET	Z

		LD	A,$07
		AND	A
		RET

l25b0:		LD	A,B
		INC	E
		DEC	E
		JR	Z,l25BE		; (+$09)
l25b5:		INI
		LD	B,A
		DEC	E
		JR	NZ,l25b5	; (-$06)
		INC	D
		JR	l25f3		; (+$35)
l25BE:		LD	E,$10
l25c0:		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		INI
		LD	B,A
		DEC	E
		JR	NZ,l25c0	; (-$33)
l25f3:		DEC	D
		JR	NZ,l25BE	; (-$38)
		LD	A,$07
		JP	l021b
l25fb:		BIT	7,C
		LD	A,$02
		RET	NZ

		PUSH	BC
		PUSH	DE
		SLA	E
		RL	D
		RL	C
		CALL	l2614
		JR	NC,l2611	; (+$04)
		INC	E
		CALL	l2614
l2611:		POP	DE
		POP	BC
		RET

l2614:		CALL	l26a8
		RET	NC

		LD	A,$30
		PUSH	BC
		LD	BC,$FFEF
		OUT	(C),A
		POP	BC
		PUSH	HL
		LD	HL,$8908
		CALL	l2678
		POP	HL
		JR	C,l263d		; (+$12)
		CP	$00
		RET	Z

		PUSH	BC
		LD	BC,$CFEF
		IN	A,(C)
		POP	BC
		AND	$40
		LD	A,$07
		RET	Z

		LD	A,$01
		RET

l263d:		PUSH	BC
		PUSH	DE
		LD	A,B
		CALL	l021b
		LD	E,$40
		LD	BC,$CFEF
		LD	A,B
l2649:		OUTI
		LD	B,A
		OUTI
		LD	B,A
		OUTI
		LD	B,A
		OUTI
		LD	B,A
		DEC	E
		JP	NZ,l2649
		LD	A,$07
		CALL	l021b
		POP	DE
		POP	BC
		PUSH	HL
		LD	HL,$8900
		CALL	l2678
		POP	HL
		RET	NC

		PUSH	BC
		LD	BC,$FFEF
		IN	A,(C)
		POP	BC
		AND	$01
		SCF
		RET	Z

		LD	A,$07
		AND	A
		RET

l2678:		PUSH	BC
		PUSH	DE
		LD	E,$09
x267c:		PUSH	DE
		LD	DE,($DF9E)
		LD	BC,$FFEF
l2684:		IN	A,(C)
		AND	H
		CP	L
		SCF
		JR	Z,l26a0		; (+$15)
		BIT	7,A
		JR	NZ,l2693	; (+$04)
		BIT	0,A
		JR	NZ,l26a4	; (+$11)
l2693:		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,l2684	; (-$14)
		POP	DE
		DEC	E
		JR	NZ,x267c	; (-$20)
		PUSH	DE
		LD	A,$00
x269f:		AND	A
l26a0:		POP	DE
		POP	DE
		POP	BC
		RET

l26a4:		LD	A,$07
		JR	x269f		; (-$09)
l26a8:		PUSH	HL
		PUSH	DE
		PUSH	BC
		XOR	A
		CALL	l2735
		JP	NC,x272c
		LD	L,C
		LD	H,$00
		LD	BC,$E883
		BIT	4,(IX+$10)
		JR	Z,l26c1		; (+$03)
		LD	BC,$E88D
l26c1:		PUSH	BC
		EX	(SP),IX
		LD	C,(IX+$04)
		LD	B,(IX+$05)
		CALL	l2748
		PUSH	BC
		LD	HL,$0000
		LD	C,(IX+$03)
		LD	B,$00
		CALL	l2748
		LD	D,C
		POP	BC
		EX	(SP),IX
		LD	A,(IX+$03)
		ADD	A,D
		EX	(SP),IX
		CP	(IX+$02)
		JR	C,l26ec		; (+$04)
		SUB	(IX+$02)
		INC	BC
l26ec:		POP	IX
		LD	L,(IX+$01)
		LD	H,(IX+$02)
		ADD	HL,BC
		LD	B,H
		LD	C,L
		LD	L,(IX+$04)
		LD	H,(IX+$05)
		AND	A
		SBC	HL,BC
		JR	C,x2730		; (+$2E)
		JR	NZ,l270b	; (+$07)
		LD	L,(IX+$06)
		INC	L
		CP	L
		JR	NC,x2730	; (+$25)
l270b:		CALL	l2735
		PUSH	BC
		LD	C,E
		POP	DE
		JR	NC,x272c	; (+$19)
		LD	A,C
		INC	A
		LD	BC,$DFEF
		OUT	(C),A
		LD	A,$01
		LD	BC,$DEEF
		OUT	(C),A
		LD	BC,$EEEF
		OUT	(C),E
		LD	BC,$EFEF
		OUT	(C),D
		SCF
x272c:		POP	BC
		POP	DE
		POP	HL
		RET

x2730:		AND	A
		LD	A,$02
		JR	x272c		; (-$09)
l2735:		OR	(IX+$10)
l2738:		PUSH	BC
		LD	BC,$FEEF
		OUT	(C),A
		PUSH	HL
		LD	HL,$C040
		CALL	l2678
		POP	HL
		POP	BC
		RET

l2748:		LD	A,$10
		SLA	E
		RL	D
l274e:		ADC	HL,HL
		JR	NC,l2758	; (+$06)
		OR	A
		SBC	HL,BC
		OR	A
		JR	l275e		; (+$06)
l2758:		SBC	HL,BC
		JR	NC,l275e	; (+$02)
		ADD	HL,BC
		SCF
l275e:		RL	E
		RL	D
		DEC	A
		JR	NZ,l274e	; (-$17)
		LD	A,D
		CPL
		LD	B,A
		LD	A,E
		CPL
		LD	C,A
		EX	DE,HL
		RET

l276d:		BIT	7,(IX+$00)
		JR	NZ,l2778	; (+$05)
		CALL	l27b9
		JR	l27a7		; (+$2F)
l2778:		PUSH	IX
		EX	(SP),HL
l277b:		LD	A,(HL)
		CALL	l2738
		JR	NC,l277b	; (-$06)
		INC	HL
		LD	BC,$F0EF
		OUTI
		LD	BC,$EFEF
		OUTI
		LD	BC,$E0EF
		OUTI
		LD	E,(HL)
		INC	HL
		res	7,(HL)
		EX	(SP),HL
		POP	IX
		CALL	l27b9
l279b:		LD	BC,$DEEF
		OUT	(C),E
		LD	D,$20
		LD	BC,$FFEF
		OUT	(C),D
l27a7:		LD	DE,$0100
l27aa:		LD	BC,$FFEF
		IN	A,(C)
		AND	$89
		CP	$08
		JR	NZ,l27aa	; (-$0B)
		LD	BC,$CEEF
		RET

l27b9:		DEC	(IX-$01)
		RET	NZ

		SET	7,(IX+$00)
		RET

x27c2:		LD	BC,$CEEF
l27c5:		LD	A,H
		OR	L
		RET	Z

		IN	A,(C)
		DEC	HL
		DEC	DE
		JR	l27c5		; (-$09)
l27ce:		IN	A,(C)
		DEC	DE
		LD	A,D
		OR	E
		JR	NZ,l27ce	; (-$07)
		RET

		SCF
		RET

		SLA	E
		SRL	D
		RR	E
		LD	C,$00
		PUSH	HL
		LD	L,(IX+$17)
		LD	H,(IX+$18)
		EX	(SP),HL
		EX	(SP),IX
		CALL	l2558
		POP	IX
		RET

		SLA	E
		SRL	D
		RR	E
		LD	C,$00
		PUSH	HL
		LD	L,(IX+$17)
		LD	H,(IX+$18)
		EX	(SP),HL
		EX	(SP),IX
		CALL	l25fb
		POP	IX
		RET

l2808:		LD	DE,l0106
		SCF
		RET

l280d:		SRL	A
		LD	IX,$E883
		JR	NC,l2819	; (+$04)
		LD	IX,$E88D
l2819:		AND	A
		LD	A,$16
		RET	NZ

		LD	A,(IX+$00)
		OR	(IX+$01)
		LD	A,$16
		RET	Z

		SCF
		RET

l2828:		LD	HL,$E897
		LD	DE,$E898
		LD	BC,$004B
		LD	(HL),$00
		LDIR
		LD	HL,$E8E3
		LD	DE,$E8E4
		LD	BC,$025F
		LD	(HL),$00
		LDIR
		LD	HL,$07D0
		LD	($DF9E),HL
		LD	HL,$E883
		LD	DE,$E897
		LD	A,$A0
		CALL	l2937
		LD	HL,$E88D
		LD	DE,$E8AA
		LD	A,$B0
		CALL	l2937
		CALL	l24b7
		LD	HL,$0000
		LD	($DF9E),HL
		LD	BC,$FEFE
		IN	A,(C)
		RRA
		CCF
		RET	C

		XOR	A
		LD	B,A
		LD	C,A
		LD	L,$08
		CALL	l306c
		JR	NC,l289f	; (+$26)
		LD	(ed_ATTR_P),A
		LD	(ed_ATTR_T),A
		LD	(ATTR_T),A
		LD	(BORDCR),A
		LD	D,A
		RRA
		RRA
		RRA
		AND	$07
		OUT	($FE),A
		LD	HL,$5800
		LD	BC,$0300
x2893:		LD	A,(HL)
		CP	$38
		JR	NZ,l2899	; (+$01)
		LD	(HL),D
l2899:		INC	HL
		DEC	BC
		LD	A,B
		OR	C
		JR	NZ,x2893	; (-$0C)
l289f:		XOR	A
		LD	B,A
		LD	C,A
		LD	L,$09
		CALL	l306c
		JR	NC,l28ac	; (+$03)
		LD	(ATTR_P),A
l28ac:		LD	HL,FLAGS3
		res	6,(HL)
		XOR	A
		LD	B,A
		LD	C,A
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l28ee	; (+$32)
		LD	HL,$EF3B
		LD	DE,o2934
		LD	B,$03
l28c4:		LD	A,(HL)
		AND	A
		JR	Z,l28d3		; (+$0B)

		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	A,(DE)
		LD	L,A
		CALL	l3370
		POP	BC
		POP	DE
		POP	HL
l28d3:		INC	HL
		INC	DE
		DJNZ	l28c4		; (-$13)
		LD	HL,$EF3E
		LD	B,$03
l28dc:		LD	A,(HL)
		AND	A
		JR	Z,l28eb		; (+$0B)
		PUSH	HL
		PUSH	BC
		LD	L,A
		LD	A,$05
		SUB	B
		CALL	l3227
		POP	BC
		POP	HL
l28eb:		INC	HL
		DJNZ	l28dc		; (-$12)
l28ee:		XOR	A
l28ef:		LD	BC,$0000
		PUSH	AF
l28f3:		POP	AF
		PUSH	AF
		LD	L,$1C
		CALL	l306c
		JR	NC,l292a	; (+$2E)
		JR	Z,l2906		; (+$08)
		LD	L,A
		POP	AF
		PUSH	AF
		PUSH	BC
		CALL	l3227
		POP	BC
l2906:		INC	BC
		LD	A,B
		OR	C
		JR	NZ,l28f3	; (-$18)
l290b:		POP	AF
		INC	A
		CP	$02
		JR	NZ,l28ef	; (-$22)
		XOR	A
		LD	B,A
		LD	C,A
		LD	L,$10
		CALL	l306c
		JR	NC,l2928	; (+$0D)
		JR	Z,l2928		; (+$0B)
		CALL	l012d
		JR	NC,l2928	; (+$06)
		LD	(LODDRV),A
		LD	(SAVDRV),A
l2928:		SCF
		RET

l292a:		CP	$38
		JR	Z,l290b		; (-$23)
		CP	$16
		JR	Z,l290b		; (-$27)
		JR	l2906		; (-$2E)
		LD	B,C
		LD	B,D
		LD	C,L
l2937:		PUSH	HL
		PUSH	DE
		LD	HL,l29a7
		LD	BC,$0010
		LDIR
		EX	DE,HL
		LD	(HL),A
		INC	HL
		LD	(HL),$00
		INC	HL
		LD	(HL),$00
		POP	HL
		POP	DE
		PUSH	DE
		PUSH	HL
		LD	HL,l29b2
		LD	BC,$0008
		LDIR
		EX	DE,HL
		POP	DE
		PUSH	DE
		LD	(HL),E
		INC	HL
		LD	(HL),D
		RLCA
		RLCA
		RLCA
		RLCA
		AND	$01
		PUSH	AF
		LD	HL,x29ba
		CALL	l2bc8
		POP	BC
		JR	NC,l2972	; (+$07)
		LD	A,($EF21)
		CP	$01
		JR	Z,l298c		; (+$1A)
l2972:		LD	A,B
		EX	(SP),IX
		INC	(IX+$03)
		INC	(IX+$06)
		EX	(SP),IX
		LD	HL,$29BA
		CALL	l2bc8
		JR	NC,l299f	; (+$1A)
		LD	A,($EF21)
		CP	$01
		JR	NZ,l299f	; (+$13)
l298c:		LD	HL,$EF21
		POP	DE
		LD	BC,$0010
		LDIR
		LD	HL,$EF31
		POP	DE
		LD	BC,$0008
		LDIR
		RET

l299f:		XOR	A
		POP	HL
		LD	(HL),A
		POP	HL
		LD	(HL),A
		INC	HL
		LD	(HL),A
		RET

l29a7:		DB	1,0,0,0,0,0,0,0,0,0,0
l29b2:		DB	1,0,2,2,4,0,0,0
x29ba:		DB	"PLUSIDEDOS      "

l29ca:		RLCA
		RLCA
		RLCA
		RLCA
		LD	C,A
		LD	IX,$E897
		LD	B,$04
		LD	E,$00
		LD	H,E
		LD	L,E
l29d9:		LD	A,(IX+$00)
		AND	A
		JR	Z,l29fc		; (+$1D)
		LD	A,(IX+$10)
		AND	$10
		CP	C
		JR	NZ,x29f0	; (+$09)
		LD	A,(IX+$11)
		OR	(IX+$12)
		JR	Z,x29f0		; (+$01)
		INC	E
x29f0:		PUSH	DE
		LD	DE,$0013
		ADD	IX,DE
		POP	DE
		DJNZ	l29d9		; (-$20)
		LD	A,E
		SCF
		RET

l29fc:		PUSH	IX
		POP	HL
		JR	x29f0		; (-$11)
l2a01:		PUSH	BC
		PUSH	HL
		PUSH	IX
		PUSH	AF
		CALL	l29ca
		AND	A
		JR	Z,l2a13		; (+$07)
		LD	A,$3B
l2a0e:		POP	BC
		POP	BC
l2a10:		POP	BC
		POP	BC
		RET

l2a13:		POP	AF
		PUSH	AF
		CALL	l280d
		JR	C,l2a26		; (+$0C)
		LD	A,H
		OR	L
		LD	A,$3C
		JR	Z,l2a0e		; (-$12)
		LD	(IX+$08),L
		LD	(IX+$09),H
l2a26:		POP	AF
		POP	DE
		LD	(IX+$00),E
		LD	(IX+$01),D
		POP	BC
		LD	(IX+$02),B
		LD	(IX+$03),C
		LD	HL,$0000
		LD	D,H
		LD	E,C
		res	7,B
l2a3c:		ADD	HL,DE
		DJNZ	l2a3c		; (-$03)
		LD	(IX+$04),L
		LD	(IX+$05),H
		POP	HL
		LD	(IX+$06),L
		LD	(IX+$07),H
		PUSH	AF
		SRL	H
		RR	L
		SRL	H
		RR	L
		SRL	H
		RR	L
		XOR	A
		CALL	l2ecd
		BIT	7,(IX+$02)
		res	7,(IX+$02)
		LD	L,(IX+$08)
		LD	H,(IX+$09)
		PUSH	HL
		EX	(SP),IX
		PUSH	AF
		LD	A,$00
		LD	(IX+$01),A
		LD	(IX+$02),A
		JR	Z,l2a7a		; (+$01)
		INC	A
l2a7a:		LD	(IX+$03),A
		POP	AF
		EX	(SP),IX
		CALL	NZ,l2ef4
		EX	(SP),IX
		LD	(IX+$00),$01
		LD	(IX+$04),E
		LD	(IX+$05),D
		LD	(IX+$06),A
		EX	(SP),IX
		LD	DE,$0010
		AND	A
		SBC	HL,DE
		CALL	l2efc
		EX	(SP),IX
		POP	DE
		POP	AF
		PUSH	AF
		RLCA
		RLCA
		RLCA
		RLCA
		OR	$A0
		LD	(IX+$10),A
		XOR	A
		LD	(IX+$11),A
		LD	(IX+$12),A
		PUSH	DE
		LD	D,A
		LD	E,A
		CALL	l2d07
		JP	NC,l2a10
		LD	HL,$EF11
		LD	D,H
		LD	E,L
		INC	DE
		LD	BC,$003F
		LD	(HL),B
		LDIR
		LD	A,$FF
		LD	($EF21),A
		LD	E,(IX+$04)
		LD	D,(IX+$05)
		LD	A,(IX+$06)
		EX	(SP),IX
		CALL	l2ef4
		LD	($EF22),DE
		LD	($EF24),A
		LD	L,(IX+$00)
		LD	H,(IX+$01)
		DEC	HL
		LD	($EF25),HL
		LD	A,(IX+$02)
		DEC	A
		LD	($EF27),A
		LD	HL,$EF11
		CALL	l2efc
		EX	(SP),IX
		POP	DE
		POP	AF
		PUSH	AF
		PUSH	DE
		LD	BC,$0001
		CALL	l2b92
		JP	NC,l2a10
		LD	HL,$29BA
		LD	DE,$EF11
		LD	BC,$0010
		LDIR
		PUSH	IX
		POP	HL
		LD	BC,$000B
		LDIR
		POP	HL
		LD	DE,$EF31
		LD	BC,$0008
		LDIR
		EX	DE,HL
		LD	(HL),$38
		INC	HL
		LD	(HL),$38
		POP	AF
		LD	HL,$EF11
		CALL	l2b92
		RET

l2b31:		CALL	l280d
		RET	NC

		LD	L,(IX+$06)
		LD	H,(IX+$07)
		AND	A
		SBC	HL,BC
		LD	A,$38
		CCF
		RET	NC

		LD	L,(IX+$08)
		LD	H,(IX+$09)
		PUSH	HL
		POP	IX
		PUSH	BC
		SRL	B
		RR	C
		SRL	B
		RR	C
		SRL	B
		RR	C
		LD	D,B
		LD	E,C
		POP	BC
		LD	A,C
		AND	$07
		LD	C,$00
		RRA
		RR	C
		RRA
		RR	C
		LD	B,A
		SCF
		RET

l2b69:		PUSH	BC
		PUSH	IX
		PUSH	HL
		CALL	l2b31
		JR	NC,l2b8d	; (+$1B)
		PUSH	BC
		LD	C,$00
		LD	B,$07
		LD	HL,tmp_buff
		CALL	l2558
		POP	BC
		JR	NC,l2b8d	; (+$0D)
		LD	HL,tmp_buff
		ADD	HL,BC
		POP	DE
		LD	BC,$0040
		LDIR
		SCF
		JR	l2b8e		; (+$01)
l2b8d:		POP	HL
l2b8e:		POP	IX
		POP	BC
		RET

l2b92:		PUSH	BC
		PUSH	IX
		PUSH	HL
		CALL	l2b31
		JR	NC,l2bc3	; (+$28)
		PUSH	DE
		PUSH	BC
		LD	C,$00
		LD	B,$07
		LD	HL,tmp_buff
		CALL	l2558
		POP	BC
		POP	DE
		JR	NC,l2bc3	; (+$18)
		LD	HL,tmp_buff
		ADD	HL,BC
		EX	DE,HL
		EX	(SP),HL
		LD	BC,$0040
		LDIR
		LD	C,$00
		LD	B,$07
		LD	HL,tmp_buff
		POP	DE
		CALL	l25fb
		JR	l2bc4		; (+$01)
l2bc3:		POP	HL
l2bc4:		POP	IX
		POP	BC
		RET

l2bc8:		PUSH	IX
		LD	BC,$0000
l2bcd:		PUSH	AF
		PUSH	BC
		PUSH	HL
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l2bfd	; (+$25)
		POP	HL
		PUSH	HL
		LD	DE,$EF11
		LD	B,$10
l2bdf:		LD	A,(DE)
		CP	(HL)
		JR	Z,l2bf8		; (+$15)
		CP	$41
		JR	C,l2beb		; (+$04)
		CP	$5B
		JR	C,l2bf3		; (+$08)
l2beb:		CP	$61
		JR	C,l2c03		; (+$14)
		CP	$7B
		JR	NC,l2c03	; (+$10)
l2bf3:		XOR	$20
		CP	(HL)
		JR	NZ,l2c03	; (+$0B)
l2bf8:		INC	DE
		INC	HL
		DJNZ	l2bdf		; (-$1D)
		SCF
l2bfd:		POP	HL
		POP	BC
l2bff:		POP	HL
		POP	IX
		RET

l2c03:		POP	HL
		POP	BC
		INC	BC
		LD	A,B
		OR	C
		LD	A,$38
		JR	Z,l2bff		; (-$0D)
		POP	AF
		JR	l2bcd		; (-$42)
l2c0f:		PUSH	HL
		PUSH	AF
		PUSH	BC
		CALL	l2bc8
		POP	BC
		CCF
		LD	A,$39
		JR	NC,l2c38	; (+$1D)
		POP	AF
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l2c38	; (+$13)
		POP	AF
		POP	HL
		LD	DE,$EF11
		PUSH	BC
		LD	BC,$0010
		LDIR
		POP	BC
		LD	HL,$EF11
		CALL	l2b92
		RET

l2c38:		POP	HL
		POP	HL
		RET

l2c3b:		PUSH	HL
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l2c58	; (+$13)
		POP	AF
		POP	HL
		LD	DE,$EF31
		PUSH	BC
		LD	BC,$0020
		LDIR
		POP	BC
		LD	HL,$EF11
		CALL	l2b92
		RET

l2c58:		POP	HL
		POP	HL
		RET

l2c5b:		PUSH	IX
		PUSH	DE
		PUSH	HL
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	D,A
		LD	IX,$E897
		LD	E,$04
l2c6a:		LD	A,(IX+$00)
		AND	A
		JR	Z,l2c84		; (+$14)
		LD	A,(IX+$10)
		AND	$10
		CP	D
		JR	NZ,l2c84	; (+$0C)
		LD	L,(IX+$11)
		LD	H,(IX+$12)
		SBC	HL,BC
		LD	A,$3B
		JR	Z,l2c8f		; (+$0B)
l2c84:		PUSH	BC
		LD	BC,$0013
		ADD	IX,BC
		POP	BC
		DEC	E
		JR	NZ,l2c6a	; (-$24)
		SCF
l2c8f:		POP	HL
		POP	DE
		POP	IX
		RET

l2c94:		PUSH	AF
		CALL	l2c5b
		JR	NC,l2cd9	; (+$3F)
		LD	IX,$E897
		LD	E,$04
l2ca0:		LD	A,(IX+$00)
		AND	A
		JR	Z,l2cb4		; (+$0E)
		PUSH	BC
		LD	BC,$0013
		ADD	IX,BC
		POP	BC
		DEC	E
		JR	NZ,l2ca0	; (-$10)
		LD	A,$3C
		JR	l2cd9		; (+$25)
l2cb4:		POP	AF
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l2cd9	; (+$1B)
		LD	A,($EF21)
		DEC	A
		CP	$FD
		LD	A,$38
		JR	NC,l2cd9	; (+$11)
		POP	AF
		CALL	l2ce1
		LD	HL,$EF21
		PUSH	IX
		POP	DE
		LD	BC,$0010
		LDIR
		SCF
		RET

l2cd9:		POP	HL
		RET

l2cdb:		LD	(IX+$00),$00
		SCF
		RET

l2ce1:		PUSH	AF
		PUSH	IX
		CALL	l280d
		LD	A,(IX+$02)
		AND	$40
		POP	IX
		LD	(IX+$10),A
		POP	AF
		AND	$01
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,A
		ADD	A,$A0
		OR	(IX+$10)
		LD	(IX+$10),A
		LD	(IX+$11),C
		LD	(IX+$12),B
		RET

l2d07:		PUSH	IX
		JR	l2d2b		; (+$20)
l2d0b:		PUSH	IX
		PUSH	HL
		PUSH	AF
		CALL	l2c5b
		JR	NC,l2d63	; (+$4F)
		POP	AF
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l2d63	; (+$45)
		POP	AF
		POP	HL
		PUSH	IX
		POP	DE
		LD	IX,$EF21
		CALL	l2ce1
		LD	A,L
l2d2b:		PUSH	DE
		LD	HL,tmp_buff
		LD	(HL),A
		LD	DE,$ED12
		LD	BC,$01FF
		LDIR
		POP	DE
		LD	C,$00
		LD	A,D
		OR	E
		JR	NZ,l2d48	; (+$09)
		LD	E,(IX+$07)
		LD	D,(IX+$08)
		LD	C,(IX+$09)
l2d48:		LD	B,$07
		LD	HL,tmp_buff
		PUSH	DE
		CALL	l25fb
		POP	DE
		JR	NC,l2d65	; (+$11)
		LD	A,C
		OR	D
		OR	E
		SCF
		JR	Z,l2d65		; (+$0B)
		DEC	DE
		LD	A,D
		AND	E
		INC	A
		JR	NZ,l2d48	; (-$18)
		DEC	C
		JR	l2d48		; (-$1B)
l2d63:		POP	HL
		POP	HL
l2d65:		POP	IX
		RET

l2d68:		LD	BC,$FFFF
		LD	($EF92),BC
		LD	($EF94),BC
		INC	BC
		LD	($EF96),BC
l2d78:		PUSH	AF
		PUSH	DE
		PUSH	HL
		LD	HL,$EF11
		CALL	l2b69
		POP	HL
		POP	DE
		JR	NC,l2dd8	; (+$53)
		LD	A,($EF21)
		CP	H
		JR	NZ,l2dd0	; (+$45)
		LD	A,($EF2B)
		AND	A
		JR	NZ,l2da0	; (+$0F)
		LD	A,L
		PUSH	HL
		LD	HL,($EF28)
		SBC	HL,DE
		LD	L,A
		LD	A,($EF2A)
		SBC	A,L
		POP	HL
		JR	C,l2dd0		; (+$30)
l2da0:		POP	AF
		PUSH	AF
		CALL	l2c5b
		JR	NC,l2dd0	; (+$29)
		PUSH	DE
		PUSH	HL
		LD	DE,($EF92)
		LD	HL,($EF28)
		AND	A
		SBC	HL,DE
		LD	DE,($EF94)
		LD	HL,($EF2A)
		SBC	HL,DE
		JR	NC,l2dce	; (+$10)
		LD	HL,($EF28)
		LD	($EF92),HL
		LD	HL,($EF2A)
		LD	($EF94),HL
		LD	($EF96),BC
l2dce:		POP	HL
		POP	DE
l2dd0:		INC	BC
		LD	A,B
		OR	C
		JR	Z,l2dd8		; (+$03)
		POP	AF
		JR	l2d78		; (-$60)
l2dd8:		POP	AF
		LD	BC,($EF96)
		LD	A,B
		OR	C
		RET	Z

		SCF
		RET

l2de2:		PUSH	IX
		PUSH	AF
		PUSH	HL
		CALL	l2bc8
		LD	A,$39
		CCF
		JP	NC,l2ec8
		POP	IX
		LD	E,(IX+$17)
		LD	D,(IX+$18)
		LD	L,(IX+$19)
		LD	H,$FF
		POP	AF
		PUSH	AF
		CALL	l2d68
		LD	A,$1A
		JP	NC,l2ec9
		POP	AF
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JP	NC,l2ec9
		LD	L,(IX+$17)
		LD	H,(IX+$18)
		LD	E,(IX+$19)
		POP	AF
		PUSH	AF
		PUSH	IX
		CALL	l280d
		JP	NC,l2ec8
		LD	A,E
		CALL	l2ecd
		LD	HL,($EF22)
		ADD	HL,DE
		LD	E,A
		LD	A,($EF24)
		ADD	A,E
		CP	(IX+$02)
		JR	C,l2e3a		; (+$04)
		SUB	(IX+$02)
		INC	HL
l2e3a:		EX	DE,HL
		LD	L,A
		LD	A,($EF27)
		CP	L
		LD	A,L
		JR	NZ,l2e4a	; (+$07)
		LD	HL,($EF25)
		SBC	HL,DE
		JR	Z,l2e9d		; (+$53)
l2e4a:		POP	HL
		EX	(SP),HL
		LD	L,A
		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	BC,$0000
l2e53:		POP	AF
		PUSH	AF
		LD	HL,$EF51
		CALL	l2b69
		JR	NC,l2ec4	; (+$67)
		LD	A,($EF61)
		AND	A
		JR	Z,l2e6a		; (+$07)
		INC	BC
		LD	A,B
		OR	C
		JR	NZ,l2e53	; (-$15)
		JR	l2ec4		; (+$5A)
l2e6a:		LD	A,$FF
		LD	($EF61),A
		LD	A,($EF27)
		LD	($EF67),A
		LD	HL,($EF25)
		LD	($EF65),HL
		POP	HL
		POP	DE
		PUSH	DE
		PUSH	HL
		LD	A,L
		CALL	l2ef4
		LD	($EF64),A
		LD	($EF62),DE
		LD	HL,$EF51
		CALL	l2efc
		POP	AF
		PUSH	AF
		CALL	l2b92
		JR	NC,l2ec6	; (+$2F)
		POP	HL
		LD	A,L
		POP	DE
		POP	BC
		EX	(SP),HL
		PUSH	HL
l2e9d:		LD	($EF27),A
		LD	($EF25),DE
		LD	HL,$EF11
		CALL	l2efc
		POP	DE
		PUSH	DE
		PUSH	BC
		LD	HL,$EF22
		LD	BC,$0011
		EX	DE,HL
		ADD	HL,BC
		EX	DE,HL
		LD	BC,$000F
		LDIR
		POP	BC
		POP	HL
		POP	AF
		CALL	l2b92
		POP	IX
		RET

l2ec4:		LD	A,$1A
l2ec6:		POP	HL
		POP	HL
l2ec8:		POP	HL
l2ec9:		POP	HL
		POP	IX
		RET

l2ecd:		PUSH	BC
		SCF
		RL	L
		RL	H
		RLA
		LD	C,(IX+$04)
		LD	B,(IX+$05)
		LD	DE,$0000
		AND	A
l2ede:		SBC	HL,BC
		SBC	A,$00
		INC	DE
		JR	NC,l2ede	; (-$07)
		ADD	HL,BC
		DEC	DE
		XOR	A
		LD	C,(IX+$03)
		LD	B,A
l2eec:		SBC	HL,BC
		INC	A
		JR	NC,l2eec	; (-$05)
		DEC	A
		POP	BC
		RET

l2ef4:		INC	A
		CP	(IX+$02)
		RET	C

		INC	DE
		XOR	A
		RET

l2efc:		PUSH	BC
		PUSH	DE
		PUSH	HL
		EX	(SP),IX
		LD	E,(IX+$14)
		LD	D,(IX+$15)
		LD	C,(IX+$16)
		INC	C
		EX	(SP),IX
		CALL	l2f63
		EX	DE,HL
		LD	A,C
		EX	(SP),IX
		LD	E,(IX+$11)
		LD	D,(IX+$12)
		LD	C,(IX+$13)
		EX	(SP),IX
		CALL	l2f63
		AND	A
		SBC	HL,DE
		SBC	A,C
		EX	DE,HL
		LD	C,A
		LD	B,$00
		LD	A,(IX+$03)
		PUSH	IX
		LD	IX,$0000
		LD	H,B
		LD	L,B
l2f35:		ADD	IX,DE
		ADC	HL,BC
		DEC	A
		JR	NZ,l2f35	; (-$07)
		PUSH	IX
		POP	BC
		SRL	H
		RR	L
		RR	B
		RR	C
		LD	A,B
		OR	C
		JR	NZ,l2f4c	; (+$01)
		DEC	HL
l2f4c:		DEC	BC
		POP	IX
		EX	(SP),IX
		LD	(IX+$17),C
		LD	(IX+$18),B
		LD	(IX+$19),L
		LD	(IX+$1A),H
		EX	(SP),IX
		POP	HL
		POP	DE
		POP	BC
		RET

l2f63:		PUSH	AF
		PUSH	HL
		PUSH	BC
		LD	B,(IX+$02)
		CALL	l2f77
		POP	BC
		LD	B,$00
		EX	DE,HL
		ADD	HL,BC
		EX	DE,HL
		ADC	A,B
		LD	C,A
		POP	HL
		POP	AF
		RET

l2f77:		EX	DE,HL
		XOR	A
		EX	AF,AF'
		PUSH	AF
		XOR	A
		LD	D,A
		LD	E,A
l2f7e:		SRL	B
		JR	NC,l2f89	; (+$07)
		LD	C,A
		EX	AF,AF'
		EX	DE,HL
		ADD	HL,DE
		EX	DE,HL
		ADC	A,C
		EX	AF,AF'
l2f89:		JR	Z,l2f90		; (+$05)
		ADD	HL,HL
		ADC	A,A
		JP	l2f7e
l2f90:		POP	AF
		EX	AF,AF'
		RET

l2f93:		PUSH	IX
		PUSH	BC
		PUSH	AF
		CALL	l280d
		JP	NC,l3047
		POP	AF
		PUSH	AF
		CALL	l2c5b
		JP	NC,l3047
		POP	AF
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JP	NC,l3047
l2fb0:		LD	BC,$0000
l2fb3:		POP	AF
		PUSH	AF
		LD	HL,$EF51
		CALL	l2b69
		JP	NC,l3052
		LD	A,($EF61)
		CP	$FF
		JP	NZ,l304c
		LD	A,($EF67)
		LD	DE,($EF65)
		CALL	l2ef4
		LD	L,A
		LD	A,($EF24)
		SUB	L
		JR	NZ,l2fde	; (+$07)
		LD	HL,($EF22)
		SBC	HL,DE
		JR	Z,l2ff8		; (+$1A)
l2fde:		LD	A,($EF27)
		LD	DE,($EF25)
		CALL	l2ef4
		LD	L,A
		LD	A,($EF64)
		SUB	L
		JR	NZ,l304c	; (+$5D)
		LD	HL,($EF62)
		SBC	HL,DE
		JR	Z,l3006		; (+$10)
		JR	l304c		; (+$54)
l2ff8:		LD	A,($EF27)
		LD	($EF67),A
		LD	HL,($EF25)
		LD	($EF65),HL
		JR	l3012		; (+$0C)
l3006:		LD	A,($EF24)
		LD	($EF64),A
		LD	HL,($EF22)
		LD	($EF62),HL
l3012:		LD	HL,$EF51
		CALL	l2efc
		POP	AF
		PUSH	AF
		CALL	l2b92
		JR	NC,l3047	; (+$28)
		POP	AF
		POP	DE
		PUSH	BC
		PUSH	AF
		PUSH	DE
		LD	B,$40
		LD	HL,$EF11
l3029:		LD	(HL),$00
		INC	HL
		DJNZ	l3029		; (-$05)
		POP	BC
		POP	AF
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b92
		JR	NC,l3047	; (+$0E)
		LD	HL,$EF51
		LD	DE,$EF11
		LD	BC,$0040
		LDIR
		JP	l2fb0
l3047:		POP	BC
		POP	BC
		POP	IX
		RET

l304c:		INC	BC
		LD	A,B
		OR	C
		JP	NZ,l2fb3
l3052:		LD	A,$FF
		LD	($EF21),A
		LD	HL,$EF11
		LD	B,$10
l305c:		LD	(HL),$00
		INC	HL
		DJNZ	l305c		; (-$05)
		LD	HL,$EF11
		POP	AF
		POP	BC
		CALL	l2b92
		POP	IX
		RET

l306c:		PUSH	HL
		LD	HL,$EF11
		CALL	l2b69
		POP	HL
		RET	NC

		LD	A,L
		AND	$1F
		LD	L,A
		LD	H,$00
		LD	DE,$EF31
		ADD	HL,DE
		LD	A,(HL)
		AND	A
		SCF
		RET

l3083:		PUSH	AF
		PUSH	HL
		LD	HL,$EF11
		CALL	l2b69
		POP	HL
		JR	NC,l30a2	; (+$14)
		LD	A,L
		AND	$1F
		LD	L,A
		LD	A,H
		LD	H,$00
		LD	DE,$EF31
		ADD	HL,DE
		LD	(HL),A
		POP	AF
		LD	HL,$EF11
		CALL	l2b92
		RET

l30a2:		POP	HL
		RET

l30a4:		LD	A,(IX+$00)
		CP	$02
		SCF
		RET	Z

		LD	A,$3D
		AND	A
		RET

l30af:		CALL	l30a4
		RET	NC

		LD	C,(IX+$0C)
		LD	B,(IX+$0D)
		SCF
		RET

l30bb:		CALL	l30a4
		RET	NC

		PUSH	HL
		LD	L,(IX+$0E)
		LD	H,(IX+$0F)
		AND	A
		SBC	HL,BC
		POP	HL
		CCF
		LD	A,$15
		RET	NC

		LD	(IX+$0C),C
		LD	(IX+$0D),B
		SCF
		RET

l30d6:		CALL	l30a4
		RET	NC

		LD	A,B
		PUSH	AF
		PUSH	HL
		LD	E,(IX+$0C)
		LD	D,(IX+$0D)
		LD	L,(IX+$0E)
		LD	H,(IX+$0F)
		AND	A
		SBC	HL,DE
		JR	NC,l30f1	; (+$03)
		LD	DE,$0000
l30f1:		PUSH	DE
		LD	B,(IX+$0B)
		CALL	l2f77
		LD	C,A
		POP	HL
		INC	HL
		LD	(IX+$0C),L
		LD	(IX+$0D),H
		POP	HL
		POP	AF
		LD	B,A
		SCF
		RET

l3106:		CALL	l30d6
		RET	NC

		CALL	l26a8
		RET	NC

		PUSH	BC
		LD	E,(IX+$0B)
		LD	A,B
		CALL	l021b
		SLA	E
		PUSH	DE
		CALL	l279b
l311c:		CALL	l25b0
		POP	DE
		DEC	E
		JR	Z,l3129		; (+$06)
		PUSH	DE
		CALL	l27a7
		JR	l311c		; (-$0D)
l3129:		LD	A,$07
		CALL	l021b
		POP	BC
		SCF
		RET

l3131:		CALL	l30d6
		RET	NC

		LD	A,(IX+$0B)
l3138:		PUSH	AF
		PUSH	DE
		CALL	l25fb
		POP	DE
		JR	NC,l314c	; (+$0C)
		INC	DE
		LD	A,D
		OR	E
		JR	NZ,l3146	; (+$01)
		INC	C
l3146:		POP	AF
		DEC	A
		JR	NZ,l3138	; (-$12)
		SCF
		RET

l314c:		POP	DE
		RET

l314e:		CALL	l30d6
		RET	NC

		LD	A,(IX+$0B)
l3155:		PUSH	AF
		PUSH	BC
		PUSH	DE
		PUSH	HL
		LD	HL,tmp_buff
		LD	B,$07
		CALL	l2558
		POP	HL
		POP	DE
		POP	BC
		JR	NC,l3189	; (+$23)
		PUSH	DE
		PUSH	HL
		CALL	l25fb
		POP	HL
		POP	DE
		JR	NC,l3189	; (+$1A)
		PUSH	BC
		PUSH	DE
		LD	DE,tmp_buff
		LD	BC,$0200
		EX	DE,HL
		LDIR
		EX	DE,HL
		POP	DE
		POP	BC
		INC	DE
		LD	A,D
		OR	E
		JR	NZ,l3183	; (+$01)
		INC	C
l3183:		POP	AF
		DEC	A
		JR	NZ,l3155	; (-$32)
		SCF
		RET

l3189:		POP	DE
		RET

l318b:		PUSH	AF
		PUSH	BC
		CALL	l31c7
		LD	H,$02
		PUSH	DE
		PUSH	HL
		LD	A,$00
		CALL	l2d68
		POP	HL
		POP	DE
		LD	A,$00
		JR	C,l31a9		; (+$0A)
		INC	A
		CALL	l2d68
		LD	A,$40
		JR	NC,l31c4	; (+$1D)
		LD	A,$01
l31a9:		CALL	l2c94
		JR	NC,l31c4	; (+$16)
l31ae:		POP	BC
		LD	(IX+$0E),C
		LD	(IX+$0F),B
		POP	AF
		LD	(IX+$0B),A
		LD	(IX+$0C),$00
		LD	(IX+$0D),$00
		SCF
		RET

l31c3:		POP	BC
l31c4:		POP	BC
		POP	BC
		RET

l31c7:		LD	HL,$0000
		LD	E,A
		DEC	E
		LD	D,A
		DEC	A
		CP	$20
		LD	A,$15
		JR	NC,l31c3	; (-$11)
		LD	A,H
l31d5:		ADD	HL,BC
		ADC	A,$00
		DEC	D
		JR	NZ,l31d5	; (-$06)
		ADD	HL,DE
		ADC	A,$00
		EX	DE,HL
		LD	L,A
		RET

l31e1:		LD	D,A
		CALL	l30a4
		RET	NC

		LD	A,D
		PUSH	AF
		PUSH	BC
		CALL	l31c7
		LD	A,(IX+$09)
		CP	L
		JR	C,l3202		; (+$10)
		JR	NZ,l31ae	; (-$46)
		LD	A,(IX+$08)
		CP	D
		JR	C,l3202		; (+$08)
		JR	NZ,l31ae	; (-$4E)
		LD	A,(IX+$07)
		CP	E
		JR	NC,l31ae	; (-$54)
l3202:		POP	BC
		POP	BC
		LD	A,$15
		AND	A
		RET

l3208:		LD	A,L
		LD	($EF91),A
		SUB	$41
		JR	C,l3223		; (+$13)
		CP	$10
		JR	NC,l3223	; (+$0F)
		LD	L,A
		LD	H,$00
		ADD	HL,HL
		LD	DE,$E2A1
		ADD	HL,DE
		LD	D,(HL)
		DEC	HL
		LD	E,(HL)
		LD	A,D
		OR	E
		SCF
		RET

l3223:		AND	A
		LD	A,$15
		RET

l3227:		CP	$02
		JP	NC,l32f5
		PUSH	IX
		PUSH	AF
		PUSH	BC
		PUSH	HL
		LD	HL,$E8E3
		LD	B,$02
		LD	DE,$0130
l3239:		LD	A,(HL)
		INC	HL
		OR	(HL)
		DEC	HL
		JR	Z,l324b		; (+$0C)
		ADD	HL,DE
		DJNZ	l3239		; (-$09)
		LD	A,$3F
		AND	A
l3245:		POP	DE
		POP	DE
		POP	DE
		POP	IX
		RET

l324b:		EX	(SP),HL
		CALL	l3208
		JR	NC,l3245	; (-$0C)
		LD	A,$3E
		CCF
		JR	NZ,l3245	; (-$11)
		POP	DE
		POP	BC
		POP	AF
		PUSH	HL
		PUSH	AF
		PUSH	DE
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l3245	; (-$1F)
		LD	A,($EF21)
		CP	$03
		JR	Z,l3270		; (+$05)
		AND	A
		LD	A,$38
		JR	l3245		; (-$2B)
l3270:		LD	HL,$EF31
		POP	DE
		POP	AF
		PUSH	DE
		PUSH	BC
		PUSH	AF
		LD	BC,o001C
		LDIR
		EX	DE,HL
		LD	A,($EF91)
		LD	(HL),A
		INC	HL
		POP	AF
		PUSH	AF
		RLCA
		RLCA
		RLCA
		RLCA
		OR	$A0
		LD	(HL),A
		INC	HL
		LD	B,$0A
l328f:		LD	(HL),$00
		INC	HL
		DJNZ	l328f		; (-$05)
		LD	DE,$0008
		EX	DE,HL
		ADD	HL,DE
		EX	DE,HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		INC	HL
		LD	(HL),$D6
		INC	HL
		LD	(HL),$27
		INC	HL
		LD	(HL),$D8
		INC	HL
		LD	(HL),$27
		INC	HL
		LD	(HL),$F0
		INC	HL
		LD	(HL),$27
		POP	AF
		POP	BC
		PUSH	BC
		PUSH	AF
		CALL	l2c94
		JR	NC,l32e9	; (+$31)
		POP	AF
		POP	BC
		POP	HL
		PUSH	HL
		LD	DE,$0017
		ADD	HL,DE
		PUSH	IX
		POP	DE
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	DE
		POP	HL
		LD	(HL),E
		INC	HL
		LD	(HL),D
		POP	IX
		LD	HL,FLAGS3
		BIT	6,(HL)
		SCF
		RET	Z

		PUSH	AF
		PUSH	BC
		LD	A,($EF91)
		LD	L,A
		CALL	l34ac
		LD	A,($EF91)
		LD	H,A
		LD	L,$1C
		POP	BC
		POP	AF
		CALL	l3083
		RET

l32e9:		POP	HL
		POP	HL
		POP	HL
		LD	(HL),$00
		INC	HL
		LD	(HL),$00
		POP	DE
		POP	IX
		RET

l32f5:		PUSH	HL
		SUB	$02
		LD	E,A
		LD	D,$00
		CP	$02
		JR	Z,l3306		; (+$07)
		JR	NC,l336c	; (+$6B)
		CALL	l0157
		JR	NC,l336c	; (+$66)
l3306:		LD	H,D
		LD	L,E
		PUSH	DE
		ADD	HL,HL
		ADD	HL,DE
		ADD	HL,HL
		LD	DE,l346f
		ADD	HL,DE
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		LD	HL,xdpb_ptrs
		LD	E,$10
l3318:		LD	A,(HL)
		INC	HL
		CP	C
		JR	NZ,l3323	; (+$06)
		LD	A,(HL)
		CP	B
		LD	A,$3B
		JR	Z,l3369		; (+$46)
l3323:		INC	HL
		DEC	E
		JR	NZ,l3318	; (-$0F)
		POP	DE
		POP	HL
		PUSH	DE
		CALL	l3208
		POP	DE
		RET	NC

		LD	A,$3E
		CCF
		RET	NZ

		LD	(HL),C
		INC	HL
		LD	(HL),B
		LD	HL,o001C
		ADD	HL,BC
		LD	A,(unit0)
		AND	A
		JR	NZ,l334a	; (+$0A)
		LD	A,E
		AND	A
		JR	NZ,l334a	; (+$06)
		LD	A,($EF91)
		LD	(unit0),A
l334a:		LD	A,($EF91)
		LD	(HL),A
		LD	HL,FLAGS3
		BIT	6,(HL)
		SCF
		RET	Z

		PUSH	AF
		PUSH	DE
		LD	L,A
		CALL	l34ac
		POP	DE
		POP	AF
		LD	HL,$000D
		ADD	HL,DE
		LD	H,A
		XOR	A
		LD	B,A
		LD	C,A
		CALL	l3083
		RET

l3369:		POP	HL
		POP	HL
		RET

l336c:		LD	A,$41
		POP	HL
		RET

l3370:		CALL	l3208
		RET	NC

		PUSH	IX
		JR	Z,l33bb		; (+$43)
		PUSH	DE
		POP	IX
		CALL	l181b
		JR	NC,l33ca	; (+$4A)
		XOR	A
		LD	(HL),A
		INC	HL
		LD	(HL),A
		EX	DE,HL
		LD	A,(IX+$1B)
		AND	$0C
		JR	NZ,l338f	; (+$03)
		LD	(HL),A
		INC	HL
		LD	(HL),A
l338f:		LD	A,(unit0)
		CP	(IX+$1C)
		JR	NZ,l339b	; (+$04)
		XOR	A
		LD	(unit0),A
l339b:		LD	(IX+$1C),$00
		LD	E,(IX+$2A)
		LD	D,(IX+$2B)
		LD	HL,$27D6
		AND	A
		SBC	HL,DE
		JR	NZ,l33bb	; (+$0E)
		LD	E,(IX+$17)
		LD	D,(IX+$18)
		PUSH	DE
		POP	IX
		CALL	l2cdb
		JR	NC,l33ca	; (+$0F)
l33bb:		LD	HL,FLAGS3
		BIT	6,(HL)
		SCF
		JR	Z,l33ca		; (+$07)
		LD	A,($EF91)
		LD	L,A
		CALL	l34ac
l33ca:		POP	IX
		RET

l33cd:		PUSH	BC
		LD	A,$20
		LD	E,$12
l33d2:		LD	(BC),A
		INC	BC
		DEC	E
		JR	NZ,l33d2	; (-$05)
		CALL	l3208
		EX	DE,HL
		POP	BC
		RET	NC

		RET	Z

		PUSH	IX
		PUSH	BC
		PUSH	HL
		POP	IX
		LD	E,(IX+$2A)
		LD	D,(IX+$2B)
		LD	HL,$27D6
		AND	A
		SBC	HL,DE
		JR	NZ,l3438	; (+$46)
		LD	E,(IX+$17)
		LD	D,(IX+$18)
		PUSH	DE
		POP	IX
		LD	A,(IX+$10)
		RRA
		RRA
		RRA
		RRA
		AND	$01
		LD	C,(IX+$11)
		LD	B,(IX+$12)
		PUSH	BC
		PUSH	AF
		LD	HL,$EF11
		CALL	l2b69
		JR	NC,l3432	; (+$1E)
		POP	AF
		POP	BC
		POP	DE
		PUSH	BC
		PUSH	AF
		ADD	A,$30
		LD	(DE),A
		INC	DE
		LD	A,$3E
		LD	(DE),A
		INC	DE
		LD	HL,$EF11
		LD	BC,$0010
		LDIR
		XOR	A
		SBC	A,$01
		POP	BC
		LD	A,B
		POP	BC
		POP	IX
		RET

l3432:		POP	DE
		POP	DE
l3434:		POP	DE
		POP	IX
		RET

l3438:		PUSH	IX
		POP	DE
		LD	IX,l346f
		LD	A,$03
l3441:		LD	L,(IX+$00)
		LD	H,(IX+$01)
		AND	A
		SBC	HL,DE
		JR	Z,l3458		; (+$0C)
		LD	BC,$0006
		ADD	IX,BC
		DEC	A
		JR	NZ,l3441	; (-$13)
		LD	A,$41
		JR	l3434		; (-$24)
l3458:		LD	L,(IX+$02)
		LD	H,(IX+$03)
		POP	DE
		LD	C,(IX+$04)
		LD	B,$00
		LDIR
		XOR	A
		SBC	A,$01
		LD	A,(IX+$05)
		POP	IX
		RET

l346f:		DB	$C0,$E2,$81,$34,$11,$02,$2D,$E3
		DB	$92,$34,$11,$03,$9A,$E3,$A3,$34
		DB	$09,$04

l3474:		DB	"2>Floppy device 0"
l3485:		DB	"3>Floppy device 1"
l3496:		DB	"4>RAMdisk"

l34ac:		PUSH	HL
		XOR	A
		LD	B,A
		LD	C,A
		LD	HL,$EF11
		CALL	l2b69
		POP	DE
		JR	NC,l34eb	; (+$32)
		LD	HL,$EF3B
		LD	A,E
		CP	$41
		JR	NZ,l34c3	; (+$02)
		LD	(HL),$01
l34c3:		INC	HL
		CP	$42
		JR	NZ,l34ca	; (+$02)
		LD	(HL),$01
l34ca:		INC	HL
		CP	$4D
		JR	NZ,l34d1	; (+$02)
		LD	(HL),$01
l34d1:		LD	HL,$EF3E
		LD	B,$03
l34d6:		LD	A,(HL)
		CP	E
		JR	NZ,l34dc	; (+$02)
		LD	(HL),$00
l34dc:		INC	HL
		DJNZ	l34d6		; (-$09)
		XOR	A
		LD	B,A
		LD	C,A
		PUSH	DE
		LD	HL,$EF11
		CALL	l2b92
		POP	DE
		RET	NC

l34eb:		XOR	A
l34ec:		LD	BC,$0000
		PUSH	AF
l34f0:		POP	AF
		PUSH	AF
		PUSH	DE
		LD	HL,$EF11
		CALL	l2b69
		POP	DE
		JR	NC,l3526	; (+$2A)
		LD	A,($EF21)
		CP	$03
		JR	NZ,l3519	; (+$16)
		LD	A,($EF4D)
		CP	E
		JR	NZ,l3519	; (+$10)
		XOR	A
		LD	($EF4D),A
		POP	AF
		PUSH	AF
		PUSH	DE
		LD	HL,$EF11
		CALL	l2b92
		POP	DE
		JR	NC,l3526	; (+$0D)
l3519:		INC	BC
		LD	A,B
		OR	C
		JR	NZ,l34f0	; (-$2E)
l351e:		POP	AF
		INC	A
		CP	$02
		JR	NZ,l34ec	; (-$38)
		SCF
		RET

l3526:		CP	$38
		JR	Z,l351e		; (-$0C)
		CP	$16
		JR	Z,l351e		; (-$10)
		POP	DE
		AND	A
		RET

l3531:		LD	D,H		; CREO QUE ACA COMIENZA LA CARGA DE UN Z80
		LD	E,L
l3533:		LD	A,(DE)
		INC	DE
		CP	$FF
		JR	NZ,l3533	; (-$06)
		DEC	DE
		DEC	DE
		LD	A,(DE)
		LD	($401D),A
		LD	BC,$0001	; NRO DE HANDLE $00 ?
		LD	D,B
		LD	E,$01
		CALL	l0106		; ABRE ARCHIVO
		RET	NC

		LD	HL,$5000
		LD	DE,$001E	; NRO DE HANDLE $00 ?
		LD	B,D
		LD	C,$07
		CALL	l0112
		RET	NC

		LD	B,$00		; NRO DE HANDLE $00 ?
		CALL	l0133
		PUSH	DE
		PUSH	HL
		LD	B,$00		; NRO DE HANDLE $00 ?
		CALL	l0139
		POP	BC
		AND	A
		SBC	HL,BC
		POP	BC
		LD	A,E
		SBC	A,C
		LD	BC,$0001
		ADD	HL,BC
		ADC	A,B
		EX	DE,HL
		LD	($4015),A
		PUSH	AF
		CALL	x0521
		POP	AF
		LD	HL,$506B
		CALL	l3886
		RET	NC

		LD	($503F),HL
		LD	B,$00		; NRO DE HANDLE $00 ?
		CALL	l0109
		DI
		LD	SP,$5067
		LD	IX,$506D
		LD	L,(IX-$02)
		LD	H,(IX-$01)
		CALL	l276d
		CALL	x27c2
		EXX
		LD	HL,$401D
		BIT	6,(HL)
		JP	Z,l3621
		LD	A,($501A)
		OUT	($FE),A
		LD	A,($5013)
		AND	$04
		LD	($5013),A
		LD	A,($4015)
		AND	A
		LD	HL,$0010
		EXX
		LD	IYL,$00
		LD	L,$03
		JP	Z,l364c
		CALL	l3772
		LD	SP,$4047
		CALL	l3829
		CALL	l3875
		LD	L,A
		CALL	l3875
		LD	H,A
		LD	($4027),HL
		LD	A,$C3
		LD	($4026),A
		CALL	l3875
		LD	($401D),A
		AND	$07
		JR	Z,l35f8		; (+$15)
		EXX
		LD	HL,src_add
		LD	E,A
l35e8:		XOR	A
		CALL	l37dd
		LD	D,(HL)
		LD	A,E
		CALL	l37dd
		LD	(HL),D
		INC	HL
		BIT	7,H
		JR	NZ,l35e8	; (-$0F)
		EXX
l35f8:		CALL	l3875
		XOR	A
l35fc:		CP	$02
		JR	Z,l361a		; (+$1A)
		CP	$05
		JR	Z,l361a		; (+$16)
		LD	HL,($401D)
		LD	H,A
		LD	A,L
		AND	$07
		CP	H
		LD	A,H
		JR	Z,l361a		; (+$0B)
		CALL	l37dd
		PUSH	AF
		LD	HL,src_add
		CALL	l3829
		POP	AF
l361a:		INC	A
		CP	$08
		JR	C,l35fc		; (-$23)
		JR	l3655		; (+$34)
l3621:		LD	HL,$500C
		LD	A,(HL)
		CP	$FF
		JR	NZ,l362c	; (+$03)
		LD	A,$01
		LD	(HL),A
l362c:		RRA
		OUT	($FE),A
		DEC	HL
		LD	A,(HL)
		res	7,A
		JR	NC,l3637	; (+$02)
		SET	7,A
l3637:		LD	(HL),A
		INC	HL
		LD	A,(HL)
		AND	$20
		LD	IYL,A
		LD	HL,l37e5
		EXX
		LD	HL,($5006)
		LD	A,H
		OR	L
		JP	Z,l3691
		LD	L,$00
l364c:		CALL	l3772
		LD	SP,$4047
		CALL	l3829
l3655:		CALL	l27ce
		IM	0
		LD	A,($4010)
		AND	$03
		JR	Z,l3668		; (+$07)
		IM	1
		DEC	A
		JR	Z,l3668		; (+$02)
		IM	2
l3668:		LD	SP,$4000
		POP	HL
		POP	DE
		POP	BC
		EXX
		POP	AF
		EX	AF,AF'
		POP	HL
		POP	DE
		POP	IY
		POP	IX
		POP	BC
		LD	A,B
		SUB	$09
		RLA
		RL	B
		RRA
		LD	R,A
		POP	AF
		LD	I,A
		LD	SP,($4016)
		LD	BC,PBANK678
		LD	A,($4015)
		JP	$4018
l3691:		CALL	l3875
		LD	L,A
		CALL	l3875
		AND	A
		JP	NZ,l3735
		CALL	l3875
		LD	($5006),A
		CALL	l3875
		LD	($5007),A
		CALL	l3875
		LD	H,A
		CALL	l3875
		LD	($5069),A
		CALL	l3875
		CALL	l3875
		CALL	l3875
		PUSH	AF
		EXX
		LD	DE,$0010
l36c0:		EXX
		CALL	l3875
		EXX
		LD	BC,$FFFD
		OUT	(C),D
		LD	B,$BF
		OUT	(C),A
		INC	D
		DEC	E
		JR	NZ,l36c0	; (-$12)
		LD	B,$FF
		POP	AF
		OUT	(C),A
		EXX
		LD	A,L
		SUB	$17
		JR	NZ,l36e5	; (+$08)
		LD	A,H
		CP	$03
		JR	C,l36f3		; (+$11)
		INC	H
		JR	l36f3		; (+$0E)
l36e5:		CP	$20
l36e7:		PUSH	AF
		CALL	l3875
		LD	L,A
		POP	AF
		DEC	A
		JR	NZ,l36e7	; (-$09)
		LD	A,L
		JR	NC,l36f5	; (+$02)
l36f3:		LD	A,$04
l36f5:		LD	($506A),A
		LD	A,H
		CP	$04
		JP	NC,l3736
		XOR	A
		CALL	l37dd
		LD	H,$03
		AND	A
l3705:		PUSH	HL
		CALL	l386a
		LD	HL,$8000
		LD	IYH,$C0
		CP	$04
		JR	Z,l372b		; (+$18)
		LD	H,$C0
		LD	IYH,$00
		CP	$05
		JR	Z,l372b		; (+$0F)
		CP	$08
		JR	NZ,l3735	; (+$15)
		CALL	l377d
		POP	AF
		LD	SP,$4047
		PUSH	AF
		LD	IYH,$80
l372b:		CALL	l3829
		POP	HL
		DEC	H
		JR	NZ,l3705	; (-$2D)
		JP	l3655
l3735:		RST	00H
l3736:		LD	H,$08
		AND	A
l3739:		PUSH	HL
		CALL	l386a
		SUB	$03
		JR	C,l3735		; (-$0C)
		CP	$08
		JR	NC,l3735	; (-$10)
		LD	HL,src_add
		CALL	l37dd
		CP	$05
		JR	NZ,l3765	; (+$16)
		CALL	l377d
		POP	AF
		LD	SP,$4047
		PUSH	AF
		LD	A,($5069)
		LD	($401D),A
		LD	A,($506A)
		LD	($4015),A
		SET	7,H
l3765:		LD	IYH,$00
		CALL	l3829
		POP	HL
		DEC	H
		JR	NZ,l3739	; (-$36)
		JP	l3655
l3772:		XOR	A
		CALL	l37dd
		CALL	l377f
		LD	IYH,$00
		RET

l377d:		LD	L,$00
l377f:		LD	H,$40
		LD	IYH,$48
		CALL	l3829
		EXX
		PUSH	HL
		LD	HL,($503F)
		DEC	IX
		PUSH	IX
		POP	DE
		AND	A
		SBC	HL,DE
		LD	B,H
		LD	C,L
		LD	HL,$4047
		PUSH	HL
		POP	IX
		INC	IX
		EX	DE,HL
		LDIR
		POP	HL
		LD	D,$00
		CALL	l37bf
		LD	D,$50
		CALL	l37bf
		LD	HL,$401D
		LD	(HL),$10
		LD	L,$15
		LD	(HL),$04
		EXX
		LD	A,($4014)
		AND	A
		RET	NZ

		LD	($4025),A
		RET

l37bf:		LD	B,$40
l37c1:		LD	E,(HL)
		INC	HL
		BIT	7,E
		RET	NZ

		LD	A,(HL)
		AND	$3F
		LD	C,A
		LD	A,(HL)
		RLCA
		RLCA
		AND	$03
		INC	HL
		PUSH	HL
		LD	L,A
		INC	L
l37d3:		LD	A,(DE)
		INC	E
		LD	(BC),A
		INC	C
		DEC	L
		JR	NZ,l37d3	; (-$07)
		POP	HL
		JR	l37c1		; (-$1C)

l37dd:		PUSH	BC
		LD	BC,PBANKM
		OUT	(C),A
		POP	BC
		RET

l37e5:		DB	$2F,$58,$32,$9A,$2F,$9E,$34,$A3
		DB	$35,$26,$80,$0A,$13,$0B,$11,$17
		DB	$CC,$04,$48,$0D,$4A,$02,$61,$08
		DB	$56,$01,$12,$00,$24,$16,$06,$15
		DB	$07,$13,$40,$11,$42,$0F,$44,$06
		DB	$67,$1D,$10,$1B,$14,$80

l3813:		LD	A,D
		OR	E
		CALL	Z,n3871
		DEC	DE
		IN	A,(C)
		LD	(HL),A
		INC	L
		JP	NZ,l3813
		INC	H
		LD	A,H
		AND	$F8
		CP	IYH
		JR	NZ,l3813	; (-$15)
		RET

l3829:		LD	A,iyl
		AND	A
l382c:		JR	Z,l3813		; (-$1B)

l382e:		LD	A,D
		OR	E
		CALL	Z,n3871
		DEC	DE
		IN	A,(C)
		CP	$ED
		JR	Z,l3848		; (+$0E)
l383a:		LD	(HL),A
		INC	L
		JP	NZ,l382e
		INC	H
l3840:		LD	A,H
		AND	$F8
		CP	IYH
		JR	NZ,l382e	; (-$19)
		RET

l3848:		CALL	l3875
		CP	$ED
		JR	NZ,l3860	; (+$11)
		CALL	l3875
		PUSH	AF
		CALL	l3875
		POP	BC
l3857:		LD	(HL),A
		INC	HL
		DJNZ	l3857		; (-$04)
		LD	BC,$CEEF
		JR	l3840		; (-$20)
l3860:		LD	(HL),$ED
		INC	L
		JR	NZ,l383a	; (-$2B)
		INC	H
		JR	NZ,l383a	; (-$2E)
		SCF
		RET

l386a:		CALL	NC,l3875
		LD	L,A
		CALL	l3875
		AND	L
		INC	A
		LD	IYL,A
l3875:		LD	A,D
		OR	E
		CALL	Z,n3871
		DEC	DE
		IN	A,(C)
		RET

n3871:		LD	A,(IX+$00)
		INC	A
		CALL	NZ,l276d
		RET

l3886:		PUSH	DE
		PUSH	AF
		LD	($F514),HL
		LD	HL,$0020
		ADD	HL,BC
		LD	A,(HL)
		AND	$03
		DEC	A
		LD	A,$1D
		JR	NZ,l38b4	; (+$1D)
		INC	HL
		LD	A,(HL)
		CALL	l17c5
		JR	NC,l38b4	; (+$16)
		LD	A,(IX+$06)
		AND	A
		JR	Z,l38b1		; (+$0D)
		LD	L,(IX+$2C)
		LD	H,(IX+$2D)
		LD	DE,$27D8
		SBC	HL,DE
		JR	Z,l38b7		; (+$06)
l38b1:		LD	A,$1D
		AND	A
l38b4:		POP	DE
		POP	DE
		RET

l38b7:		CALL	l1063
		PUSH	IX
		LD	IX,($F514)
		LD	(IX+$00),L
		LD	(IX+$01),$00
		LD	L,H
		LD	H,E
		LD	E,(IX+$00)
		LD	D,(IX+$01)
		INC	IX
		INC	IX
		LD	($F514),IX
		POP	IX
		POP	AF
		EX	(SP),HL
		PUSH	HL
		LD	HL,$0100
		AND	A
		SBC	HL,DE
		EX	DE,HL
		POP	HL
		AND	A
		SBC	HL,DE
		SBC	A,$00
		LD	E,A
		LD	A,L
		AND	A
		LD	L,H
		LD	H,E
		LD	DE,$0001
		JR	Z,l38f4		; (+$01)
		INC	DE
l38f4:		ADD	HL,DE
		LD	($F511),HL
		POP	DE
		LD	A,(IX+$03)
		SRL	A
		AND	E
		LD	($F513),A
		LD	A,(IX+$02)
		DEC	A
		CALL	NZ,l04eb
l3909:		PUSH	DE
		LD	A,(IX+$02)
		CALL	l04f5
		CALL	l0d05
		EX	DE,HL
		LD	HL,$000C
		ADD	HL,BC
		CPL
		AND	(HL)
		OR	D
		LD	(HL),A
		INC	HL
		INC	HL
		LD	(HL),E
		CALL	l0d3a
		POP	DE
		RET	NC

		LD	A,E
		AND	$07
		ADD	A,A
		ADD	A,$10
		LD	L,A
		LD	H,$00
		ADD	HL,BC
l392e:		PUSH	DE
		LD	E,(HL)
		INC	HL
		LD	D,(HL)
		INC	HL
		PUSH	HL
		PUSH	BC
		LD	A,(IX+$02)
		DEC	A
		PUSH	AF
		CALL	NZ,l04f5
		LD	C,$00
		LD	A,($F513)
		ADD	A,E
		LD	E,A
		LD	A,D
		ADC	A,C
		LD	D,A
		PUSH	IX
		LD	L,(IX+$17)
		LD	H,(IX+$18)
		PUSH	HL
		POP	IX
		CALL	l26a8
		POP	IX
		JR	NC,l39b0	; (+$57)
		LD	HL,($F514)
		LD	BC,$FEEF
		CALL	l39b8
		LD	BC,$EFEF
		CALL	l39b8
		LD	BC,$EEEF
		CALL	l39b8
		LD	BC,$DFEF
		CALL	l39b8
		POP	BC
		LD	A,($F513)
		LD	C,A
		LD	A,$01
l397b:		ADD	A,A
		DJNZ	l397b		; (-$03)
		SUB	C
		LD	B,$00
		LD	C,A
		PUSH	HL
		LD	HL,($F511)
		LD	A,L
		AND	A
		SBC	HL,BC
		JR	NC,l398e	; (+$02)
		LD	C,A
		XOR	A
l398e:		LD	($F511),HL
		POP	HL
		LD	(HL),C
		INC	HL
		LD	($F514),HL
		POP	BC
		SCF
		JR	Z,l39b2		; (+$17)
		POP	HL
		POP	DE
		XOR	A
		LD	($F513),A
		INC	DE
		PUSH	HL
		AND	A
		SBC	HL,BC
		LD	A,L
		POP	HL
		CP	$20
		JP	C,l392e
		JP	l3909
l39b0:		POP	BC
		POP	BC
l39b2:		LD	(HL),$FF
		INC	HL
		POP	BC
		POP	BC
		RET

l39b8:		IN	A,(C)
		LD	(HL),A
		INC	HL
		RET

l39bd:		LD	A,$3A
		AND	A
		RET

l39c1:		ADD	A,$03
		CP	$13
		JR	NC,l39d6	; (+$0F)
		RLCA
		LD	HL,STRMS
		LD	C,A
		LD	B,$00
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		DEC	HL
		SCF
		RET

l39d5:		POP	BC
l39d6:		LD	A,$17
		AND	A
		RET

l39da:		INC	HL
		INC	HL
l39dc:		LD	A,(HL)
		INC	HL
		AND	A
		RET	Z

		CP	C
		JR	NZ,l39da	; (-$09)
		SCF
		RET

l39e5:		PUSH	HL
		CALL	l39f6
		JR	NC,l39f4	; (+$09)
		EX	DE,HL
		EX	(SP),HL
		AND	A
		SBC	HL,DE
		CCF
		POP	HL
		EX	DE,HL
		RET

l39f4:		POP	HL
		RET

l39f6:		LD	A,B
		OR	C
		RET	Z

		LD	A,(DE)
		CP	$2C
		SCF
		CCF
		RET	NZ

		INC	DE
		DEC	BC
l3a01:		LD	HL,$0000
l3a04:		LD	A,B
		OR	C
		SCF
		RET	Z

		LD	A,(DE)
		CP	$20
		JR	Z,l3a20		; (+$13)
		SUB	$30
		RET	C

		CP	$0A
		CCF
		RET	C

		PUSH	DE
		ADD	HL,HL
		LD	D,H
		LD	E,L
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,DE
		LD	D,$00
		LD	E,A
		ADD	HL,DE
		POP	DE
l3a20:		INC	DE
		DEC	BC
		JR	l3a04		; (-$20)
l3a24:		PUSH	BC
		CALL	l39c1
		JR	NC,l39d5	; (-$55)
		LD	A,B
		OR	C
		JR	Z,l3a44		; (+$16)
		PUSH	HL
		LD	HL,(CHANS)
		ADD	HL,BC
		INC	HL
		INC	HL
		INC	HL
		LD	A,(HL)
		POP	HL
		CP	$4B
		JR	Z,l3a44		; (+$08)
		CP	$53
		JR	Z,l3a44		; (+$04)
		CP	$50
		JR	NZ,l39d5	; (-$6F)
l3a44:		POP	BC
		PUSH	HL
		LD	HL,l3a84
		LD	A,B
		OR	C
		JR	Z,l3a7f		; (+$32)
		DEC	BC
		LD	A,B
		OR	C
		INC	BC
		LD	A,(DE)
		JR	Z,l3a65		; (+$11)
		LD	HL,l3a9c
		INC	DE
		LD	A,(DE)
		DEC	DE
		CP	$3E
		LD	A,$49
		JR	NZ,l3a65	; (+$05)
		LD	A,(DE)
		INC	DE
		INC	DE
		DEC	BC
		DEC	BC
l3a65:		AND	$DF
		PUSH	BC
		LD	C,A
		CALL	l39dc
		POP	BC
		JR	NC,l3a7f	; (+$10)
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		CALL	l3a7e
		POP	HL
		JR	NC,l3a80	; (+$07)
		LD	(HL),E
		INC	HL
		LD	(HL),D
		SCF
		RET

l3a7e:		JP	(HL)

l3a7f:		POP	BC

l3a80:		LD	A,$0E
		AND	A
		RET

l3a84:		DB	$4B,$8E,$3A,$53,$92,$3A,$50,$96
		DB	$3A,$00,$1E,$01,$18,$06,$1E,$06
		DB	$18,$02,$1E,$10,$16,$00,$37,$C9

l3a9c:		DB	$49,$BD,$3A,$4F,$B6,$3A,$55,$AF
		DB	$3A,$4D,$41,$3B,$56,$7C,$3B,$57
		DB	$BC,$3B,$00

l3aaf:		LD	HL,$0202
		LD	A,$03
		JR	l3ac2		; (+$0C)
		LD	HL,$0204
		LD	A,$02
		JR	l3ac2		; (+$05)
		LD	HL,$0002
		LD	A,$01
l3ac2:		PUSH	AF
		PUSH	HL
		LD	HL,src_file
		LD	A,B
		AND	A
		JR	NZ,l3ad1	; (+$06)
		LD	A,C
		LD	B,C
		CP	$12
		JR	C,l3ad3		; (+$02)
l3ad1:		LD	B,$11
l3ad3:		EX	DE,HL
l3ad4:		LD	A,(HL)
		INC	HL
		CALL	l3d8e
		LD	(DE),A
		INC	DE
		CALL	l3d6a
		DJNZ	l3ad4		; (-$0C)
		CALL	l3d8e
		LD	A,$FF
		LD	(DE),A
		CALL	l3d6a
		LD	B,$02
l3aeb:		PUSH	BC
		CALL	l3d8e
		CALL	l3da8
		CALL	l053c
		CALL	l3de4
		CALL	l3d6a
		POP	BC
		JR	C,l3b07		; (+$09)
		INC	B
		LD	A,B
		CP	$10
		JR	C,l3aeb		; (-$19)
		POP	HL
		POP	HL
		RET

l3b07:		LD	HL,src_file
		POP	DE
		POP	AF
		LD	C,A
		PUSH	BC
		CALL	l3d8e
		CALL	l3da8
		CALL	l0106
		CALL	l3de4
		CALL	l3d6a
		POP	BC
		RET	NC

		PUSH	BC
		LD	HL,l3b34
		LD	BC,$000E
		LD	DE,$000D
		CALL	l3c99
		LD	BC,$000D
		ADD	HL,BC
		POP	BC
		LD	(HL),B
		SCF
		RET

l3b34:		DB	$00,$5B,$00,$5B,$46,$E0,$3E,$73
		DB	$3E,$4E,$3E,$0E,$00

l3b41:		CALL	l3a01
		PUSH	HL
		CALL	l39f6
		POP	DE
		RET	NC

		LD	A,B
		OR	C
		RET	NZ

		LD	A,H
		OR	L
		RET	Z

		PUSH	DE
		PUSH	HL
		LD	HL,l3b6f
		LD	BC,$0013
		LD	DE,$000D
		CALL	l3c99
		LD	BC,$000D
		ADD	HL,BC
		POP	BC
		LD	(HL),C
		INC	HL
		LD	(HL),B
		INC	HL
		INC	HL
		INC	HL
		POP	BC
		LD	(HL),C
		INC	HL
		LD	(HL),B
		SCF
		RET

l3b6f:		DB	$00,$5B,$00,$5B,$4D,$1C,$03,$21
		DB	$03,$26,$03,$13,$00

l3b7c:		LD	A,B
		AND	A
		RET	NZ

		LD	A,C
		AND	A
		RET	Z

		LD	A,(DE)
		INC	DE
		DEC	C
		RET	Z

		AND	$DF
		CP	$41
		CCF
		RET	NC

		CP	$5B
		RET	NC

		LD	L,A
		LD	A,(DE)
		CP	$24
		SCF
		CCF
		RET	NZ

		DEC	C
		RET	NZ

		SET	7,L
		PUSH	HL
		LD	HL,l3baf
		LD	BC,$0010
		LD	DE,$000D
		CALL	l3c99
		LD	BC,$000D
		ADD	HL,BC
		POP	BC
		LD	(HL),C
		SCF
		RET

l3baf:		DB	$00,$5B,$00,$5B,$56,$EF,$39,$D7
		DB	$39,$FB,$39,$10,$00

l3bbc:		CALL	l3a01
		LD	A,H
		AND	A
		RET	NZ

		LD	A,L
		CP	$18
		RET	NC

		PUSH	HL
		LD	HL,$001F
		CALL	l39e5
		JP	NC,l3c7e
		LD	A,L
		POP	HL
		LD	H,A
		PUSH	HL
		LD	A,$18
		SUB	L
		LD	L,A
		LD	H,$00
		CALL	l39e5
		JP	NC,l3c7e
		EX	(SP),HL
		PUSH	HL
		LD	A,$20
		SUB	H
		LD	L,A
		LD	H,$00
		CALL	l39e5
		LD	A,L
		POP	HL
		EX	(SP),HL
		LD	H,A
		PUSH	HL
		CALL	l39f6
		LD	A,$08
		JR	NC,l3c0d	; (+$16)
		LD	A,H
		AND	A
		JP	NZ,l3c7d
		LD	A,L
		CP	$03
		CCF
		JR	NC,l3c7d	; (+$7B)
		CP	$09
		JR	NC,l3c7d	; (+$77)
		PUSH	AF
		CALL	l39f6
		JR	C,l3c20		; (+$14)
		POP	AF
l3c0d:		PUSH	AF
		LD	HL,l3c80
		SUB	$03
		JR	Z,l3c1a		; (+$05)
l3c15:		INC	HL
		INC	HL
		DEC	A
		JR	NZ,l3c15	; (-$05)
l3c1a:		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		JR	l3c26		; (+$06)
l3c20:		LD	A,H
		AND	$C0
		JR	Z,l3c7c		; (+$57)
		DEC	H
l3c26:		LD	A,B
		OR	C
		JR	NZ,l3c7c	; (+$52)
		PUSH	HL
		LD	HL,l3c8c
		LD	BC,$004D
		LD	DE,$000D
		CALL	l3c99
		LD	BC,$0015
		ADD	HL,BC
		POP	BC
		LD	(HL),B
		DEC	HL
		LD	(HL),C
		DEC	HL
		POP	AF
		LD	(HL),A
		DEC	HL
		POP	BC
		EX	DE,HL
		EX	(SP),HL
		EX	DE,HL
		PUSH	AF
		LD	A,E
		ADD	A,A
		ADD	A,A
		ADD	A,A
		LD	(HL),A
		DEC	HL
		LD	(HL),D
		DEC	HL
		LD	A,C
		ADD	A,E
		DEC	A
		LD	(HL),A
		DEC	HL
		LD	A,B
		ADD	A,D
		DEC	A
		LD	(HL),A
		DEC	HL
		LD	(HL),E
		DEC	HL
		LD	(HL),D
		LD	E,B
		LD	D,$00
		EX	DE,HL
		ADD	HL,HL
		ADD	HL,HL
		ADD	HL,HL
		POP	AF
		LD	C,A
		LD	B,$00
		XOR	A
l3c69:		SBC	HL,BC
		INC	A
		JR	NC,l3c69	; (-$05)
		DEC	A
		LD	HL,$000B
		ADD	HL,DE
		LD	(HL),A
		INC	HL
		LD	A,(ATTR_P)
		LD	(HL),A
		POP	DE
		SCF
		RET

l3c7c:		POP	HL
l3c7d:		POP	HL
l3c7e:		POP	HL
		RET

l3c80:		DB	$62,$2B,$62,$2B,$7B,$2B,$7B,$2B
		DB	$3E,$2B,$1F,$2B

l3c8c:		DB	$00,$5B,$00,$5B,$57,$D0,$27,$9C
		DB	$39,$9C,$39,$4D,$00

l3c99:		PUSH	HL
		PUSH	DE
		PUSH	BC
		LD	HL,(PROG)
		DEC	HL
		PUSH	HL
		EXX
		CALL	n3e00
		XOR	A
		CCF
		POP	DE
		POP	HL
		POP	BC
		AND	A
		SBC	HL,BC
		EX	(SP),HL
		PUSH	DE
		LDIR
		POP	HL
		POP	BC
l3cb3:		LD	A,B
		OR	C
		JR	Z,l3cbd		; (+$06)
		XOR	A
		LD	(DE),A
		INC	DE
		DEC	BC
		JR	l3cb3		; (-$0A)
l3cbd:		PUSH	HL
		LD	DE,(CHANS)
		AND	A
		SBC	HL,DE
		INC	HL
		EX	DE,HL
		POP	HL
		RET

l3cc9:		CALL	l39c1
		RET	NC

		LD	A,B
		OR	C
		SCF
		RET	Z

		PUSH	HL
		LD	HL,(CHANS)
		ADD	HL,BC
		INC	HL
		INC	HL
		INC	HL
		LD	C,(HL)
		EX	DE,HL
		LD	HL,l3d06
		CALL	l39dc
		JP	NC,l39d6
		LD	A,(HL)
		INC	HL
		LD	H,(HL)
		LD	L,A
		CALL	l3a7e
		POP	HL
		LD	A,$12
		RET	NC

		LD	BC,$0000
		LD	DE,$A3E2
		EX	DE,HL
		ADD	HL,DE
		JR	C,l3d00		; (+$07)
		LD	BC,l3d2a
		ADD	HL,BC
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
l3d00:		EX	DE,HL
		LD	(HL),C
		INC	HL
		LD	(HL),B
		SCF
		RET

l3d06:		DB	$4B,$04,$3D,$53,$04,$3D,$50,$04
		DB	$3D,$46,$2A,$3D,$4D,$41,$3D,$56
		DB	$41,$3D,$57,$41,$3D,$00,$01,$00
		DB	$06,$00,$0B,$00,$01,$00,$01,$00
		DB	$06,$00,$10,$00

l3d2a:		LD	HL,$0009
		ADD	HL,DE
		LD	B,(HL)
		PUSH	DE
		CALL	l3d8e
		CALL	l3da8
		CALL	l0109
		CALL	l3de4
		CALL	l3d6a
		POP	DE
		RET	NC

		LD	HL,$0007
		ADD	HL,DE
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		DEC	DE
		DEC	DE
		DEC	DE
		DEC	DE
		EX	DE,HL
		EXX
		CALL	n3e00
		LD	(HL),A
		CCF
		SCF
		RET

l3d55:		EXX
		LD	DE,$0000
l3d59:		CALL	n3e00
		DEC	L
		ADD	HL,SP
		RET

l3d5f:		LD	DE,$0002
		JR	l3d59		; (-$0B)
l3d64:		EXX
		LD	DE,$0004
		JR	l3d59		; (-$11)
l3d6a:		EX	AF,AF'
		XOR	A
		DI
		CALL	l3d83
		POP	AF
		LD	(TARGET),HL
		LD	HL,(OLDSP)
		LD	(OLDSP),SP
		LD	SP,HL
		EI
		LD	HL,(TARGET)
		PUSH	AF
		EX	AF,AF'
		RET

l3d83:		PUSH	BC
		LD	BC,PBANKM
		OUT	(C),A
		LD	(BANKM),A
		POP	BC
		RET

l3d8e:		EX	AF,AF'
		DI
		POP	AF
		LD	(TARGET),HL
		LD	HL,(OLDSP)
		LD	(OLDSP),SP
		LD	SP,HL
		LD	HL,(TARGET)
		PUSH	AF
		LD	A,$07
		CALL	l3d83
		EI
		EX	AF,AF'
		RET

l3da8:		CALL	l3dd2
		LD	HL,TSTACK
		LD	DE,tmp_stack
		LD	BC,$0084
		LDDR
		POP	BC
		LD	(tmp_sp),SP
		LD	HL,TSTACK
		LD	SP,HL
		PUSH	BC
l3dc0:		LD	BC,(tmp_bc)
		LD	DE,(tmp_de)
		LD	HL,(tmp_af)
		PUSH	HL
		POP	AF
		LD	HL,(tmp_hl)
		EI
		RET

l3dd2:		DI
		LD	(tmp_hl),HL
		PUSH	AF
		POP	HL
		LD	(tmp_af),HL
		LD	(tmp_de),DE
		LD	(tmp_bc),BC
		RET

l3de4:		CALL	l3dd2
		POP	HL
		LD	(tmp_ret),HL
		LD	HL,tmp_stack
		LD	DE,TSTACK
		LD	BC,$0084
		LDDR
		LD	HL,(tmp_sp)
		LD	SP,HL
		LD	HL,(tmp_ret)
		PUSH	HL
		JR	l3dc0		; (-$40)

n3e00:		LD	(OLDHL),HL
		PUSH	AF
		POP	HL
		LD	(OLDAF),HL
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

x3e41:		LD	L,$2F
		LD	DE,$FFFF
		LD	BC,$FEFE
l3e49:		IN	A,(C)
		CPL
		AND	$1F
		JR	Z,l3e5e		; (+$0E)
		LD	H,A
		LD	A,L
l3e52:		INC	D
		RET	NZ

l3e54:		SUB	$08
		SRL	H
		JR	NC,l3e54	; (-$06)
		LD	D,E
		LD	E,A
		JR	NZ,l3e52	; (-$0C)
l3e5e:		DEC	L
		RLC	B
		JR	C,l3e49		; (-$1A)
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

l3e72:		CALL	x3e41
		RET	NZ

		LD	HL,KSTATE
l3e79:		BIT	7,(HL)
		JR	NZ,l3e84	; (+$07)
		INC	HL
		DEC	(HL)
		DEC	HL
		JR	NZ,l3e84	; (+$02)
		LD	(HL),$FF
l3e84:		LD	A,L
		LD	HL,$5C04
		CP	L
		JR	NZ,l3e79	; (-$12)
		CALL	l3ed1
		RET	NC

		LD	HL,KSTATE
		CP	(HL)
		JR	Z,l3ec3		; (+$2E)
		EX	DE,HL
		LD	HL,$5C04
		CP	(HL)
		JR	Z,l3ec3		; (+$27)
		BIT	7,(HL)
		JR	NZ,l3ea4	; (+$04)
		EX	DE,HL
		BIT	7,(HL)
		RET	Z

l3ea4:		LD	E,A
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
		CALL	x3f77
		POP	HL
		LD	(HL),A
l3ebb:		LD	(LAST_K),A
		SET	5,(IY+$01)
		RET

l3ec3:		INC	HL
		LD	(HL),$05
		INC	HL
		DEC	(HL)
		RET	NZ

		LD	A,(REPPER)
		LD	(HL),A
		INC	HL
		LD	A,(HL)
		JR	l3ebb		; (-$16)
l3ed1:		LD	B,D
		LD	D,$00
		LD	A,E
		CP	$27
		RET	NC

		CP	$18
		JR	NZ,l3edf	; (+$03)
		BIT	7,B
		RET	NZ

l3edf:		LD	HL,l2239
		ADD	HL,DE
		LD	A,(HL)
		SCF
		RET

l3ee6:		PUSH	AF
		XOR	A
		LD	(timeout),A
		LD	A,(BANK678)
		AND	$F7
		CALL	l21ba
		POP	AF
		RET

l3ef5:		CALL	l2134
		CALL	l21f7
		JP	l20ef
		NOP
		NOP

; ==============================================================

n3f00:		LD	(OLDHL),HL
		LD	(OLDBC),BC
		PUSH	AF
		POP	HL
		LD	(OLDAF),HL
		EX	(SP),HL
		LD	C,(HL)
		INC	HL
		LD	B,(HL)
		INC	HL
		EX	(SP),HL
		LD	HL,x3f42
		PUSH	HL
		PUSH	BC
		POP	HL
		LD	A,(BANKM)
		AND	$EF
		DI
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A
		LD	A,(BANK678)
		OR	$04
		LD	(BANK678),A
		LD	BC,PBANK678
		OUT	(C),A
		EI
		PUSH	HL
		LD	HL,(OLDAF)
		PUSH	HL
		POP	AF
		LD	BC,(OLDBC)
		LD	HL,(OLDHL)
		RET

x3f42:		PUSH	BC
		PUSH	AF
		LD	A,(BANK678)
		AND	$FB
		DI
		LD	(BANK678),A
		LD	BC,PBANK678
		OUT	(C),A
		LD	A,(BANKM)
		OR	$10
		LD	(BANKM),A
		LD	BC,PBANKM
		OUT	(C),A
		EI
		POP	AF
		POP	BC
		RET

n3f63:		PUSH	BC
		PUSH	AF
		LD	A,(BANK678)
		AND	$FB
		DI
		LD	(BANK678),A
		LD	BC,PBANK678
		OUT	(C),A
		EI
		POP	AF
		POP	BC
		RET

x3f77:		LD	A,E
		CP	$3A
		JR	C,l3fab		; (+$2F)
		DEC	C
		JP	m,l3f93
		JR	Z,l3f85		; (+$03)
		ADD	A,$4F
		RET

l3f85:		LD	HL,$221F
		INC	B
		JR	Z,l3f8e		; (+$03)
		LD	HL,$2239
l3f8e:		LD	D,$00
		ADD	HL,DE
		LD	A,(HL)
		RET

l3f93:		LD	HL,$225D
		BIT	0,B
		JR	Z,l3f8e		; (-$0C)
		BIT	3,D
		JR	Z,l3fa8		; (+$0A)
		BIT	3,(IY+$30)
		RET	NZ

		INC	B
		RET	NZ

		ADD	A,$20
		RET

l3fa8:		ADD	A,$A5
		RET

l3fab:		CP	$30
		RET	C

		DEC	C
		JP	m,l3fe1
		JR	NZ,l3fcd	; (+$19)
		LD	HL,$2288
		BIT	5,B
		JR	Z,l3f8e		; (-$2D)
		CP	$38
		JR	NC,l3fc6	; (+$07)
		SUB	$20
		INC	B
		RET	Z

		ADD	A,$08
		RET

l3fc6:		SUB	$36
		INC	B
		RET	Z

		ADD	A,$FE
		RET

l3fcd:		LD	HL,$2264
		CP	$39
		JR	Z,l3f8e		; (-$46)
		CP	$30
		JR	Z,l3f8e		; (-$4A)
		AND	$07
		ADD	A,$80
		INC	B
		RET	Z

		XOR	$0F
		RET

l3fe1:		INC	B
		RET	Z

		BIT	5,B
		LD	HL,$2264
		JR	NZ,l3f8e	; (-$5C)
		SUB	$10
		CP	$22
		JR	Z,l3ff6		; (+$06)
		CP	$20
		RET	NZ

		LD	A,$5F
		RET

l3ff6:		LD	A,$40
		RET

		DS	7,-1		; 7 bytes libres en 255