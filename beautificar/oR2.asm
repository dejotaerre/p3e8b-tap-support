	output	"p3t_rom2.rom"

        org $0000

; *************************************************
; *** SPECTRUM +3 ROM 2 DISASSEMBLY (+3DOS ROM) ***
; *************************************************

; ROM2 del PROYECTO +3e de la VERSION IDE 8bits
; Desensamblado por DJr, con la valiosa ayuda de DZ80 V2.0

; este desensamble no sabe nada de las constantes spanish, garry, y V41
; aca todo es IDE8 v4.0 e ingles . a.k.a no me la compliques

x0000:  jr      x0000                   ; (-$02)
        jp      l22c2
        jp      l196a

n0008:	.db    "PLUS3DOS"

n0010:	.db    $2f, $58, $32, $9a, $2f, $9e, $34, $e3
        .db    $80, $01, $c0, $05, $c4, $09, $c8, $0d
        .db    $61, $0f, $cc, $14, $51, $00, $13, $16
        .db    $24, $17, $56, $19, $10, $13, $14, $ed
        .db    $79, $01, $06, $7f, $3e, $c3, $fb, $c9
	
; The maskable interrupt routine
	
        push    af
        push    hl
        ld      hl,(FRAMES)
        inc     hl              ; increment FRAMES
        ld      (FRAMES),hl
        ld      a,h
        or      l
        jr      nz,l0048
        inc     (iy+$40)        ; increment high byte of FRAMES
l0048:  push    bc
        push    de
        call    l3e72           ; scan the keyboard
        call    l0068           ; test for disk motor timeout
        pop     de
        pop     bc
        pop     hl
        pop     af
        ei      
        ret     

        jp      l3a24
        jp      l3cc9
        jp      l3d5f
        jp      l3d55
        jp      l3d64
        nop     

; The Non-maskable interrupt

x0066   retn                    ; do nothing

l0068:  ld      bc,$7ffd
        ld      a,(BANKM)
        or      $07
        out     (c),a
        ld      a,(timeout)
        or      a
        jr      z,l0095                 ; (+$1d)
        ld      a,(FRAMES)
        bit     0,a
        jr      nz,l0095                ; (+$16)
        ld      a,(timeout)
        dec     a
        ld      (timeout),a
        jr      nz,l0095                ; (+$0d)
        ld      bc,$1ffd
        ld      a,(BANK678)
        and     $f7
        ld      (BANK678),a
        out     (c),a
l0095:  ld      bc,$7ffd
        ld      a,(BANKM)
        out     (c),a
        ret     

        defw	$0000
     
     	jp      l2808
        jp      l249a
        jp      l2828
        jp      l280d
        jp      l2558
        jp      l25fb
        jp      l2a01
        jp      l2bc8
        jp      l2de2
        jp      l2d0b
        jp      l2f93
        jp      l2c0f
        jp      l2b69
        jp      l2b92
        jp      l2c3b
        jp      l2c94
        jp      l2cdb
        jp      l306c
        jp      l3083
        jp      l318b
        jp      l2cdb
        jp      l3131
        jp      l3106
        jp      l314e
        jp      l30af
        jp      l30bb
        jp      l31e1
        jp      l3227
        jp      l3370
        jp      l33cd
        jp      l34ac
        jp      l3531

l0100:	jp      l01a8		; DOS_INITIALISE
l0103:	jp      l01dc		; DOS_VERSION
l0106:	jp      l0635		; DOS_OPEN
l0109:	jp      l074a		; DOS_CLOSE
l010c:	jp      l076b		; DOS_ABANDON
x010f:	jp      l08bb		; DOS_REF_HEAD
l0112:	jp      l19c0		; DOS_READ
l0115:	jp      l117f		; DOS_WRITE
l0118:	jp      l1129		; DOS_BYTE_READ
l011b:	jp      l1219		; DOS_BYTE_WRITE
l011e:	jp      l0a23		; DOS_CATALOG
l0121:	jp      l08fc		; DOS_FREE_SPACE
l0124:	jp      l092e		; DOS_DELETE
l0127:	jp      l0979		; DOS_RENAME
l012a:	jp      l1b4d		; DOS_BOOT
l012d:	jp      l0919		; DOS_SET_DRIVE
x0130:	jp      l0906		; DOS_SET_USER
l0133:	jp      l105f		; DOS_GET_POSITION
x0136:	jp      l107b		; DOS_SET_POSITION
l0139:	jp      l1068		; DOS_GET_EOF
l013c:	jp      l01ec		; DOS_GET_1346
l013f:	jp      l01f2		; DOS_SET_1346
l0142:	jp      l05cf		; DOS_FLUSH
l0145:	jp      l08cd		; DOS_SET_ACCESS
l0148:	jp      l0963		; DOS_SET_ATTRIBUTES
l014b:	jp      l070e		; DOS_OPEN_DRIVE
l014e:	jp      l02f0		; DOS_SET_MESSAGE
l0151:	jp      l17c5		; DOS_REF_XDPB
l0154:	jp      l1935		; DOS_MAP_B
l0157:	jp      l1f86		; DD_INTERFACE
l015a:	jp      l1f91		; DD_INIT
l015d:	jp      x1fa6		; DD_SETUP
l0160:	jp      l1966		; DD_SET_RETRY
l0163:	jp      l1c75		; DD_READ_SECTOR
l0166:	jp      x1c83		; DD_WRITE_SECTOR
l0169:	jp      x1c8c		; DD_CHECK_SECTOR
x016c:	jp      l1c9a		; DD_FORMAT
l016f:	jp      l1cac		; DD_READ_ID
l0172:	jp      l1edb		; DD_TEST_UNSUITABLE
l0175:	jp      l1cf6		; DD_LOGIN
l0178:	jp      l1d51		; DD_SEL_FORMAT
l017b:	jp      l1f4f		; DD_ASK_1
l017e:	jp      l1f5b		; DD_DRIVE_STATUS
l0181:	jp      l1eeb		; DD_EQUIPMENT
l0184:	jp      l1c50		; DD_ENCODE
l0187:	jp      l1d64		; DD_L_XDPB
l018a:	jp      l1da6		; DD_L_DPB
l018d:	jp      l1fd5		; DD_L_SEEK
l0190:	jp      l3ef5		; DD_L_READ
l0193:	jp      l2122		; DD_L_WRITE
l0196:	jp      l2181		; DD_L_ON_MOTOR
l0199:	jp      l21a6		; DD_L_T_OFF_MOTOR
l019c:	jp      l3ee6		; DD_L_OFF_MOTOR

        jp      l39bd
        jp      l24bf
        jp      l29ca

l01a8:  ld      a,($df9d)
        push    af
        ld      hl,pg_buffer
        ld      de,$db01
        ld      bc,$09ff
        ld      (hl),$00
        ldir    
        call    l1f86
        jr      nc,l01c4                ; (+$06)
        call    l1f91
        call    l1737
l01c4:  ld      hl,$0878
        ld      de,$0008
        push    de
        call    x1798
        pop     de
        call    l14a5
        call    l0508
        pop     af
        ld      ($df9d),a
        jp      l2828
l01dc:  xor     a
        ld      b,$01
        sub     b
        ld      a,$00
        ld      b,a
        ld      c,a
        ld      de,$0101
        ld      hl,$0069
        scf     
        ret     

l01ec:  call    l1ab1
        jp      l149c
l01f2:  push    de
        ex      de,hl
        call    l1ab1
        or      a
        sbc     hl,de
        ex      de,hl
        scf     
        call    nz,l1abb
        pop     de
        ret     nc

        ex      de,hl
        call    l149c
        ex      de,hl
        or      a
        sbc     hl,de
        add     hl,de
        scf     
        call    nz,l14a1
        ret     

l020f:  inc     c
        dec     c
        jr      nz,l0216                ; (+$03)
        inc     b
        dec     b
        ret     z

l0216:  call    l021b
        ldir    
l021b:  push    hl
        push    bc
        ld      b,a
        ld      hl,BANKM
        ld      a,(hl)
        and     $07
        push    af
        ld      a,(hl)
        and     $f8
        or      b
        ld      bc,$7ffd
        ld      (hl),a
        out     (c),a
        pop     af
        pop     bc
        pop     hl
        ret     

l0233:  add     a,a
        ld      l,a
        or      $c0
        ld      h,a
        ld      a,l
        ld      l,$00
        rlca    
        rlca    
        rlca    
        and     $06
        cp      $04
        ret     nc

        inc     a
        ret     

l0245:  ld      a,c
        cp      b
        jr      nz,l024f                ; (+$06)
        push    ix
        pop     bc
        jp      l020f
l024f:  push    bc
        call    l02cc
        call    l020f
        pop     bc
        push    bc
        ld      a,b
        ex      de,hl
        call    l02cc
        ex      de,hl
        call    l020f
        push    ix
        pop     bc
        ld      a,b
        or      c
        pop     bc
        ret     z

        ld      a,r
        di      
        push    af
        or      a
        call    l027b
        call    l0290
        scf     
        call    l027b
        pop     af
        ret     po

        ei      
        ret     

l027b:  push    hl
        push    de
        push    bc
        ld      bc,$0020
        ld      de,pg_buffer
        ld      hl,$bfe0
        jr      nc,l028a                ; (+$01)
        ex      de,hl
l028a:  ldir    
        pop     bc
        pop     de
        pop     hl
        ret     

l0290:  push    ix
        ex      (sp),hl
        ld      a,h
        or      a
        jr      nz,l029c                ; (+$05)
        ld      a,l
        cp      $20
        jr      c,x029e                 ; (+$02)
l029c:  ld      a,$20
x029e:  push    bc
        ld      c,a
        ld      b,$00
        or      a
        sbc     hl,bc
        pop     bc
        ex      (sp),hl
        pop     ix
        or      a
        ret     z

        push    de
        push    bc
        push    af
        ld      b,a
        ld      a,c
        ld      c,b
        ld      b,$00
        ld      de,$bfe0
        call    l020f
        pop     af
        pop     bc
        pop     de
        push    hl
        push    bc
        ld      c,a
        ld      a,b
        ld      b,$00
        ld      hl,$bfe0
        call    l020f
        pop     bc
        pop     hl
        jr      l0290                   ; (-$3c)
l02cc:  push    hl
        ld      bc,x0000
        ld      hl,src_add
        or      a
        sbc     hl,de
        jr      c,l02ee                 ; (+$16)
        jr      z,l02ee                 ; (+$14)
        push    ix
        pop     bc
        or      a
        sbc     hl,bc
        add     hl,bc
        jr      nc,l02e5                ; (+$02)
        ld      b,h
        ld      c,l
l02e5:  push    ix
        pop     hl
        or      a
        sbc     hl,bc
        push    hl
        pop     ix
l02ee:  pop     hl
        ret     

l02f0:  or      a
        jr      nz,l02f6                ; (+$03)
        ld      hl,x0000
l02f6:  ld      de,(rt_alert)
        ld      (rt_alert),hl
        ex      de,hl
        ret     

l02ff:  ld      b,a
        ld      hl,(rt_alert)
        ld      a,h
        or      l
        ld      a,b
        jr      nz,x030a                ; (+$02)
        inc     l
        ret     

x030a:  push    bc
        ld      hl,x03cd
        ld      (al_resp),hl
        call    l032d
        pop     bc
        push    bc
        call    l031f
        pop     bc
        sub     $01
        ccf     
        ld      a,b
        ret     

l031f:  push    hl
        ld      hl,(rt_alert)
        ex      (sp),hl
        ret     

l0325:  ld      a,$0a
        ld      hl,x03cd
        ld      (al_resp),hl
l032d:  ld      ix,al_mess
        push    ix
        call    l033c
        ld      (ix+$00),$ff
        pop     hl
        ret     

l033c:  and     $7f
        ld      hl,(al_resp)
        ld      b,a
        inc     b
        jr      l034a                   ; (+$05)
l0345:  ld      a,(hl)
        inc     hl
        inc     a
        jr      nz,l0345                ; (-$05)
l034a:  djnz    l0345                   ; (-$07)
l034c:  ld      a,(hl)
        inc     hl
        cp      $ff
        ret     z

        push    hl
        call    l0358
        pop     hl
        jr      l034c                   ; (-$0c)
l0358:  or      a
        jp      p,l03c7
        cp      $fe
        jr      z,l03c6                 ; (+$66)
        cp      $fd
        jr      z,l0397                 ; (+$33)
        cp      $fc
        jr      z,l0394                 ; (+$2c)
        cp      $fb
        jr      z,l037d                 ; (+$11)
        cp      $fa
        jr      nz,l033c                ; (-$34)
        ld      (ix+$00),$10
        inc     ix
        ld      a,(BORDCR)
        and     $07
        jr      l03c7                   ; (+$4a)
l037d:  ld      (ix+$00),$10
        inc     ix
        ld      a,(BORDCR)
        and     $38
        cp      $20
        jr      nc,l0390                ; (+$04)
        ld      a,$05
        jr      l0392                   ; (+$02)
l0390:  ld      a,$02
l0392:  jr      l03c7                   ; (+$33)
l0394:  ld      a,e
        jr      l0398                   ; (+$01)
l0397:  ld      a,d
l0398:  push    de
        push    bc
        ld      l,a
        ld      h,$00
        ld      d,h
        ld      bc,$ff9c
        call    l03b1
        ld      bc,$fff6
        call    l03b1
        ld      a,l
        add     a,$30
        pop     bc
        pop     de
        jr      l03c7                   ; (+$16)
l03b1:  ld      a,$ff
l03b3:  push    hl
        inc     a
        add     hl,bc
        jr      nc,l03bc                ; (+$04)
        ex      (sp),hl
        pop     hl
        jr      l03b3                   ; (-$09)
l03bc:  pop     hl
        or      a
        jr      z,l03c2                 ; (+$02)
        ld      d,$30
l03c2:  add     a,d
        ret     z

        jr      l03c7                   ; (+$01)
l03c6:  ld      a,c
l03c7:  ld      (ix+$00),a
        inc     ix
        ret     

x03cd:	.db    $8b, "not ready", $8f, $ff
	.db    $8c, "write protected", $8f, $ff
	.db    $8d, "seek fail", $8f, $ff
	.db    $8e, "data error", $8f, $ff
	.db    $8e, "no data", $8f, $ff
	.db    $8e, "missing address mark", $8f, $ff
	.db    $8b, "bad format", $8f, $ff
	.db    $8e, "unknown error", $8f, $ff
	.db    $8c, "changed, please replace", $8f, $ff
	.db    $8c, "unsuitable", $8f, $ff
	.db    "Please put the disk for ", $fe, ": into the drive then press "
	.db    "any key", $ff
	.db    "Drive ", $fe, ": ", $ff
	.db    $8b, "disk ", $ff
	.db    $8b, "track ", $fd, ", ", $ff
	.db    $8d, "sector ", $fc, ", ", $ff
	.db    " - Retry, Ignore or Cancel? ", $ff

l04eb:  or      a
        ret     z

l04ed:  srl     d
        rr      e
        dec     a
        jr      nz,l04ed                ; (-$07)
        ret     

l04f5:  or      a
        ret     z

        ex      de,hl
l04f8:  add     hl,hl
        dec     a
        jr      nz,l04f8                ; (-$04)
        ex      de,hl
        ret     

l04fe:  jp      (hl)
l04ff:  cp      $61
        ret     c

        cp      $7b
        ret     nc

        add     a,$e0
        ret     

l0508:  ld      bc,$1041
l050b:  ld      a,c
        call    l17cb
        ld      a,c
        jr      c,l0517                 ; (+$05)
        inc     c
        djnz    l050b                   ; (-$0a)
        ld      a,$41
l0517:  ld      (def_drv),a
        ld      (LODDRV),a
        ld      (SAVDRV),a
        ret     

x0521:  call    l0532
        ret     nc

        rra     
        ld      a,$1d
        ret     

l0529:  call    l0532
        ret     nc

        rra     
        rra     
        ld      a,$1d
        ret     

l0532:  call    l0558
        ret     nc

        rlca    
        rra     
        ret     c

        ld      a,$1d
        ret     

l053c:  call    l0558
        ret     nc

        rla     
        ccf     
        ld      a,$1d
        ret     nc

        push    hl
        push    de
        push    bc
        ld      h,b
        ld      l,c
        ld      (hl),$00
        ld      d,b
        ld      e,c
        inc     de
        ld      bc,$0037
        ldir    
        pop     bc
        pop     de
        pop     hl
        ret     

l0558:  push    hl
        push    de
        ld      a,b
        cp      $10
        ld      a,$15
        jr      nc,l0573                ; (+$12)
        ld      hl,$db68
        ld      de,$0038
        inc     b
l0568:  add     hl,de
        djnz    l0568                   ; (-$03)
        ld      b,h
        ld      c,l
        ld      hl,$0020
        add     hl,bc
        ld      a,(hl)
        scf     
l0573:  pop     de
        pop     hl
        ret     

l0576:  ld      hl,$0020
        add     hl,bc
        ld      e,(hl)
        ld      (hl),$00
        push    hl
        push    de
        call    l0586
        pop     de
        pop     hl
        ld      (hl),e
        ret     

l0586:  ld      hl,fcbs
        ld      e,$12
l058b:  push    hl
        push    bc
        ld      bc,$0020
        add     hl,bc
        ld      d,(hl)
        inc     hl
        ld      a,(hl)
        pop     bc
        pop     hl
        bit     7,d
        jr      z,l05c4                 ; (+$2a)
        push    hl
        ld      hl,$0021
        add     hl,bc
        cp      (hl)
        pop     hl
        jr      nz,l05c4                ; (+$21)
        ld      a,(bc)
        cp      $22
        jr      z,l05b0                 ; (+$08)
        ld      a,(hl)
        cp      $22
        call    nz,l0d7a
        jr      nz,l05c4                ; (+$14)
l05b0:  push    hl
        ld      hl,$0020
        add     hl,bc
        ld      a,(hl)
        rrca    
        rrca    
        and     $03
        ld      h,a
        ld      a,d
        and     $03
        or      h
        xor     h
        ld      a,$1e
        pop     hl
        ret     nz

l05c4:  push    de
        ld      de,$0038
        add     hl,de
        pop     de
        dec     e
        jr      nz,l058b                ; (-$42)
        scf     
        ret     

l05cf:  call    l04ff
        call    l0c13
        ret     nc

l05d6:  push    bc
        ld      bc,fcbs
        ld      e,$12
l05dc:  ld      hl,$0020
        add     hl,bc
        bit     7,(hl)
        jr      z,l05f1                 ; (+$0d)
        inc     hl
        ld      a,(hl)
        cp      (ix+$1c)
        scf     
        push    de
        call    z,l0756
        pop     de
        jr      nc,l05fb                ; (+$0a)
l05f1:  ld      hl,$0038
        add     hl,bc
        ld      b,h
        ld      c,l
        dec     e
        jr      nz,l05dc                ; (-$1e)
        scf     
l05fb:  pop     bc
        ret     

l05fd:  ld      bc,fcbs
        ld      e,$12
l0602:  push    hl
        push    de
        ld      a,d
        ex      de,hl
        ld      hl,$0021
        add     hl,bc
        cp      (hl)
        jr      nz,l0627                ; (+$1a)
        inc     hl
        bit     3,(hl)
        jr      z,l0627                 ; (+$15)
        ld      hl,$002b
        add     hl,bc
        ld      a,e
        cp      (hl)
        jr      nz,l0627                ; (+$0d)
        inc     hl
        ld      a,d
        cp      (hl)
        jr      nz,l0627                ; (+$08)
        call    l0c0c
        call    c,l12ab
        jr      nc,l062e                ; (+$07)
l0627:  ld      hl,$0038
        add     hl,bc
        ld      b,h
        ld      c,l
        scf     
l062e:  pop     de
        pop     hl
        ret     nc

        dec     e
        jr      nz,l0602                ; (-$32)
        ret     

l0635:  push    de
        push    bc
        call    l053c
        call    c,l0ae9
        call    c,l0c0c
        pop     hl
        pop     de
        ret     nc

        push    de
        ld      a,l
        ld      hl,$0020
        add     hl,bc
        ld      (hl),a
        call    l0586
        ld      hl,l0d7a
        call    c,l0d9e
        pop     de
        ret     nc

        jr      nz,l0687                ; (+$30)
        ld      a,e
        or      a
        ld      a,$18
        ret     z

        dec     e
        jr      nz,l0667                ; (+$08)
        call    l06cc
        call    c,l080b
        jr      l06a3                   ; (+$3c)
l0667:  dec     e
        jr      nz,l0672                ; (+$08)
        call    l06cc
        call    c,l0863
        jr      l06a3                   ; (+$31)
l0672:  push    de
        dec     e
        jr      nz,l067e                ; (+$08)
        call    l06e8
        call    c,l098d
        jr      l0685                   ; (+$07)
l067e:  or      a
        ld      a,$15
        dec     e
        call    z,l0938
l0685:  pop     de
        ret     nc

l0687:  ld      a,d
        or      a
        ld      a,$17
        ret     z

        dec     d
        jr      nz,l0697                ; (+$08)
        call    x06b1
        call    c,l07e6
        jr      l069e                   ; (+$07)
l0697:  or      a
        ld      a,$15
        dec     d
        call    z,x06b1
l069e:  ret     nc

        xor     a
        scf     
        jr      l06a5                   ; (+$02)
l06a3:  ret     nc

        sbc     a,a
l06a5:  push    af
        ld      hl,$0020
        add     hl,bc
        set     7,(hl)
        inc     (ix+$21)
        pop     af
        ret     

x06b1:  ld      hl,$0020
        add     hl,bc
        ld      a,(hl)
        rra     
        rra     
        ld      a,$1e
        call    c,x1871
        ld      hl,x0000
        call    c,x0caa
        ret     nc

        ld      hl,$0022
        add     hl,bc
        set     0,(hl)
        scf     
        ret     

l06cc:  call    l0d3a
        jr      nc,l06e0                ; (+$0f)
        ld      hl,$0020
        add     hl,bc
        bit     1,(hl)
        scf     
        ret     z

        call    l0eb3
        call    c,x1871
        ret     

l06e0:  cp      $19
        scf     
        ccf     
        ret     nz

        ld      a,$17
        ret     

l06e8:  push    bc
        ld      h,b
        ld      l,c
        ld      de,sysfcb0
        ld      bc,$0038
        ldir    
        pop     bc
        ld      a,$42
        ld      ($df29),a
        ld      hl,$4b41
        ld      ($df2a),hl
        push    bc
        ld      bc,sysfcb0
        call    l0938
        pop     bc
        ret     c

        cp      $17
        scf     
        ret     z

        or      a
        ret     

l070e:  call    l04ff
        ld      d,a
        ld      e,c
        call    l053c
        ret     nc

        ld      a,$22
        ld      (bc),a
        ld      hl,$0020
        add     hl,bc
        ld      (hl),e
        inc     hl
        ld      (hl),d
        ld      a,d
        call    l17cb
        call    c,l0586
        ret     nc

        ld      e,(ix+$05)
        ld      d,(ix+$06)
        inc     de
        ld      a,(ix+$02)
        sub     $02
        call    nz,l04f5
        call    x1896
        sla     e
        rl      d
        ld      hl,$0024
        add     hl,bc
        ld      (hl),e
        inc     hl
        ld      (hl),d
        scf     
        jp      l06a5
l074a:  call    l0532
        call    c,l0c0c
        call    c,l0756
        ret     nc

        jr      l0789                   ; (+$33)
l0756:  ld      hl,$0020
        add     hl,bc
        bit     1,(hl)
        scf     
        ret     z

        call    x07a1
        call    c,l12ab
        call    c,l1685
        call    c,l0cb6
        ret     

l076b:  call    l0532
        call    c,l0c0c
        ret     nc

        ld      hl,$0020
        add     hl,bc
        bit     1,(hl)
        jr      z,l0789                 ; (+$0f)
        inc     hl
        inc     hl
        bit     1,(hl)
        jr      z,x0786                 ; (+$06)
        call    l102d
        call    l0f35
x0786:  call    l1632
l0789:  ld      hl,$0020
        add     hl,bc
        ld      (hl),$00
        dec     (ix+$21)
        call    z,l181b
        scf     
        ret     

l0797:  .db    "PLUS3DOS", $1a, $01

x07a1:  ld      hl,$0022
        add     hl,bc
        bit     6,(hl)
        scf     
        ret     z

        call    l1063
        push    hl
        push    de
        call    l07b9
        pop     de
        pop     hl
        push    af
        call    l107f
        pop     af
        ret     

l07b9:  ld      hl,$000b
        ld      e,h
        call    l107f
        ld      hl,$0023
        add     hl,bc
        ld      e,$03
        call    x07dc
        ret     nc

        xor     a
        call    l1226
        ret     nc

        ld      hl,$0030
        add     hl,bc
        ld      e,$08
        call    x07dc
        ret     nc

        jp      l1267
x07dc:  ld      a,(hl)
        inc     hl
        call    l1226
        ret     nc

        dec     e
        jr      nz,x07dc                ; (-$09)
        ret     

l07e6:  ld      e,$0a
        ld      hl,l0797
        call    x07dc
        ret     nc

        ld      a,$00
        call    l1226
        ret     nc

        ld      e,$74
l07f7:  xor     a
        call    l1226
        ret     nc

        dec     e
        jr      nz,l07f7                ; (-$08)
        call    l1267
        ret     nc

l0803:  ld      hl,$0022
        add     hl,bc
        set     6,(hl)
        scf     
        ret     

l080b:  call    l125f
        jr      nc,l0857                ; (+$47)
        jr      nz,l085c                ; (+$4a)
        ld      e,$0a
        ld      hl,l0797
x0817:  call    l114c
        jr      nc,l0857                ; (+$3b)
        cp      (hl)
        inc     hl
        jr      nz,l085c                ; (+$3c)
        dec     e
        jr      nz,x0817                ; (-$0c)
        call    l114c
        jr      nc,l0857                ; (+$2f)
        cp      $01
        jr      nc,l085c                ; (+$30)
        ld      hl,$0023
        add     hl,bc
        ld      e,$03
l0832:  call    l114c
        ret     nc

        ld      (hl),a
        inc     hl
        dec     e
        jr      nz,l0832                ; (-$09)
        call    l114c
        ret     nc

        ld      hl,$0030
        add     hl,bc
        ld      e,$08
l0845:  call    l114c
        ret     nc

        ld      (hl),a
        inc     hl
        dec     e
        jr      nz,l0845                ; (-$09)
        ld      hl,$0080
        ld      e,h
        call    l107f
        jr      l0803                   ; (-$54)
l0857:  cp      $19
        scf     
        ccf     
        ret     nz

l085c:  ld      hl,x0000
        ld      e,l
        call    l107f
l0863:  ld      hl,x0000
        ld      (filerecs),hl
        xor     a
        ld      ($df92),a
        ld      hl,$0897
        call    l0d9e
        ret     nc

        ld      de,($df91)
        ld      hl,($df8f)
        ld      l,$00
        srl     d
        rr      e
        rr      h
        rr      l
        ld      a,d
        or      a
        ld      a,$22
        ret     nz

        push    hl
        ld      hl,$0025
        add     hl,bc
        ld      (hl),e
        pop     de
        dec     hl
        ld      (hl),d
        dec     hl
        ld      (hl),e
        scf     
        ret     

        call    l0d7a
        ret     nz

        push    bc
        ld      b,h
        ld      c,l
        call    l1445
        ld      b,a
        ex      de,hl
        ld      hl,(filerecs)
        or      a
        sbc     hl,de
        ld      a,($df92)
        sbc     a,b
        jr      nc,l08b7                ; (+$08)
        ld      (filerecs),de
        ld      a,b
        ld      ($df92),a
l08b7:  pop     bc
        scf     
        sbc     a,a
        ret     

l08bb:  call    l0532
        ret     nc

        ld      ix,$0030
        add     ix,bc
        ld      hl,$0022
        add     hl,bc
        bit     6,(hl)
        scf     
        ret     

l08cd:  ld      e,c
        push    de
        call    l0532
        call    c,l0c0c
        call    c,l0756
        pop     de
        ret     nc

        ld      hl,$0020
        add     hl,bc
        ld      d,(hl)
        ld      (hl),e
        push    hl
        push    de
        call    l0586
        pop     de
        pop     hl
        jr      nc,l08f9                ; (+$10)
        bit     1,e
        jr      z,l08f5                 ; (+$08)
        call    l0eb3
        call    c,x1871
        jr      nc,l08f9                ; (+$04)
l08f5:  set     7,(hl)
        scf     
        ret     

l08f9:  ld      (hl),d
        or      a
        ret     

l08fc:  call    l04ff
        call    l0c13
        ret     nc

        jp      l0f9e
l0906:  cp      $ff
        jr      z,l0914                 ; (+$0a)
        cp      $10
        ld      b,a
        ld      a,$15
        ret     nc

        ld      a,b
        ld      (def_user),a
l0914:  ld      a,(def_user)
        scf     
        ret     

l0919:  call    l04ff
        cp      $ff
        jr      z,l0929                 ; (+$09)
        ld      b,a
        call    l17cb
        ret     nc

        ld      a,b
        ld      (def_drv),a
l0929:  ld      a,(def_drv)
        scf     
        ret     

l092e:  ld      bc,sysfcb0
        call    l0aff
        call    c,l0c0c
        ret     nc

l0938:  call    l0576
        call    c,x1871
        ret     nc

        ld      hl,$0945
        jp      l09b7
        call    l0d74
        ret     nz

        call    l0eb3
        ret     nc

        push    hl
        push    de
        xor     a
        call    l0f38
        pop     de
        pop     hl
        ld      (hl),$e5
        call    l0e24
        ret     nc

        call    l1035
        sbc     a,a
        ld      (extchg),a
        ret     

l0963:  ld      (att_clr),de
        ld      bc,sysfcb0
        call    l0aff
        call    c,l0c0c
        call    c,x1871
        ret     nc

        ld      hl,$09c9
        jr      l09b1                   ; (+$38)
l0979:  push    de
        ld      bc,sysfcb1
        call    l0ae9
        call    c,l0c0c
        pop     hl
        push    bc
        ld      bc,sysfcb0
        call    c,l0ae9
        pop     bc
        ret     nc

l098d:  ld      hl,$0021
        add     hl,bc
        ld      a,($df41)
        xor     (hl)
        ld      a,$1f
        ret     nz

        call    x1871
        push    bc
        ld      bc,sysfcb0
        call    c,l0576
        ld      hl,l0d74
        call    c,l0d9e
        pop     bc
        ret     nc

        ccf     
        ld      a,$18
        ret     z

        ld      hl,$09fe
l09b1:  push    hl
        call    l0576
        pop     hl
        ret     nc

l09b7:  xor     a
        ld      (extchg),a
        call    l0d9e
        ret     nc

        ld      a,(extchg)
        or      a
        ld      a,$17
        call    nz,l1685
        ret     

        call    l0d74
        ret     nz

        push    bc
        ld      a,(att_set)
        ld      c,$ff
        push    hl
        call    l09e2
        pop     hl
        ld      a,(att_clr)
        inc     c
        call    l09e2
        pop     bc
        jr      l0a1a                   ; (+$38)
l09e2:  rla     
        ld      b,$04
        inc     hl
        call    l09ef
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      b,$03
l09ef:  rla     
        jr      nc,l09fa                ; (+$08)
        res     7,(hl)
        inc     c
        dec     c
        jr      z,l09fa                 ; (+$02)
        set     7,(hl)
l09fa:  inc     hl
        djnz    l09ef                   ; (-$0e)
        ret     

        call    l0d74
        ret     nz

        call    l0eb3
        ret     nc

        push    de
        ex      de,hl
        ld      hl,sysfcb0
        ld      a,(de)
        and     $10
        or      (hl)
        ld      (de),a
        inc     de
        inc     hl
        push    bc
        ld      bc,$000b
        ldir    
        pop     bc
        pop     de
l0a1a:  call    l0e24
        ret     nc

        sbc     a,a
        ld      (extchg),a
        ret     

l0a23:  ld      (cat_buff),de
        ld      (cat_filt),bc
        ld      a,$01
        ld      (cat_ents),a
        ld      bc,sysfcb0
        call    l0aff
        call    c,l0c0c
        call    c,l05d6
        ld      hl,$0a47
        call    c,l0d9e
        ld      bc,(cat_size)
        ret     

        push    bc
        call    l0a4f
        pop     bc
        scf     
        sbc     a,a
        ret     

l0a4f:  call    l0d7a
        ret     nz

        ld      a,(cat_filt)
        rra     
        jr      c,l0a62                 ; (+$09)
        push    hl
        ld      bc,$000a
        add     hl,bc
        bit     7,(hl)
        pop     hl
        ret     nz

l0a62:  ld      de,(cat_buff)
        call    l0ad3
        ret     nc

        ld      bc,(cat_size)
l0a6e:  push    hl
        ld      hl,$000d
        add     hl,de
        ex      de,hl
        pop     hl
        dec     c
        djnz    l0a7b                   ; (+$03)
        ret     z

        jr      l0aab                   ; (+$30)
l0a7b:  call    l0ad3
        jr      c,l0a6e                 ; (-$12)
        jr      z,x0ac3                 ; (+$41)
        push    hl
        push    de
        ld      hl,(cat_size)
        ld      h,$00
        dec     hl
        ld      b,h
        ld      c,l
        add     hl,hl
        add     hl,bc
        add     hl,hl
        add     hl,hl
        add     hl,bc
        ld      bc,(cat_buff)
        add     hl,bc
        ld      a,l
        sub     e
        ld      c,a
        ld      a,h
        sbc     a,d
        ld      b,a
        dec     hl
        ld      de,$000d
        ex      de,hl
        add     hl,de
        ex      de,hl
        ld      a,b
        or      c
        jr      z,l0aa9                 ; (+$02)
        lddr    
l0aa9:  pop     de
        pop     hl
l0aab:  push    hl
        push    de
        inc     hl
        ld      bc,$000b
        ldir    
        xor     a
        ld      (de),a
        inc     de
        ld      (de),a
        ld      hl,(cat_size)
        ld      a,h
        cp      l
        adc     a,$00
        ld      (cat_ents),a
        pop     de
        pop     hl
x0ac3:  call    l0f6a
        ex      de,hl
        ld      bc,$000b
        add     hl,bc
        ld      a,(hl)
        add     a,e
        ld      (hl),a
        inc     hl
        ld      a,(hl)
        adc     a,d
        ld      (hl),a
        ret     

l0ad3:  push    hl
        push    de
        push    bc
        ld      b,$0b
        inc     hl
l0ad9:  ld      a,(hl)
        add     a,a
        ld      c,a
        ld      a,(de)
        add     a,a
        cp      c
        jr      nz,x0ae5                ; (+$04)
        inc     de
        inc     hl
        djnz    l0ad9                   ; (-$0c)
x0ae5:  pop     bc
        pop     de
        pop     hl
        ret     

l0ae9:  call    l0b49
        ret     nc

        ld      hl,$0001
        add     hl,bc
        ld      e,$0b
l0af3:  ld      a,(hl)
        inc     hl
        cp      $3f
        ld      a,$14
        ret     z

        dec     e
        jr      nz,l0af3                ; (-$0a)
        scf     
        ret     

l0aff:  jp      l0b49
l0b02:  call    l0b0c
        jr      nc,l0b2b                ; (+$24)
        call    l0b2b
        scf     
        ret     

l0b0c:  call    l0b42
        ret     nc

        ld      e,a
        call    l0bdd
        call    c,l0b42
        jr      nc,l0b25                ; (+$0c)
        ld      d,a
        ld      a,e
        add     a,a
        ld      e,a
        add     a,a
        add     a,a
        add     a,e
        add     a,d
        ld      e,a
        call    l0bdd
l0b25:  ld      a,e
        cp      $10
        ret     nc

        ld      (bc),a
        ret     

l0b2b:  call    l0bd3
        ret     nc

        cp      $41
        ccf     
        ret     nc

        cp      $51
        ret     nc

        push    hl
        ld      hl,$0021
        add     hl,bc
        ld      (hl),a
        pop     hl
        call    l0bdd
        scf     
        ret     

l0b42:  sub     $30
        ccf     
        ret     nc

        cp      $0a
        ret     

l0b49:  push    bc
        call    l0b51
        pop     bc
        ld      a,$14
        ret     

l0b51:  push    hl
        ld      hl,$0021
        add     hl,bc
        ld      a,(def_drv)
        ld      (hl),a
        pop     hl
        ld      a,(def_user)
        ld      (bc),a
        call    l0bd3
        jr      nc,l0b84                ; (+$20)
        ld      e,a
        push    hl
l0b66:  cp      $3a
        scf     
        jr      z,l0b70                 ; (+$05)
        call    l0bdd
        jr      c,l0b66                 ; (-$0a)
l0b70:  pop     hl
        ld      a,e
        jr      nc,l0b8a                ; (+$16)
        call    l0b02
        ret     nc

        call    l0bd3
        ret     nc

        xor     $3a
        ret     nz

        call    l0bcf
        jr      c,l0b8a                 ; (+$06)
l0b84:  inc     bc
        ld      e,$0b
        scf     
        jr      l0bc6                   ; (+$3c)
l0b8a:  inc     bc
        cp      $2e
        ret     z

        ld      e,$08
        call    l0ba0
        ccf     
        ld      e,$03
        jr      nc,l0bbd                ; (+$25)
        xor     $2e
        ret     nz

        call    l0bcf
        jr      nc,l0bbd                ; (+$1d)
l0ba0:  push    hl
        cp      $20
        ld      hl,l0bf9
        call    nc,l0bef
        pop     hl
        jr      c,l0bbd                 ; (+$11)
        dec     e
        ret     m
        cp      $2a
        call    z,l0bc6
        ld      (bc),a
        inc     bc
        call    l0bdd
        jr      nz,l0ba0                ; (-$1a)
        call    c,l0bcf
l0bbd:  push    af
        ld      a,$20
        call    l0bc8
        pop     af
        ccf     
        ret     

l0bc6:  ld      a,$3f
l0bc8:  inc     e
l0bc9:  dec     e
        ret     z

        ld      (bc),a
        inc     bc
        jr      l0bc9                   ; (-$06)
l0bcf:  call    l0bdd
        ret     nc

l0bd3:  call    l0be2
x0bd6:  ret     nz

        call    l0bdd
        jr      c,x0bd6                 ; (-$06)
        ret     

l0bdd:  ld      a,(hl)
        cp      $ff
        ret     z

        inc     hl
l0be2:  ld      a,(hl)
        cp      $ff
        ret     z

        and     $7f
        call    l04ff
        cp      $20
        scf     
        ret     

l0bef:  cp      (hl)
        scf     
        ret     z
        inc     hl
        bit     7,(hl)
        jr      z,l0bef                 ; (-$08)
        or      a
        ret     

l0bf9:	.db    "!&()+,-./:"
	.db    ';'
	.db    "<=>[\\]|"
	.db    $80

l0c0c:  push    hl
        ld      hl,$0021
        add     hl,bc
        ld      a,(hl)
        pop     hl
l0c13:  call    l17ef
        ret     nc

        bit     0,(ix+$1b)
        scf     
        ret     nz

        push    hl
        push    de
        push    bc
        call    l0ec1
        set     1,(ix+$1b)
        xor     a
        ld      (ix+$22),a
        ld      (ix+$23),a
        call    l0c60
        ld      bc,x0000
        ld      hl,$0c4d
        call    l0d9e
        ld      (ix+$24),c
        ld      (ix+$25),b
        pop     bc
        pop     de
        pop     hl
        ret     nc

        set     0,(ix+$1b)
        res     1,(ix+$1b)
        ret     

        call    l0c53
        scf     
        sbc     a,a
        ret     

l0c53:  ld      a,(hl)
        cp      $e5
        jp      z,l1035
        ld      b,d
        ld      c,e
        ld      a,$ff
        jp      l0f38
l0c60:  ld      a,(ix+$07)
        ld      (ix+$24),a
        ld      a,(ix+$08)
        ld      (ix+$25),a
        ret     

l0c6d:  call    l0d1e
        ret     c

        push    hl
        call    l0cb6
        pop     hl
        ret     nc

l0c77:  ex      de,hl
        ld      hl,$000c
        add     hl,bc
        ld      (hl),d
        inc     hl
        inc     hl
        ld      (hl),e
        ld      e,$11
        xor     a
l0c83:  inc     hl
        ld      (hl),a
        dec     e
        jr      nz,l0c83                ; (-$05)
        call    l0d3a
        ret     c

        ld      hl,$0022
        add     hl,bc
        set     2,(hl)
        or      a
        ret     

l0c94:  call    l0d1e
        jr      nc,l0ca4                ; (+$0b)
        ld      hl,$0022
        add     hl,bc
        bit     2,(hl)
        jp      nz,l0fcb
        scf     
        ret     

l0ca4:  push    hl
        call    l0cb6
        pop     hl
        ret     nc

x0caa:  call    l0c77
        ret     c

        cp      $19
        scf     
        ccf     
        ret     nz

        jp      l0fcb
l0cb6:  ld      hl,$0022
        add     hl,bc
        ld      a,(hl)
        and     $03
        scf     
        ret     z

        cp      $02
        jp      z,l102d
        and     $02
        jr      nz,l0cd5                ; (+$0d)
        ld      hl,$0d50
        call    l0d9e
        ret     nc

        ld      a,$20
        ccf     
        ret     nz

        jr      l0ceb                   ; (+$16)
l0cd5:  call    l0feb
        ret     nc

        push    hl
        push    de
        push    bc
        call    l103e
        jr      nz,l0ce8                ; (+$07)
        ld      b,$0a
l0ce3:  ld      (hl),$00
        inc     hl
        djnz    l0ce3                   ; (-$05)
l0ce8:  pop     bc
        pop     de
        pop     hl
l0ceb:  push    de
        push    bc
        ex      de,hl
        ld      h,b
        ld      l,c
        ld      bc,$0020
        ldir    
        pop     bc
        pop     de
        call    l0e24
        call    c,l1685
        ld      hl,$0022
        add     hl,bc
        res     0,(hl)
        scf     
        ret     

l0d05:  push    bc
        ld      a,(ix+$04)
        cpl     
        and     $1f
        ld      b,a
        ld      a,d
        rra     
        rra     
        rra     
        rra     
        and     $0f
        ld      l,a
        ld      a,e
        add     a,a
        ld      a,d
        adc     a,a
        and     b
        ld      h,a
        ld      a,b
        pop     bc
        ret     

l0d1e:  call    l0d05
        push    hl
        push    de
        push    bc
        ex      de,hl
        ld      hl,$000e
        add     hl,bc
        ld      b,a
        ld      a,(hl)
        xor     e
        jr      nz,l0d36                ; (+$08)
        dec     hl
        dec     hl
        ld      a,(hl)
        xor     d
        and     b
        jr      nz,l0d36                ; (+$01)
        scf     
l0d36:  pop     bc
        pop     de
        pop     hl
        ret     

l0d3a:  ld      hl,$0d50
        call    l0d9e
        ret     nc

        ccf     
        ld      a,$19
        ret     nz

        push    bc
        ld      d,b
        ld      e,c
        ld      bc,$0020
        ldir    
        pop     bc
        scf     
        ret     

        push    hl
        push    de
        push    bc
        ld      a,(bc)
        xor     (hl)
        call    z,l0d87
        jr      nz,l0d72                ; (+$18)
        ld      a,(de)
        inc     a
        jr      z,l0d68                 ; (+$0a)
        ld      a,(ix+$04)
        cpl     
        ld      b,a
        ld      a,(de)
        xor     (hl)
        and     b
        jr      nz,l0d72                ; (+$0a)
l0d68:  inc     de
        inc     hl
        inc     de
        inc     hl
        ld      a,(de)
        cp      $ff
        jr      z,l0d72                 ; (+$01)
        xor     (hl)
l0d72:  jr      l0d82                   ; (+$0e)
l0d74:  ld      a,(bc)
        xor     (hl)
        and     $ef
        jr      l0d7c                   ; (+$02)
l0d7a:  ld      a,(bc)
        xor     (hl)
l0d7c:  push    hl
        push    de
        push    bc
        call    z,l0d87
l0d82:  pop     bc
        pop     de
        pop     hl
        scf     
        ret     

l0d87:  push    bc
        ld      d,b
        ld      e,c
        inc     de
        inc     hl
        ld      b,$0b
l0d8e:  ld      a,(de)
        cp      $3f
        jr      z,l0d98                 ; (+$05)
        xor     (hl)
        and     $7f
        jr      nz,l0d9c                ; (+$04)
l0d98:  inc     de
        inc     hl
        djnz    l0d8e                   ; (-$0e)
l0d9c:  pop     bc
        ret     

l0d9e:  ld      (rt_dirent),hl
        call    l15bf
        ld      de,x0000
        push    af
l0da8:  ld      a,e
        and     $0f
        jr      nz,l0db3                ; (+$06)
        pop     af
        call    l0def
        ret     nc

        push    af
l0db3:  pop     af
        push    af
        push    hl
        push    ix
        push    de
        push    bc
        ld      c,a
        ld      b,$07
        ld      a,e
        call    l0e5a
        call    l0245
        pop     bc
        pop     de
        pop     ix
        push    de
        call    l0ddd
        pop     de
        pop     hl
        jr      nc,l0dd7                ; (+$07)
        jr      z,l0dd7                 ; (+$05)
        call    l0de5
        jr      nc,l0da8                ; (-$2f)
l0dd7:  ld      hl,direntry
        inc     sp
        inc     sp
        ret     

l0ddd:  ld      hl,(rt_dirent)
        push    hl
        ld      hl,direntry
        ret     

l0de5:  inc     de
        ld      a,(ix+$24)
        sub     e
        ld      a,(ix+$25)
        sbc     a,d
        ret     

l0def:  push    bc
        push    de
        ld      a,$04
        call    l04eb
        call    x1896
l0df9:  call    l1560
        jr      nc,l0e21                ; (+$23)
        ld      b,a
        push    hl
        call    l0e6d
        jr      c,l0e1d                 ; (+$18)
        bit     1,(ix+$1b)
        jr      z,l0e0c                 ; (+$01)
        ld      (hl),a
l0e0c:  cp      (hl)
        scf     
        jr      z,l0e1d                 ; (+$0d)
        call    l15d8
        ld      a,$08
        call    l1a9d
        jr      nz,l0e1d                ; (+$03)
        pop     hl
        jr      l0df9                   ; (-$24)
l0e1d:  pop     hl
        jr      nc,l0e21                ; (+$01)
        ld      a,b
l0e21:  pop     de
        pop     bc
        ret     

l0e24:  push    hl
        push    de
        push    bc
        ld      c,e
        ld      a,$04
        call    l04eb
        call    x1896
        push    bc
        ld      bc,$0001
        call    x1590
        pop     bc
        jr      nc,l0e56                ; (+$1c)
        ld      b,a
        push    ix
        push    hl
        push    de
        push    bc
        ld      a,c
        call    l0e5a
        ex      de,hl
        ld      c,$07
        call    l0245
        pop     bc
        pop     de
        pop     hl
        pop     ix
        call    l0e6d
        jr      c,l0e56                 ; (+$02)
        ld      (hl),a
        scf     
l0e56:  pop     bc
        pop     de
        pop     hl
        ret     

l0e5a:  and     $0f
        jr      z,l0e65                 ; (+$07)
        ld      de,$0020
l0e61:  add     hl,de
        dec     a
        jr      nz,l0e61                ; (-$04)
l0e65:  ld      de,direntry
        ld      ix,$0020
        ret     

l0e6d:  push    hl
        push    de
        ex      de,hl
        ld      e,(ix+$0b)
        ld      a,(ix+$0c)
        and     $7f
        ld      d,a
        ld      a,$02
        call    l04eb
        call    x1896
        sbc     hl,de
        ccf     
        pop     de
        pop     hl
        ret     c

        push    bc
        ld      a,b
        call    l021b
        push    af
        xor     a
        ld      bc,$0002
l0e91:  add     a,(hl)
        inc     hl
        djnz    l0e91                   ; (-$04)
        dec     c
        jr      nz,l0e91                ; (-$07)
        ld      b,a
        pop     af
        call    l021b
        ld      l,(ix+$26)
        ld      h,(ix+$27)
        add     hl,de
        push    de
        ld      de,x0000
        call    x1896
        or      a
        sbc     hl,de
        pop     de
        ld      a,b
        or      a
        pop     bc
        ret     

l0eb3:  push    de
        ex      de,hl
        ld      hl,$0009
        add     hl,de
        ld      a,(hl)
        add     a,a
        ex      de,hl
        pop     de
        ccf     
        ld      a,$1c
        ret     

l0ec1:  call    l0fbe
        ld      a,$03
        call    l04eb
        inc     de
        push    hl
l0ecb:  ld      (hl),$00
        inc     hl
        dec     de
        ld      a,d
        or      e
        jr      nz,l0ecb                ; (-$08)
        pop     hl
        ld      a,(ix+$09)
        ld      (hl),a
        inc     hl
        ld      a,(ix+$0a)
        ld      (hl),a
        ret     

l0ede:  push    bc
        push    hl
        push    de
        ld      a,$03
        call    l04eb
        push    de
        call    l0fbe
        pop     de
        add     hl,de
        pop     de
        ld      a,e
        and     $07
        ld      b,a
        ld      a,$01
        inc     b
l0ef4:  rrca    
        djnz    l0ef4                   ; (-$03)
        ld      b,a
        and     c
        ld      c,a
        ld      a,b
        cpl     
        and     (hl)
        or      c
        ld      (hl),a
        pop     hl
        pop     bc
        ret     

l0f02:  push    hl
        push    bc
        call    l0fbe
l0f07:  ld      bc,$0880
l0f0a:  ld      a,(hl)
        and     c
        jr      z,l0f1c                 ; (+$0e)
        rrc     c
        ld      a,d
        or      e
        ld      a,$1a
        jr      z,l0f32                 ; (+$1c)
        dec     de
        djnz    l0f0a                   ; (-$0f)
        inc     hl
        jr      l0f07                   ; (-$15)
l0f1c:  ld      a,(hl)
        or      c
        ld      (hl),a
        ld      a,(ix+$05)
        sub     e
        ld      e,a
        ld      a,(ix+$06)
        sbc     a,d
        ld      d,a
        pop     bc
        push    bc
        ld      hl,$0022
        add     hl,bc
        set     0,(hl)
        scf     
l0f32:  pop     bc
        pop     hl
        ret     

l0f35:  ld      h,b
        ld      l,c
        xor     a
l0f38:  push    bc
        ld      c,a
        ld      a,$0f
        cp      (hl)
        jr      c,l0f67                 ; (+$28)
        ld      de,$0010
        add     hl,de
        ld      b,$10
        inc     b
        jr      l0f65                   ; (+$1d)
l0f48:  ld      e,(hl)
        inc     hl
        ld      a,(ix+$06)
        or      a
        ld      d,a
        jr      z,l0f54                 ; (+$03)
        dec     b
        ld      d,(hl)
        inc     hl
l0f54:  ld      a,d
        or      e
        jr      z,l0f65                 ; (+$0d)
        push    hl
        ld      a,(ix+$05)
        sub     e
        ld      a,(ix+$06)
        sbc     a,d
        call    nc,l0ede
        pop     hl
l0f65:  djnz    l0f48                   ; (-$1f)
l0f67:  pop     bc
        scf     
        ret     

l0f6a:  push    de
        ex      de,hl
        ld      a,(de)
        cp      $10
        ld      hl,x0000
        jr      nc,l0f8e                ; (+$1a)
        ld      hl,$0010
        add     hl,de
        ld      de,$1000
l0f7b:  ld      a,(ix+$06)
        or      a
        ld      a,(hl)
        inc     hl
        jr      z,l0f86                 ; (+$03)
        or      (hl)
        dec     d
        inc     hl
l0f86:  or      a
        jr      z,l0f8a                 ; (+$01)
        inc     e
l0f8a:  dec     d
        jr      nz,l0f7b                ; (-$12)
        ex      de,hl
l0f8e:  pop     de
l0f8f:  ld      a,(ix+$02)
        dec     a
        dec     a
l0f94:  dec     a
        jr      z,x0f9a                 ; (+$03)
        add     hl,hl
        jr      l0f94                   ; (-$06)
x0f9a:  ld      a,h
        or      l
        scf     
        ret     

l0f9e:  ld      hl,x0000
        push    hl
        call    l0fbe
l0fa5:  ld      bc,$0880
l0fa8:  ld      a,(hl)
        and     c
        jr      nz,l0faf                ; (+$03)
        ex      (sp),hl
        inc     hl
        ex      (sp),hl
l0faf:  rrc     c
        ld      a,d
        or      e
        jr      z,l0fbb                 ; (+$06)
        dec     de
        djnz    l0fa8                   ; (-$10)
        inc     hl
        jr      l0fa5                   ; (-$16)
l0fbb:  pop     hl
        jr      l0f8f                   ; (-$2f)
l0fbe:  ld      l,(ix+$28)
        ld      h,(ix+$29)
        ld      e,(ix+$05)
        ld      d,(ix+$06)
        ret     

l0fcb:  ld      a,(ix+$22)
        or      (ix+$23)
        ld      a,$1b
        ret     z

        ld      hl,$0022
        add     hl,bc
        set     1,(hl)
        res     2,(hl)
        ld      a,(ix+$22)
        sub     $01
        ld      (ix+$22),a
        jr      nc,l0fe9                ; (+$03)
        dec     (ix+$23)
l0fe9:  scf     
        ret     

l0feb:  push    bc
        ld      c,(ix+$24)
        ld      b,(ix+$25)
        call    l0c60
        ld      hl,$1028
        call    l0d9e
        jr      nc,l1026                ; (+$29)
        ex      (sp),hl
        push    hl
        push    de
        push    af
        ld      de,$0022
        add     hl,de
        res     1,(hl)
        pop     af
        pop     de
        pop     hl
        ex      (sp),hl
        jr      nz,l1020                ; (+$13)
        ex      de,hl
        or      a
        sbc     hl,bc
        add     hl,bc
        ex      de,hl
        jr      c,l1017                 ; (+$02)
        ld      b,d
        ld      c,e
l1017:  ld      (ix+$24),c
        ld      (ix+$25),b
        scf     
        jr      l1026                   ; (+$06)
l1020:  call    l1035
        ld      a,$20
        or      a
l1026:  pop     bc
        ret     

        ld      a,(hl)
        xor     $e5
        scf     
        ret     

l102d:  push    hl
        ld      hl,$0022
        add     hl,bc
        res     1,(hl)
        pop     hl
l1035:  scf     
        inc     (ix+$22)
        ret     nz

        inc     (ix+$23)
        ret     

l103e:  ld      a,e
        and     $03
        cpl     
        add     a,$04
        jr      z,l104d                 ; (+$07)
        ld      bc,$0020
l1049:  add     hl,bc
        dec     a
        jr      nz,l1049                ; (-$04)
l104d:  ld      a,(hl)
        cp      $21
        ret     nz

        ld      a,e
        and     $03
        jr      z,l105d                 ; (+$07)
        ld      bc,$000a
l1059:  add     hl,bc
        dec     a
        jr      nz,l1059                ; (-$04)
l105d:  inc     hl
        ret     

l105f:  call    l0532
        ret     nc

l1063:  ld      hl,$0026
        jr      l106f                   ; (+$07)
l1068:  call    l0532
        ret     nc

        ld      hl,$0023
l106f:  add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        ld      a,(hl)
        ex      de,hl
        ld      e,a
        ld      d,$00
        scf     
        ret     

l107b:  call    l0532
        ret     nc

l107f:  ld      a,l
        ld      d,e
        ld      e,h
        ld      hl,$0026
        add     hl,bc
        jr      l109c                   ; (+$14)
l1088:  ld      a,$80
        jr      l108e                   ; (+$02)
l108c:  ld      a,$01
l108e:  ld      hl,$0026
        add     hl,bc
        add     a,(hl)
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        jr      nc,l109a                ; (+$01)
        inc     de
l109a:  dec     hl
        dec     hl
l109c:  push    hl
        push    af
        xor     (hl)
        jp      m,l10ac
        inc     hl
        ld      a,(hl)
        cp      e
        jr      nz,l10ac                ; (+$05)
        inc     hl
        ld      a,(hl)
        cp      d
        jr      z,l10b2                 ; (+$06)
l10ac:  ld      hl,$0022
        add     hl,bc
        res     5,(hl)
l10b2:  pop     af
        pop     hl
        ld      (hl),a
        inc     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        scf     
        ret     

l10bb:  push    bc
        push    af
        ld      hl,$0022
        add     hl,bc
        ex      de,hl
        ld      hl,$0025
        add     hl,bc
        ld      b,$03
        or      a
l10c9:  inc     de
        inc     hl
        ld      a,(de)
        sbc     a,(hl)
        djnz    l10c9                   ; (-$06)
        jr      nc,l10d6                ; (+$05)
        ld      bc,$0003
        lddr    
l10d6:  pop     af
        pop     bc
        ret     

l10d9:  call    l114c
        ret     nc

        push    de
        ld      e,a
        ld      a,(rw_page)
        ld      hl,(rw_add)
        call    l021b
        ld      (hl),e
        call    l021b
        inc     hl
        ld      (rw_add),hl
        pop     de
        ld      a,d
        or      e
        dec     de
        scf     
        ret     

l10f6:  push    de
        call    l12fe
        pop     de
        ret     nc

        push    de
        call    l1088
        ld      hl,$002d
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        push    bc
        ld      c,(hl)
        ld      a,(rw_page)
        ld      b,a
        ld      hl,(rw_add)
        ex      de,hl
        ld      ix,$0080
        call    l0245
        pop     bc
        ld      (rw_add),de
        pop     de
        ld      hl,$ff81
        add     hl,de
        ex      de,hl
        ld      a,d
        or      e
        dec     de
        scf     
        ret     

; DOS_BYTE_READ

l1129:  call    x0521		       ; get FCB & test if open for reading
        ret     nc		       ; exit if error
        ld      hl,$0026
        add     hl,bc
        ex      de,hl		       ; DE=filepointer address
        ld      hl,$0023
        add     hl,bc		       ; HL=filelength address
        push    bc
        ld      b,$03
        or      a
l113a:  ld      a,(de)		       ; test filepointer (carry must be set
        sbc     a,(hl)		       ; if pointer within file)
        inc     de
        inc     hl
        djnz    l113a                  
        pop     bc
        ld      a,$19		       ; error "end of file"
        call    c,l114c		       ; get a byte if within file
        ret     nc		       ; exit if error
        ld      c,a		       ; C=byte read
        cp      $1a		       ; set Z if soft-EOF
        scf     		       ; success
        ret     

; Subroutine to read a byte (A) from the file

l114c:  push    hl
        push    de
        ld      hl,$0022
        add     hl,bc
        bit     5,(hl)		       ; has record been changed?
        jr      nz,l115b               
        call    l12fe		       ; if so, get new record details into FCB
        jr      nc,x117c               ; exit if error
l115b:  ld      hl,$0026
        add     hl,bc
        ld      a,(hl)		       ; low byte of filepointer
        and     $7f		       ; offset into record
        ld      hl,$002d
        add     hl,bc
        add     a,(hl)
        ld      e,a
        inc     hl
        adc     a,(hl)
        sub     e
        ld      d,a		       ; DE=address of byte in buffer
        inc     hl
        ld      a,(hl)		       ; A=bank of buffer		*** de DONDE mierda sale este dato quien le manda decir que = 1????
        ex      de,hl
        call    l021b		       ; page in buffer bank
        ld      d,(hl)		       ; get byte
        call    l021b		       ; page back original bank
        push    de
        call    l108c		       ; increment filepointer
        pop     af		       ; A=byte
        scf     		       ; success
x117c:  pop     de
        pop     hl
        ret     

l117f:  ld      a,c
        ld      (rw_page),a
        ld      (rw_add),hl
        call    l0529
        ret     nc

        add     hl,de
        push    hl
        call    l119f
        call    l10bb
        pop     hl
        ret     c

        push    af
        ld      de,(rw_add)
        or      a
        sbc     hl,de
        ex      de,hl
        pop     af
        ret     

l119f:  dec     de
l11a0:  ld      hl,$0026
        add     hl,bc
        ld      a,(hl)
        and     $7f
        jr      z,l11b0                 ; (+$07)
        call    l11c4
        ret     nc

        ret     z

        jr      l11a0                   ; (-$10)
l11b0:  ld      hl,$ff81
        add     hl,de
        jr      nc,l11bd                ; (+$07)
        call    l11e2
        ret     nc

        ret     z

        jr      l11b0                   ; (-$0d)
l11bd:  call    l11c4
        ret     nc

        ret     z

        jr      l11bd                   ; (-$07)
l11c4:  ld      a,(rw_page)
        ld      hl,(rw_add)
        call    l021b
        ld      l,(hl)
        call    l021b
        ld      a,l
        call    l1226
        ret     nc

        ld      hl,(rw_add)
        inc     hl
        ld      (rw_add),hl
        ld      a,d
        or      e
        dec     de
        scf     
        ret     

l11e2:  push    de
        call    l12cf
        pop     de
        ret     nc

        ld      hl,$0022
        add     hl,bc
        set     4,(hl)
        push    de
        call    l1088
        ld      hl,$002d
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        push    bc
        ld      b,(hl)
        ld      a,(rw_page)
        ld      c,a
        ld      hl,(rw_add)
        ld      ix,$0080
        call    l0245
        pop     bc
        ld      (rw_add),hl
        pop     de
        ld      hl,$ff81
        add     hl,de
        ex      de,hl
        ld      a,d
        or      e
        dec     de
        scf     
        ret     

l1219:  ld      e,c
        call    l0529
        ret     nc

        ld      a,e
        call    l1226
        call    c,l10bb
        ret     

l1226:  push    hl
        push    de
        ld      e,a
        ld      hl,$0022
        add     hl,bc
        bit     5,(hl)
        jr      nz,l123a                ; (+$09)
        push    hl
        push    de
        call    l12cf
        pop     de
        pop     hl
        jr      nc,l125c                ; (+$22)
l123a:  set     4,(hl)
        ld      hl,$0026
        add     hl,bc
        ld      a,(hl)
        and     $7f
        ld      hl,$002d
        add     hl,bc
        push    de
        add     a,(hl)
        ld      e,a
        inc     hl
        adc     a,(hl)
        sub     e
        ld      d,a
        inc     hl
        ld      a,(hl)
        ex      de,hl
        pop     de
        call    l021b
        ld      (hl),e
        call    l021b
        call    l108c
l125c:  pop     de
        pop     hl
        ret     

l125f:  call    l1278
        ret     nc

        ld      a,d
        cp      e
        scf     
        ret     

l1267:  call    l1278
        ret     nc

        call    l021b
        ld      (hl),e
        call    l021b
        call    l1088
        jp      l10bb
l1278:  ld      hl,x0000
        ld      e,h
        call    l107f
        ld      hl,$0022
        add     hl,bc
        bit     5,(hl)
        jr      nz,l128b                ; (+$04)
        call    l12fe
        ret     nc

l128b:  ld      hl,$002d
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        ld      a,(hl)
        ex      de,hl
        push    af
        call    l021b
        push    af
        xor     a
        ld      e,$7f
l129d:  add     a,(hl)
        inc     hl
        dec     e
        jr      nz,l129d                ; (-$05)
        ld      e,a
        ld      d,(hl)
        pop     af
        call    l021b
        pop     af
        scf     
        ret     

l12ab:  push    hl
        ld      hl,$0022
        add     hl,bc
        bit     3,(hl)
        jr      z,l12c8                 ; (+$14)
        bit     4,(hl)
        jr      z,l12c8                 ; (+$10)
        push    hl
        push    de
        ld      hl,$002b
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        call    x1590
        pop     de
        pop     hl
        jr      nc,l12cd                ; (+$05)
l12c8:  ld      a,(hl)
        and     $c7
        ld      (hl),a
        scf     
l12cd:  pop     hl
        ret     

l12cf:  ld      a,(bc)
        cp      $22
        ld      hl,$12dc
        jr      nz,l1309                ; (+$32)
        ld      hl,$146e
        jr      l1309                   ; (+$2d)
        push    de
        call    l0c94
        pop     de
        call    c,l1371
        ret     nc

        push    hl
        push    de
        push    af
        call    l1353
        call    l1433
        call    nc,l1416
        pop     af
        pop     de
        pop     hl
        ret     

; Subroutine to get the abs log sector (DE) and address (AHL) of
; record DE in the current file

        push    de
        call    l0c6d		; find extent HL for file
        pop     de
        call    c,x13a4		; if found, get address of record
        ret     

; Subroutine to get the current record into a buffer and update the FCB
; with its details

l12fe:  ld      a,(bc)		
        cp      $22		; test for "drive open as file"
        ld      hl,$12f5	; routine to use for normal file
        jr      nz,l1309                
        ld      hl,$146e	; routine to use for drive
l1309:  call    l135d		; DE=record number for filepointer
        ret     nc		; exit if file too big
        push    hl
        ld      hl,$0022
        add     hl,bc
        bit     3,(hl)
        pop     hl
        jr      z,l1323         ; move on if no sector currently in buffer        
        push    hl
        ex      de,hl
        call    l1353		; get record number from FCB
        ex      de,hl
        or      a
        sbc     hl,de
        pop     hl
        jr      z,l1349         ; move on if record numbers match        
l1323:  call    l0c0c		; ensure correct disk logged in
        call    c,l12ab		; get sector in FCB to buffer
        ret     nc		; exit if error
        push    hl
        ld      hl,$0029
        add     hl,bc
        ld      (hl),e
        inc     hl
        ld      (hl),d
        pop     hl
        call    l1353		; DE=record number required
        call    l04fe		; call routine in HL
        ret     nc		; exit if error
        push    hl
        ld      hl,$002b
        add     hl,bc
        ld      (hl),e
        inc     hl
        ld      (hl),d		; store abs logical sector number
        pop     de
        inc     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d		; store add of record
        inc     hl
        ld      (hl),a		; store bank for buffer
l1349:  ld      hl,$0022
        add     hl,bc
        ld      a,(hl)
        or      $28		; set bit 3 (valid sector) & bit 5
        ld      (hl),a		; (valid filepointer)
        scf     
        ret     

l1353:  push    hl
        ld      hl,$0029
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        pop     hl
        ret     

l135d:  push    hl
        ld      hl,$0026
        add     hl,bc
        ld      a,(hl)
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        ex      de,hl
        add     a,a
        adc     hl,hl
        ex      de,hl
        ccf     
        ld      a,$22
        pop     hl
        ret     

l1371:  push    de
        call    l13dc
        ex      de,hl
        ex      (sp),hl
        ex      de,hl
        jr      c,x138b                 ; (+$11)
        call    l0f02
        jr      nc,l1394                ; (+$15)
        ld      (hl),e
        ld      a,(ix+$06)
        or      a
        jr      z,l1388                 ; (+$02)
        inc     hl
        ld      (hl),d
l1388:  ex      de,hl
        jr      l1392                   ; (+$07)
x138b:  ld      a,e
        and     $03
        scf     
        call    z,l1433
l1392:  sbc     a,a
        scf     
l1394:  pop     de
        call    c,l13bd
        push    hl
        call    c,l139e
        jr      l13b6                   ; (+$18)
l139e:  jp      z,l1578
        jp      l1560

; Subroutine to find abs log sector (DE) and record address (AHL)
; for record DE in current file

x13a4:  push    de
        call    l13dc			; get block & offset
        ex      de,hl
        ex      (sp),hl
        ex      de,hl
        call    c,l1433			; check record is in this extent
        pop     de
        call    c,l13bd			; calculate sector & offset
x13b2:  push    hl			; stack offset into sector
        call    c,l1560			; get AHL=address of sector DE
l13b6:  ex      de,hl
        ex      (sp),hl			; stack sector, restore offset
        push    af
        add     hl,de			; now AHL=address of record
        pop     af
        pop     de			; restore sector
        ret     

l13bd:  push    bc
        push    af
        ex      de,hl
        ld      a,(ix+$02)
        sub     $02
        call    nz,l04f5
        call    x1896
        ex      de,hl
        ld      b,d
        ld      a,d
        and     $01
        ld      d,a
        ex      de,hl
        xor     b
        rrca    
        add     a,e
        ld      e,a
        adc     a,d
        sub     e
        ld      d,a
        pop     af
        pop     bc
        ret     

l13dc:  push    bc
        ld      h,b
        ld      l,c
        ld      a,(ix+$03)
        and     e
        rra     
        ld      b,a
        ld      a,$00
        rra     
        ld      c,a
        ld      a,(ix+$02)
        call    l04eb
        ld      d,$00
        ld      a,(ix+$06)
        or      a
        ld      a,e
        jr      z,l1403                 ; (+$0b)
        and     $07
        add     a,a
        add     a,$11
        ld      e,a
        add     hl,de
        ld      d,(hl)
        dec     hl
        jr      l1409                   ; (+$06)
l1403:  and     $0f
        add     a,$10
        ld      e,a
        add     hl,de
l1409:  ld      e,(hl)
        ld      a,d
        or      e
        ld      a,$19
        jr      z,l1412                 ; (+$02)
        ex      de,hl
        scf     
l1412:  ld      d,b
        ld      e,c
        pop     bc
        ret     

l1416:  push    hl
        ld      a,e
        and     $7f
        inc     a
        ld      hl,$000f
        add     hl,bc
        ld      (hl),a
        ld      a,e
        rla     
        ld      a,d
        rla     
        and     $1f
        dec     hl
        dec     hl
        dec     hl
        ld      (hl),a
        ld      hl,$0022
        add     hl,bc
        set     0,(hl)
        scf     
        pop     hl
        ret     

l1433:  push    de
        push    hl
        call    l1445
        or      a
        ld      a,$22
        jr      nz,x1442                ; (+$05)
        ex      de,hl
        sbc     hl,de
        ld      a,$19
x1442:  pop     hl
        pop     de
        ret     

l1445:  push    de
        ld      hl,$000c
        add     hl,bc
        ld      d,(hl)
        ld      e,$00
        srl     d
        rr      e
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        or      a
        jp      p,l145b
        ld      a,$80
l145b:  add     a,e
        ld      e,a
        adc     a,d
        sub     e
        dec     hl
        ld      l,(hl)
        ld      h,$00
        add     hl,hl
        add     hl,hl
        add     hl,hl
        add     hl,hl
        add     a,l
        ld      d,a
        adc     a,h
        sub     d
        ex      de,hl
        pop     de
        ret     

        ld      a,e
        and     $03
        ld      h,a
        ld      l,$00
        srl     h
        rr      l
        ld      a,$02
        call    l04eb
        push    hl
        push    de
        ex      de,hl
        ld      a,(ix+$02)
        ld      e,(ix+$05)
        ld      d,(ix+$06)
        inc     de
        sub     $02
        call    nz,l04f5
        call    x1896
        or      a
        sbc     hl,de
        pop     de
        pop     hl
        ld      a,$19
        jp      x13b2
l149c:  ld      de,(cachenum)
        ret     

l14a1:  call    l1672
        ret     nc

l14a5:  ld      hl,x0000
        ld      (bcb_inuse),hl
        ld      (bcb_free),hl
        ld      h,d
        ld      (cachenum),hl
        ld      ix,bcbs
        ld      b,$10
        ld      a,$07
        ld      hl,cache7
        jr      l14cd                   ; (+$0e)
l14bf:  ld      a,e
        or      a
        scf     
        ret     z

        ld      hl,cachenum
        inc     (hl)
        ld      a,d
        inc     d
        dec     e
        call    l0233
l14cd:  ld      (ix+$08),l
        ld      (ix+$09),h
        ld      (ix+$0a),a
        ld      hl,(bcb_free)
        ld      (ix+$00),l
        ld      (ix+$01),h
        ld      (bcb_free),ix
        ex      de,hl
        ld      de,$000b
        add     ix,de
        ex      de,hl
        djnz    l14bf                   ; (-$2d)
        scf     
        ret     

l14ee:  ld      de,bcb_inuse
l14f1:  ex      de,hl
        call    l1707
        ccf     
        ld      a,$21
        ret     z

        push    hl
        ld      hl,$0005
        add     hl,de
        ld      a,(hl)
        cp      (ix+$1c)
        jr      nz,x150c                ; (+$08)
        inc     hl
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        or      a
        sbc     hl,bc
x150c:  pop     hl
        jr      nz,l14f1                ; (-$1e)
        scf     
        ret     

l1511:  call    l1704
        jr      nz,l1530                ; (+$1a)
        ld      de,bcb_inuse
l1519:  ex      de,hl
        call    l1707
        ccf     
        ld      a,$21
        ret     z

        call    l170f
        jr      nz,l1519                ; (-$0d)
        call    x1547
        call    l1658
        ret     nc

        call    l1716
l1530:  push    hl
        ld      hl,$0002
        add     hl,de
        xor     a
        ld      (hl),a
        inc     hl
        ld      (hl),a
        inc     hl
        ld      (hl),a
        inc     hl
        ld      a,(ix+$1c)
        ld      (hl),a
        inc     hl
        ld      (hl),c
        inc     hl
        ld      (hl),b
        pop     hl
        scf     
        ret     

x1547:  push    ix
        push    hl
        push    de
        push    bc
        ld      hl,$0005
        add     hl,de
        ld      a,(hl)
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        ex      de,hl
        ld      d,a
        call    l05fd
        pop     bc
        pop     de
        pop     hl
        pop     ix
        ret     

l1560:  bit     3,(ix+$1b)
        jp      nz,l1b35
        push    de
        push    bc
        ld      b,d
        ld      c,e
        call    l14ee
        jr      c,l1576                 ; (+$06)
        call    l1511
        call    c,l16e4
l1576:  jr      l1589                   ; (+$11)
l1578:  bit     3,(ix+$1b)
        jp      nz,l1b35
        push    de
        push    bc
        ld      b,d
        ld      c,e
        call    l14ee
        call    nc,l1511
l1589:  push    af
        call    c,l171e
        pop     af
        jr      l15af                   ; (+$1f)
x1590:  bit     3,(ix+$1b)
        jp      nz,l1b35
        push    de
        push    bc
        push    bc
        ld      b,d
        ld      c,e
        call    l14ee
        pop     bc
        jr      nc,l15af                ; (+$0d)
        push    hl
        ld      hl,$0002
        add     hl,de
        set     0,(hl)
        inc     hl
        ld      (hl),c
        inc     hl
        ld      (hl),b
        pop     hl
        scf     
l15af:  jr      nc,l15bc                ; (+$0b)
        ld      hl,$0008
        add     hl,de
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        ld      a,(hl)
        ex      de,hl
        scf     
l15bc:  pop     bc
        pop     de
        ret     

l15bf:  call    l1829
        ret     c

        push    hl
        push    de
        push    bc
        ld      e,(ix+$07)
        ld      d,(ix+$08)
        inc     de
        ld      a,$04
        call    l04eb
        call    x1896
        dec     de
        jr      l15de                   ; (+$06)
l15d8:  push    hl
        push    de
        push    bc
        ld      de,$ffff
l15de:  ld      b,d
        ld      c,e
        ld      de,bcb_inuse
l15e3:  ex      de,hl
x15e4:  call    l1707
        jr      z,l1654                 ; (+$6b)
        push    hl
        ld      hl,$0005
        add     hl,de
        ld      a,(hl)
        cp      (ix+$1c)
        jr      nz,l1610                ; (+$1c)
        inc     hl
        ld      a,c
        sub     (hl)
        inc     hl
        ld      a,b
        sbc     a,(hl)
        jr      c,l1610                 ; (+$14)
        call    x1547
        dec     hl
        dec     hl
        dec     hl
        dec     hl
        dec     hl
        bit     0,(hl)
        jr      nz,l1610                ; (+$08)
        pop     hl
        push    hl
        call    l1716
        pop     hl
        jr      x15e4                   ; (-$2c)
l1610:  pop     hl
        jr      l15e3                   ; (-$30)
l1613:  push    hl
        push    de
        push    bc
        ld      de,bcb_inuse
l1619:  ex      de,hl
l161a:  call    l1707
        jr      z,l1654                 ; (+$35)
        push    hl
        ld      hl,$0005
        add     hl,de
        ld      a,(hl)
        pop     hl
        cp      (ix+$1c)
        jr      nz,l1619                ; (-$12)
        push    hl
        call    l1716
        pop     hl
        jr      l161a                   ; (-$18)
l1632:  push    hl
        push    de
        push    bc
        ld      de,bcb_inuse
l1638:  ex      de,hl
l1639:  call    l1707
        jr      z,l1654                 ; (+$16)
        push    hl
        ld      hl,$0003
        add     hl,de
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        or      a
        sbc     hl,bc
        pop     hl
        jr      nz,l1638                ; (-$15)
        push    hl
        call    l1716
        pop     hl
        jr      l1639                   ; (-$1b)
l1654:  pop     bc
        pop     de
        pop     hl
        ret     

l1658:  push    ix
        push    hl
        ld      hl,$0002
        add     hl,de
        bit     0,(hl)
        scf     
        jr      z,l166e                 ; (+$0a)
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        call    l17cb
        call    l1685
l166e:  pop     hl
        pop     ix
        ret     

l1672:  push    hl
        push    de
        ld      de,bcb_inuse
l1677:  ex      de,hl
        call    l1707
        jr      z,l1682                 ; (+$05)
        call    l1658
        jr      c,l1677                 ; (-$0b)
l1682:  pop     de
        pop     hl
        ret     

l1685:  push    hl
        push    de
        push    bc
l1688:  ld      de,bcb_inuse
        ld      bc,$ffff
        call    l16a6
        jr      z,l16a2                 ; (+$0f)
l1693:  push    de
        call    l16a6
        jr      z,l169c                 ; (+$03)
        pop     af
        jr      l1693                   ; (-$09)
l169c:  pop     de
        call    l16ce
        jr      c,l1688                 ; (-$1a)
l16a2:  pop     bc
        pop     de
        pop     hl
        ret     

l16a6:  ex      de,hl
        call    l1707
        ret     z

        ld      hl,$0002
        add     hl,de
        bit     0,(hl)
        jr      z,l16a6                 ; (-$0d)
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        cp      (ix+$1c)
        jr      nz,l16a6                ; (-$16)
        inc     hl
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        or      a
        sbc     hl,bc
        add     hl,bc
        jr      z,l16c9                 ; (+$02)
        jr      nc,l16a6                ; (-$23)
l16c9:  ld      b,h
        ld      c,l
        scf     
        sbc     a,a
        ret     

l16ce:  push    de
        call    l16f1
        call    l18e8
        pop     de
        ret     nc

        ld      hl,$0002
        add     hl,de
        res     0,(hl)
        inc     hl
        xor     a
        ld      (hl),a
        inc     hl
        ld      (hl),a
        scf     
        ret     

; Subroutine to read buffer (DE=BCB) from disk

l16e4:  push    hl
        push    de
        push    bc
        call    l16f1			; get buffer & sector details
        call    l18df			; read the sector
        pop     bc
        pop     de
        pop     hl
        ret     

l16f1:  ld      hl,$000a
        add     hl,de
        ld      b,(hl)
        dec     hl
        ld      d,(hl)
        dec     hl
        ld      e,(hl)
        push    de
        dec     hl
        ld      d,(hl)
        dec     hl
        ld      e,(hl)
        call    l18b6
        pop     hl
        ret     

l1704:  ld      hl,bcb_free
l1707:  ld      e,(hl)
        inc     hl
        ld      d,(hl)
        dec     hl
        ld      a,d
        or      e
        scf     
        ret     

l170f:  ex      de,hl
        ld      a,(hl)
        inc     hl
        or      (hl)
        dec     hl
        ex      de,hl
        ret     

l1716:  call    l172f
        ld      hl,bcb_free
        jr      l1724                   ; (+$06)
l171e:  call    l172f
        ld      hl,bcb_inuse
l1724:  ld      a,(hl)
        ld      (de),a
        inc     hl
        inc     de
        ld      a,(hl)
        ld      (de),a
        dec     de
        ld      (hl),d
        dec     hl
        ld      (hl),e
        ret     

l172f:  ld      a,(de)
        ld      (hl),a
        inc     hl
        inc     de
        ld      a,(de)
        ld      (hl),a
        dec     de
        ret     

l1737:  ld      a,'A'
        ld      (unit0),a
        ld      hl,l176e
        ld      de,xdpb_a+$1b
        ld      bc,$0015
        ldir    
        ld      hl,xdpb_a
        ld      (xdpb_ptrs),hl
        ld      hl,l1783
        ld      de,xdpb_b+$1b
        ld      bc,$0015
        ldir    
        ld      hl,xdpb_b
        ld      ($e2a2),hl
        ld      c,$01
        call    l1935
        call    l1f4f
        ret     nc

        ld      a,$03
        ld      l,$42
        jp      l3227

; The extended XDPB info for drive A:

l176e:  .db    $04,'A',$00     ; flags,drive,unit
        .db    $00,$00,$00,$00 ; last access,filesopen
        defw    $0000,$0000     ; #free direntries,last used
        defw    chksm_a,alloc_a ; checksum vector,alloc bitmap
        defw    l1988           ; login disk
        defw    l197c           ; read sector
        defw    l1982           ; write sector

; The extended XDPB info for drive B:

l1783:  .db    $04,'B',$01     ; flags,drive,unit
        .db    $00,$00,$00,$00 ; last access,filesopen
        defw    $0000,$0000     ; #free direntries,lastused
        defw    chksm_b,alloc_b ; checksum vector,alloc bitmap
        defw    l1988           ; login disk
        defw    l197c           ; read sector
        defw    l1982           ; write sector

x1798:  push    hl
        ld      hl,l17ae
        ld      de,xdpb_m+$1b
        ld      bc,$0015
        ldir    
        ld      hl,xdpb_m
        ld      ($e2b8),hl
        pop     hl
        jp      l1ac9

; The extended XDPB info for drive M:

l17ae:	.db    $08,'M',$ff     ; flags,drive,unit
        .db    $00,$00,$00,$00 ; last access,filesopen
        defw    $0000,$0000     ; #free direntries,lastused
        defw    $0000,alloc_m   ; no checksum;alloc bitmap
        defw    l1845           ; login disk
        defw    l1845           ; read sector
        defw    l1845           ; write sector

l1845:	scf     
        ret     

l17c5:  call    l04ff
        ld      hl,xdpb_ptrs
l17cb:  sub     $41
        jr      c,l17eb                 ; (+$1c)
        cp      $10
        jr      nc,l17eb                ; (+$18)
        push    hl
        add     a,a
        add     a,$a0
        ld      l,a
        adc     a,$e2
        sub     l
        ld      h,a
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        push    hl
        pop     ix
        ld      a,h
        or      l
        add     a,$ff
        pop     hl
        ld      a,$16
        ret     

l17eb:  ld      a,$15
        or      a
        ret     

l17ef:  call    l17cb
        ret     nc

        push    hl
        push    de
        push    bc
        call    l180d
        bit     0,(ix+$1b)
        scf     
        call    z,l1805
        pop     bc
        pop     de
        pop     hl
        ret     

l1805:  ld      a,(ix+$1a)
        rla     
        ret     c

        jp      l18d3
l180d:  bit     0,(ix+$1b)
        ret     z

        ld      a,(ix+$21)
        or      a
        ret     nz

        call    l1829
        ret     c

l181b:  ld      a,(ix+$21)
        or      a
        ld      a,$24
        ret     nz

        res     0,(ix+$1b)
        jp      l1613
l1829:  bit     7,(ix+$0c)
        scf     
        ret     nz

        push    hl
        push    de
        push    bc
        ld      a,r
        di      
        ld      a,(FRAMES)
        ld      hl,($5c79)
        jp      po,l183f
        ei      
l183f:  ld      b,a
        ld      a,(ix+$1e)
        ld      e,(ix+$1f)
        ld      d,(ix+$20)
        add     a,$64
        jr      nc,l184e                ; (+$01)
        inc     de
l184e:  ld      c,a
        ld      a,b
        sub     c
        sbc     hl,de
        push    af
        ld      hl,FRAMES
        ld      a,r
        di      
        ld      a,(hl)
        ld      (ix+$1e),a
        inc     hl
        ld      a,(hl)
        ld      (ix+$1f),a
        inc     hl
        ld      a,(hl)
        ld      (ix+$20),a
        jp      po,l186c
        ei      
l186c:  pop     af
        pop     bc
        pop     de
        pop     hl
        ret     

x1871:  push    hl
        push    de
        push    bc
        call    l187b
        pop     bc
        pop     de
        pop     hl
        ret     

l187b:  bit     2,(ix+$1b)
        scf     
        ret     z

        call    l190a
        call    l1edb
        ret     nc

        call    l1f5b
        ld      c,a
        and     $20
        ret     z

        bit     6,c
        ld      a,$01
        ret     nz

        scf     
        ret     

x1896:  push    hl
        push    bc
        ld      c,(ix+$0d)
        ld      b,(ix+$0e)
        ex      de,hl
        ld      e,(ix+$00)
        ld      d,(ix+$01)
        ld      a,$02
        call    l04eb
        jr      l18ae                   ; (+$02)
l18ac:  add     hl,de
        dec     bc
l18ae:  ld      a,b
        or      c
        jr      nz,l18ac                ; (-$06)
        ex      de,hl
        pop     bc
        pop     hl
        ret     

l18b6:  push    hl
        push    bc
        ex      de,hl
        ld      e,(ix+$00)
        ld      d,(ix+$01)
        ld      a,$02
        call    l04eb
        ld      bc,$ffff
        or      a
l18c8:  inc     bc
        sbc     hl,de
        jr      nc,l18c8                ; (-$05)
        add     hl,de
        ex      de,hl
        ld      d,c
        pop     bc
        pop     hl
        ret     

l18d3:  push    hl
        ld      l,(ix+$2a)
        ld      h,(ix+$2b)
        ld      de,x0000
        jr      l18ef                   ; (+$10)
l18df:  push    hl
        ld      l,(ix+$2c)
        ld      h,(ix+$2d)
        jr      l18ef                   ; (+$07)
l18e8:  push    hl
        ld      l,(ix+$2e)
        ld      h,(ix+$2f)
l18ef:  ld      (rt_temp),hl
        pop     hl
l18f3:  push    hl
        push    de
        push    bc
        call    l1903
        pop     bc
        pop     de
        pop     hl
        ret     c

        call    l1a9d
        jr      z,l18f3                 ; (-$0f)
        ret     

l1903:  push    hl
        ld      hl,(rt_temp)
        ex      (sp),hl
        ret     

l1909:  jp      (hl)
l190a:  push    hl
        ld      c,(ix+$1d)
        ld      a,c
        or      a
        jr      nz,l1927                ; (+$15)
        ld      hl,unit0
        ld      a,(ix+$1c)
        cp      (hl)
        jr      z,l1927                 ; (+$0c)
        ld      (hl),a
        push    ix
        push    de
        push    bc
        call    l1929
        pop     bc
        pop     de
        pop     ix
l1927:  pop     hl
        ret     

l1929:  push    af
        ld      c,a
        call    l0325
        pop     af
        push    hl
        ld      hl,(rt_chgdsk)
        ex      (sp),hl
        ret     

l1935:  ld      ix,xdpb_b
        call    l181b
        ret     nc

        ld      a,c
        or      a
        jr      z,l1944                 ; (+$03)
        ld      hl,x0000
l1944:  ld      de,(rt_chgdsk)
        ld      (rt_chgdsk),hl
        ld      ix,xdpb_b
        ld      (ix+$1d),c
        call    l1f86
        jr      nc,l1963                ; (+$0c)
        ld      a,c
        or      a
        scf     
        call    nz,l1f4f
        jr      c,l1963                 ; (+$04)
        ld      (ix+$1d),$00
l1963:  scf     
        ex      de,hl
        ret     

l1966:  ld      (retry_cnt),a
        ret     

l196a:  ld      a,$0a
        or      a
        jp      l22c4

	defs	12

; Low-level read sector subroutine for drives A: & B:

l197c:	call    l190a			; check if disk change required
        jp      l1c75			; DD_READ_SECTOR

; Low-level write sector subroutine for drives A: & B:

l1982:	call    l190a			; check if disk change required
        jp      x1c83			; DD_WRITE_SECTOR

; Low-level login disk subroutine for drives A: & B:

l1988:	call    l190a			; check if disk change required
        call    l1cf6			; DD_LOGIN
        ret     nc			; exit if error
        ld      a,(ix+$0f)
        xor     $02
        ld      a,$06			; "unrecognised disk format"
        ret     nz			; error if sectorsize <> 512
        rr      d
        rr      e
        ld      hl,$ffd2
        add     hl,de
        ccf     
        ret     nc			; error if alloc vector size/2 >$2d
        ld      e,(ix+$0b)
        ld      a,(ix+$0c)
        and     $7f
        ld      d,a
        ld      hl,$ffbf
        add     hl,de
        ccf     
        ret     c			; success if chksum size <= $40
        ld      (ix+$0b),$40
        ld      a,(ix+$0c)
        and     $80
        or      $00
        ld      (ix+$0c),a		; else set chksum size to $40
        scf     
        ret     

l19c0:  ld      a,c
        ld      (rw_page),a
        ld      (rw_add),hl
        call    x0521
        ret     nc

        add     hl,de
        push    hl
        call    l19dd
        pop     hl
        ret     c

        push    af
        ld      de,(rw_add)
        or      a
        sbc     hl,de
        ex      de,hl
        pop     af
        ret     

l19dd:  push    bc
        push    de
        ld      hl,$0023
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        ld      a,e
        scf     
        sbc     a,(hl)
        ld      e,a
        inc     hl
        ld      a,d
        sbc     a,(hl)
        ld      d,a
        inc     hl
        ld      a,b
        sbc     a,(hl)
        ex      de,hl
        pop     de
        pop     bc
        jr      c,l1a06                 ; (+$0d)
        dec     de
        sbc     hl,de
        add     hl,de
        sbc     a,$00
        jr      nc,l1a0a                ; (+$09)
        ex      de,hl
        call    l1a0a
        ret     nc

l1a06:  ld      a,$19
        or      a
        ret     

l1a0a:  inc     de
        ld      a,d
        and     $fe
        jr      z,l1a65                 ; (+$55)
        push    bc
        push    de
        xor     a
        ld      hl,$f516
        call    l3886
        pop     de
        pop     bc
        jr      nc,l1a65                ; (+$48)
        ld      ix,$f518
        push    de
        ld      hl,$0026
        add     hl,bc
        ld      a,(hl)
        add     a,e
        ld      (hl),a
        inc     hl
        ld      a,(hl)
        adc     a,d
        ld      (hl),a
        inc     hl
        ld      a,(hl)
        adc     a,$00
        ld      (hl),a
        ld      hl,$0022
        add     hl,bc
        res     5,(hl)
        ld      l,(ix-$02)
        ld      h,(ix-$01)
        call    l276d
        call    x27c2
        ld      hl,(rw_add)
        jr      l1a4c                   ; (+$03)
l1a49:  call    l276d
l1a4c:  ld      a,(rw_page)
        call    l021b
        ex      (sp),hl
        and     a
        sbc     hl,de
        jr      c,l1a8b                 ; (+$33)
        ex      (sp),hl
        call    l25b0
        ex      (sp),hl
        ld      a,h
        or      l
        ex      (sp),hl
        jr      nz,l1a49                ; (-$19)
        pop     de
        jr      l1a98                   ; (+$33)
l1a65:  dec     de
        ld      hl,$0026
        add     hl,bc
        ld      a,(hl)
        and     $7f
        jr      z,l1a77                 ; (+$08)
        call    l10d9
        ret     nc

        ret     z

        jp      l1a0a
l1a77:  ld      hl,$ff81
        add     hl,de
        jr      nc,l1a84                ; (+$07)
        call    l10f6
        ret     nc

        ret     z

        jr      l1a77                   ; (-$0d)
l1a84:  call    l10d9
        ret     nc

        ret     z

        jr      l1a84                   ; (-$07)
l1a8b:  add     hl,de
        ex      de,hl
        and     a
        sbc     hl,de
        ex      (sp),hl
        call    l25b0
        pop     de
        call    l27ce
l1a98:  ld      (rw_add),hl
        scf     
        ret     

l1a9d:  push    ix
        push    hl
        push    de
        push    bc
        call    l3ee6
        ld      c,(ix+$1c)
        call    l02ff
        pop     bc
        pop     de
        pop     hl
        pop     ix
        ret     

l1ab1:  ld      a,($e3f4)
        ld      h,a
        ld      a,($e3f1)
        sub     h
        ld      l,a
        ret     

l1abb:  ld      ix,xdpb_m
        ld      a,(ix+$1c)
        and     a
        jr      z,l1ac9                 ; (+$04)
        call    l181b
        ret     nc

l1ac9:  push    hl
        ld      hl,l1b2d
        ld      de,spec_m
        ld      bc,$0008
        ldir    
        pop     de
        ld      a,e
        cp      $04
        jr      c,l1b11                 ; (+$36)
        add     a,d
        ld      ($e3f1),a
        ld      a,d
        ld      ($e3f4),a
l1ae3:  ld      a,d
        push    de
        call    l0233
        call    l021b
        ld      d,h
        ld      e,l
        inc     de
        ld      (hl),$e5
        ld      bc,$01ff
        ldir    
        call    l021b
        pop     de
        inc     d
        dec     e
        jr      nz,l1ae3                ; (-$1a)
        ld      ix,xdpb_m
        ld      hl,spec_m
        call    l1da6
        ld      (ix+$0b),$00
        ld      (ix+$0c),$80
        scf     
        ret     

l1b11:  ld      a,(ix+$1c)
        ld      (ix+$1c),$00
        call    l04ff
        sub     $41
        ret     c

        ld      e,a
        ld      d,$00
        ld      hl,xdpb_ptrs
        add     hl,de
        add     hl,de
        ld      (hl),$00
        inc     hl
        ld      (hl),$00
        scf     
        ret     

l1b2d:	.db    $00,$00,$00,$01
        .db    $02,$00,$03,$00

l1b35:  push    de
        call    l18b6
        ld      a,e
        or      a
        jr      nz,x1b48                ; (+$0b)
        ld      a,d
        ld      hl,$e3f1
        cp      (hl)
        jr      nc,x1b48                ; (+$04)
        call    l0233
        scf     
x1b48:  pop     de
        ret     c

        ld      a,$02
        ret     

l1b4d:  ld      a,$41
        call    l17ef
        call    c,l181b
        ld      de,x0000
        call    c,l1560
        ret     nc

        ld      c,a
        push    hl
        call    l021b
        push    af
        xor     a
        ld      b,a
        ld      e,$02
l1b66:  add     a,(hl)
        inc     hl
        djnz    l1b66                   ; (-$04)
        dec     e
        jr      nz,l1b66                ; (-$07)
        ld      e,a
        pop     af
        call    l021b
        pop     hl
        ld      a,e
        xor     $03
        ld      a,$23
        ret     nz

        di      
        ld      b,$03
        ld      de,$fe00
        ld      ix,$0200
        call    l0245
        ld      a,$03
        call    l021b
        ld      hl,$1ba1
        ld      de,$fdfb
        ld      bc,$0005
        ldir    
        ld      bc,$1ffd
        ld      a,$07
        ld      sp,$fe00
        jp      $fdfb
        out     (c),a
        jp      $fe10
l1ba6:  ld      a,(ix+$19)
        and     $40
        or      $0d
        call    x1c12
        ld      l,(ix+$0f)
        ld      h,(ix+$13)
        ld      ($e408),hl
        ld      h,e
        ld      l,(ix+$18)
        ld      ($e40a),hl
        ld      a,$06
        ld      ($e405),a
        ret     

l1bc6:  ld      a,(ix+$19)
        or      $11
        call    l1bdf
        ld      (hl),$01
        ret     

l1bd1:  ld      a,(ix+$19)
        or      $06
        jr      l1bdf                   ; (+$07)
l1bd8:  ld      a,(ix+$19)
        and     $c0
        or      $05
l1bdf:  call    x1c12
        ld      a,e
        add     a,(ix+$14)
        ld      e,a
        push    de
        ld      hl,(rt_encode)
        ld      a,h
        or      l
        call    nz,l1909
        ld      a,e
        ld      ($e40a),a
        ld      l,(ix+$0f)
        ld      h,e
        ld      ($e40b),hl
        ld      a,(ix+$17)
        ld      ($e40d),a
        ld      h,b
        ld      l,d
        ld      ($e408),hl
        ld      a,$09
        ld      ($e405),a
        ld      hl,$e40e
        ld      (hl),$ff
        pop     de
        ret     

x1c12:  ld      ($e401),hl
        ld      l,a
        ld      a,b
        ld      (ddl_parms),a
        call    l1c2b
        ld      h,c
        ld      ($e406),hl
        ld      l,(ix+$15)
        ld      h,(ix+$16)
        ld      ($e403),hl
        ret     

l1c2b:  ld      a,(ix+$11)
        and     $7f
        ld      b,$00
        ret     z

        dec     a
        jr      nz,x1c3e                ; (+$08)
        ld      a,d
        rra     
        ld      d,a
        ld      a,b
        rla     
        ld      b,a
        jr      l1c4a                   ; (+$0c)
x1c3e:  ld      a,d
        sub     (ix+$12)
        jr      c,l1c4a                 ; (+$06)
        sub     (ix+$12)
        cpl     
        ld      d,a
        inc     b
l1c4a:  ld      a,b
        add     a,a
        add     a,a
        or      c
        ld      c,a
        ret     

l1c50:  or      a
        jr      nz,l1c56                ; (+$03)
        ld      hl,x0000
l1c56:  ld      de,(rt_encode)
        ld      (rt_encode),hl
        ex      de,hl
        ret     

l1c5f:  push    af
        call    l1bd1
        pop     af
        ld      l,a
        ld      h,$00
        ld      ($e403),hl
        ld      hl,$1c6f
        jr      l1cc5                   ; (+$56)
        ld      hl,ddl_parms
        jp      l2119
l1c75:  call    l1bd1
        ld      hl,$1c7d
        jr      l1cc5                   ; (+$48)
        ld      hl,ddl_parms
        jp      l3ef5
x1c83:  call    l1edb
        ret     nc

        call    l1bd8
        jr      l1ca1                   ; (+$15)
x1c8c:  call    l1bc6
        call    l1ca1
        ret     nc

        ld      a,($e433)
        cp      $08
        scf     
        ret     

l1c9a:  call    l1edb
        ret     nc

        call    l1ba6
l1ca1:  ld      hl,$1ca6
        jr      l1cc5                   ; (+$1f)
        ld      hl,ddl_parms
        jp      l2122
l1cac:  call    l1cb7
        ld      hl,fdc_res
        ret     nc

        ld      a,($e436)
        ret     

l1cb7:  call    l1c2b
        ld      hl,$1cbf
        jr      l1cc5                   ; (+$06)
        ld      a,(ix+$19)
        jp      l2159

; Routine to turn on motor, try an operation in HL on track D multiple
; times & then start the motor off timeout

l1cc5:  call    l2181			; turn on motor
        call    l1ef2			; try the operation multiple times
        jp      l21a6			; start motor off timeout & exit

; Tables of specifications for disk types 0-3
; Format as on p215 of +3 Manual

; Type 0 - Spectrum +3 format

l1cce:  .db    $00,$00,$28,$09
        .db    $02,$01,$03,$02
        .db    $2a,$52

; Type 1 - CPC system format

        .db    $01,$00,$28,$09
        .db    $02,$02,$03,$02
        .db    $2a,$52

; Type 2 - CPC data format

        .db    $02,$00,$28,$09
        .db    $02,$00,$03,$02
        .db    $2a,$52

; Type 3 - PCW format

        .db    $03,$81,$50,$09
        .db    $02,$01,$04,$04
        .db    $2a,$52

l1cf6:  xor     a
        call    l1d51
        ld      d,$00
        push    bc
        call    c,l1cac
        pop     bc
        ret     nc

        and     $c0
        ld      e,$01
        cp      $40
        jr      z,l1d0f                 ; (+$05)
        inc     e
        cp      $c0
        jr      nz,l1d15                ; (+$06)
l1d0f:  ld      a,e
        call    l1d51
        jr      x1d49                   ; (+$34)
l1d15:  push    bc
        ld      hl,$e40f
        ld      de,x0000
        ld      b,$07
        ld      a,$0a
        push    hl
        call    l1c5f
        pop     hl
        pop     bc
        jr      c,l1d30                 ; (+$08)
        cp      $08
        scf     
        ccf     
        ret     nz

        ld      a,$06
        ret     

l1d30:  push    bc
        ld      d,h
        ld      e,l
        ld      c,(hl)
        ld      b,$0a
l1d36:  ld      a,(de)
        inc     de
        cp      c
        jr      nz,l1d40                ; (+$05)
        djnz    l1d36                   ; (-$07)
        ld      hl,l1cce
l1d40:  pop     bc
        ld      a,(hl)
        cp      $04
        ld      a,$06
        call    c,l1d64
x1d49:  push    hl
        push    bc
        call    c,l1e64
        pop     bc
        pop     hl
        ret     

l1d51:  ld      e,a
        cp      $04
        ld      a,$06
        ret     nc

        ld      a,e
        add     a,a
        ld      e,a
        add     a,a
        add     a,a
        add     a,e
        adc     a,l1cce AND $ff
        ld      l,a
        adc     a,l1cce/$100
        sub     l
        ld      h,a
l1d64:  push    hl
        push    bc
        ld      a,(hl)
        ld      b,$41
        dec     a
        jr      z,l1d73                 ; (+$07)
        ld      b,$c1
        dec     a
        jr      z,l1d73                 ; (+$02)
        ld      b,$01
l1d73:  ld      (ix+$14),b
        inc     hl
        ld      a,(hl)
        ld      (ix+$11),a
        inc     hl
        ld      a,(hl)
        ld      (ix+$12),a
        inc     hl
        ld      a,(hl)
        ld      (ix+$13),a
        inc     hl
        ld      b,(hl)
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        ld      (ix+$17),a
        inc     hl
        ld      a,(hl)
        ld      (ix+$18),a
        ld      hl,$0080
        call    l1f6e
        ld      (ix+$15),l
        ld      (ix+$16),h
        ld      (ix+$19),$60
        pop     bc
        pop     hl
l1da6:  push    bc
        push    hl
        ex      de,hl
        ld      hl,$0004
        add     hl,de
        ld      a,(hl)
        ld      (ix+$0f),a
        push    af
        call    l1f64
        ld      (ix+$10),a
        dec     hl
        ld      l,(hl)
        ld      h,$00
        pop     bc
        call    l1f6e
        ld      (ix+$00),l
        ld      (ix+$01),h
        ld      hl,$0006
        add     hl,de
        ld      a,(hl)
        ld      (ix+$02),a
        ld      c,a
        push    hl
        call    l1f64
        ld      (ix+$03),a
        dec     hl
        ld      e,(hl)
        ld      (ix+$0d),e
        ld      (ix+$0e),$00
        dec     hl
        dec     hl
        ld      b,(hl)
        dec     hl
        ld      d,(hl)
        dec     hl
        ld      a,(hl)
        ld      l,d
        ld      h,$00
        ld      d,h
        and     $7f
        jr      z,l1def                 ; (+$01)
        add     hl,hl
l1def:  sbc     hl,de
        ex      de,hl
        ld      hl,x0000
l1df5:  add     hl,de
        djnz    l1df5                   ; (-$03)
        ld      a,c
        sub     (ix+$0f)
        ld      b,a
        call    l1f75
        dec     hl
        ld      (ix+$05),l
        ld      (ix+$06),h
        ld      b,$03
        ld      a,h
        or      a
        jr      z,l1e0e                 ; (+$01)
        inc     b
l1e0e:  ld      a,c
        sub     b
        call    l1f64
        ld      (ix+$04),a
        pop     de
        push    hl
        ld      b,$02
        call    l1f75
        inc     hl
        inc     hl
        ex      (sp),hl
        inc     de
        ld      a,(de)
        or      a
        jr      nz,l1e2d                ; (+$08)
        add     hl,hl
        ld      a,h
        inc     a
        cp      $02
        jr      nc,l1e2d                ; (+$01)
        inc     a
l1e2d:  ld      b,a
        ld      hl,x0000
l1e31:  scf     
        rr      h
        rr      l
        djnz    l1e31                   ; (-$07)
        ld      (ix+$09),h
        ld      (ix+$0a),l
        ld      h,$00
        ld      l,a
        ld      b,c
        inc     b
        inc     b
        call    l1f6e
        push    hl
        dec     hl
        ld      (ix+$07),l
        ld      (ix+$08),h
        ld      b,$02
        call    l1f75
        inc     hl
        ld      (ix+$0b),l
        ld      (ix+$0c),h
        pop     hl
        add     hl,hl
        add     hl,hl
        pop     de
        pop     bc
        ld      a,(bc)
        scf     
        pop     bc
        ret     

l1e64:  ld      b,a
        push    de
        push    bc
        call    l1e86
        pop     bc
        pop     de
        ret     nc

        ld      a,(ix+$11)
        and     $03
        jr      z,l1e78                 ; (+$04)
        bit     1,(hl)
        jr      z,l1e82                 ; (+$0a)
l1e78:  ld      a,b
        scf     
        bit     7,(ix+$11)
        ret     z

        bit     3,(hl)
        ret     nz

l1e82:  ld      a,$09
        or      a
        ret     

l1e86:  call    l1fc9
        ld      a,(hl)
        and     $0c
        jr      z,l1e9a                 ; (+$0c)
        ld      a,(hl)
        and     $03
        scf     
        ret     nz

        ld      a,(ix+$11)
        and     $03
        scf     
        ret     z

l1e9a:  ld      a,(ix+$11)
        and     $03
        ld      d,$02
        jr      z,l1eaf                 ; (+$0c)
        dec     a
        ld      d,$05
        jr      z,l1eaf                 ; (+$07)
        ld      a,(ix+$12)
        add     a,a
        sub     $03
        ld      d,a
l1eaf:  push    hl
        call    l1cac
        pop     hl
        ret     nc

        ld      de,($e434)
        ld      a,(ix+$11)
        and     $03
        jr      z,l1ec9                 ; (+$09)
        dec     d
        jr      z,l1ec7                 ; (+$04)
        set     0,(hl)
        jr      l1ec9                   ; (+$02)
l1ec7:  set     1,(hl)
l1ec9:  ld      a,(ix+$11)
        dec     e
        dec     e
        jr      z,l1ed1                 ; (+$01)
        cpl     
l1ed1:  rla     
        jr      nc,l1ed7                ; (+$03)
        set     3,(hl)
        ret     

l1ed7:  set     2,(hl)
        scf     
        ret     

l1edb:  push    hl
        call    l1fc9
        bit     3,(hl)
        pop     hl
        scf     
        ret     z

        ld      a,(ix+$11)
        rla     
        ld      a,$09
        ret     

l1eeb:  call    l1fc9
        ld      a,(hl)
        and     $0f
        ret     

l1ef2:  ld      a,(retry_cnt)
        ld      b,a
l1ef6:  push    bc
        call    x1f22
        pop     bc
        ret     z

        cp      $04
        jr      nz,l1f1e                ; (+$1e)
        push    hl
        push    de
        push    bc
        ld      a,(ix+$19)
        call    l2159
        call    l20a9
        pop     bc
        pop     de
        pop     hl
        jr      nz,l1f1e                ; (+$0d)
        ret     nc

        ld      a,($e436)
        xor     (ix+$14)
        and     $c0
        ld      a,$08
        ret     nz

        rra     
l1f1e:  djnz    l1ef6                   ; (-$2a)
        or      a
        ret     

x1f22:  ld      a,b
        and     $07
        jr      z,l1f34                 ; (+$0d)
        and     $03
        jr      nz,l1f3e                ; (+$13)
        push    hl
        call    l1fc9
        res     6,(hl)
        pop     hl
        jr      l1f3e                   ; (+$0a)
l1f34:  push    de
        ld      d,(ix+$12)
        dec     d
        call    l1fd5
        pop     de
        ret     nc

l1f3e:  call    l1fd5
        ret     nc

        push    hl
        push    de
        push    bc
        call    l1909
        pop     bc
        pop     de
        call    l20a9
        pop     hl
        ret     

l1f4f:  push    bc
        ld      c,$01
        call    l1f5b
        pop     bc
        and     $60
        ret     z

        scf     
        ret     

l1f5b:  call    l2181
        call    l20e6
        jp      l21a6
l1f64:  or      a
        ret     z

        ld      b,a
        ld      a,$01
l1f69:  add     a,a
        djnz    l1f69                   ; (-$03)
        dec     a
        ret     

l1f6e:  ld      a,b
        or      a
        ret     z

l1f71:  add     hl,hl
        djnz    l1f71                   ; (-$03)
        ret     

l1f75:  ld      a,b
        or      a
        ret     z

l1f78:  srl     h
        rr      l
        djnz    l1f78                   ; (-$06)
        ret     

l1f7f:	.db    $0a             ; motor on time
        .db    $32             ; motor off time
        .db    $af             ; write off time
        .db    $1e             ; head settle time
        .db    $0c             ; step rate
        .db    $0f             ; head unload time
        .db    $03             ; head load time x2+1

l1f86:  push    bc
        ld      bc,$2ffd
        in      a,(c)
        add     a,$01
        ccf     
        pop     bc
        ret     

l1f91:  ld      hl,equipment
        ld      b,$10
x1f96:  ld      (hl),$00
        inc     hl
        djnz    x1f96                   ; (-$05)
        ld      a,$0f
        ld      (retry_cnt),a
        call    l3ee6
        ld      hl,l1f7f
x1fa6:  ld      de,tm_mtron
        ld      bc,$0005
        ldir    
        ld      a,(tm_step)
        dec     a
        rlca    
        rlca    
        rlca    
        cpl     
        and     $f0
        or      (hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        ld      a,$03
        call    x216a
        ld      a,l
        call    x216a
        ld      a,h
        jp      x216a
l1fc9:  ld      a,c
        and     $03
        add     a,a
        add     a,$20
        ld      l,a
        adc     a,$e4
        sub     l
        ld      h,a
        ret     

l1fd5:  push    hl
        call    l1fc9
        call    l1fde
        pop     hl
        ret     

l1fde:  ld      a,(retry_cnt)
        ld      b,a
l1fe2:  bit     6,(hl)
        jr      nz,l1ff1                ; (+$0b)
        inc     hl
        ld      (hl),$00
        dec     hl
        call    l2016
        jr      nc,x2007                ; (+$18)
        set     6,(hl)
l1ff1:  ld      a,d
        inc     hl
        cp      (hl)
        dec     hl
        scf     
        ret     z

        or      a
        jr      nz,l1fff                ; (+$05)
        call    l2016
        jr      l2002                   ; (+$03)
l1fff:  call    l203a
l2002:  jr      nc,l200c                ; (+$08)
        inc     hl
        ld      (hl),d
        ret     

x2007:  push    de
        call    nz,l2036
        pop     de
l200c:  res     6,(hl)
        ret     z

        call    l20ce
        djnz    l1fe2                   ; (-$32)
        cp      a
        ret     

l2016:  call    l201a
        ret     z

l201a:  push    bc
        ld      b,(ix+$12)
        dec     b
        bit     7,(ix+$11)
        jr      nz,x202c                ; (+$07)
        bit     3,(hl)
        jr      z,x202c                 ; (+$03)
        ld      a,b
        add     a,a
        ld      b,a
x202c:  ld      a,$07
        call    x216a
        ld      a,c
        and     $03
        jr      l205d                   ; (+$27)
l2036:  ld      d,(ix+$12)
        dec     d
l203a:  push    bc
        ld      a,d
        inc     hl
        sub     (hl)
        dec     hl
        jr      nc,l2043                ; (+$02)
        cpl     
        inc     a
l2043:  ld      b,a
        ld      a,$0f
        call    x216a
        ld      a,c
        call    x216a
        ld      a,d
        bit     7,(ix+$11)
        jr      nz,l205d                ; (+$09)
        bit     3,(hl)
        jr      z,l205d                 ; (+$05)
        ld      a,b
        add     a,a
        ld      b,a
        ld      a,d
        add     a,a
l205d:  push    hl
        call    x216a
l2061:  ld      a,(tm_step)
        call    l207b
        djnz    l2061                   ; (-$08)
        ld      a,(tm_hdset)
        call    l207b
        ld      hl,fdc_res
        call    l20df
        call    l2084
        pop     hl
        pop     bc
        ret     

l207b:  ld      l,$dc
l207d:  dec     l
        jr      nz,l207d                ; (-$03)
        dec     a
        jr      nz,l207b                ; (-$08)
        ret     

l2084:  ld      a,c
        or      $20
        inc     hl
        xor     (hl)
        and     $fb
        scf     
        ret     z

        ld      a,(hl)
        and     $c0
        xor     $80
        jr      z,l20a5                 ; (+$11)
        ld      a,(hl)
        xor     c
        and     $03
        jr      z,l209f                 ; (+$05)
        call    l20df
        jr      l2084                   ; (-$1b)
l209f:  ld      a,(hl)
        and     $08
        xor     $08
        ret     z

l20a5:  ld      a,$02
        or      a
        ret     

l20a9:  inc     hl
        ld      a,(hl)
        xor     c
        scf     
        ret     z

        and     $08
        xor     $08
        ret     z

        inc     hl
        ld      a,(hl)
        cp      $80
        scf     
        ret     z

        xor     $02
        ld      a,$01
        ret     z

        ld      a,$03
        bit     5,(hl)
        ret     nz

        inc     a
        bit     2,(hl)
        ret     nz

        inc     a
        bit     0,(hl)
        ret     nz

        inc     a
        inc     a
        ret     

l20ce:  push    hl
        push    af
        ld      hl,fdc_res
l20d3:  call    l20df
        and     $c0
        cp      $80
        jr      nz,l20d3                ; (-$09)
        pop     af
        pop     hl
        ret     

l20df:  ld      a,$08
        call    x216a
        jr      l20f2                   ; (+$0c)
l20e6:  ld      a,$04
        call    x216a
        ld      a,c
        call    x216a
l20ef:  ld      hl,fdc_res
l20f2:  push    de
        push    bc
        ld      bc,$2ffd
        ld      d,$00
        inc     hl
        push    hl
l20fb:  in      a,(c)
        add     a,a
        jr      nc,l20fb                ; (-$05)
        jp      p,l2112
        ld      b,$3f
        in      a,(c)
        ld      b,$2f
        ld      (hl),a
        inc     hl
        inc     d
        ex      (sp),hl
        ex      (sp),hl
        ex      (sp),hl
        ex      (sp),hl
        jr      l20fb                   ; (-$17)
l2112:  pop     hl
        ld      a,(hl)
        dec     hl
        ld      (hl),d
        pop     bc
        pop     de
        ret     

l2119:  call    l2134
        call    l21c5
        jp      l20ef
l2122:  call    l2134
        call    l2214
        ld      a,(tm_wroff)
l212b:  dec     a
        inc     bc
        inc     bc
        inc     bc
        jr      nz,l212b                ; (-$06)
        jp      l20ef

; Subroutine to get page (D) and address (HL) of buffer for low-level
; command, and output all command bytes except last (in A)
; On entry, HL=address of parameter block

l2134:  call    l20ce			; wait until ready for new command
        ld      a,(BANKM)		; get old BANKM
        and     $f8
        or      (hl)			; set page required
        ld      b,a
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)			; DE=buffer address
        inc     hl
        ld      c,(hl)			; C=# bytes to transfer (low)
        push    bc
        inc     hl
        inc     hl
        ld      b,(hl)			; B=# command bytes
        inc     hl
        dec     b
l214a:  ld      a,(hl)			; get next command byte
        inc     hl
        call    x216a			; send it
        djnz    l214a                   ; back for all except last
        ld      a,(hl)
        ex      de,hl
        pop     de			; D=page required, E=#bytes to transfer (low)
        ld      bc,$7ffd
        di      			; turn off interrupts
        ret     

l2159:  call    l20ce
        and     $40
        or      $0a
        call    x216a
        ld      a,c
        call    x216a
        jp      l20ef
x216a:  push    de
        push    bc
        ld      d,a
        ld      bc,$2ffd
l2170:  in      a,(c)
        add     a,a
        jr      nc,l2170                ; (-$05)
        add     a,a
        jr      c,l217e                 ; (+$06)
        ld      b,$3f
        out     (c),d
        ex      (sp),hl
        ex      (sp),hl
l217e:  pop     bc
        pop     de
        ret     

l2181:  push    bc
        push    af
        xor     a
        ld      (timeout),a
        ld      a,(BANK678)
        bit     3,a
        jr      nz,l21a3                ; (+$15)
        or      $08
        call    l21ba
        ld      a,(tm_mtron)
l2196:  push    af
        ld      bc,$3548
l219a:  dec     bc
        ld      a,b
        or      c
        jr      nz,l219a                ; (-$05)
        pop     af
        dec     a
        jr      nz,l2196                ; (-$0d)
l21a3:  pop     af
        pop     bc
        ret     

l21a6:  push    af
        xor     a
        ld      (timeout),a
        ld      a,(BANK678)
        and     $08
        jr      z,l21b8                 ; (+$06)
        ld      a,(tm_mtroff)
        ld      (timeout),a
l21b8:  pop     af
        ret     

l21ba:  push    bc
        ld      bc,$1ffd
        ld      (BANK678),a
        out     (c),a
        pop     bc
        ret     

l21c5:  call    x216a
        out     (c),d
        ld      bc,$2ffd
        ld      d,$20
        jr      l21db                   ; (+$0a)

l21d1:  ld      b,$3f
        ini     
        ld      b,$2f
        dec     e
        jp      z,l21ec
l21db:  in      a,(c)
        jp      p,l21db
        and     d
        jp      nz,l21d1
        jr      l222f                   ; (+$49)

l21e6:  ld      b,$3f
        in      a,(c)
        ld      b,$2f
l21ec:  in      a,(c)
        jp      p,l21ec
        and     d
        jp      nz,l21e6
        jr      l222f                   ; (+$38)

; Subroutine to output last byte of command to FDC and read bytes to buffer

l21f7:  call    x216a			; send command
        out     (c),d			; page in required bank		**** AVERIGUAR DE DONDE SALE "D" ****
        ld      bc,$2ffd
        ld      d,$20
        jr      l2209                   
l2203:  ld      b,$3f
        ini     			; read a byte
        ld      b,$2f
l2209:  in      a,(c)
        jp      p,l2209			; wait until FDC ready
        and     d
        jp      nz,l2203		; loop back if more bytes to read
        jr      l222f                   ; go to repage bank 7 & exit

l2214:  call    x216a
        out     (c),d
        ld      bc,$2ffd
        ld      d,$20
        jr      l2226                   ; (+$06)

l2220:  ld      b,$40
        outi    
        ld      b,$2f
l2226:  in      a,(c)
        jp      p,l2226
        and     d
        jp      nz,l2220
l222f:  ld      a,(BANKM)
        ld      bc,$7ffd
        out     (c),a
        ei      
        ret     

; ******************** KEYBOARD SCANNING ROUTINES *****************

; These are copies of the keytables from ROM 3

; The L-mode keytable with CAPS-SHIFT

l2239:  .db    "BHY65TGV"
        .db    "NJU74RFC"
        .db    "MKI83EDX"
        .db    $0e, "LO92WSZ"
        .db    " ", $0d, "P01QA"

; The extended-mode keytable (unshifted letters)

n22c5   .db    $e3,$c4,$e0,$e4
        .db    $b4,$bc,$bd,$bb
        .db    $af,$b0,$b1,$c0
        .db    $a7,$a6,$be,$ad
        .db    $b2,$ba,$e5,$a5
        .db    $c2,$e1,$b3,$b9
        .db    $c1,$b8

; The extended mode keytable (shifted letters)

n22df   .db    $7e,$dc,$da,$5c
        .db    $b7,$7b,$7d,$d8
        .db    $bf,$ae,$aa,$ab
        .db    $dd,$de,$df,$7f
        .db    $b5,$d6,$7c,$d5
        .db    $5d,$db,$b6,$d9
        .db    $5b,$d7

; The control code keytable (CAPS-SHIFTed digits)

n22f9   .db    $0c,$07,$06,$04
        .db    $05,$08,$0a,$0b
        .db    $09,$0f

; The symbol code keytable (letters with symbol shift)

n2303   .db    $e2,$2a,$3f,$cd
        .db    $c8,$cc,$cb,$5e
        .db    $ac,$2d,$2b,$3d
        .db    $2e,$2c,$3b,$22
        .db    $c7,$3c,$c3,$3e
        .db    $c5,$2f,$c9,$60
        .db    $c6,$3a

; The extended mode keytable (SYM-SHIFTed digits)

n231d   .db    $d0,$ce,$a8,$ca
        .db    $d3,$d4,$d1,$d2
        .db    $a9,$cf

l22c2:  xor     a
        ld      a,b
l22c4:  push    hl
        ld      hl,(CURCHL)
        ex      (sp),hl
        push    hl
        ld      hl,(FLAGS)
        ex      (sp),hl
        push    hl
        ld      hl,(FLAGS2)
        ex      (sp),hl
        ld      hl,$22f0
        ld      (al_resp),hl
        push    af
        call    l032d
        pop     af
        call    n3e00
        rst     $30
        ccf     
        pop     hl
        ld      (FLAGS2),hl
        pop     hl
        ld      (FLAGS),hl
        pop     hl
        ld      (CURCHL),hl
        ret     

        .db    $8b, 13, $fb, "Not ready", 13
        .db    $8e, $ff, $8b, 13, $fb, "Write protected", 13
        .db    $8e, $ff, $8c, 13, $fb, "Seek fail", 13
        .db    $8e, $ff, $8d, "Data error", 13
        .db    $8e, $ff, $8d, "No data", 13
        .db    $8e, $ff, $8d, "Missing address mark", 13
        .db    $8e, $ff, $8b, 13, $fb, "Bad format", 13
        .db    $8e, $ff, $8d, "Unknown error", 13
        .db    $8e, $ff, $8b, 13, $fb, "Disk changed, please replace", 13
        .db    $8e, $ff, $8b, 13, $fb, "Disk unsuitable", 13
        .db    $8e, $ff, "Please put the disk for ", $fe
        .db    ": into the drive then press any key", $ff
        .db    "Drive ", $fe
        .db    ":", $ff, $8b, " track ", $fd, $ff, $8c
        .db    " sector ", $fc, 13, $fb, $ff, $fa
        .db    "Retry, Ignore or Cancel?", $ff

	.ds	119

l249a:  ld      bc,$0200
l249d:  push    bc
        ld      a,$02
        sub     b
        ld      c,a
        ld      b,$00
        ld      hl,tmp_buff
        call    l24bf
        pop     bc
        cp      $42
        jr      nz,l24b0                ; (+$01)
        inc     c
l24b0:  djnz    l249d                   ; (-$15)
        ld      a,c
        ld      ($df9d),a
        ret     

l24b7:  ld      bc,$feef
        ld      a,$a0
	out     (c),a
        ret     

l24bf:  push    bc
        push    de
        ld      de,$07d0
        ld      ($df9e),de
        ld      a,c
        and     a
        ld      a,$a0
        jr      z,l24d0                 ; (+$02)
        ld      a,$b0
l24d0:  call    l2738
        jr      nc,l24ec                ; (+$17)
        ld      e,$00
l24d7:  ld      bc,$deef
        out     (c),e
        in      a,(c)
        cp      e
        jr      nz,l2553                ; (+$72)
        inc     e
        jr      nz,l24d7                ; (-$0d)
        ld      a,$ec
        ld      bc,$ffef
        out     (c),a
        scf     
l24ec:  pop     de
        pop     bc
        call    c,l257e
        jr      nc,l254a                ; (+$57)
        push    de
        push    bc
        push    hl
        ld      d,$00
l24f8:  xor     a
        ld      bc,$eeef
        out     (c),a
        ld      bc,$dfef
        inc     a
        out     (c),a
        ld      bc,$deef
        out     (c),a
        inc     d
        ld      bc,$efef
        out     (c),d
        jr      z,l2538                 ; (+$27)
        ld      bc,$ffef
        ld      a,$20
        out     (c),a
        nop     
l2519:  in      a,(c)
        rlca    
        jr      c,l2519                 ; (-$05)
        and     $12
        cp      $10
        jr      nz,l2538                ; (+$14)
        ld      b,$ce
        push    de
        ld      de,$0100
        ld      h,e
        ld      l,e
        call    l25b0
        ld      hl,$8900
        call    l2678
        pop     de
        jr      c,l24f8                 ; (-$40)
l2538:  dec     d
        pop     hl
        pop     bc
        ld      a,b
        call    l021b
        ld      (hl),d
        inc     h
        pop     de
        ld      a,$07
        call    l021b
        ld      a,$42
        and     a
l254a:  push    hl
        ld      hl,x0000
        ld      ($df9e),hl
        pop     hl
        ret     

l2553:  ld      a,$00
        and     a
        jr      l24ec                   ; (-$6c)
l2558:  bit     7,c
        ld      a,$02
        ret     nz

        push    bc
        push    de
        sla     e
        rl      d
        rl      c
        call    l2571
        jr      nc,l256e                ; (+$04)
        inc     e
        call    l2571
l256e:  pop     de
        pop     bc
        ret     

l2571:  call    l26a8
        ret     nc

        ld      a,$20
        push    bc
        ld      bc,$ffef
        out     (c),a
        pop     bc
l257e:  push    hl
        ld      hl,$8908
        call    l2678
        pop     hl
        ret     nc

        push    bc
        push    de
        ld      a,b
        call    l021b
        ld      bc,$ceef
        ld      de,$0100
        call    l25b0
        pop     de
        pop     bc
        push    hl
        ld      hl,$8900
        call    l2678
        pop     hl
        ret     nc

        push    bc
        ld      bc,$ffef
        in      a,(c)
        pop     bc
        and     $01
        scf     
        ret     z

        ld      a,$07
        and     a
        ret     

l25b0:  ld      a,b
        inc     e
        dec     e
        jr      z,x25be                 ; (+$09)
l25b5:  ini     
        ld      b,a
        dec     e
        jr      nz,l25b5                ; (-$06)
        inc     d
        jr      l25f3                   ; (+$35)
x25be:  ld      e,$10
l25c0:  ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        ini     
        ld      b,a
        dec     e
        jr      nz,l25c0                ; (-$33)
l25f3:  dec     d
        jr      nz,x25be                ; (-$38)
        ld      a,$07
        jp      l021b
l25fb:  bit     7,c
        ld      a,$02
        ret     nz

        push    bc
        push    de
        sla     e
        rl      d
        rl      c
        call    l2614
        jr      nc,l2611                ; (+$04)
        inc     e
        call    l2614
l2611:  pop     de
        pop     bc
        ret     

l2614:  call    l26a8
        ret     nc

        ld      a,$30
        push    bc
        ld      bc,$ffef
        out     (c),a
        pop     bc
        push    hl
        ld      hl,$8908
        call    l2678
        pop     hl
        jr      c,l263d                 ; (+$12)
        cp      $00
        ret     z

        push    bc
        ld      bc,$cfef
        in      a,(c)
        pop     bc
        and     $40
        ld      a,$07
        ret     z

        ld      a,$01
        ret     

l263d:  push    bc
        push    de
        ld      a,b
        call    l021b
        ld      e,$40
        ld      bc,$cfef
        ld      a,b
l2649:  outi    
        ld      b,a
        outi    
        ld      b,a
        outi    
        ld      b,a
        outi    
        ld      b,a
        dec     e
        jp      nz,l2649
        ld      a,$07
        call    l021b
        pop     de
        pop     bc
        push    hl
        ld      hl,$8900
        call    l2678
        pop     hl
        ret     nc

        push    bc
        ld      bc,$ffef
        in      a,(c)
        pop     bc
        and     $01
        scf     
        ret     z

        ld      a,$07
        and     a
        ret     

l2678:  push    bc
        push    de
        ld      e,$09
x267c:  push    de
        ld      de,($df9e)
        ld      bc,$ffef
l2684:  in      a,(c)
        and     h
        cp      l
        scf     
        jr      z,l26a0                 ; (+$15)
        bit     7,a
        jr      nz,l2693                ; (+$04)
        bit     0,a
        jr      nz,l26a4                ; (+$11)
l2693:  dec     de
        ld      a,d
        or      e
        jr      nz,l2684                ; (-$14)
        pop     de
        dec     e
        jr      nz,x267c                ; (-$20)
        push    de
        ld      a,$00
x269f:  and     a
l26a0:  pop     de
        pop     de
        pop     bc
        ret     

l26a4:  ld      a,$07
        jr      x269f                   ; (-$09)
l26a8:  push    hl
        push    de
        push    bc
        xor     a
        call    l2735
        jp      nc,x272c
        ld      l,c
        ld      h,$00
        ld      bc,$e883
        bit     4,(ix+$10)
        jr      z,l26c1                 ; (+$03)
        ld      bc,$e88d
l26c1:  push    bc
        ex      (sp),ix
        ld      c,(ix+$04)
        ld      b,(ix+$05)
        call    l2748
        push    bc
        ld      hl,x0000
        ld      c,(ix+$03)
        ld      b,$00
        call    l2748
        ld      d,c
        pop     bc
        ex      (sp),ix
        ld      a,(ix+$03)
        add     a,d
        ex      (sp),ix
        cp      (ix+$02)
        jr      c,l26ec                 ; (+$04)
        sub     (ix+$02)
        inc     bc
l26ec:  pop     ix
        ld      l,(ix+$01)
        ld      h,(ix+$02)
        add     hl,bc
        ld      b,h
        ld      c,l
        ld      l,(ix+$04)
        ld      h,(ix+$05)
        and     a
        sbc     hl,bc
        jr      c,x2730                 ; (+$2e)
        jr      nz,l270b                ; (+$07)
        ld      l,(ix+$06)
        inc     l
        cp      l
        jr      nc,x2730                ; (+$25)
l270b:  call    l2735
        push    bc
        ld      c,e
        pop     de
        jr      nc,x272c                ; (+$19)
        ld      a,c
        inc     a
        ld      bc,$dfef
        out     (c),a
        ld      a,$01
        ld      bc,$deef
        out     (c),a
        ld      bc,$eeef
        out     (c),e
        ld      bc,$efef
        out     (c),d
        scf     
x272c:  pop     bc
        pop     de
        pop     hl
        ret     

x2730:  and     a
        ld      a,$02
        jr      x272c                   ; (-$09)
l2735:  or      (ix+$10)
l2738:  push    bc
        ld      bc,$feef
        out     (c),a
        push    hl
        ld      hl,$c040
        call    l2678
        pop     hl
        pop     bc
        ret     

l2748:  ld      a,$10
        sla     e
        rl      d
l274e:  adc     hl,hl
        jr      nc,l2758                ; (+$06)
        or      a
        sbc     hl,bc
        or      a
        jr      l275e                   ; (+$06)
l2758:  sbc     hl,bc
        jr      nc,l275e                ; (+$02)
        add     hl,bc
        scf     
l275e:  rl      e
        rl      d
        dec     a
        jr      nz,l274e                ; (-$17)
        ld      a,d
        cpl     
        ld      b,a
        ld      a,e
        cpl     
        ld      c,a
        ex      de,hl
        ret     

l276d:  bit     7,(ix+$00)
        jr      nz,l2778                ; (+$05)
        call    l27b9
        jr      l27a7                   ; (+$2f)
l2778:  push    ix
        ex      (sp),hl
l277b:  ld      a,(hl)
        call    l2738
        jr      nc,l277b                ; (-$06)
        inc     hl
        ld      bc,$f0ef
        outi    
        ld      bc,$efef
        outi    
        ld      bc,$e0ef
        outi    
        ld      e,(hl)
        inc     hl
        res     7,(hl)
        ex      (sp),hl
        pop     ix
        call    l27b9
l279b:  ld      bc,$deef
        out     (c),e
        ld      d,$20
        ld      bc,$ffef
        out     (c),d
l27a7:  ld      de,$0100
l27aa:  ld      bc,$ffef
        in      a,(c)
        and     $89
        cp      $08
        jr      nz,l27aa                ; (-$0b)
        ld      bc,$ceef
        ret     

l27b9:  dec     (ix-$01)
        ret     nz

        set     7,(ix+$00)
        ret     

x27c2:  ld      bc,$ceef
l27c5:  ld      a,h
        or      l
        ret     z

        in      a,(c)
        dec     hl
        dec     de
        jr      l27c5                   ; (-$09)
l27ce:  in      a,(c)
        dec     de
        ld      a,d
        or      e
        jr      nz,l27ce                ; (-$07)
        ret     

        scf     
        ret     

        sla     e
        srl     d
        rr      e
        ld      c,$00
        push    hl
        ld      l,(ix+$17)
        ld      h,(ix+$18)
        ex      (sp),hl
        ex      (sp),ix
        call    l2558
        pop     ix
        ret     

        sla     e
        srl     d
        rr      e
        ld      c,$00
        push    hl
        ld      l,(ix+$17)
        ld      h,(ix+$18)
        ex      (sp),hl
        ex      (sp),ix
        call    l25fb
        pop     ix
        ret     

l2808:  ld      de,l0106
        scf     
        ret     

l280d:  srl     a
        ld      ix,$e883
        jr      nc,l2819                ; (+$04)
        ld      ix,$e88d
l2819:  and     a
        ld      a,$16
        ret     nz

        ld      a,(ix+$00)
        or      (ix+$01)
        ld      a,$16
        ret     z

        scf     
        ret     

l2828:  ld      hl,$e897
        ld      de,$e898
        ld      bc,$004b
        ld      (hl),$00
        ldir    
        ld      hl,$e8e3
        ld      de,$e8e4
        ld      bc,$025f
        ld      (hl),$00
        ldir    
        ld      hl,$07d0
        ld      ($df9e),hl
        ld      hl,$e883
        ld      de,$e897
        ld      a,$a0
        call    l2937
        ld      hl,$e88d
        ld      de,$e8aa
        ld      a,$b0
        call    l2937
        call    l24b7
        ld      hl,x0000
        ld      ($df9e),hl
        ld      bc,$fefe
        in      a,(c)
        rra     
        ccf     
        ret     c

        xor     a
        ld      b,a
        ld      c,a
        ld      l,$08
        call    l306c
        jr      nc,l289f                ; (+$26)
        ld      (ed_ATTR_P),a
        ld      (ed_ATTR_T),a
        ld      (ATTR_T),a
        ld      (BORDCR),a
        ld      d,a
        rra     
        rra     
        rra     
        and     $07
        out     ($fe),a
        ld      hl,$5800
        ld      bc,$0300
x2893:  ld      a,(hl)
        cp      $38
        jr      nz,l2899                ; (+$01)
        ld      (hl),d
l2899:  inc     hl
        dec     bc
        ld      a,b
        or      c
        jr      nz,x2893                ; (-$0c)
l289f:  xor     a
        ld      b,a
        ld      c,a
        ld      l,$09
        call    l306c
        jr      nc,l28ac                ; (+$03)
        ld      (ATTR_P),a
l28ac:  ld      hl,FLAGS3
        res     6,(hl)
        xor     a
        ld      b,a
        ld      c,a
        ld      hl,$ef11
        call    l2b69
        jr      nc,l28ee                ; (+$32)
        ld      hl,$ef3b
        ld      de,$2934
        ld      b,$03
l28c4:  ld      a,(hl)
        and     a
        jr      z,l28d3                 ; (+$0b)
        push    hl
        push    de
        push    bc
        ld      a,(de)
        ld      l,a
        call    l3370
        pop     bc
        pop     de
        pop     hl
l28d3:  inc     hl
        inc     de
        djnz    l28c4                   ; (-$13)
        ld      hl,$ef3e
        ld      b,$03
l28dc:  ld      a,(hl)
        and     a
        jr      z,l28eb                 ; (+$0b)
        push    hl
        push    bc
        ld      l,a
        ld      a,$05
        sub     b
        call    l3227
        pop     bc
        pop     hl
l28eb:  inc     hl
        djnz    l28dc                   ; (-$12)
l28ee:  xor     a
l28ef:  ld      bc,x0000
        push    af
l28f3:  pop     af
        push    af
        ld      l,$1c
        call    l306c
        jr      nc,l292a                ; (+$2e)
        jr      z,l2906                 ; (+$08)
        ld      l,a
        pop     af
        push    af
        push    bc
        call    l3227
        pop     bc
l2906:  inc     bc
        ld      a,b
        or      c
        jr      nz,l28f3                ; (-$18)
l290b:  pop     af
        inc     a
        cp      $02
        jr      nz,l28ef                ; (-$22)
        xor     a
        ld      b,a
        ld      c,a
        ld      l,$10
        call    l306c
        jr      nc,l2928                ; (+$0d)
        jr      z,l2928                 ; (+$0b)
        call    l012d
        jr      nc,l2928                ; (+$06)
        ld      (LODDRV),a
        ld      (SAVDRV),a
l2928:  scf     
        ret     

l292a:  cp      $38
        jr      z,l290b                 ; (-$23)
        cp      $16
        jr      z,l290b                 ; (-$27)
        jr      l2906                   ; (-$2e)
        ld      b,c
        ld      b,d
        ld      c,l
l2937:  push    hl
        push    de
        ld      hl,l29a7
        ld      bc,$0010
        ldir    
        ex      de,hl
        ld      (hl),a
        inc     hl
        ld      (hl),$00
        inc     hl
        ld      (hl),$00
        pop     hl
        pop     de
        push    de
        push    hl
        ld      hl,l29b2
        ld      bc,$0008
        ldir    
        ex      de,hl
        pop     de
        push    de
        ld      (hl),e
        inc     hl
        ld      (hl),d
        rlca    
        rlca    
        rlca    
        rlca    
        and     $01
        push    af
        ld      hl,x29ba
        call    l2bc8
        pop     bc
        jr      nc,l2972                ; (+$07)
        ld      a,($ef21)
        cp      $01
        jr      z,l298c                 ; (+$1a)
l2972:  ld      a,b
        ex      (sp),ix
        inc     (ix+$03)
        inc     (ix+$06)
        ex      (sp),ix
        ld      hl,$29ba
        call    l2bc8
        jr      nc,l299f                ; (+$1a)
        ld      a,($ef21)
        cp      $01
        jr      nz,l299f                ; (+$13)
l298c:  ld      hl,$ef21
        pop     de
        ld      bc,$0010
        ldir    
        ld      hl,$ef31
        pop     de
        ld      bc,$0008
        ldir    
        ret     

l299f:  xor     a
        pop     hl
        ld      (hl),a
        pop     hl
        ld      (hl),a
        inc     hl
        ld      (hl),a
        ret     

l29a7:	.db    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
l29b2:  .db    1, 0, 2, 2, 4, 0, 0, 0
x29ba:	.db    "PLUSIDEDOS      "

l29ca:  rlca    
        rlca    
        rlca    
        rlca    
        ld      c,a
        ld      ix,$e897
        ld      b,$04
        ld      e,$00
        ld      h,e
        ld      l,e
l29d9:  ld      a,(ix+$00)
        and     a
        jr      z,l29fc                 ; (+$1d)
        ld      a,(ix+$10)
        and     $10
        cp      c
        jr      nz,x29f0                ; (+$09)
        ld      a,(ix+$11)
        or      (ix+$12)
        jr      z,x29f0                 ; (+$01)
        inc     e
x29f0:  push    de
        ld      de,$0013
        add     ix,de
        pop     de
        djnz    l29d9                   ; (-$20)
        ld      a,e
        scf     
        ret     

l29fc:  push    ix
        pop     hl
        jr      x29f0                   ; (-$11)
l2a01:  push    bc
        push    hl
        push    ix
        push    af
        call    l29ca
        and     a
        jr      z,l2a13                 ; (+$07)
        ld      a,$3b
l2a0e:  pop     bc
        pop     bc
l2a10:  pop     bc
        pop     bc
        ret     

l2a13:  pop     af
        push    af
        call    l280d
        jr      c,l2a26                 ; (+$0c)
        ld      a,h
        or      l
        ld      a,$3c
        jr      z,l2a0e                 ; (-$12)
        ld      (ix+$08),l
        ld      (ix+$09),h
l2a26:  pop     af
        pop     de
        ld      (ix+$00),e
        ld      (ix+$01),d
        pop     bc
        ld      (ix+$02),b
        ld      (ix+$03),c
        ld      hl,x0000
        ld      d,h
        ld      e,c
        res     7,b
l2a3c:  add     hl,de
        djnz    l2a3c                   ; (-$03)
        ld      (ix+$04),l
        ld      (ix+$05),h
        pop     hl
        ld      (ix+$06),l
        ld      (ix+$07),h
        push    af
        srl     h
        rr      l
        srl     h
        rr      l
        srl     h
        rr      l
        xor     a
        call    l2ecd
        bit     7,(ix+$02)
        res     7,(ix+$02)
        ld      l,(ix+$08)
        ld      h,(ix+$09)
        push    hl
        ex      (sp),ix
        push    af
        ld      a,$00
        ld      (ix+$01),a
        ld      (ix+$02),a
        jr      z,l2a7a                 ; (+$01)
        inc     a
l2a7a:  ld      (ix+$03),a
        pop     af
        ex      (sp),ix
        call    nz,l2ef4
        ex      (sp),ix
        ld      (ix+$00),$01
        ld      (ix+$04),e
        ld      (ix+$05),d
        ld      (ix+$06),a
        ex      (sp),ix
        ld      de,$0010
        and     a
        sbc     hl,de
        call    l2efc
        ex      (sp),ix
        pop     de
        pop     af
        push    af
        rlca    
        rlca    
        rlca    
        rlca    
        or      $a0
        ld      (ix+$10),a
        xor     a
        ld      (ix+$11),a
        ld      (ix+$12),a
        push    de
        ld      d,a
        ld      e,a
        call    l2d07
        jp      nc,l2a10
        ld      hl,$ef11
        ld      d,h
        ld      e,l
        inc     de
        ld      bc,$003f
        ld      (hl),b
        ldir    
        ld      a,$ff
        ld      ($ef21),a
        ld      e,(ix+$04)
        ld      d,(ix+$05)
        ld      a,(ix+$06)
        ex      (sp),ix
        call    l2ef4
        ld      ($ef22),de
        ld      ($ef24),a
        ld      l,(ix+$00)
        ld      h,(ix+$01)
        dec     hl
        ld      ($ef25),hl
        ld      a,(ix+$02)
        dec     a
        ld      ($ef27),a
        ld      hl,$ef11
        call    l2efc
        ex      (sp),ix
        pop     de
        pop     af
        push    af
        push    de
        ld      bc,$0001
        call    l2b92
        jp      nc,l2a10
        ld      hl,$29ba
        ld      de,$ef11
        ld      bc,$0010
        ldir    
        push    ix
        pop     hl
        ld      bc,$000b
        ldir    
        pop     hl
        ld      de,$ef31
        ld      bc,$0008
        ldir    
        ex      de,hl
        ld      (hl),$38
        inc     hl
        ld      (hl),$38
        pop     af
        ld      hl,$ef11
        call    l2b92
        ret     

l2b31:  call    l280d
        ret     nc

        ld      l,(ix+$06)
        ld      h,(ix+$07)
        and     a
        sbc     hl,bc
        ld      a,$38
        ccf     
        ret     nc

        ld      l,(ix+$08)
        ld      h,(ix+$09)
        push    hl
        pop     ix
        push    bc
        srl     b
        rr      c
        srl     b
        rr      c
        srl     b
        rr      c
        ld      d,b
        ld      e,c
        pop     bc
        ld      a,c
        and     $07
        ld      c,$00
        rra     
        rr      c
        rra     
        rr      c
        ld      b,a
        scf     
        ret     

l2b69:  push    bc
        push    ix
        push    hl
        call    l2b31
        jr      nc,l2b8d                ; (+$1b)
        push    bc
        ld      c,$00
        ld      b,$07
        ld      hl,tmp_buff
        call    l2558
        pop     bc
        jr      nc,l2b8d                ; (+$0d)
        ld      hl,tmp_buff
        add     hl,bc
        pop     de
        ld      bc,$0040
        ldir    
        scf     
        jr      l2b8e                   ; (+$01)
l2b8d:  pop     hl
l2b8e:  pop     ix
        pop     bc
        ret     

l2b92:  push    bc
        push    ix
        push    hl
        call    l2b31
        jr      nc,l2bc3                ; (+$28)
        push    de
        push    bc
        ld      c,$00
        ld      b,$07
        ld      hl,tmp_buff
        call    l2558
        pop     bc
        pop     de
        jr      nc,l2bc3                ; (+$18)
        ld      hl,tmp_buff
        add     hl,bc
        ex      de,hl
        ex      (sp),hl
        ld      bc,$0040
        ldir    
        ld      c,$00
        ld      b,$07
        ld      hl,tmp_buff
        pop     de
        call    l25fb
        jr      l2bc4                   ; (+$01)
l2bc3:  pop     hl
l2bc4:  pop     ix
        pop     bc
        ret     

l2bc8:  push    ix
        ld      bc,x0000
l2bcd:  push    af
        push    bc
        push    hl
        ld      hl,$ef11
        call    l2b69
        jr      nc,l2bfd                ; (+$25)
        pop     hl
        push    hl
        ld      de,$ef11
        ld      b,$10
l2bdf:  ld      a,(de)
        cp      (hl)
        jr      z,l2bf8                 ; (+$15)
        cp      $41
        jr      c,l2beb                 ; (+$04)
        cp      $5b
        jr      c,l2bf3                 ; (+$08)
l2beb:  cp      $61
        jr      c,l2c03                 ; (+$14)
        cp      $7b
        jr      nc,l2c03                ; (+$10)
l2bf3:  xor     $20
        cp      (hl)
        jr      nz,l2c03                ; (+$0b)
l2bf8:  inc     de
        inc     hl
        djnz    l2bdf                   ; (-$1d)
        scf     
l2bfd:  pop     hl
        pop     bc
l2bff:  pop     hl
        pop     ix
        ret     

l2c03:  pop     hl
        pop     bc
        inc     bc
        ld      a,b
        or      c
        ld      a,$38
        jr      z,l2bff                 ; (-$0d)
        pop     af
        jr      l2bcd                   ; (-$42)
l2c0f:  push    hl
        push    af
        push    bc
        call    l2bc8
        pop     bc
        ccf     
        ld      a,$39
        jr      nc,l2c38                ; (+$1d)
        pop     af
        push    af
        ld      hl,$ef11
        call    l2b69
        jr      nc,l2c38                ; (+$13)
        pop     af
        pop     hl
        ld      de,$ef11
        push    bc
        ld      bc,$0010
        ldir    
        pop     bc
        ld      hl,$ef11
        call    l2b92
        ret     

l2c38:  pop     hl
        pop     hl
        ret     

l2c3b:  push    hl
        push    af
        ld      hl,$ef11
        call    l2b69
        jr      nc,l2c58                ; (+$13)
        pop     af
        pop     hl
        ld      de,$ef31
        push    bc
        ld      bc,$0020
        ldir    
        pop     bc
        ld      hl,$ef11
        call    l2b92
        ret     

l2c58:  pop     hl
        pop     hl
        ret     

l2c5b:  push    ix
        push    de
        push    hl
        add     a,a
        add     a,a
        add     a,a
        add     a,a
        ld      d,a
        ld      ix,$e897
        ld      e,$04
l2c6a:  ld      a,(ix+$00)
        and     a
        jr      z,l2c84                 ; (+$14)
        ld      a,(ix+$10)
        and     $10
        cp      d
        jr      nz,l2c84                ; (+$0c)
        ld      l,(ix+$11)
        ld      h,(ix+$12)
        sbc     hl,bc
        ld      a,$3b
        jr      z,l2c8f                 ; (+$0b)
l2c84:  push    bc
        ld      bc,$0013
        add     ix,bc
        pop     bc
        dec     e
        jr      nz,l2c6a                ; (-$24)
        scf     
l2c8f:  pop     hl
        pop     de
        pop     ix
        ret     

l2c94:  push    af
        call    l2c5b
        jr      nc,l2cd9                ; (+$3f)
        ld      ix,$e897
        ld      e,$04
l2ca0:  ld      a,(ix+$00)
        and     a
        jr      z,l2cb4                 ; (+$0e)
        push    bc
        ld      bc,$0013
        add     ix,bc
        pop     bc
        dec     e
        jr      nz,l2ca0                ; (-$10)
        ld      a,$3c
        jr      l2cd9                   ; (+$25)
l2cb4:  pop     af
        push    af
        ld      hl,$ef11
        call    l2b69
        jr      nc,l2cd9                ; (+$1b)
        ld      a,($ef21)
        dec     a
        cp      $fd
        ld      a,$38
        jr      nc,l2cd9                ; (+$11)
        pop     af
        call    l2ce1
        ld      hl,$ef21
        push    ix
        pop     de
        ld      bc,$0010
        ldir    
        scf     
        ret     

l2cd9:  pop     hl
        ret     

l2cdb:  ld      (ix+$00),$00
        scf     
        ret     

l2ce1:  push    af
        push    ix
        call    l280d
        ld      a,(ix+$02)
        and     $40
        pop     ix
        ld      (ix+$10),a
        pop     af
        and     $01
        add     a,a
        add     a,a
        add     a,a
        add     a,a
        add     a,$a0
        or      (ix+$10)
        ld      (ix+$10),a
        ld      (ix+$11),c
        ld      (ix+$12),b
        ret     

l2d07:  push    ix
        jr      l2d2b                   ; (+$20)
l2d0b:  push    ix
        push    hl
        push    af
        call    l2c5b
        jr      nc,l2d63                ; (+$4f)
        pop     af
        push    af
        ld      hl,$ef11
        call    l2b69
        jr      nc,l2d63                ; (+$45)
        pop     af
        pop     hl
        push    ix
        pop     de
        ld      ix,$ef21
        call    l2ce1
        ld      a,l
l2d2b:  push    de
        ld      hl,tmp_buff
        ld      (hl),a
        ld      de,$ed12
        ld      bc,$01ff
        ldir    
        pop     de
        ld      c,$00
        ld      a,d
        or      e
        jr      nz,l2d48                ; (+$09)
        ld      e,(ix+$07)
        ld      d,(ix+$08)
        ld      c,(ix+$09)
l2d48:  ld      b,$07
        ld      hl,tmp_buff
        push    de
        call    l25fb
        pop     de
        jr      nc,l2d65                ; (+$11)
        ld      a,c
        or      d
        or      e
        scf     
        jr      z,l2d65                 ; (+$0b)
        dec     de
        ld      a,d
        and     e
        inc     a
        jr      nz,l2d48                ; (-$18)
        dec     c
        jr      l2d48                   ; (-$1b)
l2d63:  pop     hl
        pop     hl
l2d65:  pop     ix
        ret     

l2d68:  ld      bc,$ffff
        ld      ($ef92),bc
        ld      ($ef94),bc
        inc     bc
        ld      ($ef96),bc
l2d78:  push    af
        push    de
        push    hl
        ld      hl,$ef11
        call    l2b69
        pop     hl
        pop     de
        jr      nc,l2dd8                ; (+$53)
        ld      a,($ef21)
        cp      h
        jr      nz,l2dd0                ; (+$45)
        ld      a,($ef2b)
        and     a
        jr      nz,l2da0                ; (+$0f)
        ld      a,l
        push    hl
        ld      hl,($ef28)
        sbc     hl,de
        ld      l,a
        ld      a,($ef2a)
        sbc     a,l
        pop     hl
        jr      c,l2dd0                 ; (+$30)
l2da0:  pop     af
        push    af
        call    l2c5b
        jr      nc,l2dd0                ; (+$29)
        push    de
        push    hl
        ld      de,($ef92)
        ld      hl,($ef28)
        and     a
        sbc     hl,de
        ld      de,($ef94)
        ld      hl,($ef2a)
        sbc     hl,de
        jr      nc,l2dce                ; (+$10)
        ld      hl,($ef28)
        ld      ($ef92),hl
        ld      hl,($ef2a)
        ld      ($ef94),hl
        ld      ($ef96),bc
l2dce:  pop     hl
        pop     de
l2dd0:  inc     bc
        ld      a,b
        or      c
        jr      z,l2dd8                 ; (+$03)
        pop     af
        jr      l2d78                   ; (-$60)
l2dd8:  pop     af
        ld      bc,($ef96)
        ld      a,b
        or      c
        ret     z

        scf     
        ret     

l2de2:  push    ix
        push    af
        push    hl
        call    l2bc8
        ld      a,$39
        ccf     
        jp      nc,l2ec8
        pop     ix
        ld      e,(ix+$17)
        ld      d,(ix+$18)
        ld      l,(ix+$19)
        ld      h,$ff
        pop     af
        push    af
        call    l2d68
        ld      a,$1a
        jp      nc,l2ec9
        pop     af
        push    af
        ld      hl,$ef11
        call    l2b69
        jp      nc,l2ec9
        ld      l,(ix+$17)
        ld      h,(ix+$18)
        ld      e,(ix+$19)
        pop     af
        push    af
        push    ix
        call    l280d
        jp      nc,l2ec8
        ld      a,e
        call    l2ecd
        ld      hl,($ef22)
        add     hl,de
        ld      e,a
        ld      a,($ef24)
        add     a,e
        cp      (ix+$02)
        jr      c,l2e3a                 ; (+$04)
        sub     (ix+$02)
        inc     hl
l2e3a:  ex      de,hl
        ld      l,a
        ld      a,($ef27)
        cp      l
        ld      a,l
        jr      nz,l2e4a                ; (+$07)
        ld      hl,($ef25)
        sbc     hl,de
        jr      z,l2e9d                 ; (+$53)
l2e4a:  pop     hl
        ex      (sp),hl
        ld      l,a
        push    bc
        push    de
        push    hl
        ld      bc,x0000
l2e53:  pop     af
        push    af
        ld      hl,$ef51
        call    l2b69
        jr      nc,l2ec4                ; (+$67)
        ld      a,($ef61)
        and     a
        jr      z,l2e6a                 ; (+$07)
        inc     bc
        ld      a,b
        or      c
        jr      nz,l2e53                ; (-$15)
        jr      l2ec4                   ; (+$5a)
l2e6a:  ld      a,$ff
        ld      ($ef61),a
        ld      a,($ef27)
        ld      ($ef67),a
        ld      hl,($ef25)
        ld      ($ef65),hl
        pop     hl
        pop     de
        push    de
        push    hl
        ld      a,l
        call    l2ef4
        ld      ($ef64),a
        ld      ($ef62),de
        ld      hl,$ef51
        call    l2efc
        pop     af
        push    af
        call    l2b92
        jr      nc,l2ec6                ; (+$2f)
        pop     hl
        ld      a,l
        pop     de
        pop     bc
        ex      (sp),hl
        push    hl
l2e9d:  ld      ($ef27),a
        ld      ($ef25),de
        ld      hl,$ef11
        call    l2efc
        pop     de
        push    de
        push    bc
        ld      hl,$ef22
        ld      bc,$0011
        ex      de,hl
        add     hl,bc
        ex      de,hl
        ld      bc,$000f
        ldir    
        pop     bc
        pop     hl
        pop     af
        call    l2b92
        pop     ix
        ret     

l2ec4:  ld      a,$1a
l2ec6:  pop     hl
        pop     hl
l2ec8:  pop     hl
l2ec9:  pop     hl
        pop     ix
        ret     

l2ecd:  push    bc
        scf     
        rl      l
        rl      h
        rla     
        ld      c,(ix+$04)
        ld      b,(ix+$05)
        ld      de,x0000
        and     a
l2ede:  sbc     hl,bc
        sbc     a,$00
        inc     de
        jr      nc,l2ede                ; (-$07)
        add     hl,bc
        dec     de
        xor     a
        ld      c,(ix+$03)
        ld      b,a
l2eec:  sbc     hl,bc
        inc     a
        jr      nc,l2eec                ; (-$05)
        dec     a
        pop     bc
        ret     

l2ef4:  inc     a
        cp      (ix+$02)
        ret     c

        inc     de
        xor     a
        ret     

l2efc:  push    bc
        push    de
        push    hl
        ex      (sp),ix
        ld      e,(ix+$14)
        ld      d,(ix+$15)
        ld      c,(ix+$16)
        inc     c
        ex      (sp),ix
        call    l2f63
        ex      de,hl
        ld      a,c
        ex      (sp),ix
        ld      e,(ix+$11)
        ld      d,(ix+$12)
        ld      c,(ix+$13)
        ex      (sp),ix
        call    l2f63
        and     a
        sbc     hl,de
        sbc     a,c
        ex      de,hl
        ld      c,a
        ld      b,$00
        ld      a,(ix+$03)
        push    ix
        ld      ix,x0000
        ld      h,b
        ld      l,b
l2f35:  add     ix,de
        adc     hl,bc
        dec     a
        jr      nz,l2f35                ; (-$07)
        push    ix
        pop     bc
        srl     h
        rr      l
        rr      b
        rr      c
        ld      a,b
        or      c
        jr      nz,l2f4c                ; (+$01)
        dec     hl
l2f4c:  dec     bc
        pop     ix
        ex      (sp),ix
        ld      (ix+$17),c
        ld      (ix+$18),b
        ld      (ix+$19),l
        ld      (ix+$1a),h
        ex      (sp),ix
        pop     hl
        pop     de
        pop     bc
        ret     

l2f63:  push    af
        push    hl
        push    bc
        ld      b,(ix+$02)
        call    l2f77
        pop     bc
        ld      b,$00
        ex      de,hl
        add     hl,bc
        ex      de,hl
        adc     a,b
        ld      c,a
        pop     hl
        pop     af
        ret     

l2f77:  ex      de,hl
        xor     a
        ex      af,af'
        push    af
        xor     a
        ld      d,a
        ld      e,a
l2f7e:  srl     b
        jr      nc,l2f89                ; (+$07)
        ld      c,a
        ex      af,af'
        ex      de,hl
        add     hl,de
        ex      de,hl
        adc     a,c
        ex      af,af'
l2f89:  jr      z,l2f90                 ; (+$05)
        add     hl,hl
        adc     a,a
        jp      l2f7e
l2f90:  pop     af
        ex      af,af'
        ret     

l2f93:  push    ix
        push    bc
        push    af
        call    l280d
        jp      nc,l3047
        pop     af
        push    af
        call    l2c5b
        jp      nc,l3047
        pop     af
        push    af
        ld      hl,$ef11
        call    l2b69
        jp      nc,l3047
l2fb0:  ld      bc,x0000
l2fb3:  pop     af
        push    af
        ld      hl,$ef51
        call    l2b69
        jp      nc,l3052
        ld      a,($ef61)
        cp      $ff
        jp      nz,l304c
        ld      a,($ef67)
        ld      de,($ef65)
        call    l2ef4
        ld      l,a
        ld      a,($ef24)
        sub     l
        jr      nz,l2fde                ; (+$07)
        ld      hl,($ef22)
        sbc     hl,de
        jr      z,l2ff8                 ; (+$1a)
l2fde:  ld      a,($ef27)
        ld      de,($ef25)
        call    l2ef4
        ld      l,a
        ld      a,($ef64)
        sub     l
        jr      nz,l304c                ; (+$5d)
        ld      hl,($ef62)
        sbc     hl,de
        jr      z,l3006                 ; (+$10)
        jr      l304c                   ; (+$54)
l2ff8:  ld      a,($ef27)
        ld      ($ef67),a
        ld      hl,($ef25)
        ld      ($ef65),hl
        jr      l3012                   ; (+$0c)
l3006:  ld      a,($ef24)
        ld      ($ef64),a
        ld      hl,($ef22)
        ld      ($ef62),hl
l3012:  ld      hl,$ef51
        call    l2efc
        pop     af
        push    af
        call    l2b92
        jr      nc,l3047                ; (+$28)
        pop     af
        pop     de
        push    bc
        push    af
        push    de
        ld      b,$40
        ld      hl,$ef11
l3029:  ld      (hl),$00
        inc     hl
        djnz    l3029                   ; (-$05)
        pop     bc
        pop     af
        push    af
        ld      hl,$ef11
        call    l2b92
        jr      nc,l3047                ; (+$0e)
        ld      hl,$ef51
        ld      de,$ef11
        ld      bc,$0040
        ldir    
        jp      l2fb0
l3047:  pop     bc
        pop     bc
        pop     ix
        ret     

l304c:  inc     bc
        ld      a,b
        or      c
        jp      nz,l2fb3
l3052:  ld      a,$ff
        ld      ($ef21),a
        ld      hl,$ef11
        ld      b,$10
l305c:  ld      (hl),$00
        inc     hl
        djnz    l305c                   ; (-$05)
        ld      hl,$ef11
        pop     af
        pop     bc
        call    l2b92
        pop     ix
        ret     

l306c:  push    hl
        ld      hl,$ef11
        call    l2b69
        pop     hl
        ret     nc

        ld      a,l
        and     $1f
        ld      l,a
        ld      h,$00
        ld      de,$ef31
        add     hl,de
        ld      a,(hl)
        and     a
        scf     
        ret     

l3083:  push    af
        push    hl
        ld      hl,$ef11
        call    l2b69
        pop     hl
        jr      nc,l30a2                ; (+$14)
        ld      a,l
        and     $1f
        ld      l,a
        ld      a,h
        ld      h,$00
        ld      de,$ef31
        add     hl,de
        ld      (hl),a
        pop     af
        ld      hl,$ef11
        call    l2b92
        ret     

l30a2:  pop     hl
        ret     

l30a4:  ld      a,(ix+$00)
        cp      $02
        scf     
        ret     z

        ld      a,$3d
        and     a
        ret     

l30af:  call    l30a4
        ret     nc

        ld      c,(ix+$0c)
        ld      b,(ix+$0d)
        scf     
        ret     

l30bb:  call    l30a4
        ret     nc

        push    hl
        ld      l,(ix+$0e)
        ld      h,(ix+$0f)
        and     a
        sbc     hl,bc
        pop     hl
        ccf     
        ld      a,$15
        ret     nc

        ld      (ix+$0c),c
        ld      (ix+$0d),b
        scf     
        ret     

l30d6:  call    l30a4
        ret     nc

        ld      a,b
        push    af
        push    hl
        ld      e,(ix+$0c)
        ld      d,(ix+$0d)
        ld      l,(ix+$0e)
        ld      h,(ix+$0f)
        and     a
        sbc     hl,de
        jr      nc,l30f1                ; (+$03)
        ld      de,x0000
l30f1:  push    de
        ld      b,(ix+$0b)
        call    l2f77
        ld      c,a
        pop     hl
        inc     hl
        ld      (ix+$0c),l
        ld      (ix+$0d),h
        pop     hl
        pop     af
        ld      b,a
        scf     
        ret     

l3106:  call    l30d6
        ret     nc

        call    l26a8
        ret     nc

        push    bc
        ld      e,(ix+$0b)
        ld      a,b
        call    l021b
        sla     e
        push    de
        call    l279b
l311c:  call    l25b0
        pop     de
        dec     e
        jr      z,l3129                 ; (+$06)
        push    de
        call    l27a7
        jr      l311c                   ; (-$0d)
l3129:  ld      a,$07
        call    l021b
        pop     bc
        scf     
        ret     

l3131:  call    l30d6
        ret     nc

        ld      a,(ix+$0b)
l3138:  push    af
        push    de
        call    l25fb
        pop     de
        jr      nc,l314c                ; (+$0c)
        inc     de
        ld      a,d
        or      e
        jr      nz,l3146                ; (+$01)
        inc     c
l3146:  pop     af
        dec     a
        jr      nz,l3138                ; (-$12)
        scf     
        ret     

l314c:  pop     de
        ret     

l314e:  call    l30d6
        ret     nc

        ld      a,(ix+$0b)
l3155:  push    af
        push    bc
        push    de
        push    hl
        ld      hl,tmp_buff
        ld      b,$07
        call    l2558
        pop     hl
        pop     de
        pop     bc
        jr      nc,l3189                ; (+$23)
        push    de
        push    hl
        call    l25fb
        pop     hl
        pop     de
        jr      nc,l3189                ; (+$1a)
        push    bc
        push    de
        ld      de,tmp_buff
        ld      bc,$0200
        ex      de,hl
        ldir    
        ex      de,hl
        pop     de
        pop     bc
        inc     de
        ld      a,d
        or      e
        jr      nz,l3183                ; (+$01)
        inc     c
l3183:  pop     af
        dec     a
        jr      nz,l3155                ; (-$32)
        scf     
        ret     

l3189:  pop     de
        ret     

l318b:  push    af
        push    bc
        call    l31c7
        ld      h,$02
        push    de
        push    hl
        ld      a,$00
        call    l2d68
        pop     hl
        pop     de
        ld      a,$00
        jr      c,l31a9                 ; (+$0a)
        inc     a
        call    l2d68
        ld      a,$40
        jr      nc,l31c4                ; (+$1d)
        ld      a,$01
l31a9:  call    l2c94
        jr      nc,l31c4                ; (+$16)
l31ae:  pop     bc
        ld      (ix+$0e),c
        ld      (ix+$0f),b
        pop     af
        ld      (ix+$0b),a
        ld      (ix+$0c),$00
        ld      (ix+$0d),$00
        scf     
        ret     

l31c3:  pop     bc
l31c4:  pop     bc
        pop     bc
        ret     

l31c7:  ld      hl,x0000
        ld      e,a
        dec     e
        ld      d,a
        dec     a
        cp      $20
        ld      a,$15
        jr      nc,l31c3                ; (-$11)
        ld      a,h
l31d5:  add     hl,bc
        adc     a,$00
        dec     d
        jr      nz,l31d5                ; (-$06)
        add     hl,de
        adc     a,$00
        ex      de,hl
        ld      l,a
        ret     

l31e1:  ld      d,a
        call    l30a4
        ret     nc

        ld      a,d
        push    af
        push    bc
        call    l31c7
        ld      a,(ix+$09)
        cp      l
        jr      c,l3202                 ; (+$10)
        jr      nz,l31ae                ; (-$46)
        ld      a,(ix+$08)
        cp      d
        jr      c,l3202                 ; (+$08)
        jr      nz,l31ae                ; (-$4e)
        ld      a,(ix+$07)
        cp      e
        jr      nc,l31ae                ; (-$54)
l3202:  pop     bc
        pop     bc
        ld      a,$15
        and     a
        ret     

l3208:  ld      a,l
        ld      ($ef91),a
        sub     $41
        jr      c,l3223                 ; (+$13)
        cp      $10
        jr      nc,l3223                ; (+$0f)
        ld      l,a
        ld      h,$00
        add     hl,hl
        ld      de,$e2a1
        add     hl,de
        ld      d,(hl)
        dec     hl
        ld      e,(hl)
        ld      a,d
        or      e
        scf     
        ret     

l3223:  and     a
        ld      a,$15
        ret     

l3227:  cp      $02
        jp      nc,l32f5
        push    ix
        push    af
        push    bc
        push    hl
        ld      hl,$e8e3
        ld      b,$02
        ld      de,$0130
l3239:  ld      a,(hl)
        inc     hl
        or      (hl)
        dec     hl
        jr      z,l324b                 ; (+$0c)
        add     hl,de
        djnz    l3239                   ; (-$09)
        ld      a,$3f
        and     a
l3245:  pop     de
        pop     de
        pop     de
        pop     ix
        ret     

l324b:  ex      (sp),hl
        call    l3208
        jr      nc,l3245                ; (-$0c)
        ld      a,$3e
        ccf     
        jr      nz,l3245                ; (-$11)
        pop     de
        pop     bc
        pop     af
        push    hl
        push    af
        push    de
        ld      hl,$ef11
        call    l2b69
        jr      nc,l3245                ; (-$1f)
        ld      a,($ef21)
        cp      $03
        jr      z,l3270                 ; (+$05)
        and     a
        ld      a,$38
        jr      l3245                   ; (-$2b)
l3270:  ld      hl,$ef31
        pop     de
        pop     af
        push    de
        push    bc
        push    af
        ld      bc,$001c
        ldir    
        ex      de,hl
        ld      a,($ef91)
        ld      (hl),a
        inc     hl
        pop     af
        push    af
        rlca    
        rlca    
        rlca    
        rlca    
        or      $a0
        ld      (hl),a
        inc     hl
        ld      b,$0a
l328f:  ld      (hl),$00
        inc     hl
        djnz    l328f                   ; (-$05)
        ld      de,$0008
        ex      de,hl
        add     hl,de
        ex      de,hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        inc     hl
        ld      (hl),$d6
        inc     hl
        ld      (hl),$27
        inc     hl
        ld      (hl),$d8
        inc     hl
        ld      (hl),$27
        inc     hl
        ld      (hl),$f0
        inc     hl
        ld      (hl),$27
        pop     af
        pop     bc
        push    bc
        push    af
        call    l2c94
        jr      nc,l32e9                ; (+$31)
        pop     af
        pop     bc
        pop     hl
        push    hl
        ld      de,$0017
        add     hl,de
        push    ix
        pop     de
        ld      (hl),e
        inc     hl
        ld      (hl),d
        pop     de
        pop     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        pop     ix
        ld      hl,FLAGS3
        bit     6,(hl)
        scf     
        ret     z

        push    af
        push    bc
        ld      a,($ef91)
        ld      l,a
        call    l34ac
        ld      a,($ef91)
        ld      h,a
        ld      l,$1c
        pop     bc
        pop     af
        call    l3083
        ret     

l32e9:  pop     hl
        pop     hl
        pop     hl
        ld      (hl),$00
        inc     hl
        ld      (hl),$00
        pop     de
        pop     ix
        ret     

l32f5:  push    hl
        sub     $02
        ld      e,a
        ld      d,$00
        cp      $02
        jr      z,l3306                 ; (+$07)
        jr      nc,l336c                ; (+$6b)
        call    l0157
        jr      nc,l336c                ; (+$66)
l3306:  ld      h,d
        ld      l,e
        push    de
        add     hl,hl
        add     hl,de
        add     hl,hl
        ld      de,l346f
        add     hl,de
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        ld      hl,xdpb_ptrs
        ld      e,$10
l3318:  ld      a,(hl)
        inc     hl
        cp      c
        jr      nz,l3323                ; (+$06)
        ld      a,(hl)
        cp      b
        ld      a,$3b
        jr      z,l3369                 ; (+$46)
l3323:  inc     hl
        dec     e
        jr      nz,l3318                ; (-$0f)
        pop     de
        pop     hl
        push    de
        call    l3208
        pop     de
        ret     nc

        ld      a,$3e
        ccf     
        ret     nz

        ld      (hl),c
        inc     hl
        ld      (hl),b
        ld      hl,$001c
        add     hl,bc
        ld      a,(unit0)
        and     a
        jr      nz,l334a                ; (+$0a)
        ld      a,e
        and     a
        jr      nz,l334a                ; (+$06)
        ld      a,($ef91)
        ld      (unit0),a
l334a:  ld      a,($ef91)
        ld      (hl),a
        ld      hl,FLAGS3
        bit     6,(hl)
        scf     
        ret     z

        push    af
        push    de
        ld      l,a
        call    l34ac
        pop     de
        pop     af
        ld      hl,$000d
        add     hl,de
        ld      h,a
        xor     a
        ld      b,a
        ld      c,a
        call    l3083
        ret     

l3369:  pop     hl
        pop     hl
        ret     

l336c:  ld      a,$41
        pop     hl
        ret     

l3370:  call    l3208
        ret     nc

        push    ix
        jr      z,l33bb                 ; (+$43)
        push    de
        pop     ix
        call    l181b
        jr      nc,l33ca                ; (+$4a)
        xor     a
        ld      (hl),a
        inc     hl
        ld      (hl),a
        ex      de,hl
        ld      a,(ix+$1b)
        and     $0c
        jr      nz,l338f                ; (+$03)
        ld      (hl),a
        inc     hl
        ld      (hl),a
l338f:  ld      a,(unit0)
        cp      (ix+$1c)
        jr      nz,l339b                ; (+$04)
        xor     a
        ld      (unit0),a
l339b:  ld      (ix+$1c),$00
        ld      e,(ix+$2a)
        ld      d,(ix+$2b)
        ld      hl,$27d6
        and     a
        sbc     hl,de
        jr      nz,l33bb                ; (+$0e)
        ld      e,(ix+$17)
        ld      d,(ix+$18)
        push    de
        pop     ix
        call    l2cdb
        jr      nc,l33ca                ; (+$0f)
l33bb:  ld      hl,FLAGS3
        bit     6,(hl)
        scf     
        jr      z,l33ca                 ; (+$07)
        ld      a,($ef91)
        ld      l,a
        call    l34ac
l33ca:  pop     ix
        ret     

l33cd:  push    bc
        ld      a,$20
        ld      e,$12
l33d2:  ld      (bc),a
        inc     bc
        dec     e
        jr      nz,l33d2                ; (-$05)
        call    l3208
        ex      de,hl
        pop     bc
        ret     nc

        ret     z

        push    ix
        push    bc
        push    hl
        pop     ix
        ld      e,(ix+$2a)
        ld      d,(ix+$2b)
        ld      hl,$27d6
        and     a
        sbc     hl,de
        jr      nz,l3438                ; (+$46)
        ld      e,(ix+$17)
        ld      d,(ix+$18)
        push    de
        pop     ix
        ld      a,(ix+$10)
        rra     
        rra     
        rra     
        rra     
        and     $01
        ld      c,(ix+$11)
        ld      b,(ix+$12)
        push    bc
        push    af
        ld      hl,$ef11
        call    l2b69
        jr      nc,l3432                ; (+$1e)
        pop     af
        pop     bc
        pop     de
        push    bc
        push    af
        add     a,$30
        ld      (de),a
        inc     de
        ld      a,$3e
        ld      (de),a
        inc     de
        ld      hl,$ef11
        ld      bc,$0010
        ldir    
        xor     a
        sbc     a,$01
        pop     bc
        ld      a,b
        pop     bc
        pop     ix
        ret     

l3432:  pop     de
        pop     de
l3434:  pop     de
        pop     ix
        ret     

l3438:  push    ix
        pop     de
        ld      ix,l346f
        ld      a,$03
l3441:  ld      l,(ix+$00)
        ld      h,(ix+$01)
        and     a
        sbc     hl,de
        jr      z,l3458                 ; (+$0c)
        ld      bc,$0006
        add     ix,bc
        dec     a
        jr      nz,l3441                ; (-$13)
        ld      a,$41
        jr      l3434                   ; (-$24)
l3458:  ld      l,(ix+$02)
        ld      h,(ix+$03)
        pop     de
        ld      c,(ix+$04)
        ld      b,$00
        ldir    
        xor     a
        sbc     a,$01
        ld      a,(ix+$05)
        pop     ix
        ret     

l346f:	.db    $c0, $e2, $81, $34, $11, $02, $2d, $e3
        .db    $92, $34, $11, $03, $9a, $e3, $A3, $34
        .db    $09, $04

l3474:	.db    "2>Floppy device 0"
l3485:	.db    "3>Floppy device 1"
l3496:  .db    "4>RAMdisk"

l34ac:  push    hl
        xor     a
        ld      b,a
        ld      c,a
        ld      hl,$ef11
        call    l2b69
        pop     de
        jr      nc,l34eb                ; (+$32)
        ld      hl,$ef3b
        ld      a,e
        cp      $41
        jr      nz,l34c3                ; (+$02)
        ld      (hl),$01
l34c3:  inc     hl
        cp      $42
        jr      nz,l34ca                ; (+$02)
        ld      (hl),$01
l34ca:  inc     hl
        cp      $4d
        jr      nz,l34d1                ; (+$02)
        ld      (hl),$01
l34d1:  ld      hl,$ef3e
        ld      b,$03
l34d6:  ld      a,(hl)
        cp      e
        jr      nz,l34dc                ; (+$02)
        ld      (hl),$00
l34dc:  inc     hl
        djnz    l34d6                   ; (-$09)
        xor     a
        ld      b,a
        ld      c,a
        push    de
        ld      hl,$ef11
        call    l2b92
        pop     de
        ret     nc

l34eb:  xor     a
l34ec:  ld      bc,x0000
        push    af
l34f0:  pop     af
        push    af
        push    de
        ld      hl,$ef11
        call    l2b69
        pop     de
        jr      nc,l3526                ; (+$2a)
        ld      a,($ef21)
        cp      $03
        jr      nz,l3519                ; (+$16)
        ld      a,($ef4d)
        cp      e
        jr      nz,l3519                ; (+$10)
        xor     a
        ld      ($ef4d),a
        pop     af
        push    af
        push    de
        ld      hl,$ef11
        call    l2b92
        pop     de
        jr      nc,l3526                ; (+$0d)
l3519:  inc     bc
        ld      a,b
        or      c
        jr      nz,l34f0                ; (-$2e)
l351e:  pop     af
        inc     a
        cp      $02
        jr      nz,l34ec                ; (-$38)
        scf     
        ret     

l3526:  cp      $38
        jr      z,l351e                 ; (-$0c)
        cp      $16
        jr      z,l351e                 ; (-$10)
        pop     de
        and     a
        ret     

l3531:  ld      d,h			; CREO QUE ACA COMIENZA LA CARGA DE UN Z80
        ld      e,l
l3533:  ld      a,(de)
        inc     de
        cp      $ff
        jr      nz,l3533                ; (-$06)
        dec     de
        dec     de
        ld      a,(de)
        ld      ($401d),a
        ld      bc,$0001		; NRO DE HANDLE $00 ?
        ld      d,b
        ld      e,$01
        call    l0106			; ABRE ARCHIVO
        ret     nc

        ld      hl,$5000
        ld      de,$001e		; NRO DE HANDLE $00 ?
        ld      b,d
        ld      c,$07
        call    l0112
        ret     nc

        ld      b,$00			; NRO DE HANDLE $00 ?
        call    l0133
        push    de
        push    hl
        ld      b,$00			; NRO DE HANDLE $00 ?
        call    l0139
        pop     bc
        and     a
        sbc     hl,bc
        pop     bc
        ld      a,e
        sbc     a,c
        ld      bc,$0001
        add     hl,bc
        adc     a,b
        ex      de,hl
        ld      ($4015),a
        push    af
        call    x0521
        pop     af
        ld      hl,$506b
        call    l3886
        ret     nc

        ld      ($503f),hl
        ld      b,$00			; NRO DE HANDLE $00 ?
        call    l0109
        di      
        ld      sp,$5067
        ld      ix,$506d
        ld      l,(ix-$02)
        ld      h,(ix-$01)
        call    l276d
        call    x27c2
        exx     
        ld      hl,$401d
        bit     6,(hl)
        jp      z,l3621
        ld      a,($501a)
        out     ($fe),a
        ld      a,($5013)
        and     $04
        ld      ($5013),a
        ld      a,($4015)
        and     a
        ld      hl,$0010
        exx     
        ld      iyl,$00
        ld      l,$03
        jp      z,l364c
        call    l3772
        ld      sp,$4047
        call    l3829
        call    l3875
        ld      l,a
        call    l3875
        ld      h,a
        ld      ($4027),hl
        ld      a,$c3
        ld      ($4026),a
        call    l3875
        ld      ($401d),a
        and     $07
        jr      z,l35f8                 ; (+$15)
        exx     
        ld      hl,src_add
        ld      e,a
l35e8:  xor     a
        call    l37dd
        ld      d,(hl)
        ld      a,e
        call    l37dd
        ld      (hl),d
        inc     hl
        bit     7,h
        jr      nz,l35e8                ; (-$0f)
        exx     
l35f8:  call    l3875
        xor     a
l35fc:  cp      $02
        jr      z,l361a                 ; (+$1a)
        cp      $05
        jr      z,l361a                 ; (+$16)
        ld      hl,($401d)
        ld      h,a
        ld      a,l
        and     $07
        cp      h
        ld      a,h
        jr      z,l361a                 ; (+$0b)
        call    l37dd
        push    af
        ld      hl,src_add
        call    l3829
        pop     af
l361a:  inc     a
        cp      $08
        jr      c,l35fc                 ; (-$23)
        jr      l3655                   ; (+$34)
l3621:  ld      hl,$500c
        ld      a,(hl)
        cp      $ff
        jr      nz,l362c                ; (+$03)
        ld      a,$01
        ld      (hl),a
l362c:  rra     
        out     ($fe),a
        dec     hl
        ld      a,(hl)
        res     7,a
        jr      nc,l3637                ; (+$02)
        set     7,a
l3637:  ld      (hl),a
        inc     hl
        ld      a,(hl)
        and     $20
        ld      iyl,a
        ld      hl,l37e5
        exx     
        ld      hl,($5006)
        ld      a,h
        or      l
        jp      z,l3691
        ld      l,$00
l364c:  call    l3772
        ld      sp,$4047
        call    l3829
l3655:  call    l27ce
        im      0
        ld      a,($4010)
        and     $03
        jr      z,l3668                 ; (+$07)
        im      1
        dec     a
        jr      z,l3668                 ; (+$02)
        im      2
l3668:  ld      sp,$4000
        pop     hl
        pop     de
        pop     bc
        exx     
        pop     af
        ex      af,af'
        pop     hl
        pop     de
        pop     iy
        pop     ix
        pop     bc
        ld      a,b
        sub     $09
        rla     
        rl      b
        rra     
        ld      r,a
        pop     af
        ld      i,a
        ld      sp,($4016)
        ld      bc,$1ffd
        ld      a,($4015)
        jp      $4018
l3691:  call    l3875
        ld      l,a
        call    l3875
        and     a
        jp      nz,l3735
        call    l3875
        ld      ($5006),a
        call    l3875
        ld      ($5007),a
        call    l3875
        ld      h,a
        call    l3875
        ld      ($5069),a
        call    l3875
        call    l3875
        call    l3875
        push    af
        exx     
        ld      de,$0010
l36c0:  exx     
        call    l3875
        exx     
        ld      bc,$fffd
        out     (c),d
        ld      b,$bf
        out     (c),a
        inc     d
        dec     e
        jr      nz,l36c0                ; (-$12)
        ld      b,$ff
        pop     af
        out     (c),a
        exx     
        ld      a,l
        sub     $17
        jr      nz,l36e5                ; (+$08)
        ld      a,h
        cp      $03
        jr      c,l36f3                 ; (+$11)
        inc     h
        jr      l36f3                   ; (+$0e)
l36e5:  cp      $20
l36e7:  push    af
        call    l3875
        ld      l,a
        pop     af
        dec     a
        jr      nz,l36e7                ; (-$09)
        ld      a,l
        jr      nc,l36f5                ; (+$02)
l36f3:  ld      a,$04
l36f5:  ld      ($506a),a
        ld      a,h
        cp      $04
        jp      nc,l3736
        xor     a
        call    l37dd
        ld      h,$03
        and     a
l3705:  push    hl
        call    l386a
        ld      hl,$8000
        ld      iyh,$c0
        cp      $04
        jr      z,l372b                 ; (+$18)
        ld      h,$c0
        ld      iyh,$00
        cp      $05
        jr      z,l372b                 ; (+$0f)
        cp      $08
        jr      nz,l3735                ; (+$15)
        call    l377d
        pop     af
        ld      sp,$4047
        push    af
        ld      iyh,$80
l372b:  call    l3829
        pop     hl
        dec     h
        jr      nz,l3705                ; (-$2d)
        jp      l3655
l3735:  rst     $00
l3736:  ld      h,$08
        and     a
l3739:  push    hl
        call    l386a
        sub     $03
        jr      c,l3735                 ; (-$0c)
        cp      $08
        jr      nc,l3735                ; (-$10)
        ld      hl,src_add
        call    l37dd
        cp      $05
        jr      nz,l3765                ; (+$16)
        call    l377d
        pop     af
        ld      sp,$4047
        push    af
        ld      a,($5069)
        ld      ($401d),a
        ld      a,($506a)
        ld      ($4015),a
        set     7,h
l3765:  ld      iyh,$00
        call    l3829
        pop     hl
        dec     h
        jr      nz,l3739                ; (-$36)
        jp      l3655
l3772:  xor     a
        call    l37dd
        call    l377f
        ld      iyh,$00
        ret     

l377d:  ld      l,$00
l377f:  ld      h,$40
        ld      iyh,$48
        call    l3829
        exx     
        push    hl
        ld      hl,($503f)
        dec     ix
        push    ix
        pop     de
        and     a
        sbc     hl,de
        ld      b,h
        ld      c,l
        ld      hl,$4047
        push    hl
        pop     ix
        inc     ix
        ex      de,hl
        ldir    
        pop     hl
        ld      d,$00
        call    l37bf
        ld      d,$50
        call    l37bf
        ld      hl,$401d
        ld      (hl),$10
        ld      l,$15
        ld      (hl),$04
        exx     
        ld      a,($4014)
        and     a
        ret     nz

        ld      ($4025),a
        ret     

l37bf:  ld      b,$40
l37c1:  ld      e,(hl)
        inc     hl
        bit     7,e
        ret     nz

        ld      a,(hl)
        and     $3f
        ld      c,a
        ld      a,(hl)
        rlca    
        rlca    
        and     $03
        inc     hl
        push    hl
        ld      l,a
        inc     l
l37d3:  ld      a,(de)
        inc     e
        ld      (bc),a
        inc     c
        dec     l
        jr      nz,l37d3                ; (-$07)
        pop     hl
        jr      l37c1                   ; (-$1c)

l37dd:  push    bc
        ld      bc,$7ffd
        out     (c),a
        pop     bc
        ret     

l37e5:  .db    $2f, $58, $32, $9a, $2f, $9e, $34, $a3
        .db    $35, $26, $80, $0a, $13, $0b, $11, $17
        .db    $cc, $04, $48, $0d, $4a, $02, $61, $08
        .db    $56, $01, $12, $00, $24, $16, $06, $15
        .db    $07, $13, $40, $11, $42, $0f, $44, $06
        .db    $67, $1d, $10, $1b, $14, $80

l3813:  ld      a,d
        or      e
        call    z,n3871
        dec     de
        in      a,(c)
        ld      (hl),a
        inc     l
        jp      nz,l3813
        inc     h
        ld      a,h
        and     $f8
        cp      iyh
        jr      nz,l3813                ; (-$15)
        ret     

l3829:  ld      a,iyl
        and     a
l382c:  jr      z,l3813                 ; (-$1b)

l382e:  ld      a,d
        or      e
        call    z,n3871
        dec     de
        in      a,(c)
        cp      $ed
        jr      z,l3848                 ; (+$0e)
l383a:  ld      (hl),a
        inc     l
        jp      nz,l382e
        inc     h
l3840:  ld      a,h
        and     $f8
        cp      iyh
        jr      nz,l382e                ; (-$19)
        ret     

l3848:  call    l3875
        cp      $ed
        jr      nz,l3860                ; (+$11)
        call    l3875
        push    af
        call    l3875
        pop     bc
l3857:  ld      (hl),a
        inc     hl
        djnz    l3857                   ; (-$04)
        ld      bc,$ceef
        jr      l3840                   ; (-$20)
l3860:  ld      (hl),$ed
        inc     l
        jr      nz,l383a                ; (-$2b)
        inc     h
        jr      nz,l383a                ; (-$2e)
        scf     
        ret     

l386a:  call    nc,l3875
        ld      l,a
        call    l3875
        and     l
        inc     a
        ld      iyl,a
l3875:  ld      a,d
        or      e
        call    z,n3871
        dec     de
        in      a,(c)
        ret     

n3871:  ld      a,(ix+$00)
        inc     a
        call    nz,l276d
        ret     

l3886:  push    de
        push    af
        ld      ($f514),hl
        ld      hl,$0020
        add     hl,bc
        ld      a,(hl)
        and     $03
        dec     a
        ld      a,$1d
        jr      nz,l38b4                ; (+$1d)
        inc     hl
        ld      a,(hl)
        call    l17c5
        jr      nc,l38b4                ; (+$16)
        ld      a,(ix+$06)
        and     a
        jr      z,l38b1                 ; (+$0d)
        ld      l,(ix+$2c)
        ld      h,(ix+$2d)
        ld      de,$27d8
        sbc     hl,de
        jr      z,l38b7                 ; (+$06)
l38b1:  ld      a,$1d
        and     a
l38b4:  pop     de
        pop     de
        ret     

l38b7:  call    l1063
        push    ix
        ld      ix,($f514)
        ld      (ix+$00),l
        ld      (ix+$01),$00
        ld      l,h
        ld      h,e
        ld      e,(ix+$00)
        ld      d,(ix+$01)
        inc     ix
        inc     ix
        ld      ($f514),ix
        pop     ix
        pop     af
        ex      (sp),hl
        push    hl
        ld      hl,$0100
        and     a
        sbc     hl,de
        ex      de,hl
        pop     hl
        and     a
        sbc     hl,de
        sbc     a,$00
        ld      e,a
        ld      a,l
        and     a
        ld      l,h
        ld      h,e
        ld      de,$0001
        jr      z,l38f4                 ; (+$01)
        inc     de
l38f4:  add     hl,de
        ld      ($f511),hl
        pop     de
        ld      a,(ix+$03)
        srl     a
        and     e
        ld      ($f513),a
        ld      a,(ix+$02)
        dec     a
        call    nz,l04eb
l3909:  push    de
        ld      a,(ix+$02)
        call    l04f5
        call    l0d05
        ex      de,hl
        ld      hl,$000c
        add     hl,bc
        cpl     
        and     (hl)
        or      d
        ld      (hl),a
        inc     hl
        inc     hl
        ld      (hl),e
        call    l0d3a
        pop     de
        ret     nc

        ld      a,e
        and     $07
        add     a,a
        add     a,$10
        ld      l,a
        ld      h,$00
        add     hl,bc
l392e:  push    de
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        push    hl
        push    bc
        ld      a,(ix+$02)
        dec     a
        push    af
        call    nz,l04f5
        ld      c,$00
        ld      a,($f513)
        add     a,e
        ld      e,a
        ld      a,d
        adc     a,c
        ld      d,a
        push    ix
        ld      l,(ix+$17)
        ld      h,(ix+$18)
        push    hl
        pop     ix
        call    l26a8
        pop     ix
        jr      nc,l39b0                ; (+$57)
        ld      hl,($f514)
        ld      bc,$feef
        call    l39b8
        ld      bc,$efef
        call    l39b8
        ld      bc,$eeef
        call    l39b8
        ld      bc,$dfef
        call    l39b8
        pop     bc
        ld      a,($f513)
        ld      c,a
        ld      a,$01
l397b:  add     a,a
        djnz    l397b                   ; (-$03)
        sub     c
        ld      b,$00
        ld      c,a
        push    hl
        ld      hl,($f511)
        ld      a,l
        and     a
        sbc     hl,bc
        jr      nc,l398e                ; (+$02)
        ld      c,a
        xor     a
l398e:  ld      ($f511),hl
        pop     hl
        ld      (hl),c
        inc     hl
        ld      ($f514),hl
        pop     bc
        scf     
        jr      z,l39b2                 ; (+$17)
        pop     hl
        pop     de
        xor     a
        ld      ($f513),a
        inc     de
        push    hl
        and     a
        sbc     hl,bc
        ld      a,l
        pop     hl
        cp      $20
        jp      c,l392e
        jp      l3909
l39b0:  pop     bc
        pop     bc
l39b2:  ld      (hl),$ff
        inc     hl
        pop     bc
        pop     bc
        ret     

l39b8:  in      a,(c)
        ld      (hl),a
        inc     hl
        ret     

l39bd:  ld      a,$3a
        and     a
        ret     

l39c1:  add     a,$03
        cp      $13
        jr      nc,l39d6                ; (+$0f)
        rlca    
        ld      hl,STRMS
        ld      c,a
        ld      b,$00
        add     hl,bc
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        dec     hl
        scf     
        ret     

l39d5:  pop     bc
l39d6:  ld      a,$17
        and     a
        ret     

l39da:  inc     hl
        inc     hl
l39dc:  ld      a,(hl)
        inc     hl
        and     a
        ret     z

        cp      c
        jr      nz,l39da                ; (-$09)
        scf     
        ret     

l39e5:  push    hl
        call    l39f6
        jr      nc,l39f4                ; (+$09)
        ex      de,hl
        ex      (sp),hl
        and     a
        sbc     hl,de
        ccf     
        pop     hl
        ex      de,hl
        ret     

l39f4:  pop     hl
        ret     

l39f6:  ld      a,b
        or      c
        ret     z

        ld      a,(de)
        cp      $2c
        scf     
        ccf     
        ret     nz

        inc     de
        dec     bc
l3a01:  ld      hl,x0000
l3a04:  ld      a,b
        or      c
        scf     
        ret     z

        ld      a,(de)
        cp      $20
        jr      z,l3a20                 ; (+$13)
        sub     $30
        ret     c

        cp      $0a
        ccf     
        ret     c

        push    de
        add     hl,hl
        ld      d,h
        ld      e,l
        add     hl,hl
        add     hl,hl
        add     hl,de
        ld      d,$00
        ld      e,a
        add     hl,de
        pop     de
l3a20:  inc     de
        dec     bc
        jr      l3a04                   ; (-$20)
l3a24:  push    bc
        call    l39c1
        jr      nc,l39d5                ; (-$55)
        ld      a,b
        or      c
        jr      z,l3a44                 ; (+$16)
        push    hl
        ld      hl,(CHANS)
        add     hl,bc
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        pop     hl
        cp      $4b
        jr      z,l3a44                 ; (+$08)
        cp      $53
        jr      z,l3a44                 ; (+$04)
        cp      $50
        jr      nz,l39d5                ; (-$6f)
l3a44:  pop     bc
        push    hl
        ld      hl,l3a84
        ld      a,b
        or      c
        jr      z,l3a7f                 ; (+$32)
        dec     bc
        ld      a,b
        or      c
        inc     bc
        ld      a,(de)
        jr      z,l3a65                 ; (+$11)
        ld      hl,l3a9c
        inc     de
        ld      a,(de)
        dec     de
        cp      $3e
        ld      a,$49
        jr      nz,l3a65                ; (+$05)
        ld      a,(de)
        inc     de
        inc     de
        dec     bc
        dec     bc
l3a65:  and     $df
        push    bc
        ld      c,a
        call    l39dc
        pop     bc
        jr      nc,l3a7f                ; (+$10)
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        call    l3a7e
        pop     hl
        jr      nc,l3a80                ; (+$07)
        ld      (hl),e
        inc     hl
        ld      (hl),d
        scf     
        ret     

l3a7e:  jp      (hl)

l3a7f:  pop     bc

l3a80:  ld      a,$0e
        and     a
        ret     

l3a84:	.db    $4b, $8e, $3a, $53, $92, $3a, $50, $96
        .db    $3a, $00, $1e, $01, $18, $06, $1e, $06
        .db    $18, $02, $1e, $10, $16, $00, $37, $c9
l3a9c:  .db    $49, $bd, $3a, $4f, $b6, $3a, $55, $af
        .db    $3a, $4d, $41, $3b, $56, $7c, $3b, $57, $bc, $3b, $00

l3aaf:	ld      hl,$0202
        ld      a,$03
        jr      l3ac2                   ; (+$0c)
        ld      hl,$0204
        ld      a,$02
        jr      l3ac2                   ; (+$05)
        ld      hl,$0002
        ld      a,$01
l3ac2:  push    af
        push    hl
        ld      hl,src_file
        ld      a,b
        and     a
        jr      nz,l3ad1                ; (+$06)
        ld      a,c
        ld      b,c
        cp      $12
        jr      c,l3ad3                 ; (+$02)
l3ad1:  ld      b,$11
l3ad3:  ex      de,hl
l3ad4:  ld      a,(hl)
        inc     hl
        call    l3d8e
        ld      (de),a
        inc     de
        call    l3d6a
        djnz    l3ad4                   ; (-$0c)
        call    l3d8e
        ld      a,$ff
        ld      (de),a
        call    l3d6a
        ld      b,$02
l3aeb:  push    bc
        call    l3d8e
        call    l3da8
        call    l053c
        call    l3de4
        call    l3d6a
        pop     bc
        jr      c,l3b07                 ; (+$09)
        inc     b
        ld      a,b
        cp      $10
        jr      c,l3aeb                 ; (-$19)
        pop     hl
        pop     hl
        ret     

l3b07:  ld      hl,src_file
        pop     de
        pop     af
        ld      c,a
        push    bc
        call    l3d8e
        call    l3da8
        call    l0106
        call    l3de4
        call    l3d6a
        pop     bc
        ret     nc

        push    bc
        ld      hl,l3b34
        ld      bc,$000e
        ld      de,$000d
        call    l3c99
        ld      bc,$000d
        add     hl,bc
        pop     bc
        ld      (hl),b
        scf     
        ret     

l3b34:	.db    $00, $5b, $00, $5b, $46, $e0, $3e, $73
        .db    $3e, $4e, $3e, $0e, $00

l3b41:	call    l3a01
        push    hl
        call    l39f6
        pop     de
        ret     nc

        ld      a,b
        or      c
        ret     nz

        ld      a,h
        or      l
        ret     z

        push    de
        push    hl
        ld      hl,l3b6f
        ld      bc,$0013
        ld      de,$000d
        call    l3c99
        ld      bc,$000d
        add     hl,bc
        pop     bc
        ld      (hl),c
        inc     hl
        ld      (hl),b
        inc     hl
        inc     hl
        inc     hl
        pop     bc
        ld      (hl),c
        inc     hl
        ld      (hl),b
        scf     
        ret     

l3b6f:	.db    $00, $5b, $00, $5b, $4d, $1c, $03, $21
        .db    $03, $26, $03, $13, $00

l3b7c:	ld      a,b
        and     a
        ret     nz

        ld      a,c
        and     a
        ret     z

        ld      a,(de)
        inc     de
        dec     c
        ret     z

        and     $df
        cp      $41
        ccf     
        ret     nc

        cp      $5b
        ret     nc

        ld      l,a
        ld      a,(de)
        cp      $24
        scf     
        ccf     
        ret     nz

        dec     c
        ret     nz

        set     7,l
        push    hl
        ld      hl,l3baf
        ld      bc,$0010
        ld      de,$000d
        call    l3c99
        ld      bc,$000d
        add     hl,bc
        pop     bc
        ld      (hl),c
        scf     
        ret     

l3baf:	.db    $00, $5b, $00, $5b, $56, $ef, $39, $d7
        .db    $39, $fb, $39, $10, $00

l3bbc:  call    l3a01
        ld      a,h
        and     a
        ret     nz

        ld      a,l
        cp      $18
        ret     nc

        push    hl
        ld      hl,$001f
        call    l39e5
        jp      nc,l3c7e
        ld      a,l
        pop     hl
        ld      h,a
        push    hl
        ld      a,$18
        sub     l
        ld      l,a
        ld      h,$00
        call    l39e5
        jp      nc,l3c7e
        ex      (sp),hl
        push    hl
        ld      a,$20
        sub     h
        ld      l,a
        ld      h,$00
        call    l39e5
        ld      a,l
        pop     hl
        ex      (sp),hl
        ld      h,a
        push    hl
        call    l39f6
        ld      a,$08
        jr      nc,l3c0d                ; (+$16)
        ld      a,h
        and     a
        jp      nz,l3c7d
        ld      a,l
        cp      $03
        ccf     
        jr      nc,l3c7d                ; (+$7b)
        cp      $09
        jr      nc,l3c7d                ; (+$77)
        push    af
        call    l39f6
        jr      c,l3c20                 ; (+$14)
        pop     af
l3c0d:  push    af
        ld      hl,l3c80
        sub     $03
        jr      z,l3c1a                 ; (+$05)
l3c15:  inc     hl
        inc     hl
        dec     a
        jr      nz,l3c15                ; (-$05)
l3c1a:  ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        jr      l3c26                   ; (+$06)
l3c20:  ld      a,h
        and     $c0
        jr      z,l3c7c                 ; (+$57)
        dec     h
l3c26:  ld      a,b
        or      c
        jr      nz,l3c7c                ; (+$52)
        push    hl
        ld      hl,l3c8c
        ld      bc,$004d
        ld      de,$000d
        call    l3c99
        ld      bc,$0015
        add     hl,bc
        pop     bc
        ld      (hl),b
        dec     hl
        ld      (hl),c
        dec     hl
        pop     af
        ld      (hl),a
        dec     hl
        pop     bc
        ex      de,hl
        ex      (sp),hl
        ex      de,hl
        push    af
        ld      a,e
        add     a,a
        add     a,a
        add     a,a
        ld      (hl),a
        dec     hl
        ld      (hl),d
        dec     hl
        ld      a,c
        add     a,e
        dec     a
        ld      (hl),a
        dec     hl
        ld      a,b
        add     a,d
        dec     a
        ld      (hl),a
        dec     hl
        ld      (hl),e
        dec     hl
        ld      (hl),d
        ld      e,b
        ld      d,$00
        ex      de,hl
        add     hl,hl
        add     hl,hl
        add     hl,hl
        pop     af
        ld      c,a
        ld      b,$00
        xor     a
l3c69:  sbc     hl,bc
        inc     a
        jr      nc,l3c69                ; (-$05)
        dec     a
        ld      hl,$000b
        add     hl,de
        ld      (hl),a
        inc     hl
        ld      a,(ATTR_P)
        ld      (hl),a
        pop     de
        scf     
        ret     

l3c7c:  pop     hl
l3c7d:  pop     hl
l3c7e:  pop     hl
        ret     

l3c80:	.db    $62, $2b, $62, $2b, $7b, $2b, $7b, $2b
        .db    $3e, $2b, $1f, $2b

l3c8c:  .db    $00, $5b, $00, $5b, $57, $d0, $27, $9c
        .db    $39, $9c, $39, $4d, $00

l3c99:  push    hl
        push    de
        push    bc
        ld      hl,(PROG)
        dec     hl
        push    hl
        exx     
        call    n3e00
        xor     a
        ccf     
        pop     de
        pop     hl
        pop     bc
        and     a
        sbc     hl,bc
        ex      (sp),hl
        push    de
        ldir    
        pop     hl
        pop     bc
l3cb3:  ld      a,b
        or      c
        jr      z,l3cbd                 ; (+$06)
        xor     a
        ld      (de),a
        inc     de
        dec     bc
        jr      l3cb3                   ; (-$0a)
l3cbd:  push    hl
        ld      de,(CHANS)
        and     a
        sbc     hl,de
        inc     hl
        ex      de,hl
        pop     hl
        ret     

l3cc9:  call    l39c1
        ret     nc

        ld      a,b
        or      c
        scf     
        ret     z

        push    hl
        ld      hl,(CHANS)
        add     hl,bc
        inc     hl
        inc     hl
        inc     hl
        ld      c,(hl)
        ex      de,hl
        ld      hl,l3d06
        call    l39dc
        jp      nc,l39d6
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        call    l3a7e
        pop     hl
        ld      a,$12
        ret     nc

        ld      bc,x0000
        ld      de,$a3e2
        ex      de,hl
        add     hl,de
        jr      c,l3d00                 ; (+$07)
        ld      bc,l3d2a
        add     hl,bc
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
l3d00:  ex      de,hl
        ld      (hl),c
        inc     hl
        ld      (hl),b
        scf     
        ret     

l3d06:	.db    $4b, $04, $3d, $53, $04, $3d, $50, $04
        .db    $3d, $46, $2a, $3d, $4d, $41, $3d, $56
        .db    $41, $3d, $57, $41, $3d, $00, $01, $00
        .db    $06, $00, $0B, $00, $01, $00, $01, $00
        .db    $06, $00, $10, $00

l3d2a:  ld      hl,$0009
        add     hl,de
        ld      b,(hl)
        push    de
        call    l3d8e
        call    l3da8
        call    l0109
        call    l3de4
        call    l3d6a
        pop     de
        ret     nc

        ld      hl,$0007
        add     hl,de
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        dec     de
        dec     de
        dec     de
        dec     de
        ex      de,hl
        exx     
        call    n3e00
        ld      (hl),a
        ccf     
        scf     
        ret     

l3d55:  exx     
        ld      de,x0000
l3d59:  call    n3e00
        dec     l
        add     hl,sp
        ret     

l3d5f:  ld      de,$0002
        jr      l3d59                   ; (-$0b)
l3d64:  exx     
        ld      de,$0004
        jr      l3d59                   ; (-$11)
l3d6a:  ex      af,af'
        xor     a
        di      
        call    l3d83
        pop     af
        ld      (TARGET),hl
        ld      hl,(OLDSP)
        ld      (OLDSP),sp
        ld      sp,hl
        ei      
        ld      hl,(TARGET)
        push    af
        ex      af,af'
        ret     

l3d83:  push    bc
        ld      bc,$7ffd
        out     (c),a
        ld      (BANKM),a
        pop     bc
        ret     

l3d8e:  ex      af,af'
        di      
        pop     af
        ld      (TARGET),hl
        ld      hl,(OLDSP)
        ld      (OLDSP),sp
        ld      sp,hl
        ld      hl,(TARGET)
        push    af
        ld      a,$07
        call    l3d83
        ei      
        ex      af,af'
        ret     

l3da8:  call    l3dd2
        ld      hl,TSTACK
        ld      de,tmp_stack
        ld      bc,$0084
        lddr    
        pop     bc
        ld      (tmp_sp),sp
        ld      hl,TSTACK
        ld      sp,hl
        push    bc
l3dc0:  ld      bc,(tmp_bc)
        ld      de,(tmp_de)
        ld      hl,(tmp_af)
        push    hl
        pop     af
        ld      hl,(tmp_hl)
        ei      
        ret     

l3dd2:  di      
        ld      (tmp_hl),hl
        push    af
        pop     hl
        ld      (tmp_af),hl
        ld      (tmp_de),de
        ld      (tmp_bc),bc
        ret     

l3de4:  call    l3dd2
        pop     hl
        ld      (tmp_ret),hl
        ld      hl,tmp_stack
        ld      de,TSTACK
        ld      bc,$0084
        lddr    
        ld      hl,(tmp_sp)
        ld      sp,hl
        ld      hl,(tmp_ret)
        push    hl
        jr      l3dc0                   ; (-$40)

n3e00:  ld      (OLDHL),hl
        push    af
        pop     hl
        ld      (OLDAF),hl
        ex      (sp),hl
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        ex      (sp),hl
        push    bc
        pop     hl
        ld      a,(BANK678)
        ld      bc,$1ffd
        res     2,a
        di      
        ld      (BANK678),a
        out     (c),a
        ei      
        ld      bc,$3e2d
        push    bc
        push    hl
        ld      hl,(OLDAF)
        push    hl
        pop     af
        ld      hl,(OLDHL)
        ret     

        push    bc
        push    af
        ld      a,(BANK678)
        ld      bc,$1ffd
        set     2,a
        di      
        ld      (BANK678),a
        out     (c),a
        ei      
        pop     af
        pop     bc
        ret     

x3e41:  ld      l,$2f
        ld      de,$ffff
        ld      bc,$fefe
l3e49:  in      a,(c)
        cpl     
        and     $1f
        jr      z,l3e5e                 ; (+$0e)
        ld      h,a
        ld      a,l
l3e52:  inc     d
        ret     nz

l3e54:  sub     $08
        srl     h
        jr      nc,l3e54                ; (-$06)
        ld      d,e
        ld      e,a
        jr      nz,l3e52                ; (-$0c)
l3e5e:  dec     l
        rlc     b
        jr      c,l3e49                 ; (-$1a)
        ld      a,d
        inc     a
        ret     z

        cp      $28
        ret     z

        cp      $19
        ret     z

        ld      a,e
        ld      e,d
        ld      d,a
        cp      $18
        ret     

l3e72:  call    x3e41
        ret     nz

        ld      hl,KSTATE
l3e79:  bit     7,(hl)
        jr      nz,l3e84                ; (+$07)
        inc     hl
        dec     (hl)
        dec     hl
        jr      nz,l3e84                ; (+$02)
        ld      (hl),$ff
l3e84:  ld      a,l
        ld      hl,$5c04
        cp      l
        jr      nz,l3e79                ; (-$12)
        call    l3ed1
        ret     nc

        ld      hl,KSTATE
        cp      (hl)
        jr      z,l3ec3                 ; (+$2e)
        ex      de,hl
        ld      hl,$5c04
        cp      (hl)
        jr      z,l3ec3                 ; (+$27)
        bit     7,(hl)
        jr      nz,l3ea4                ; (+$04)
        ex      de,hl
        bit     7,(hl)
        ret     z

l3ea4:  ld      e,a
        ld      (hl),a
        inc     hl
        ld      (hl),$05
        inc     hl
        ld      a,(REPDEL)
        ld      (hl),a
        inc     hl
        ld      c,(iy+$07)
        ld      d,(iy+$01)
        push    hl
        call    x3f77
        pop     hl
        ld      (hl),a
l3ebb:  ld      (LAST_K),a
        set     5,(iy+$01)
        ret     

l3ec3:  inc     hl
        ld      (hl),$05
        inc     hl
        dec     (hl)
        ret     nz

        ld      a,(REPPER)
        ld      (hl),a
        inc     hl
        ld      a,(hl)
        jr      l3ebb                   ; (-$16)
l3ed1:  ld      b,d
        ld      d,$00
        ld      a,e
        cp      $27
        ret     nc

        cp      $18
        jr      nz,l3edf                ; (+$03)
        bit     7,b
        ret     nz

l3edf:  ld      hl,l2239
        add     hl,de
        ld      a,(hl)
        scf     
        ret     

l3ee6:  push    af
        xor     a
        ld      (timeout),a
        ld      a,(BANK678)
        and     $f7
        call    l21ba
        pop     af
        ret     

l3ef5:  call    l2134
        call    l21f7
        jp      l20ef
        nop     
        nop

; ==============================================================

n3f00:	ld      (OLDHL),hl
        ld      (OLDBC),bc
        push    af
        pop     hl
        ld      (OLDAF),hl
        ex      (sp),hl
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        ex      (sp),hl
        ld      hl,x3f42
        push    hl
        push    bc
        pop     hl
        ld      a,(BANKM)
        and     $ef
        di      
        ld      (BANKM),a
        ld      bc,$7ffd
        out     (c),a
        ld      a,(BANK678)
        or      $04
        ld      (BANK678),a
        ld      bc,$1ffd
        out     (c),a
        ei      
        push    hl
        ld      hl,(OLDAF)
        push    hl
        pop     af
        ld      bc,(OLDBC)
        ld      hl,(OLDHL)
        ret     

x3f42:	push    bc
        push    af
        ld      a,(BANK678)
        and     $fb
        di      
        ld      (BANK678),a
        ld      bc,$1ffd
        out     (c),a
        ld      a,(BANKM)
        or      $10
        ld      (BANKM),a
        ld      bc,$7ffd
        out     (c),a
        ei      
        pop     af
        pop     bc
        ret     

n3f63:	push    bc
        push    af
        ld      a,(BANK678)
        and     $fb
        di      
        ld      (BANK678),a
        ld      bc,$1ffd
        out     (c),a
        ei      
        pop     af
        pop     bc
        ret     

x3f77:  ld      a,e
        cp      $3a
        jr      c,l3fab                 ; (+$2f)
        dec     c
        jp      m,l3f93
        jr      z,l3f85                 ; (+$03)
        add     a,$4f
        ret     

l3f85:  ld      hl,$221f
        inc     b
        jr      z,l3f8e                 ; (+$03)
        ld      hl,$2239
l3f8e:  ld      d,$00
        add     hl,de
        ld      a,(hl)
        ret     

l3f93:  ld      hl,$225d
        bit     0,b
        jr      z,l3f8e                 ; (-$0c)
        bit     3,d
        jr      z,l3fa8                 ; (+$0a)
        bit     3,(iy+$30)
        ret     nz

        inc     b
        ret     nz

        add     a,$20
        ret     

l3fa8:  add     a,$a5
        ret     

l3fab:  cp      $30
        ret     c

        dec     c
        jp      m,l3fe1
        jr      nz,l3fcd                ; (+$19)
        ld      hl,$2288
        bit     5,b
        jr      z,l3f8e                 ; (-$2d)
        cp      $38
        jr      nc,l3fc6                ; (+$07)
        sub     $20
        inc     b
        ret     z

        add     a,$08
        ret     

l3fc6:  sub     $36
        inc     b
        ret     z

        add     a,$fe
        ret     

l3fcd:  ld      hl,$2264
        cp      $39
        jr      z,l3f8e                 ; (-$46)
        cp      $30
        jr      z,l3f8e                 ; (-$4a)
        and     $07
        add     a,$80
        inc     b
        ret     z

        xor     $0f
        ret     

l3fe1:  inc     b
        ret     z

        bit     5,b
        ld      hl,$2264
        jr      nz,l3f8e                ; (-$5c)
        sub     $10
        cp      $22
        jr      z,l3ff6                 ; (+$06)
        cp      $20
        ret     nz

        ld      a,$5f
        ret     

l3ff6:  ld      a,$40
        ret     

	.db    $ff, $ff, $ff, $ff, $ff, $ff, $ff
