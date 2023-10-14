	output	"p3t_rom1.rom"

	org $0000
	
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

	define  DOS_INITIALISE      $0100
	define  DOS_VERSION         $0103
	define  DOS_OPEN            $0106
	define  DOS_CLOSE           $0109
	define  DOS_ABANDON         $010c
	define  DOS_REF_HEAD        $010f
	define  DOS_READ            $0112
	define  DOS_WRITE           $0115
	define  DOS_BYTE_READ       $0118
	define  DOS_BYTE_WRITE      $011b
	define  DOS_CATALOG         $011e
	define  DOS_FREE_SPACE      $0121
	define  DOS_DELETE          $0124
	define  DOS_RENAME          $0127
	define  DOS_BOOT            $012a
	define  DOS_SET_DRIVE       $012d
	define  DOS_SET_USER        $0130
	define  DOS_GET_POSITION    $0133
	define  DOS_SET_POSITION    $0136
	define  DOS_GET_EOF         $0139
	define  DOS_GET_1346        $013c
	define  DOS_SET_1346        $013f
	define  DOS_FLUSH           $0142
	define  DOS_SET_ACCESS      $0145
	define  DOS_SET_ATTRIBUTES  $0148
	define  DOS_OPENDRIVE       $014b
	define  DOS_SET_MESSAGE     $014e
	define  DOS_REF_XDPB        $0151
	define  DOS_MAP_B           $0154

	define  DD_INTERFACE        $0157
	define  DD_INIT             $015a
	define  DD_SETUP            $015d
	define  DD_SET_RETRY        $0160
	define  DD_READ_SECTOR      $0163
	define  DD_WRITE_SECTOR     $0166
	define  DD_CHECK_SECTOR     $0169
	define  DD_FORMAT           $016c
	define  DD_READ_ID          $016f
	define  DD_TEST_UNSUITABLE  $0172
	define  DD_LOGIN            $0175
	define  DD_SEL_FORMAT       $0178
	define  DD_ASK_1            $017b
	define  DD_DRIVE_STATUS     $017e
	define  DD_EQUIPMENT        $0181
	define  DD_ENCODE           $0184
	define  DD_L_XDPB           $0187
	define  DD_L_DPB            $018a
	define  DD_L_SEEK           $018d
	define  DD_L_READ           $0190
	define  DD_L_WRITE          $0193
	define  DD_L_ON_MOTOR       $0196
	define  DD_L_T_OFF_MOTOR    $0199
	define  DD_L_OFF_MOTOR      $019c

; +3DOS Error codes

	define  rc_ready    $00
	define  rc_wp       $01
	define  rc_seek     $02
	define  rc_crc      $03
	define  rc_nodata   $04
	define  rc_mark     $05
	define  rc_unrecog  $06
	define  rc_unknown  $07
	define  rc_diskchg  $08
	define  rc_unsuit   $09

	define  rc_badname  $14
	define  rc_badparam $15
	define  rc_nodrive  $16
	define  rc_nofile   $17
	define  rc_exists   $18
	define  rc_eof      $19
	define  rc_diskfull $1a
	define  rc_dirfull  $1b
	define  rc_ro       $1c
	define  rc_number   $1d
	define  rc_denied   $1e
	define  rc_norename $1f
	define  rc_extent   $20
	define  rc_uncached $21
	define  rc_toobig   $22
	define  rc_notboot  $23
	define  rc_inuse    $24
;--------------------------------------------------

; The floating-point calculator commands

;        include "fpcalc.def"
	define  jump_true $00
	define  exchange  $01
	define  delete    $02
	define  subtract  $03
	define  multiply  $04
	define  division  $05
	define  to_power  $06
	define  logic_or  $07
	define  no_and_no $08
	define  no_l_eql  $09
	define  no_gr_eq  $0a
	define  nos_neql  $0b
	define  no_grtr   $0c
	define  no_less   $0d
	define  nos_eql   $0e
	define  addition  $0f
	define  strandno  $10
	define  str_l_eql $11
	define  str_gr_eq $12
	define  strs_neql $13
	define  str_grtr  $14
	define  str_less  $15
	define  strs_eql  $16
	define  strs_add  $17
	define  val_str   $18
	define  usr_str   $19
	define  read_in   $1a
	define  negate    $1b
	define  code      $1c
	define  val       $1d
	define  len       $1e
	define  sin       $1f
	define  cos       $20
	define  tan       $21
	define  asn       $22
	define  acs       $23
	define  atn       $24
	define  ln        $25
	define  exp       $26
	define  int       $27
	define  sqr       $28
	define  sgn       $29
	define  abs       $2a
	define  peek      $2b
	define  in_port   $2c
	define  usr_no    $2d
	define  str_str   $2e
	define  chr_str   $2f
	define  not       $30
	define  duplicate $31
	define  n_mod_m   $32
	define  jump      $33
	define  stk_data  $34
	define  dec_jr_nz $35
	define  less_0    $36
	define  greater_0 $37
	define  end_calc  $38
	define  get_argt  $39
	define  truncate  $3a
	define  fp_calc_2 $3b
	define  e_to_fp   $3c
	define  re_stack  $3d
	define  series_06 $86
	define  series_08 $88
	define  series_0c $8c
	define  stk_zero  $a0
	define  stk_one   $a1
	define  stk_half  $a2
	define  stk_pi_2  $a3
	define  stk_ten   $a4
	define  st_mem_0  $c0
	define  st_mem_1  $c1
	define  st_mem_2  $c2
	define  st_mem_3  $c3
	define  st_mem_4  $c4
	define  st_mem_5  $c5
	define  get_mem_0 $e0
	define  get_mem_1 $e1
	define  get_mem_2 $e2
	define  get_mem_3 $e3
	define  get_mem_4 $e4
	define  get_mem_5 $e5
;--------------------------------------------------

	.org     $0000

; ROM 1 Header

m0000   defm    "Syntax"
	defs    2

; RST $08 - The "Error" restart

m0008   jp      m2ada           ; jump to error handler

	defs    5

; RST $10 - The "Print a character restart"

m0010   rst     $28
	DW      $0010           ; call RST $10 in ROM 3
	ret

	defs    4

; RST $18 - The "Collect character" restart

m0018   rst     $28
	DW      $0018           ; call RST $18 in ROM 3
	ret

	defs    4

; RST $20 - The "Collect next character" restart

m0020   rst     $28
	DW      $0020           ; call RST $20 in ROM 3
	ret

	defs    4

; RST $28 : Call a routine in ROM 3, then return to ROM 1
; The address following the RST 28 instruction is called, then control
; is returned to the instruction following the address

m0028   ex      (sp),hl         ; save HL, get return address
	push    af              ; save AF
	ld      a,(hl)          ; A=low byte of address to call
	inc     hl
	inc     hl              ; HL=address of instruction to return to
	ld      (RETADDR),hl    ; save
m0030   dec     hl
	ld      h,(hl)
	ld      l,a             ; HL=address to call in ROM 3
	pop     af              ; restore AF
	jp      m00aa           ; jump on

	nop

; The maskable interrupt routine

m0038   push    af              ; save registers
	push    hl
	ld      hl,(FRAMES)     ; increment FRAMES
	inc     hl
	ld      (FRAMES),hl
	ld      a,h
	or      l
	jr      nz,m0048
	inc     (iy+$40)
m0048   push    bc
	push    de
	call    m0176           ; scan keyboard
	call    m0074           ; call disk motor timeout routine
	pop     de              ; restore registers
	pop     bc
	pop     hl
	pop     af
	ei                      ; re-enable interrupts & exit
	ret
      IF garry
m0056   defm    "Start: ", 0
m005e   defm    "system", 0, 0
      ELSE
	defs    $10
      ENDIF

; The NMI routine

m0066   push    af              ; save registers
	push    hl
	ld      hl,(NMIADD)     ; get routine address
	ld      a,h
	or      l
	jr      z,m0070
	jp      (hl)            ; execute if non-zero address
m0070   pop     hl              ; restore registers & exit
	pop     af
	retn

; The disk motor timeout routine

m0074   ld      bc,$7ffd
	ld      a,(BANKM)
	or      $07
	out     (c),a           ; page in page 7
	ld      a,(timeout)
	or      a
	jr      z,m00a1         ; move on if already off
	ld      a,(FRAMES)
	bit     0,a
	jr      nz,m00a1        ; only decrement counter every other frame
	ld      a,(timeout)
	dec     a               ; decrement timeout counter
	ld      (timeout),a
	jr      nz,m00a1        ; move on if non-zero
	ld      bc,$1ffd
	ld      a,(BANK678)
	and     $f7
	ld      (BANK678),a
	out     (c),a           ; turn motor off
m00a1   ld      bc,$7ffd
	ld      a,(BANKM)
	out     (c),a           ; page in last memory configuration
	ret

; Continuation of RST 28: call a routine in ROM 3

m00aa   ld      (TARGET),hl     ; save ROM 3 address in TARGET
	ld      hl,REGNUOY
	ex      (sp),hl         ; stack REGNUOY address beneath TOS
	push    hl
	ld      hl,(TARGET)     ; get HL=target address in ROM 3
	ex      (sp),hl         ; restore HL & save target address on stack
	push    af              ; stack AF & BC
	push    bc
	di                      ; disable interrupts
	jp      STOO            ; jump to STOO - pages in ROM 3, returns to
				; target routine which returns to REGNUOY
				; where ROM 1 is paged back and jump made
				; back to RETADDR


; These are copies of the key tables from ROM 3


; The L-mode keytable with CAPS-SHIFT

m00bc   defm    "BHY65TGV"
	defm    "NJU74RFC"
	defm    "MKI83EDX"
	defm    $0e, "LO92WSZ"
	defm    " ", $0d, "P01QA"

; The extended-mode keytable (unshifted letters)

m00e3   defb    $e3,$c4,$e0,$e4
	defb    $b4,$bc,$bd,$bb
	defb    $af,$b0,$b1,$c0
	defb    $a7,$a6,$be,$ad
	defb    $b2,$ba,$e5,$a5
	defb    $c2,$e1,$b3,$b9
	defb    $c1,$b8

; The extended mode keytable (shifted letters)

m00fd   defb    $7e,$dc,$da,$5c
	defb    $b7,$7b,$7d,$d8
	defb    $bf,$ae,$aa,$ab
	defb    $dd,$de,$df,$7f
	defb    $b5,$d6,$7c,$d5
	defb    $5d,$db,$b6,$d9
	defb    $5b,$d7

; The control code keytable (CAPS-SHIFTed digits)

m0117   defb    $0c,$07,$06,$04
	defb    $05,$08,$0a,$0b
	defb    $09,$0f

; The symbol code keytable (letters with symbol shift)

m0121   defb    $e2,$2a,$3f,$cd
	defb    $c8,$cc,$cb,$5e
	defb    $ac,$2d,$2b,$3d
	defb    $2e,$2c,$3b,$22
	defb    $c7,$3c,$c3,$3e
	defb    $c5,$2f,$c9,$60
	defb    $c6,$3a

; The extended mode keytable (SYM-SHIFTed digits)

m013b   defb    $d0,$ce,$a8,$ca
	defb    $d3,$d4,$d1,$d2
	defb    $a9,$cf


; This is a copy of the "keyboard scanning" subroutine from
; $028e in ROM 3

m0145   ld      l,$2f
	ld      de,$ffff
	ld      bc,$fefe
m014d   in      a,(c)
	cpl
	and     $1f
	jr      z,m0162
	ld      h,a
	ld      a,l
m0156   inc     d
	ret     nz
m0158   sub     $08
	srl     h
	jr      nc,m0158
	ld      d,e
	ld      e,a
	jr      nz,m0156
m0162   dec     l
	rlc     b
	jr      c,m014d
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

; This is a copy of the "keyboard" subroutines from $02bf in ROM 3

m0176   call    m0145
	ret     nz
	ld      hl,KSTATE
m017d   bit     7,(hl)
	jr      nz,m0188
	inc     hl
	dec     (hl)
	dec     hl
	jr      nz,m0188
	ld      (hl),$ff
m0188   ld      a,l
	ld      hl,KSTATE+$04
	cp      l
	jr      nz,m017d
	call    m01d5
	ret     nc
	ld      hl,KSTATE
	cp      (hl)
	jr      z,m01c7
	ex      de,hl
	ld      hl,KSTATE+$04
	cp      (hl)
	jr      z,m01c7
	bit     7,(hl)
	jr      nz,m01a8
	ex      de,hl
	bit     7,(hl)
	ret     z
m01a8   ld      e,a
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
	call    m01ea
	pop     hl
	ld      (hl),a
m01bf   ld      (LAST_K),a
	set     5,(iy+$01)
	ret
m01c7   inc     hl
	ld      (hl),$05
	inc     hl
	dec     (hl)
	ret     nz
	ld      a,(REPPER)
	ld      (hl),a
	inc     hl
	ld      a,(hl)
	jr      m01bf

; This is a copy of the "K-Test" subroutine from $031e in ROM 3

m01d5   ld      b,d
	ld      d,$00
	ld      a,e
	cp      $27
	ret     nc
	cp      $18
	jr      nz,m01e3
	bit     7,b
	ret     nz
m01e3   ld      hl,m00bc        ; the main keytable
	add     hl,de
	ld      a,(hl)
	scf
	ret

; This is a copy of the "Keyboard decoding" subroutine from $0333 in
; ROM 3

m01ea   ld      a,e
	cp      $3a
	jr      c,m021e
	dec     c
	jp      m,m0206
	jr      z,m01f8
	add     a,$4f
	ret
m01f8   ld      hl,m00e3-'A'
	inc     b
	jr      z,m0201
	ld      hl,m00fd-'A'
m0201   ld      d,$00
	add     hl,de
	ld      a,(hl)
	ret
m0206   ld      hl,m0121-'A'
	bit     0,b
	jr      z,m0201
	bit     3,d
	jr      z,m021b
	bit     3,(iy+$30)
	ret     nz
	inc     b
	ret     nz
	add     a,$20
	ret
m021b   add     a,$a5
	ret
m021e   cp      $30
	ret     c
	dec     c
	jp      m,m0254
	jr      nz,m0240
	ld      hl,m013b-'0'
	bit     5,b
	jr      z,m0201
	cp      $38
	jr      nc,m0239
	sub     $20
	inc     b
	ret     z
	add     a,$08
	ret
m0239   sub     $36
	inc     b
	ret     z
	add     a,$fe
	ret
m0240   ld      hl,m0117-'0'
	cp      $39
	jr      z,m0201
	cp      $30
	jr      z,m0201
	and     $07
	add     a,$80
	inc     b
	ret     z
	xor     $0f
	ret
m0254   inc     b
	ret     z
	bit     5,b
	ld      hl,m0117-'0'
	jr      nz,m0201
	sub     $10
	cp      $22
	jr      z,m0269
	cp      $20
	ret     nz
	ld      a,$5f
	ret
m0269   ld      a,$40
	ret


; The FORMAT command

m026c   rst     $28
	DW      $0018           ; get character after FORMAT
m026f   cp      $e0
	jp      z,m03e3         ; move on if LPRINT
	cp      $ca
      IF garry
	jp      z, m1e02        ; move on if not LINE
	cp      $cc
	jp      z, m1dd9
      ELSE
	jr      nz,m027e        ; move on if not LINE
	rst     $28
	DW      $0020           ; get next character
	jp      m1e05           ; and move on for FORMAT LINE
      ENDIF
m027e   rst     $28
	DW      $1c8c           ; get a string expression
	call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2bf1           ; get string from stack
	ld      a,c
	dec     a
	dec     a
	or      b
	jr      z,m0291         ; move on if length is 2
m028d   call    m2ada
	defb    $4e             ; else error "Invalid drive"
m0291   inc     de
	ld      a,(de)          ; check 2nd char
	dec     de
	cp      ':'
	jr      z,m029c
	call    m2ada
	defb    $4e             ; error "Invalid drive" if not colon
m029c   ld      a,(de)
	and     $df             ; get capitalised drive letter
	cp      'A'
	jr      z,m02ab         ; move on if A:
	cp      'B'
	jr      z,m02ab         ; or B:
	call    m2ada
	defb    $4e             ; else error "Invalid drive"
m02ab   call    m2b89           ; page in DOS workspace
	sub     'A'
	push    af              ; save unit number to format
	ld      hl,FLAGS3
	bit     4,(hl)
	jr      nz,m02bf        ; move on if disk interface present
	call    m2b64           ; page in normal memory
	call    m2ada
	defb    $4c             ; else error "Format not supported on +2A"
m02bf   pop     af
	or      a
	jr      z,m02d3         ; move on for unit 0
	push    af
	ld      hl,FLAGS3
	bit     5,(hl)
	jr      nz,m02d2        ; move on if drive B: present
	call    m2b64           ; page in normal memory
	call    m2ada
	defb    $4b             ; else error "Drive B: not present"
m02d2   pop     af              ; get unit
m02d3   push    af
	ld      c,a
	push    bc
	add     a,'A'
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_REF_XDPB    ; point IX at XDPB
	call    m32ee           ; restore TSTACK
	jr      c,m02ec         ; move on if no error
	call    m2b64           ; page in DOS memory
	call    m0e9a           ; cause DOS error
	defb    $ff
m02ec   pop     bc
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DD_LOGIN        ; login disk
	call    m32ee           ; restore TSTACK
	jr      nc,m0306        ; move on if error
	or      a
	jr      nz,m0315        ; move on if disk isn't +3 format
	call    m0381           ; ask if wish to abandon
	jr      nz,m0315        ; move on if not
	call    m2b64           ; page in normal memory
	ret                     ; exit
m0306   cp      $05
	jr      z,m0315         ; move on if error was "missing address mark"
	cp      $09
	jr      z,m0315         ; or "unsuitable media"
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause DOS error
	defb    $ff
m0315   pop     af              ; get unit number
	push    af
	add     a,'A'
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_REF_XDPB    ; point IX to XDPB
	call    m32ee           ; restore TSTACK
	jr      c,m032d
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause any DOS error
	defb    $ff
m032d   xor     a
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DD_SEL_FORMAT   ; select +3 format
	call    m32ee           ; restore TSTACK
	jr      c,m0342
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause any DOS error
	defb    $ff
m0342   pop     af
	ld      c,a             ; C=unit number
	xor     a               ; start at track 0
m0345   ld      d,a
	call    m036f           ; fill format buffer
	ld      e,$e5           ; filler byte
	ld      b,$07           ; page 7
	ld      hl,tmp_buff     ; buffer address
	push    af
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DD_FORMAT       ; format a track
	call    m32ee           ; restore TSTACK
	jr      c,m0365
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause any DOS error
	defb    $ff
m0365   pop     af
	inc     a               ; increment track
	cp      $28
	jr      nz,m0345        ; back if more to do
	call    m2b64           ; page in normal memory
	ret                     ; done

; Subroutine to fill scratch area with format buffer details

m036f   ld      b,$09           ; 9 sectors
	ld      hl,tmp_buff+$23 ; end of scratch area
m0374   ld      (hl),$02        ; 512-byte sectors
	dec     hl
	ld      (hl),b          ; sector number
	dec     hl
	ld      (hl),$00        ; head 0
	dec     hl
	ld      (hl),d          ; track number
	dec     hl
	djnz    m0374
	ret

; Subroutine to display "disk already formatted message",
; and get a key, exiting with Z set if user wishes to abandon

m0381   ld      hl,m03a7
m0384   ld      a,(hl)          ; get next char
	or      a
	jr      z,m038e         ; move on if null
	rst     $28
	DW      $0010           ; output char
m038b   inc     hl
	jr      m0384           ; loop back
m038e   res     5,(iy+$01)      ; signal "no key"
m0392   bit     5,(iy+$01)
	jr      z,m0392         ; wait for key
	ld      a,(LAST_K)      ; get key
	and     $df             ; capitalise
	cp      'A'             ; is it "A"?
	push    af
	push    hl
	rst     $28
	DW      $0d6e           ; clear lower screen
	pop     hl
	pop     af
	ret                     ; exit with Z set if abandon requested

; Formatting message

      IF spanish
m03a7   defm    "Ya formateado. Tecla A para", $0d
	defm    "abandonar/otra para continuar", 0
      ELSE
m03a7   defm    "Disk is already formatted.", $0d
	defm    "A to abandon, other key continue", 0
      ENDIF

; The FORMAT LPRINT command

m03e3   rst     $28
	DW      $0020           ; get next char
m03e6   rst     $28
	DW      $1c8c           ; get string expression
	rst     $28
	DW      $0018           ; get next char
m03ec   cp      ';'
	call    nz,m10b1        ; check for end-of-statement if not ";"
	jr      nz,m041c        ; move on if not ";"
	rst     $28
	DW      $0020           ; get next char
m03f6   rst     $28
	DW      $1c8c           ; get string expression
	call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2bf1           ; get 2nd string from stack
	ld      a,c
	dec     a
	or      b               ; check length
	jr      z,m0407
      IF v41
	jp      m045b           ; "Invalid drive" error if not 1
      ELSE
	jp      m028d           ; "Invalid drive" error if not 1
      ENDIF
m0407   ld      a,(de)
	and     $df             ; capitalise 2nd string character
	ld      hl,FLAGS3       ; prepare to change FLAGS3
	cp      'E'
	jr      nz,m0415
m0411   set     2,(hl)          ; if 2nd string "E", set "expand tokens" flag
	jr      m041c
m0415   cp      'U'
      IF v41
	jp      nz,m045b        ; if 2nd string not "U", error
      ELSE
	jp      nz,m028d        ; if 2nd string not "U", error
      ENDIF
	res     2,(hl)          ; if "U", reset "expand tokens" flag
m041c   rst     $28
	DW      $2bf1           ; get first string from stack
	ld      a,c
	dec     a
	or      b               ; check length
	jr      z,m0427
      IF v41
	jp      m045b           ; "Invalid drive" error if not 1
      ELSE
	jp      m028d           ; "Invalid drive" error if not 1
      ENDIF
m0427   ld      a,(de)
	and     $df             ; capitalise 1st string character
	ld      hl,FLAGS3       ; prepare to change FLAGS3
	cp      'R'
	jr      nz,m0434
	set     3,(hl)          ; if "R", set print to RS232 flag
	ret
m0434   cp      'C'
	jr      nz,m043b
	res     3,(hl)          ; if "C", reset print to RS232 flag
	ret
m043b   cp      'E'
	jr      nz,m0442
	set     2,(hl)          ; if "E", set "expand tokens" flag
	ret
m0442   cp      'U'
      IF v41
	jr      nz,m0449
	res     2,(hl)
	ret
m0449   ld      hl,FLAGS2
	cp      'N'
	jr      nz,m0453
	res     6,(hl)
	ret
m0453   cp      'A'
	jp      nz,m045b
	set     6,(hl)
	ret
m045b   call    m2ada
	dec     bc
      ELSE
	jp      nz,m028d        ; if not "U", error
	res     2,(hl)          ; if "U", reset "expand tokens" flag
	ret
      ENDIF

; The ERASE command
; *BUG* No channel is opened before outputting the "Erase (Y/N)?" message,
;       so this is output to the last used stream.
; *BUG* The lower screen is not cleared if "N" is pressed

m044a   rst     $28
	DW      $2bf1           ; get string from stack
	ld      a,b
	or      c               ; check length
	jr      nz,m0455
	call    m2ada
	defb    $2c             ; bad filename error if zero
m0455   push    bc              ; save addresses
	push    de
	push    de
	pop     hl              ; HL=address of filename
	push    bc
	ld      a,'*'
	cpir
	pop     bc
	jr      z,m046d         ; move on if * wildcard present
	push    de
	pop     hl
	push    bc
	ld      a,'?'
	cpir
	pop     bc
	jr      z,m046d         ; move on if ? wildcard present
	jr      m0499           ; move on for a single file
m046d
      IF garry
	ld      hl, merase
      ELSE
	ld      hl,m04d5
      ENDIF
	call    m04c1           ; output "Erase "
	call    m04ca           ; output filespec
      IF garry
	ld      hl, myn
      ELSE
	ld      hl,m04dc
      ENDIF
	call    m04c1           ; output "? (Y/N"
m047c   ld      hl,FLAGS
	res     5,(hl)          ; signal "no key available"
m0481   bit     5,(hl)
	jr      z,m0481         ; loop until keypress
	res     5,(hl)          ; signal "no key available"
	ld      a,(LAST_K)      ; get key
	and     $df             ; make uppercase
	cp      'N'
	jr      nz,m0493        ; move on if not "N"
	pop     de              ; exit without doing anything
	pop     bc              ; (lower screen should have been cleared)
	ret
      IF spanish
m0493   cp      'S'
      ELSE
m0493   cp      'Y'
      ENDIF
	jr      z,m0499
	jr      m047c           ; loop back for another key if not "Y"
m0499   rst     $28
	DW      $0d6e           ; clear lower screen
	pop     de
	pop     bc
	ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy filespec into page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
	ld      hl,tmp_fspec
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_DELETE      ; delete filespec
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	ret     c               ; exit if ok
	call    m0e9a           ; cause DOS error
	defb    $ff

; Subroutine to output a null-terminated string

m04c1   ld      a,(hl)          ; get next char
	or      a
	ret     z               ; exit if null
	inc     hl
	rst     $28
	DW      $0010           ; output char
m04c8   jr      m04c1           ; loop back

; Subroutine to output a filespec at DE, length BC

m04ca   ld      a,(de)          ; get next char
	rst     $28
	DW      $0010           ; output char
m04ce   inc     de
	dec     bc
	ld      a,b
	or      c
	jr      nz,m04ca        ; back for more
	ret

; Erase messages
    IF garry
m04d5   rst     $28
	DW      $c101
	ld      (bc), a
	inc     (hl)
	ld      b, b
	ld      b, c
	nop
	nop
	ld      ($38e1), a
	ret
	defb    0, 0, 0
    ELSE
      IF spanish
m04d5   defm    "]Borrar ", 0
m04dc   defm    " (S/N)?", 0
      ELSE
m04d5   defm    "Erase ", 0
m04dc   defm    " ? (Y/N)", 0
      ENDIF
    ENDIF

; The MOVE command

m04e5   rst     $28
	DW      $2bf1           ; get 2nd string
	ld      a,b
	or      c               ; check length
	jr      nz,m04f0
	call    m2ada
	defb    $2c             ; bad filename error if zero
m04f0   ld      a,(de)
	cp      '+'
	jp      z,m0541         ; move on if changing attributes
	cp      '-'
	jp      z,m0541         ; move on if changing attributes
	ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy filename to page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
m0508   inc     de
	call    m2b64           ; page in normal memory
	push    de              ; save pointer for source filename
	rst     $28
	DW      $2bf1           ; get 1st string
	ld      a,b
	or      c               ; check length
	jr      nz,m0518
	call    m2ada
	defb    $2c             ; bad filename error if zero
m0518   pop     hl              ; HL=address to place source filename
	push    hl
	ex      de,hl
	call    m3f63           ; copy source filename to page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
	call    m2b64           ; page in normal memory
	pop     hl
	ld      de,tmp_fspec
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_RENAME      ; do rename
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	ret     c               ; exit if done ok
	call    m0e9a           ; cause DOS error
	defb    $ff

; Here we use MOVE to alter attributes of a file

m0541   ld      a,c
	dec     a
	dec     a
	or      b
	jr      z,m054b         ; move on if 2nd string length=2
	call    m2ada
	defb    $47             ; invalid attribute error
m054b   ld      a,(de)
	ld      b,a             ; B='+' or '-'
	inc     de
	ld      a,(de)
	and     $df             ; A=uppercase attribute
	cp      'P'             ; check attribute letter
	jr      z,m0561
	cp      'S'
	jr      z,m0561
	cp      'A'
	jr      z,m0561
	call    m2ada
	defb    $47             ; invalid attribute error
m0561   push    bc              ; save attribute flags
	push    af
	rst     $28
	DW      $2bf1           ; get 1st string
	ld      a,b
	or      c               ; check length
	jr      nz,m056e
	call    m2ada
	defb    $2c             ; bad filename error if zero
m056e   ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy to page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
	call    m2b64           ; page in normal memory
	ld      de,$0000        ; don't set or clear anything yet
	ld      c,$00           ; attribute byte to set/clear
	pop     af              ; get attribute letter
	cp      'P'
	jr      nz,m058c
	set     2,c             ; bit 2 for P
	jr      m0596
m058c   cp      'S'
	jr      nz,m0594
	set     1,c             ; bit 1 for S
	jr      m0596
m0594   set     0,c             ; bit 0 for A
m0596   pop     af              ; get '+' or '-'
	cp      '+'
	jr      nz,m059e
	ld      d,c             ; if +, we're setting attributes
	jr      m059f
m059e   ld      e,c             ; if -, we're clearing attributes
m059f   ld      hl,tmp_fspec
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_ATTRIBUTES ; set the attributes
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	ret     c               ; exit if done ok
	call    m0e9a           ; else cause DOS error
	defb    $ff

; The CAT command
; *BUG* Only one buffer of entries is ever considered (64 entries), as a
;       SUB $40 is used (should be SUB $3f)

m05b8   ld      hl,FLAGS3
	res     6,(hl)          ; signal "standard catalog"
	rst     $28
	DW      $2070           ; consider stream information
	jr      c,m05dd         ; move on if default stream to be used
	ld      hl,(CH_ADD)
	ld      a,(hl)          ; get next char
	cp      ','
	jr      z,m05da         ; move on if comma to get filespec
	cp      $0d
	jr      z,m062b         ; move on if end-of-line
	cp      ':'
	jr      z,m062b         ; or if end-of-statement
	cp      $b9
	jr      z,m062b         ; or if EXP
      IF garry
	jp      m3b48
	nop
      ELSE
	call    m2ada
	defb    $0b             ; else nonsense in BASIC error
      ENDIF
m05da   rst     $20             ; get next char
	jr      m05f8
m05dd   ld      a,$02           ; use stream 2
	bit     7,(iy+$01)
	jr      z,m05e8         ; move on if only syntax-checking
	rst     $28
	DW      $1601           ; else open channel to stream
m05e8   ld      hl,(CH_ADD)
	ld      a,(hl)          ; check next char
	cp      $0d
	jr      z,m062b         ; move on if end-of-line
	cp      ':'
	jr      z,m062b         ; or if end-of-statement
      IF garry
	jp      n3b4d
	nop
      ELSE
	cp      $b9
	jr      z,m062b         ; or if EXP
      ENDIF
m05f8   rst     $28
	DW      $1c8c           ; get string expression
	rst     $28
	DW      $0018           ; get next char
m05fe   cp      $b9
	jr      nz,m060a        ; move on if not EXP
	ld      hl,FLAGS3
	set     6,(hl)          ; signal "expanded catalog"
	rst     $28
	DW      $0020           ; get next char
m060a   call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2bf1           ; get string value from stack
	push    bc
	push    de
	pop     hl              ; HL=string address
	ld      a,':'           ; check for drive specification
	cpir
	jr      nz,m0623        ; move on if not found
	dec     hl
	dec     hl
	ld      a,(hl)
	and     $df
	ld      (DEFADD),a      ; else save capitalised drive letter
	jr      m0628           ; move on
m0623   ld      a,$00
	ld      (DEFADD),a      ; signal "use default drive"
m0628   pop     bc
	jr      m0645           ; move on
m062b   rst     $28
	DW      $0018           ; get next char
m062e   cp      $b9
	jr      nz,m063a        ; move on if not EXP
	ld      hl,FLAGS3
	set     6,(hl)          ; signal "expanded catalog"
	rst     $28
	DW      $0020           ; get next char
m063a   call    m10b1           ; check for end-of-statement
	ld      bc,$0000        ; filespec length=0
	ld      a,$00
	ld      (DEFADD),a      ; signal "use default drive"
m0645   ld      a,c
	dec     a
	dec     a
	or      b
	jr      nz,m065c        ; move on unless just 2 chars specified
	inc     de
	ld      a,(de)
	dec     de
	cp      ':'
	jr      nz,m065c        ; move on if not drive specifier
	ld      a,(de)
	and     $df             ; get drive letter capitalised
	cp      'T'
	jr      nz,m065c
	jp      m34c6           ; move on to catalog tape
m065c   ld      hl,tmp_fspec
	ex      de,hl
	push    bc
	ld      a,b
	or      c
	jr      z,m0668         ; move on if no filespec
	call    m3f63           ; copy to page 7 (entry 0)
m0668   pop     bc
	ld      hl,tmp_fspec
	add     hl,bc
	call    m2b89
	ld      (hl),$ff        ; add terminator
	ld      hl,tmp_buff
	ld      de,tmp_buff+1
	ld      bc,$000b
	ld      (hl),$00
	ldir                    ; zero entry 0
m067f   ld      b,$40           ; 64 entries in buffer
	ld      c,$00           ; C=0 for standard catalog
	ld      hl,FLAGS3
	bit     6,(hl)
	jr      z,m068c
	ld      c,$01           ; C=1 for expanded catalog (inc system files)
m068c   ld      de,tmp_buff
	ld      hl,tmp_fspec
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CATALOG     ; get next lot of entries
	call    m32ee           ; restore TSTACK
	jp      nc,m06ab        ; move on if error
	ld      hl,tmp_buff+$0d ; address of first returned entry
	dec     b               ; B=# entries found (discard preloaded one)
	ld      a,b
	or      a
	jr      nz,m06b7        ; move on if entries to display
	jp      m07ba           ; move on if catalog finished
m06ab   cp      $17
	jp      z,m07ba         ; move on if error "file not found"
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause DOS error
	defb    $ff
m06b7   push    bc              ; save number of entries to do
m06b8   push    af
	ld      b,$08           ; 8 bytes in first half of filename
m06bb   ld      a,(hl)
	and     $7f             ; get byte and mask bit
	call    m2b64           ; page in normal memory
	rst     $28
	DW      $0010           ; output char
m06c4   call    m2b89           ; page in DOS workspace
	inc     hl
	djnz    m06bb           ; loop back for rest of filename
	call    m2b64           ; page in normal memory
	ld      a,'.'
	rst     $28
	DW      $0010           ; output "."
m06d2   xor     a
	ld      (RAMERR),a      ; zeroise attributes
	ld      b,$03
m06d8   call    m2b89           ; page in DOS workspace
	ld      a,(hl)          ; get next byte
	bit     7,a
	jr      z,m06fc         ; move on if bit 7 not set
	push    af
	push    hl
	ld      hl,RAMERR
	ld      a,b
	cp      $03
	jr      nz,m06ee
	set     3,(hl)          ; set bit 3 if first extension byte
	jr      m06f8
m06ee   cp      $02
	jr      nz,m06f6
	set     2,(hl)          ; set bit 2 if second extension byte
	jr      m06f8
m06f6   set     1,(hl)          ; set bit 1 if third extension byte
m06f8   pop     hl              ; restore values
	pop     af
	and     $7f             ; mask bit 7
m06fc   call    m2b64           ; page in normal memory
	rst     $28
	DW      $0010           ; output char
m0702   inc     hl
	djnz    m06d8           ; loop back for more extension
	push    hl
	ld      hl,FLAGS3
	bit     6,(hl)          ; test if want expanded catalog
	pop     hl
	jr      z,m073e         ; if not, move on
m070e   ld      a,(RAMERR)      ; get attributes
	push    hl
	ld      hl,m0812        ; blank message
	bit     3,a
	jr      z,m071c
	ld      hl,m0818        ; if bit 3 set, PROT message
m071c   push    af
	call    m07e2           ; output message
	pop     af
	ld      hl,m0813        ; blank message
	bit     2,a
	jr      z,m072b
	ld      hl,m081e        ; if bit 2 set, SYS message
m072b   push    af
	call    m07e2           ; output message
	pop     af
	ld      hl,m0813        ; blank message
	bit     1,a
	jr      z,m073a
	ld      hl,m0823        ; if bit 1 set, ARC message
m073a   call    m07e2           ; output message
	pop     hl
m073e   ld      a,' '
	rst     $28
	DW      $0010           ; output space
m0743   push    hl
	call    m2b89           ; page in DOS workspace
	ld      a,(hl)
	inc     hl
	ld      h,(hl)
	inc     hl              ; HA=filesize in K
	call    m2b64           ; page in normal memory
	ld      l,a
	ld      e,' '
      IF garry
	call    m07f1
      ELSE
	call    m0800           ; output filesize
      ENDIF
	pop     hl
	inc     hl
	inc     hl              ; move to next file entry
	ld      a,'K'
	rst     $28
	DW      $0010           ; output "K"
m075c   call    m07eb           ; output CR
	call    m2b89           ; page in DOS workspace
	pop     af
	dec     a
	jp      nz,m06b8        ; move back for more files in buffer
	pop     bc
	ld      a,b
      IF garry
	sub     $3f
      ELSE
	sub     $40             ; was buffer full? (*BUG* should be $3f)
      ENDIF
	jr      c,m077b         ; if not, move on
	ld      hl,$f044
	ld      de,tmp_buff
      IF garry
	ld      bc, 11
	ldir                    ; if so, copy last entry to first
	ex      de, hl
	ld      (hl), d
      ELSE
	ld      bc, 13
	ldir                    ; if so, copy last entry to first
      ENDIF
	jp      m067f           ; and back for more
m077b   call    m2b64           ; page in normal memory
m077e   call    m07eb           ; output CR
	call    m2b89           ; page in DOS workspace
m0784   ld      a,(DEFADD)      ; get drive letter
	or      a
	jr      nz,m079a        ; move on if not default
	ld      a,$ff
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_DRIVE   ; get default drive
	call    m32ee           ; restore TSTACK
	jp      nc,m06ab        ; go if error
m079a   call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_FREE_SPACE  ; get free space on drive
	call    m32ee           ; restore TSTACK
	jp      nc,m06ab        ; go if error
	call    m2b64           ; page in normal memory
      IF garry
	ld      e, $ff
	call    m07f1
      ELSE
	ld      e,' '
	call    m0800           ; output number
      ENDIF
	ld      hl,m07c9
	call    m07e2           ; output "K free" message
	call    m07eb           ; output CR
	ret                     ; done
m07ba
    IF garry
	ld      a, ($ed1c)
	and     a
	call    m2b64           ; page in normal memory
	ld      hl, m07d1
	call    z, m07e2
	jp      m077e
	defb    0, 0, 0
m07cf   call    m2b64
	rst     $10
m07d3   call    m2b89
	ret
m07d7   call    m2b64
	call    m07e2
	jr      m07d3
m07df   call    m2b64
	call    m07f1
	jr      m07d3
    ELSE
	call    m2b64           ; page in normal memory
	ld      hl,m07d1
	call    m07e2           ; output no files message
	call    m2b89           ; page in DOS workspace
	jp      m0784           ; go to display free space
      IF spanish
m07c9   defm    "K LIBRES", $0d, 0
m07d1   defm    "NINGUN FICHERO ENCONTRADO", $0d, $0d, 0
      ELSE
m07c9   defm    "K free", $0d, 0
m07d1   defm    "No files found", $0d, $0d, 0
      ENDIF
    ENDIF

; Subroutine to output a null-terminated message

m07e2   ld      a,(hl)          ; get next char
	or      a
	ret     z               ; exit if null
	rst     $28
	DW      $0010           ; output char
m07e8   inc     hl
	jr      m07e2           ; loop back

; Subroutine to output a CR char

m07eb   ld      a,$0d
	rst     $28
	DW      $0010           ; output CR
m07f0   ret


; Subroutine to output a number up to 65535 (in HL)

m07f1   push    hl
	ld      bc,$d8f0        ; -10000
	rst     $28
	DW      $192a           ; output 10000s
	ld      bc,$fc18        ; -1000
	rst     $28
	DW      $192a           ; output 1000s
	jr      m0801

; Subroutine to output a number up to 999 (in HL)

m0800   push    hl
m0801   ld      bc,$ff9c        ; -100
	rst     $28
	DW      $192a           ; output 100s
	ld      c,$f6           ; -10
	rst     $28
	DW      $192a           ; output 10s
	ld      a,l             ; units
	rst     $28
	DW      $15ef           ; output units
	pop     hl              ; restore number
	ret

; Catalog attribute messages

      IF garry
	defb    0
m0812   
m0813   defm    "    ", 0
	defb    0
m0818   defm    " PRT", 0
      ELSE
m0812   defm    " "
m0813   defm    "    ", 0
m0818   defm    " PROT", 0
      ENDIF
m081e   defm    " SYS", 0
m0823   defm    " ARC", 0

; Subroutine to save a block to tape

m0828   ld      hl,m0830
	push    hl              ; stack SA-RET routine address (why??)
	rst     $28
	DW      $04c6           ; save bytes
	ret
m0830   rst     $28
	DW      $053f           ; SA-RET
	ret

; Subroutine to LOAD/VERIFY a block of data, from tape or disk
; On entry, IX=start, DE=length, A=type (usually $ff), carry set for LOAD
; or reset for VERIFY
; File 0 will be open for disk operations, which should be closed before exit
; On exit, carry is set if okay, reset if error

m0834   push    af
	ld      a,(RAMERR)
	cp      'T'
	jp      z,m0883         ; move on for tape operations
	pop     af
	jr      nc,m087a        ; go to exit for disk verify (won't get here)
	push    hl              ; save registers
	push    de
	push    bc
	ld      b,$00           ; file 0
	ld      c,$00           ; page 0
	push    ix
	pop     hl              ; HL=address
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_READ        ; read the block
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	jr      c,m0865         ; move on to exit if okay
	cp      $19
	jr      nz,m087f        ; move on if error not end-of-file
	call    m0e9a           ; cause error
	defb    $31
m0865   ld      b,$00
	call    m2b89           ; page in DOS workspace
m086a   call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	jr      nc,m087f        ; move on if error
m087a   scf                     ; signal success
	pop     bc              ; restore registers
	pop     de
	pop     hl
	ret
m087f   call    m0e9a           ; cause DOS error
	defb    $ff
m0883   pop     af              ; if tape,restore flags and enter next routine

; Subroutine to call LD-BYTES subroutine in ROM 3

m0884   rst     $28
	DW      $0556           ; call it
	ret

; The SAVE/LOAD/VERIFY/MERGE commands

; section 1 - initialisation

m0888   pop     af              ; discard return address of scan-loop
	ld      a,(T_ADDR)
	sub     m0f83 and $ff   ; store command code (0=SAVE,1=LOAD,
	ld      (T_ADDR),a      ; 2=VERIFY,3=MERGE)
	call    m1129           ; get a string expression
	bit     7,(iy+$01)
	jp      z,m09ba         ; move on if syntax-checking
	ld      bc,$0011        ; 17 bytes required for LOAD
	ld      a,(T_ADDR)
	and     a
	jr      z,m08a6
	ld      c,$22           ; but 34 for others
m08a6   rst     $28
	DW      $0030           ; make space
	push    de
	pop     ix              ; IX points to space
	ld      b,$0b
	ld      a,' '
m08b0   ld      (de),a          ; fill 11-byte name with spaces
	inc     de
	djnz    m08b0
	ld      (ix+$01),$ff    ; place terminator in 2nd byte
	rst     $28
	DW      $2bf1           ; get string value from stack
	push    de
	push    bc

; section 2 - booting a disk

	ld      a,c
	dec     a
	or      b               ; check length
	jr      nz,m08e4        ; move on if not 1
	ld      a,(de)
	cp      '*'
	jr      nz,m08e4        ; or if not "*"
      IF v41
	ld      a,(T_ADDR)
	cp      $01
	jr      nz,m08e4
      ENDIF
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_BOOT        ; boot a disk
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	cp      $23
	jr      nz,m08e0        ; if error isn't "disk not bootable", move on
	call    m0e9a           ; cause error
	defb    $3b
m08e0   call    m0e9a           ; cause DOS error
	defb    $ff

; section 3 - setting drive for operation in RAMERR

m08e4   inc     de
	ld      a,(de)
	dec     de
	cp      ':'
	jr      nz,m08fe        ; move on if no drive specified
	ld      a,(de)
	and     $df             ; get capitalised drive letter
      IF garry
	cp      'T'             ; check for valid drives
	jr      z,m090f         ; moving on if found
	cp      'A'             ; check for valid drives
	jr      c,m08fe         ; moving on if found
	cp      'Q'             ; check for valid drives
	jr      c,m090f         ; moving on if found
      ELSE
	cp      'A'             ; check for valid drives
	jr      z,m090f         ; moving on if found
	cp      'B'
	jr      z,m090f
	cp      'M'
	jr      z,m090f
	cp      'T'
	jr      z,m090f
      ENDIF
m08fe   ld      a,(T_ADDR)
	or      a
	ld      a,(SAVDRV)      ; use SAVDRV as drive for SAVE
	jr      z,m090a
	ld      a,(LODDRV)      ; or LODDRV otherwise
m090a   ld      (RAMERR),a      ; store drive in RAMERR
	jr      m096c           ; move on

; section 4 - changing default drives for LOAD "A:" etc

m090f
      IF garry
	ld      (RAMERR), a
      ELSE
	ld      l,a             ; save drive in L
      ENDIF
	ld      a,c
	dec     a
	dec     a
	or      b               ; check string length
      IF garry
	jr      nz,m0969        ; move on if not 2
	ld      a, (RAMERR)
      ELSE
	jr      nz,m0966        ; move on if not 2
	ld      a,(T_ADDR)
	or      a
	jr      z,m0923         ; if SAVE, go to set default SAVE drive
	cp      $01
	jr      z,m0960         ; if LOAD, go to set default LOAD drive
	ld      a,l
	jr      m096c           ; else move on
m0923   ld      a,l
	cp      'M'
	jr      z,m093f         ; move on if setting drive M:
	cp      'T'
	jr      z,m093f         ; or T: as default
	ld      hl,FLAGS3
	bit     4,(hl)
	jr      z,m093b         ; go to error if no disk interface
	cp      'A'
	jr      z,m093f         ; move on if setting A:
	bit     5,(hl)
	jr      nz,m093f        ; move on if setting B: and drive B: present
m093b   call    m2ada
	defb    $4e             ; cause "Invalid drive" error
m093f   ld      (SAVDRV),a      ; store in SAVDRV
      ENDIF
m0942   cp      'T'
	jr      z,m095d         ; move on for T:
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_DRIVE   ; set default drive
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	jr      c,m095d         ; move on if no error
	call    m0e9a
	defb    $ff             ; cause DOS error
      IF garry
m095d   ld      a, (T_ADDR)
	or      a
	ld      a, (RAMERR)
	jr      z, m0945
	ld      (LODDRV), a
	jr      m0948
m0945   ld      (SAVDRV), a
m0948   ld      d, a
	ld      e, $10
	rst     $18
	cp      $b5
	jr      nz, m0966
	rst     $20
	ex      de, hl
	xor     a
	ld      b, a
	ld      c, a
	call    m2b89           ; page in DOS workspace
	call    m32b6
	call    m3f00
	DW      $00d6
	call    m32ee
	call    m2b64
m0966   pop     bc
	pop     de
	ret
m0969   ld      a, (RAMERR)
	cp      $54
	jr      nz, m096c
m0970   pop     bc
	pop     de
	inc     de
	inc     de
	dec     bc
	dec     bc
	jr      m099a
      ELSE
m095d   pop     bc              ; exit
	pop     de
	ret
m0960   ld      a,l
	ld      (LODDRV),a      ; store in LODDRV
	jr      m0942           ; go to set default DOS drive
m0966   ld      a,(de)
	and     $df
	ld      (RAMERR),a      ; save capitalised drive in RAMERR
      ENDIF

; section 5 - copying filename to page 7 (disk operations only)

m096c   cp      'T'
	jr      z,m0998         ; move on for tape operations
	ld      a,(T_ADDR)
	cp      $02
	jr      nz,m097a        ; move on if not VERIFY
	pop     hl              ; for VERIFY on disk, just exit
	pop     hl
	ret
m097a   ld      a,b
	or      c               ; test length of string
	jr      nz,m0982
	call    m0e9a
	defb    $0e             ; invalid filename error if zero
m0982   ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy filename to page 7
	pop     bc
	ld      bc,$000a
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
	call    m2b64           ; page in normal memory
	jr      m0999           ; move on with disk operations

; section 6 - copying filename into 1st header
; *BUG* If filename was specified as "T:name", the "T:" is never stripped

m0998   pop     bc              ; restore length & add of filename
m0999   pop     de
m099a   ld      hl,$fff6
	dec     bc
	add     hl,bc
	inc     bc
	jr      nc,m09b3        ; move on if filename 10 letters or less
	ld      a,(T_ADDR)
	and     a
	jr      nz,m09ac
	call    m0e9a
	defb    $0e             ; bad filename error if SAVEing
m09ac   ld      a,b
	or      c
	jr      z,m09ba         ; move on if no filename
	ld      bc,$000a        ; copy 10 chars
m09b3   push    ix
	pop     hl
	inc     hl
	ex      de,hl
	ldir                    ; copy filename to header+1 in workspace

; At this point, syntax-checking rejoins the routines
; Each of the following sections fills in the remaining header information
; for their filetype & ensures syntax is correct to end-of-statement.
; At run-time the sections recombine at section 11, with HL=address
; to load/verify/merge/save at

; section 7 - DATA operations
m09ba
      IF garry
	rst     $18
      ELSE
	rst     $28
	DW      $0018           ; get next char
      ENDIF
	cp      $e4
	jr      nz,m0a11        ; move on if not DATA
	ld      a,(T_ADDR)
	cp      $03
	jp      z,m1125         ; error if used with MERGE
      IF garry
	rst     $20
      ELSE
	rst     $28
	DW      $0020           ; get current char
      ENDIF
m09cc   rst     $28
	DW      $28b2           ; search for variable
	set     7,c             ; set bit 7 of array's name
	jr      nc,m09e0        ; jump if handling existing array
	ld      hl,$0000
	ld      a,(T_ADDR)
	dec     a
	jr      z,m09f4
	call    m0e9a
	defb    $01             ; error 2 if trying to SAVE/VERIFY new array
m09e0   jp      nz,m1125        ; error if just a numeric variable
	bit     7,(iy+$01)
	jr      z,m0a01         ; move on if checking syntax
	inc     hl
	ld      a,(hl)
	ld      (ix+$0b),a      ; copy array length into workspace header
	inc     hl
	ld      a,(hl)
	ld      (ix+$0c),a
	inc     hl
m09f4   ld      (ix+$0e),c      ; copy array name into workspace header
	ld      a,$01           ; type 1
	bit     6,c
	jr      z,m09fe         ; move on if numeric array
	inc     a               ; else type 2
m09fe   ld      (ix+$00),a      ; copy type into workspace header
m0a01   ex      de,hl
      IF garry
	rst     $20
      ELSE
	rst     $28
	DW      $0020           ; get current char
      ENDIF
	cp      ')'
	jr      nz,m09e0        ; error if not ")"
	rst     $20
	call    m10b1           ; check for end-of-statement
m0a0d   ex      de,hl
	jp      m0ad5           ; jump on

; section 8 - SCREEN$ operations

m0a11   cp      $aa             ; check for SCREEN$
	jr      nz,m0a36        ; move on if not
	ld      a,(T_ADDR)
	cp      $03
	jp      z,m1125         ; error if trying to MERGE
      IF garry
	rst     $20
      ELSE
	rst     $28
	DW      $0020           ; get current char
      ENDIF
	call    m10b1           ; check for end-of-statement
	ld      (ix+$0b),$00    ; store screen length
	ld      (ix+$0c),$1b
	ld      hl,$4000
	ld      (ix+$0d),l      ; and start
	ld      (ix+$0e),h
	jr      m0a89           ; jump on

; section 9 - CODE operations

m0a36   cp      $af             ; check for CODE
	jr      nz,m0a8f        ; move on if not
	ld      a,(T_ADDR)
	cp      $03
	jp      z,m1125         ; error if trying to MERGE
      IF garry
	rst     $20
      ELSE
	rst     $28
	DW      $0020           ; get current char
      ENDIF
m0a45   call    m0e94
	jr      nz,m0a56        ; move on if not end-of-statement
	ld      a,(T_ADDR)
	and     a
	jp      z,m1125         ; error if trying to SAVE with no parameters
	rst     $28
	DW      $1ce6           ; get zero to calculator stack
	jr      m0a67           ; move on
m0a56   call    m1121           ; get numeric expression
      IF garry
	rst     $18
      ELSE
	rst     $28
	DW      $0018           ; get next char
      ENDIF
	cp      ','
	jr      z,m0a6c         ; move on if comma
	ld      a,(T_ADDR)
	and     a
	jp      z,m1125         ; error if trying to SAVE with 1 parameter
m0a67   rst     $28
	DW      $1ce6           ; get zero to calculator stack
	jr      m0a72           ; move on
m0a6c   rst     $28
	DW      $0020           ; get next char
m0a6f   call    m1121           ; get numeric expression
m0a72   call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $1e99           ; get length to BC
	ld      (ix+$0b),c      ; store in workspace header
	ld      (ix+$0c),b
	rst     $28
	DW      $1e99           ; get address to BC
	ld      (ix+$0d),c      ; store in workspace header
	ld      (ix+$0e),b
	ld      h,b             ; HL=address
	ld      l,c
m0a89   ld      (ix+$00),$03    ; type 3 to workspace header
	jr      m0ad5           ; move on

; section 10 - BASIC operations

m0a8f   cp      $ca             ; check for LINE
	jr      z,m0a9c         ; move on if present
      IF garry
	jp      n24c7           ; check for end-of-statement
      ELSE
	call    m10b1           ; check for end-of-statement
      ENDIF
m0a96   ld      (ix+$0e),$80    ; no auto-run
	jr      m0ab5           ; move on
m0a9c   ld      a,(T_ADDR)
	and     a
	jp      nz,m1125        ; error unless SAVE with LINE
	rst     $28
	DW      $0020           ; get next char
m0aa6   call    m1121           ; get numeric expression
	call    m10b1           ; check for end-of-line
	rst     $28
	DW      $1e99           ; get line to BC
	ld      (ix+$0d),c      ; store in workspace header
	ld      (ix+$0e),b
m0ab5   ld      (ix+$00),$00    ; type 0
	ld      hl,(E_LINE)
	ld      de,(PROG)
	scf
	sbc     hl,de           ; HL=program+vars length
	ld      (ix+$0b),l      ; store in workspace header
	ld      (ix+$0c),h
	ld      hl,(VARS)
	sbc     hl,de           ; HL=program only length
	ld      (ix+$0f),l      ; store in workspace header
	ld      (ix+$10),h
	ex      de,hl

; section 11 - LOAD/VERIFY/MERGE tape operations

m0ad5   ld      a,(T_ADDR)
	and     a
	jp      z,m0d6e         ; move on if saving
	push    hl
	ld      bc,$0011
	add     ix,bc           ; IX points to 2nd header
	ld      a,(RAMERR)
	cp      'T'
	jr      nz,m0b41        ; move on if disk operation
m0ae9   push    ix
	ld      de,$0011
	xor     a
	scf
	call    m0884           ; load header from tape to 2nd header area
	pop     ix
	jr      nc,m0ae9        ; loop back if error
	ld      a,$fe
	rst     $28
	DW      $1601           ; open channel to stream -2
	ld      (iy+$52),$03    ; set scroll count
	ld      c,$80           ; signal "names don't match"
	ld      a,(ix+$00)
	cp      (ix-$11)        ; compare types
	jr      nz,m0b0c        ; jump if no match
	ld      c,$f6           ; C must be incremented 10 times to match
m0b0c   cp      $04
	jr      nc,m0ae9        ; error for types 4+
	ld      de,$09c0        ; address of message block in ROM 3
	push    bc
	rst     $28
	DW      $0c0a           ; print filetype message
	pop     bc
	push    ix
	pop     de              ; DE points to filename to check for
	ld      hl,$fff0
	add     hl,de           ; HL points to loaded filename
	ld      b,$0a           ; check 10 chars
	ld      a,(hl)          ; get next char
	inc     a
	jr      nz,m0b28        ; move on if name to check not null
	ld      a,c             ; if null, signal "10 chars match"
	add     a,b
	ld      c,a
m0b28   inc     de
	ld      a,(de)
	cp      (hl)            ; compare names
	inc     hl
	jr      nz,m0b2f
	inc     c               ; increment C if chars match
m0b2f   rst     $28
	DW      $0010           ; output char
	djnz    m0b28           ; loop back
	bit     7,c
	jr      nz,m0ae9        ; loop back if no match
	ld      a,$0d
	rst     $28
	DW      $0010           ; output CR
	pop     hl
	jp      m0ba6           ; move on

; section 12 - LOAD/MERGE disk operations

m0b41   ld      a,(T_ADDR)
	cp      $02
	jr      z,m0ba6         ; move on if VERIFY (can't be here if so!)
	push    ix
	ld      b,$00           ; file 0
	ld      c,$01           ; exclusive-read
	ld      d,$00
	ld      e,$01
	ld      hl,tmp_fspec
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; open file
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	jr      c,m0b6c
	call    m0e9a           ; cause any DOS error
	defb    $ff
m0b6c   ld      b,$00
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_REF_HEAD    ; IX to header
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	ex      (sp),ix
	pop     hl
	call    m2b89           ; page in DOS workspace
	ld      a,(hl)
	call    m2b64           ; page in normal memory
	cp      (ix-$11)        ; compare types
	jr      z,m0b92
	call    m0e9a
	defb    $1d             ; error if different
m0b92   ld      (ix+$00),a      ; store in 2nd header area
	push    ix
	pop     de
	ex      de,hl
	ld      bc,$000b
	add     hl,bc
	ex      de,hl
	inc     hl
	ld      bc,$0006
	call    m3f8a           ; copy parameters from page 7 into 2nd header
	pop     hl

; section 13 - Perform tape/disk VERIFY (any type) or LOAD (CODE only)

m0ba6   ld      a,(ix+$00)
	cp      $03
	jr      z,m0bb9         ; move on for type 3
	ld      a,(T_ADDR)
	dec     a
	jp      z,m0c04         ; move on if LOADing
	cp      $02
	jp      z,m0cb2         ; move on if MERGEing
m0bb9   push    hl              ; save address to LOAD/VERIFY at
	ld      l,(ix-$06)
	ld      h,(ix-$05)      ; HL=length from 1st header
	ld      e,(ix+$0b)
	ld      d,(ix+$0c)      ; DE=length from 2nd header
	ld      a,h
	or      l
	jr      z,m0bd7         ; move on if length not specified (CODE only)
	sbc     hl,de
	jr      c,m0c00         ; error if block larger than requested
	jr      z,m0bd7
	ld      a,(ix+$00)
	cp      $03
	jr      nz,m0bfc        ; error for uneven lengths except in CODE
m0bd7   pop     hl              ; restore address
	ld      a,h
	or      l
	jr      nz,m0be2
	ld      l,(ix+$0d)
	ld      h,(ix+$0e)      ; if zero, use start address of 2nd header
m0be2   push    hl
	pop     ix              ; IX=address to load
	ld      a,(T_ADDR)
	cp      $02
	scf                     ; set carry for LOAD
	jr      nz,m0bf6
	and     a               ; reset carry for VERIFY
	ld      a,(RAMERR)
	cp      'T'
	jr      z,m0bf6
	ret                     ; exit if VERIFY with disk (won't get here!)
m0bf6   ld      a,$ff           ; data block
m0bf8   call    m0834           ; load/verify block from tape/disk
	ret     c               ; exit if okay
m0bfc   call    m0e9a           ; error R - tape loading error
	defb    $1a
m0c00   call    m0e9a           ; error ???
	defb    $4f

; section 14 - Perform tape/disk LOAD (types 0-2)

m0c04   ld      e,(ix+$0b)
	ld      d,(ix+$0c)      ; DE=length from 2nd header
m0c0a   push    hl
	ld      a,h
	or      l               ; test start=0 (previously undeclared array)
	jr      nz,m0c15        ; move on if not
	inc     de
	inc     de
	inc     de              ; add 3 bytes for name (1) & length (2)
	ex      de,hl
	jr      m0c21           ; move on
m0c15   ld      l,(ix-$06)
	ld      h,(ix-$05)      ; HL=size of existing prog+vars or array
	ex      de,hl
	scf
	sbc     hl,de
	jr      c,m0c2a         ; move on if no extra space required
m0c21   ld      de,$0005        ; allow for 5-byte overhead
	add     hl,de
	ld      b,h
	ld      c,l
	rst     $28
	DW      $1f05           ; test if space available
m0c2a   pop     hl              ; restore destination address
	ld      a,(ix+$00)
	and     a
	jr      z,m0c6f         ; move on for BASIC programs
	ld      a,h
	or      l
	jr      z,m0c48         ; move on if loading new array
	dec     hl
	ld      b,(hl)
	dec     hl
	ld      c,(hl)          ; get existing array length from vars area
	dec     hl
	inc     bc
	inc     bc
	inc     bc              ; add 3 for name & length
	ld      (X_PTR),ix      ; save IX
	rst     $28
	DW      $19e8           ; reclaim space
	ld      ix,(X_PTR)      ; restore IX
m0c48   ld      hl,(E_LINE)
	dec     hl              ; HL points to $80 at end of vars
	ld      c,(ix+$0b)
	ld      b,(ix+$0c)      ; get length of new array
	push    bc              ; save
	inc     bc
	inc     bc
	inc     bc              ; add 3 for name & length
	ld      a,(ix-$03)
	push    af              ; save array name (from old header)
	rst     $28
	DW      $1655           ; make the room
	defb    $23
	pop     af
	ld      (hl),a          ; store name
	pop     de
	inc     hl
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; and length
	inc     hl
	push    hl
	pop     ix              ; IX points to array start
	scf                     ; set carry for LOAD
	ld      a,$ff           ; data block
	jp      m0bf8           ; go to load it
m0c6f   ex      de,hl           ; save DE=destination
	ld      hl,(E_LINE)
	dec     hl              ; end of vars
	ld      (X_PTR),ix      ; save IX
	ld      c,(ix+$0b)
	ld      b,(ix+$0c)      ; get length of new data block
	push    bc
	rst     $28
	DW      $19e5           ; reclaim entire prog+vars
	pop     bc
	push    hl
	push    bc
	rst     $28
	DW      $1655           ; make room for new block
	ld      ix,(X_PTR)      ; restore IX
	inc     hl
	ld      c,(ix+$0f)
	ld      b,(ix+$10)      ; BC=length of program only
	add     hl,bc
	ld      (VARS),hl       ; set new start of vars
	ld      h,(ix+$0e)
	ld      a,h
	and     $c0
	jr      nz,m0ca9        ; move on if no auto-run number
	ld      l,(ix+$0d)
	ld      (NEWPPC),hl     ; set new line & statement
	ld      (iy+$0a),$00
m0ca9   pop     de              ; restore length & start
	pop     ix
	scf                     ; set carry for LOAD
	ld      a,$ff           ; data block
	jp      m0bf8           ; go to load it

; section 15 - Perform tape/disk MERGE

m0cb2   ld      c,(ix+$0b)
	ld      b,(ix+$0c)      ; fetch length of new block
	push    bc
	inc     bc
	rst     $28
	DW      $0030           ; make length+1 bytes in workspace
m0cbd   ld      (hl),$80        ; terminate with an end-marker
	ex      de,hl           ; HL=start
	pop     de              ; DE=length
	push    hl              ; save start
	push    hl
	pop     ix              ; IX=start
	scf                     ; set carry for LOAD
	ld      a,$ff           ; data block
	call    m0bf8           ; load the block
	pop     hl              ; HL=start of new prog
	ld      de,(PROG)       ; DE=start of old prog
m0cd0   ld      a,(hl)
	and     $c0
	jr      nz,m0cee        ; move on if all lines done
m0cd5   ld      a,(de)
	inc     de
	cp      (hl)            ; compare high bytes of line number
	inc     hl
	jr      nz,m0cdd        ; skip next test if no match
	ld      a,(de)
	cp      (hl)            ; compare low bytes of line number
m0cdd   dec     de
	dec     hl
	jr      nc,m0ce9        ; move on if can place line here
	push    hl
	ex      de,hl
	rst     $28
	DW      $19b8           ; get address of next line in old prog
	pop     hl
	jr      m0cd5           ; loop back
m0ce9   call    m0d2a           ; enter the new line
	jr      m0cd0           ; loop back
m0cee   ld      a,(hl)          ; get var name from workspace
	ld      c,a
	cp      $80
	ret     z               ; exit if all done
	push    hl
	ld      hl,(VARS)       ; fetch start of vars
m0cf7   ld      a,(hl)
	cp      $80
	jr      z,m0d21         ; move on if reached end
	cp      c
	jr      z,m0d07         ; move on if found match
m0cff   push    bc
	rst     $28
	DW      $19b8           ; get to next var
	pop     bc
	ex      de,hl
	jr      m0cf7           ; loop back
m0d07   and     $e0
	cp      $a0
	jr      nz,m0d1f        ; move on if not long-named var
	pop     de
	push    de
	push    hl
m0d10   inc     hl
	inc     de
	ld      a,(de)
	cp      (hl)            ; compare long names
	jr      nz,m0d1c        ; move on if mismatch
	rla
	jr      nc,m0d10        ; loop back
	pop     hl
	jr      m0d1f
m0d1c   pop     hl
	jr      m0cff           ; go back if unsuccessful
m0d1f   ld      a,$ff           ; signal "replace variable"
m0d21   pop     de
	ex      de,hl
	inc     a
	scf                     ; signal "variables"
	call    m0d2a           ; merge in the variable
	jr      m0cee           ; loop back

; Subroutine to merge a line or variable (part of section 15)

m0d2a   jr      nz,m0d3c        ; move on if not replacing a line/variable
	ex      af,af'          ; save flags
	ld      (X_PTR),hl      ; save pointer in new program/vars
	ex      de,hl
	rst     $28
	DW      $19b8
	rst     $28
	DW      $19e8           ; reclaim old line/var
	ex      de,hl
	ld      hl,(X_PTR)      ; restore
	ex      af,af'
m0d3c   ex      af,af'          ; save flags
	push    de
	rst     $28
	DW      $19b8           ; find length of new line/var
	ld      (X_PTR),hl      ; save pointer in new program/vars
	ld      hl,(PROG)
	ex      (sp),hl         ; save PROG to avoid corruption
	push    bc
	ex      af,af'
	jr      c,m0d53         ; move on if adding a variable
	dec     hl
	rst     $28
	DW      $1655           ; make room for new line
	inc     hl
	jr      m0d56
m0d53   rst     $28
	DW      $1655           ; make room for new var
m0d56   inc     hl              ; point to first new location
	pop     bc
	pop     de
	ld      (PROG),de       ; restore PROG
	ld      de,(X_PTR)      ; retrieve new pointer
	push    bc
	push    de
	ex      de,hl
	ldir                    ; copy new var/line into space made
	pop     hl
	pop     bc
	push    de
	rst     $28
	DW      $19e8           ; reclaim workspace holding new var/line
	pop     de
	ret

; section 16 - Perform disk SAVE

m0d6e   ld      a,(RAMERR)
	cp      'T'
	jp      z,m0e10         ; move on for tape operations
	call    m2b89           ; page in DOS workspace
	push    hl
	ld      b,$00           ; file 0
	ld      c,$03           ; exclusive read-write
	ld      d,$01
	ld      e,$03
	ld      hl,tmp_fspec
	push    ix
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; create the file
	call    m32ee           ; restore TSTACK
	jr      c,m0d9b         ; move on unless error
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause DOS error
	defb    $ff
m0d9b   ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_REF_HEAD    ; get IX=header data
	call    m32ee           ; restore TSTACK
	jr      c,m0db1
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause DOS error
	defb    $ff
m0db1   ex      (sp),ix         ; IX=pointer to header in normal memory
	pop     hl              ; HL=pointer to header in page 7
	call    m2b64           ; page in normal memory
	ld      a,(ix+$00)
	call    m2b89           ; page in DOS workspace
	ld      (hl),a          ; transfer type
	inc     hl
	push    ix
	pop     de
	ex      de,hl           ; DE=DOS header address+1
	ld      bc,$000b
	add     hl,bc           ; HL=page 0 header parameters
	ld      bc,$0006
	call    m2b64           ; page in normal memory
	call    m3f63           ; copy parameters to DOS header
	ld      b,$00           ; file 0
	ld      c,$00
	ld      e,(ix+$0b)
	ld      d,(ix+$0c)      ; DE=length
	ld      a,d
	or      e
	call    m2b89           ; page in DOS workspace
	jr      z,m0df6         ; move on if zero length
	pop     hl              ; restore start address
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_WRITE       ; write block to file
	call    m32ee           ; restore TSTACK
	jr      c,m0df6
	call    m2b64           ; page in normal memory
	call    m0e9a           ; cause any DOS error
	defb    $ff
m0df6   ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file
	call    m32ee           ; restore TSTACK
	jr      c,m0e0c
	call    m2b64
	call    m0e9a           ; cause any DOS error
	defb    $ff
m0e0c   call    m2b64           ; page in normal memory
	ret                     ; done

; section 17 - Perform tape SAVE

m0e10   push    hl
	ld      a,$fd
	rst     $28
	DW      $1601           ; open channel to stream -3
	xor     a
	ld      de,$09a1
	rst     $28
	DW      $0c0a           ; output ROM 3's start tape message
	set     5,(iy+$02)      ; signal "screen needs clearing"
	rst     $28
	DW      $15d4           ; wait for a key
	push    ix              ; save header address
	ld      de,$0011
	xor     a               ; header block
	call    m0828           ; save header
	pop     ix
	ld      b,$32
m0e31   halt                    ; delay for 1 sec
	djnz    m0e31
	ld      e,(ix+$0b)
	ld      d,(ix+$0c)      ; DE=length
	ld      a,$ff           ; data block
	pop     ix              ; IX=start
	jp      m0828           ; save & exit

; Looks like these bits aren't used
      IF garry
msg18   defb    $16, 0, 0, $10, 0, $11, $07, $13, 0
	defm    "Insert tape and press PLAY", 13
	defm    "To cancel - press BREAK twice", $ff
	defb    0, 0, 0, 0, 0, 0, 0, 0
	defb    0, 0, 0, 0, 0, 0, 0, 0, 0
      ELSE
	defb    $80
m0e42   defm    "Press REC & PLAY, then any key", $ae
	defm    $0d, "Program:", $a0
	defm    $0d, "Number array:", $a0
	defm    $0d, "Character array:", $a0
	defm    $0d, "Bytes:", $a0
      ENDIF

; Subroutine to check if char in A is a statement terminator

m0e94   cp      $0d
	ret     z
	cp      ':'
	ret

; Subroutine to cause a +3DOS error
; Routine will attempt to close file 0 before causing error
; On entry, A=+3DOS error code and byte following call is $ff
; or, byte following call is +3 BASIC error

m0e9a   push    af              ; save error code
m0e9b   ld      a,(RAMERR)
	cp      $54
	jr      z,m0eca         ; move on if SAVE/LOAD was using drive T:
	ld      b,$00
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file 0
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	jr      c,m0eca         ; move on if no error
	ld      b,$00
	call    m2b89           ; page in DOS workspace
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_ABANDON     ; else abandon file 0
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
m0eca   pop     af              ; restore error code
m0ecb   pop     hl              ; get return address
	ld      e,(hl)          ; get inline code
	bit     7,e
	jr      z,m0edc         ; use it as error code if not $ff
	cp      $0a             ; convert DOS error code to +3 BASIC error
	jr      nc,m0ed9
m0ed5   add     a,$3d
	jr      m0edb
m0ed9   add     a,$18
m0edb   ld      e,a
m0edc   ld      h,e             ; place code CALL m2ada, DEFB error
	ld      l,m2ada/$100    ; on stack
	push    hl
	ld      l,$cd
	ld      h,m2ada and $ff
	push    hl
	xor     a
	ld      hl,$0000
	add     hl,sp
	jp      (hl)            ; jump to execute code on stack & cause error

; The parameter offset table
; This contains offsets from each entry's position into the following
; parameter table

m0eeb   defb    m0f9c-$         ; DEF FN
	defb    m0fb6-$         ; CAT
	defb    m0fa9-$         ; FORMAT
	defb    m0fac-$         ; MOVE
	defb    m0fb2-$         ; ERASE
	defb    m0f9f-$         ; OPEN#
	defb    m0fa5-$         ; CLOSE#
	defb    m0f85-$         ; MERGE
	defb    m0f84-$         ; VERIFY
	defb    m0f86-$         ; BEEP
	defb    m0f8a-$         ; CIRCLE
	defb    m0f8e-$         ; INK
	defb    m0f8f-$         ; PAPER
	defb    m0f90-$         ; FLASH
	defb    m0f91-$         ; BRIGHT
	defb    m0f92-$         ; INVERSE
	defb    m0f93-$         ; OVER
	defb    m0f94-$         ; OUT
	defb    m0f7c-$         ; LPRINT
	defb    m0f7f-$         ; LLIST
	defb    m0f2d-$         ; STOP
	defb    m0f6c-$         ; READ
	defb    m0f6f-$         ; DATA
	defb    m0f72-$         ; RESTORE
	defb    m0f4b-$         ; NEW
	defb    m0f98-$         ; BORDER
	defb    m0f5b-$         ; CONTINUE
	defb    m0f45-$         ; DIM
	defb    m0f48-$         ; REM
	defb    m0f33-$         ; FOR
	defb    m0f20-$         ; GOTO
	defb    m0f29-$         ; GOSUB
	defb    m0f42-$         ; INPUT
	defb    m0f83-$         ; LOAD
	defb    m0f51-$         ; LIST
	defb    m0f1d-$         ; LET
	defb    m0f68-$         ; PAUSE
	defb    m0f3b-$         ; NEXT
	defb    m0f54-$         ; POKE
	defb    m0f3f-$         ; PRINT
	defb    m0f64-$         ; PLOT
	defb    m0f4e-$         ; RUN
	defb    m0f82-$         ; SAVE
	defb    m0f58-$         ; RANDOMIZE
	defb    m0f24-$         ; IF
	defb    m0f61-$         ; CLS
	defb    m0f75-$         ; DRAW
	defb    m0f5e-$         ; CLEAR
	defb    m0f30-$         ; RETURN
	defb    m0f79-$         ; COPY

; The parameter table for the BASIC commands
; These comprise command classes ($00-$0e), separators and
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

m0f1d   defb    $01,'=',$02     ; LET
      IF garry
m0f20   defb    $0e
	DW      $35e1           ; GOTO
	defb    $00
      ELSE
m0f20   defb    $06,$00
	DW      o1E67           ; GOTO
      ENDIF
m0f24   defb    $06,$cb,$0e
	DW      m115e           ; IF
m0f29   defb    $06,$0c
	DW      m124a           ; GOSUB
m0f2d   defb    $00
	DW      o1CEE           ; STOP
      IF garry
m0f30   defb    $0e
	DW      m2b09
      ELSE
m0f30   defb    $0c
	DW      m1266           ; RETURN
      ENDIF
m0f33   defb    $04,'=',$06
	defb    $cc,$06,$0e
	DW      m1178           ; FOR
      IF garry
m0f3b   defb    $0e
	DW      $1506           ; NEXT
	defb    $00
      ELSE
m0f3b   defb    $04,$00
	DW      o1DAB           ; NEXT
      ENDIF
m0f3f   defb    $0e
	DW      m217b           ; PRINT
m0f42   defb    $0e
	DW      m218f           ; INPUT
m0f45   defb    $0e
      IF garry
	DW      m14e2           ; DIM
      ELSE
	DW      m22ad           ; DIM
      ENDIF
m0f48   defb    $0e
	DW      m1072           ; REM
      IF garry
m0f4b   defb    $0e
	DW      $3877           ; NEW
      ELSE
m0f4b   defb    $0c
	DW      m2280           ; NEW
      ENDIF
m0f4e   defb    $0d
	DW      m11f9           ; RUN
m0f51   defb    $0e
	DW      m1539           ; LIST
m0f54   defb    $08,$00
	DW      $1e80           ; POKE
m0f58   defb    $03
	DW      $1e4f           ; RANDOMIZE
m0f5b   defb    $00
	DW      $1e5f           ; CONTINUE
m0f5e   defb    $0d
	DW      m1204           ; CLEAR
m0f61   defb    $00
	DW      $0d6b           ; CLS
m0f64   defb    $09,$00
	DW      $22dc           ; PLOT
m0f68   defb    $06,$00
	DW      $1f3a           ; PAUSE
m0f6c   defb    $0e
	DW      m11a2           ; READ
m0f6f   defb    $0e
	DW      m11e2           ; DATA
m0f72   defb    $03
	DW      $1e42           ; RESTORE
m0f75   defb    $09,$0e
	DW      m2296           ; DRAW
m0f79   defb    $0e
	DW      m21aa           ; COPY
m0f7c   defb    $0e
	DW      m2177           ; LPRINT
m0f7f   defb    $0e
	DW      m1535           ; LLIST
m0f82   defb    $0b             ; SAVE
m0f83   defb    $0b             ; LOAD
m0f84   defb    $0b             ; VERIFY
m0f85   defb    $0b             ; MERGE
m0f86   defb    $08,$00
	DW      $03f8           ; BEEP
m0f8a   defb    $09,$0e
	DW      m2286           ; CIRCLE
m0f8e   defb    $07             ; INK
m0f8f   defb    $07             ; PAPER
m0f90   defb    $07             ; FLASH
m0f91   defb    $07             ; BRIGHT
m0f92   defb    $07             ; INVERSE
m0f93   defb    $07             ; OVER
m0f94   defb    $08,$00
	DW      $1e7a           ; OUT
m0f98   defb    $06,$00
	DW      $2294           ; BORDER
m0f9c   defb    $0e
	DW      m1283           ; DEF FN
      IF garry
m0f9f   defb    $06,',',$0a,$0c
	DW      $35c4           ; OPEN#
m0fa5   defb    $06,$0c
	DW      $35d7           ; CLOSE#
      ELSE
m0f9f   defb    $06,',',$0a,$00
	DW      o1736           ; OPEN#
m0fa5   defb    $06,$00
	DW      $16e5           ; CLOSE#
      ENDIF
m0fa9   defb    $0e
	DW      m026c           ; FORMAT
      IF garry
m0fac   defb    $0a,$0e
	DW      $39b3           ; MOVE
	defb    0, 0
      ELSE
m0fac   defb    $0a,$cc,$0a,$0c
	DW      m04e5           ; MOVE
      ENDIF
m0fb2   defb    $0a,$0c
	DW      m044a           ; ERASE
m0fb6   defb    $0e
	DW      m05b8           ; CAT
      IF garry
m0fb9   defb    $0e
	DW      cmdspec		; SPECTRUM
      ELSE
m0fb9   defb    $0c
	DW      m1465           ; SPECTRUM
      ENDIF
m0fbc   defb    $0e
	DW      m23f1           ; PLAY

; The main parser entry point
; Enter here for syntax checking

m0fbf   res     7,(iy+$01)      ; signal "syntax checking"
	rst     $28
	DW      $19fb           ; point to the first code after any line no
	xor     a
	ld      (SUBPPC),a      ; initialise SUBPPC to zero statements
	dec     a
	ld      (ERR_NR),a      ; signal "OK" error code
	jr      m0fd1           ; jump to start checking

; The statement loop

m0fd0   rst     $20             ; advance CH_ADD
m0fd1   rst     $28
	DW      $16bf           ; clear workspace
	inc     (iy+$0d)        ; increment SUBPPC on each statement
	jp      m,m1125         ; error if more than 127 statements on line
	rst     $18             ; fetch character
	ld      b,$00
	cp      $0d
	jp      z,m1073         ; move on if end-of-line
	cp      ':'
	jr      z,m0fd0         ; loop back if end-of-statement
	ld      hl,m1031
	push    hl              ; load stack with return address to STMT-RET
	ld      c,a             ; save command code
	rst     $20             ; advance CH_ADD
	ld      a,c
	sub     $ce             ; put command code in range $00..$31
	jr      nc,m1004        ; move on if valid keyword
	add     a,$ce           ; else reform character
	ld      hl,m0fb9        ; address of SPECTRUM parameter entries
	cp      $a3
	jr      z,m1010         ; move on if SPECTRUM command
	ld      hl,m0fbc        ; address of PLAY parameter entries
	cp      $a4
	jr      z,m1010         ; move on if PLAY command
	jp      m1125           ; else give Nonsense in BASIC
m1004   ld      c,a
	ld      hl,m0eeb        ; syntax offset table start
	add     hl,bc
	ld      c,(hl)
	add     hl,bc           ; get start of entries in parameter table
	jr      m1010           ; move on
m100d   ld      hl,(T_ADDR)     ; get pointer into parameter table
m1010   ld      a,(hl)          ; get next parameter type
	inc     hl
	ld      (T_ADDR),hl     ; save pointer
	ld      bc,m100d
	push    bc              ; stack return address back into this loop
	ld      c,a
	cp      $20
	jr      nc,m102a        ; move on if entry is a separator
	ld      hl,m10c5        ; base of command class table
	ld      b,$00
	add     hl,bc
	ld      c,(hl)          ; get offset
	add     hl,bc
	push    hl              ; stack computed command class routine address
	rst     $18             ; get next char to A
	dec     b               ; B=$ff
	ret                     ; call command class routine
m102a   rst     $18             ; get next char
	cp      c
	jp      nz,m1125        ; nonsense in BASIC if not required separator
	rst     $20             ; get next character
	ret                     ; back into loop at m100d

; The "STMT-RET" routine. A return is made here after correct interpretation
; of a statement

m1031   call    m2af9           ; test BREAK key
	jr      c,m103a         ; move on if not pressed
	call    m2ada
	defb    $14             ; error L - BREAK into program
m103a   bit     7,(iy+$0a)
	jp      nz,m10b8        ; move on if a jump is not being made
	ld      hl,(NEWPPC)     ; get new line number
	bit     7,h             ; check if running line in edit area
	jr      z,m105c         ; move on if not

; Enter here if running a line in the edit area

m1048   ld      hl,$fffe
	ld      (PPC),hl        ; this is line "-2"
	ld      hl,(WORKSP)
	dec     hl              ; HL points to end of edit area
	ld      de,(E_LINE)
	dec     de              ; DE points to location before edit area
	ld      a,(NSPPC)       ; fetch number of next statement to handle
	jr      m1092           ; move on

; Perform a jump in the program

m105c   rst     $28
	DW      $196e           ; get start address of line to jump to
	ld      a,(NSPPC)       ; get statement number
	jr      z,m1080         ; move on if correct line was found
	and     a               ; else check statement number
	jr      nz,m10ad        ; if not zero, N - statement lost error
	ld      b,a
	ld      a,(hl)
	and     $c0             ; check for end of program
	ld      a,b
	jr      z,m1080         ; move on if not
	call    m2ada
	defb    $ff             ; else end with 0 - OK error

m1072   pop     bc              ; REM command - drop STMT-RET address to
				; ignore rest of command

; The Line-end routine

m1073   bit     7,(iy+$01)
	ret     z               ; exit if syntax-checking
	ld      hl,(NXTLIN)     ; get address of NXTLIN
	ld      a,$c0
	and     (hl)
	ret     nz              ; exit if end of program
	xor     a               ; use statement zero

; The line-use routine

m1080   cp      $01
	adc     a,$00           ; change statement zero to 1
	ld      d,(hl)
	inc     hl
	ld      e,(hl)
	ld      (PPC),de        ; store line number in PPC
	inc     hl
	ld      e,(hl)
	inc     hl
	ld      d,(hl)
	ex      de,hl           ; DE holds start of line
	add     hl,de
	inc     hl              ; HL holds start of next line
m1092   ld      (NXTLIN),hl     ; store next line address
	ex      de,hl
	ld      (CH_ADD),hl     ; update CH_ADD to current line start
	ld      d,a
	ld      e,$00
	ld      (iy+$0a),$ff    ; signal "no jump"
	dec     d
	ld      (iy+$0d),d      ; statement number-1 to SUBPPC
	jp      z,m0fd0         ; enter loop if want first statement
	inc     d
	rst     $28
	DW      $198b           ; else find required statement
	jr      z,m10b8         ; move on if found
m10ad   call    m2ada
	defb    $16             ; report N - statement lost


; The "Check-end" subroutine. During syntax-checking, it ensures that
; the end of the statement has been reached, generating an error if not.

m10b1   bit     7,(iy+$01)      ; check bit 7 of FLAGS
	ret     nz              ; return if run-time
	pop     bc              ; drop return address of statement routine
	pop     bc              ; drop return address of scan-loop routine
m10b8   rst     $18             ; get next character
	cp      $0d
	jr      z,m1073         ; move back if end-of-line
	cp      $3a
	jp      z,m0fd0         ; move back if end-of-statement
	jp      m1125           ; else Nonsense in BASIC error


; The command class offset table
; This contains offsets from the entry in the table to the
; actual command class routines following

m10c5   defb    m10e9-$
	defb    m110c-$
	defb    m1110-$
	defb    m10e6-$
	defb    m1118-$
	defb    m10ea-$
	defb    m1121-$
	defb    m112d-$
	defb    m111d-$
	defb    m1157-$
	defb    m1129-$
	defb    m115b-$
	defb    m10d7-$
	defb    m10d4-$
	defb    m10d8-$

; Class $0c,$0d,$0e routines
; Enter at m10d4 for $0d, m10d7 for $0c and m10d8 for $0e

m10d4   rst     $28
	DW      $1cde           ; fetch a number (or 0 if none)
m10d7   cp      a               ; set zero flag for classes $0c & $0d
m10d8   pop     bc              ; drop the scan-loop return address
	call    z,m10b1         ; for classes $0c,$0d check for statement end
	ex      de,hl           ; save line pointer in DE
	ld      hl,(T_ADDR)     ; get address in parameter table
	ld      c,(hl)
	inc     hl
	ld      b,(hl)          ; BC=command address
	ex      de,hl           ; restore line pointer in HL
	push    bc              ; stack command address
	ret                     ; and "return" to it

; Class $00,$03,$05 routines
; Enter at m10e6 for $03, m10e9 for $00 and m10ea for $05

m10e6   rst     $28
	DW      $1cde           ; fetch a number (or 0 if none)
m10e9   cp      a               ; set zero flag for classes $00 & $03
m10ea   pop     bc              ; drop the scan-loop return address
	call    z,m10b1         ; for classes $00,$03 check for statement end
	ex      de,hl           ; save line pointer in DE
	ld      hl,(T_ADDR)     ; get address in parameter table
	ld      c,(hl)
	inc     hl
	ld      b,(hl)          ; BC=command address in ROM 3
	ex      de,hl           ; restore line pointer
	push    hl              ; and stack it
	ld      hl,m110b        ; place ROM 1 return address in RETADDR
	ld      (RETADDR),hl
	ld      hl,REGNUOY
	ex      (sp),hl         ; place REGNUOY routine as return address
	push    hl
	ld      h,b
	ld      l,c
	ex      (sp),hl         ; stack ROM 3 address, restore line pointer
	push    af              ; stack registers
	push    bc
	di                      ; disable interrupts
	jp      STOO            ; call ROM 3 routine
m110b   ret                     ; done

; Class $01 routine

m110c   rst     $28
	DW      $1c1f           ; use ROM 3 to deal with class $01
	ret

; Class $02 routine

m1110   pop     bc              ; drop scan-loop return address
	rst     $28
	DW      $1c56           ; fetch a value
	call    m10b1           ; check for end of statement
	ret

; Class $04 routine

m1118   rst     $28
	DW      $1c6c           ; use ROM 3 to deal with class $04
	ret

; Class $08 routine

m111c   rst     $20
m111d   rst     $28             ; use ROM 3 to deal with class $08
	DW      $1c7a
	ret

; Class $06 routine

m1121   rst     $28
	DW      $1c82           ; use ROM 3 to deal with class $06
	ret

; Generate C - Nonsense in BASIC error

m1125   call    m2ada
	defb    $0b             ; error C

; Class $0a routine

m1129   rst     $28
	DW      $1c8c           ; use ROM 3 to deal with class $0a
	ret

; Class $07 routine

m112d   bit     7,(iy+$01)      ; are we running or checking syntax?
	res     0,(iy+$02)      ; signal "main screen"
	jr      z,m113a
	rst     $28
	DW      $0d4d           ; if running, make sure temp colours are used
m113a   pop     af              ; drop scan-loop return address
	ld      a,(T_ADDR)
	sub     (m0f8e and $ff)+$28 ; form token code INK to OVER
	rst     $28
	DW      $21fc           ; change temporary colours as directed
	call    m10b1           ; check for statement end
	ld      hl,(ATTR_T)
	ld      (ATTR_P),hl     ; make temporary colours permanent
	ld      hl,P_FLAG       ; now copy even bits of P_FLAG to odd bits
	ld      a,(hl)
	rlca
	xor     (hl)
	and     $aa
	xor     (hl)
	ld      (hl),a
	ret

; Class $09 routine

m1157   rst     $28
	DW      $1cbe           ; use ROM 3 to handle class $09
	ret

; Class $0b routine

m115b   jp      m0888           ; jump to cassette/disk handling routines


; The IF command

m115e   pop     bc              ; drop return address to STMT-RET
	bit     7,(iy+$01)
	jr      z,m1175         ; move on if syntax-checking
	ld      hl,(STKEND)     ; "delete" item on calculator stack
	ld      de,$fffb
	add     hl,de
	ld      (STKEND),hl
	rst     $28
	DW      $34e9           ; call "test zero" with HL holding add of value
	jp      c,m1073         ; if false, go to next line
m1175   jp      m0fd1           ; if true or syntax-checking, do next statement

; The FOR command

m1178   cp      $cd
	jr      nz,m1185        ; move on if no STEP
	rst     $20             ; advance CH_ADD
	call    m1121           ; fetch step value
	call    m10b1           ; check end of statement if syntax-checking
	jr      m119d           ; else move on
m1185   call    m10b1           ; if no STEP, check end of statement
	ld      hl,(STKEND)     ; and stack value "1"
	ld      (hl),$00
	inc     hl
	ld      (hl),$00
	inc     hl
	ld      (hl),$01
	inc     hl
	ld      (hl),$00
	inc     hl
	ld      (hl),$00
	inc     hl
	ld      (STKEND),hl
m119d   rst     $28
	DW      $1d16           ; use ROM 3 to perform command
	ret


; The READ command (enter at m11a2)

m11a1   rst     $20             ; move along statement
m11a2   call    m110c           ; check for existing variable
	bit     7,(iy+$01)
	jr      z,m11d9         ; move on if syntax-checking
	rst     $18             ; save current CH_ADD in X_PTR
	ld      (X_PTR),hl
	ld      hl,(DATADD)     ; fetch data list pointer
	ld      a,(hl)
	cp      ','
	jr      z,m11c2         ; move on unless new statement must be found
	ld      e,$e4
	rst     $28
	DW      $1d86           ; search for "DATA" statement
	jr      nc,m11c2
	call    m2ada
	defb    $0d             ; error E - out of data if not found
m11c2   inc     hl              ; advance pointer
	ld      (CH_ADD),hl
	ld      a,(hl)
	rst     $28
	DW      $1c56           ; assign value to variable
	rst     $18
	ld      (DATADD),hl     ; store CH_ADD as data pointer
	ld      hl,(X_PTR)      ; get pointer to READ statement
	ld      (iy+$26),$00    ; clear high byte of X_PTR
	ld      (CH_ADD),hl
	ld      a,(hl)          ; get next READ statement character
m11d9   rst     $18
	cp      ','
	jr      z,m11a1         ; loop back if more items
	call    m10b1           ; check for statement end
	ret

; The DATA command

m11e2   bit     7,(iy+$01)
	jr      nz,m11f3        ; move on if not syntax-checking
m11e8   rst     $28
	DW      $24fb           ; scan next expression
	cp      ','
	call    nz,m10b1        ; if no more items, check for statement end
	rst     $20
	jr      m11e8           ; loop back for more
m11f3   ld      a,$e4           ; we're passing by a DATA statement

; Subroutine to pass by a DEF FN or DATA statement during run-time

m11f5   rst     $28
	DW      $1e39           ; use ROM 3 routine
	ret

; The RUN command

m11f9   rst     $28
	DW      $1e67           ; set NEWPPC as required
	ld      bc,$0000
	rst     $28
	DW      $1e45           ; do a RESTORE 0
	jr      m1207           ; exit via CLEAR command

; The CLEAR command

m1204   rst     $28
	DW      $1e99           ; get operand, use 0 as default
m1207   ld      a,b
	or      c
	jr      nz,m120f        ; move on if non-zero
	ld      bc,(RAMTOP)     ; use existing RAMTOP if zero
m120f   push    bc
	ld      de,(VARS)
	ld      hl,(E_LINE)
	dec     hl
	rst     $28
	DW      $19e5           ; reclaim whole variables area
	rst     $28
	DW      $0d6b           ; cls
	ld      hl,(STKEND)
	ld      de,$0032
	add     hl,de
	pop     de
	sbc     hl,de
	jr      nc,m1232        ; move on if ramtop value too low
	ld      hl,(P_RAMT)
	and     a
	sbc     hl,de
	jr      nc,m1236        ; move on if ramtop value not too high
m1232   call    m2ada
	defb    $15             ; error M - RAMTOP no good
m1236   ld      (RAMTOP),de     ; store new RAMTOP
	pop     de
	pop     hl
	pop     bc
	ld      sp,(RAMTOP)     ; reset SP
	inc     sp
	push    bc
	push    hl
	ld      (ERR_SP),sp     ; reset ERR_SP
	push    de
	ret

; The GOSUB command

m124a   pop     de              ; save STMT_RET address
	ld      h,(iy+$0d)
	inc     h               ; increment SUBPPC statement number
	ex      (sp),hl         ; exchange error address with statement number
	inc     sp              ; reclaim a location
	ld      bc,(PPC)
	push    bc              ; save line number
	push    hl              ; restack error address
	ld      (ERR_SP),sp     ; reset ERR_SP to error address
	push    de              ; restack STMT_RET address
	rst     $28
	DW      $1e67           ; set NEWPPC & NSPPC to required values
	ld      bc,$0014
	rst     $28
	DW      $1f05           ; test for room before making jump
	ret

; The RETURN command

m1266   pop     bc              ; get STMT_RET address
	pop     hl              ; get error address
	pop     de              ; get next stack entry
	ld      a,d
	cp      $3e
	jr      z,m127d         ; move on if end of GOSUB stack
	dec     sp              ; full entry only uses 3 bytes
	ex      (sp),hl         ; exchange error address with statement no
	ex      de,hl
	ld      (ERR_SP),sp     ; reset ERR_SP
	push    bc              ; restack STMT_RET
	ld      (NEWPPC),hl     ; store new line
	ld      (iy+$0a),d      ; and statement
	ret
m127d   push    de              ; reform stack
	push    hl
	call    m2ada
	defb    $06             ; error 7 - RETURN without GOSUB

; The DEF FN command

m1283   bit     7,(iy+$01)
	jr      z,m128e         ; move on if checking syntax
	ld      a,$ce
	jp      m11f5           ; else go to skip DEF FN
m128e   set     6,(iy+$01)      ; signal "numeric variable" in FLAGS
	rst     $28
	DW      $2c8d           ; check present code is a letter
	jr      nc,m12ad        ; error C if not
	rst     $20             ; get next char
	cp      '$'
	jr      nz,m12a1        ; move on if not a string
	res     6,(iy+$01)      ; signal "string variable" in FLAGS
	rst     $20             ; get next char
m12a1   cp      '('
	jr      nz,m12e1        ; error if not (
	rst     $20
	cp      ')'
	jr      z,m12ca         ; move on if no parameters
m12aa   rst     $28
	DW      $2c8d           ; check present code is letter
m12ad   jp      nc,m1125        ; error if not
	ex      de,hl
	rst     $20
	cp      '$'
	jr      nz,m12b8        ; move on if not string
	ex      de,hl
	rst     $20
m12b8   ex      de,hl
	ld      bc,$0006
	rst     $28
	DW      $1655           ; make 6 bytes of room after parameter name
	inc     hl
	inc     hl
	ld      (hl),$0e        ; store a number marker in first location
	cp      ','
	jr      nz,m12ca        ; move on if end of parameters
	rst     $20
	jr      m12aa           ; loop back
m12ca   cp      ')'
	jr      nz,m12e1        ; error if no )
	rst     $20
	cp      '='
	jr      nz,m12e1        ; error if no =
	rst     $20
	ld      a,(FLAGS)
	push    af              ; save nature (number/string) of variable
	rst     $28
	DW      $24fb           ; scan the expression
	pop     af
	xor     (iy+$01)
	and     $40
m12e1   jp      nz,m1125        ; error if expression not correct type
	call    m10b1           ; check for end of statement
	ret


; The Loader routine, called from ROM 0

m12e8   call    m2b89           ; page in DOS workspace
	ld      hl,process
	ld      (hl),$ff        ; signal "current process is Loader"
      IF garry
	ld      a, (LODDRV)
	cp      $54
      ELSE
	ld      hl,FLAGS3
	bit     4,(hl)
      ENDIF
	jp      z,m13c6         ; move on if no disk interface present
	xor     a
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_MESSAGE ; disable ALERT routine
	call    m32ee           ; restore TSTACK
	push    hl
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_BOOT        ; attempt to boot a disk from the boot sector
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	rst     $28
	DW      $16b0           ; clear editing workspaces
	ld      hl,(E_LINE)
	ld      bc,$0007
	rst     $28
	DW      $1655           ; create 7 bytes of space at E_LINE
	ld      hl,m152e
	ld      de,(E_LINE)
	ld      bc,$0007
	ldir                    ; copy LOAD "disk" into E_LINE
	ld      hl,(E_LINE)
	ld      (CH_ADD),hl     ; set CH_ADD
	call    m22c7           ; clear whole display if necessary
	bit     6,(iy+$02)
	jr      nz,m133d        ; move on if shouldn't clear lower screen
	rst     $28
	DW      $0d6e           ; clear lower screen
m133d   res     6,(iy+$02)      ; signal "lower screen can be cleared"
	call    m2b89           ; page in DOS workspace
	ld      hl,ed_flags
	bit     6,(hl)
	jr      nz,m1356        ; ???
	inc     hl
	ld      a,(hl)
	cp      $00
	jr      nz,m1356        ; ???
	call    m3e80
	DW      l1a8e
m1356   call    m2b64           ; page in normal memory
	ld      hl,TV_FLAG
	res     3,(hl)          ; signal "mode unchanged"
	ld      a,$19
	sub     (iy+$4f)
	ld      (SCR_CT),a      ; set scroll count according to current line
	set     7,(iy+$01)      ; signal "execution mode"
	ld      (iy+$0a),$01    ; statement 1
	ld      hl, n3e00
	push    hl
	ld      hl,ONERR
	push    hl
	ld      (ERR_SP),sp     ; set up error stack
	ld      hl,m1383
	ld      (SYNRET),hl     ; error return address
	jp      m1048           ; execute the edit line, returning here on error
m1383   call    m2b89           ; page in DOS workspace
	pop     hl              ; retrieve old ALERT address
	ld      a,$ff
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_MESSAGE ; re-enable ALERT routine
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in normal memory
	ld      a,(ERR_NR)      ; get error code
	bit     7,a
	jp      nz,m25cb        ; if OK, exit via standard syntax return
	ld      hl,(PPC)
	ld      de,$fffe
	xor     a
	sbc     hl,de
	ld      a,h
	or      l
	jp      nz,m25cb        ; or if error occurred in program
	ld      a,'T'
	ld      (LODDRV),a      ; set default load drive to cassette
	ld      hl,TV_FLAG
	set     0,(hl)          ; signal "using lower screen"
	ld      hl,msg18
	call    m1524           ; output cassette loader message
	ld      hl,TV_FLAG
	res     0,(hl)          ; signal "using main screen"
	set     6,(hl)          ; signal "don't clear lower screen"
	jr      m13c9
m13c6   call    m2b64           ; page in normal memory
m13c9   rst     $28
	DW      $16b0           ; clear editing workspaces
	ld      hl,(E_LINE)
	ld      bc,$0003
	rst     $28
	DW      $1655           ; make 3 bytes space at E_LINE
	ld      hl,m14df
	ld      de,(E_LINE)
	ld      bc,$0003
	ldir                    ; copy LOAD "" command into E_LINE
	ld      hl,(E_LINE)
	ld      (CH_ADD),hl     ; set interpretation address
	call    m22c7           ; clear whole screen if necessary
	bit     6,(iy+$02)
	jr      nz,m13f3
	rst     $28
	DW      $0d6e           ; clear lower screen if necessary
m13f3   res     6,(iy+$02)      ; signal "lower screen can be cleared"
	call    m2b89           ; page in DOS workspace
	ld      hl,ed_flags
	bit     6,(hl)
	jr      nz,m140c        ; ???
	inc     hl
	ld      a,(hl)
	cp      $00
	jr      nz,m140c        ; ???
	call    m3e80
	DW      l1a8e
m140c   call    m2b64           ; page in normal memory
	ld      hl,TV_FLAG
	res     3,(hl)          ; signal "mode unchanged"
	ld      a,$19
	sub     (iy+$4f)
	ld      (SCR_CT),a      ; set scroll count according to current line
	set     7,(iy+$01)      ; signal "execution mode"
	ld      (iy+$0a),$01    ; set statement 1
	ld      hl, n3e00
	push    hl
	ld      hl,ONERR
	push    hl
	ld      (ERR_SP),sp     ; set up error stack
	ld      hl,m1439
	ld      (SYNRET),hl     ; set error return address
	jp      m1048           ; execute line in editing area
m1439   ld      a,(ERR_NR)      ; get error code
	bit     7,a
	jp      nz,m25cb        ; exit via standard error return if OK
	ld      hl,FLAGS3
	bit     4,(hl)
	jp      z,m25cb         ; or if no disk interface present
	ld      a,'A'
	ld      (LODDRV),a      ; else set default load drive to A:
	jp      m25cb           ; and exit

; The Print option, called from ROM 0

m1451   rst     $28
	DW      $16b0           ; clear E_LINE, WORKSP, STKBOT etc
	ld      hl,(E_LINE)
	ld      bc,$0001
	rst     $28
	DW      $1655           ; make a byte of space at E_LINE
	ld      hl,(E_LINE)
	ld      (hl),$e1        ; insert LLIST command
	call    m24f0           ; execute it

; The SPECTRUM command

m1465   call    m14c4           ; set "P" channel to use screen
	ld      sp,(ERR_SP)
	pop     hl              ; discard current error return address
	ld      hl,$1303
	push    hl              ; address to enter ROM 3 at, in main loop
	ld      hl,$0013        ; "AF"
	push    hl
	ld      hl,$0008        ; "BC"
	push    hl
	ld      a,$20
	ld      (BANKM),a       ; page 0, paging disabled
	push    af
	push    bc
	di
	res     4,(iy+$01)      ; signal "48K BASIC" mode
	jp      STOO            ; enter 48K ROM

; The routine to enter 48K BASIC, called from ROM 0

m1488   ld      hl,$6000
	push    hl              ; stack address to return to (in RAM)
	ld      de,$6000
	ld      hl, m14b5
	ld      bc,$000f
	ldir                    ; copy following routine into RAM
	ld      a,(BANK678)
	res     3,a             ; turn off disk motor
	ld      bc,$1ffd
	di
	ld      (BANK678),a
	out     (c),a
	ei
	ld      a,$30           ; select ROM 3 with paging disabled
	di
	res     4,(iy+$01)      ; signal "48K BASIC" mode
	ld      (BANKM),a
	push    af
	push    bc
	jp      STOO            ; page in ROM 3 & jump to following routine

m14b5   ld      a,$30
	ld      bc,$7ffd
	di
	out     (c),a           ; page in ROM 3 & disable paging
	ei
	jp      $0000           ; reset machine with 48K ROM locked in

; Cliff J Lawson's initials

	defm    "CJL"

; Subroutine to copy screen channel addresses over printer channel addresses

m14c4   ld      hl,(CHANS)
	ld      de,$0005
	add     hl,de           ; HL=channel info for "S"
	ld      de,$000a
	ex      de,hl
	add     hl,de
	ex      de,hl           ; DE=channel info for "P"
	ld      bc,$0004
	ldir                    ; overwrite it
	res     3,(iy+$30)      ; set caps lock off
	res     4,(iy+$01)      ; signal "48K BASIC" mode
	ret

; LOAD "" command

m14df   defb    $ef,$22,$22

; The Loader message
    IF garry
m14e2   rst     $18
	cp      '#'
	jp      nz, m22ad
	call    m2b35
	ld      b, 2
	jp      m2b19
m14f0   ld      a, b
	cpl
	ld      h, a
	ld      a, c
	cpl
	ld      l, a
	inc     hl
	add     hl, sp
	ld      sp, hl
	push    bc
	push    hl
	ex      de, hl
	ldir
	pop     hl
	rst     $28
	DW      $250e
	pop     hl
	add     hl, sp
	ld      sp, hl
	ret
m1506   rst     $18
	cp      '#'
	jr      z, m1515
	rst     $28
	DW      $1c6c
	call    m10b1
	rst     $28
	DW      $1dab
	ret
m1515   call    m2b35
	call    m3f00
	DW      $005c
	rst     $28
	DW      $2d28
	rst     $28
	DW      $2aff
	ret
    ELSE
msg18   defm    $16, $00, $00
m14e5   defm    $10, $00, $11, $07, $13, $00
      IF spanish
	defm    "Introduzca la cinta y pulse PLAY", $0d
	defm    "Cancelar: pulse BREAK dos veces.", $ff
      ELSE
	defm    "Insert tape and press PLAY", $0d
	defm    "To cancel - press BREAK twice", $ff
      ENDIF
    ENDIF


; Subroutine to output a $ff-terminated message

m1524   ld      a,(hl)          ; get next char
	cp      $ff
	ret     z               ; exit if end
	rst     $28
	DW      $0010           ; output
	inc     hl
	jr      m1524           ; loop back

; LOAD "disk" command

m152e   defb    $ef             ; LOAD keyword
m152f   defm    $22, "disk", $22

; The LIST & LLIST commands (enter at m1535 for LLIST, m1539 for LIST)

m1535   ld      a,$03           ; default stream 3 for LLIST
	jr      m153b
m1539   ld      a,$02           ; default stream 2 for LIST
m153b   ld      (iy+$02),$00    ; signal ordinary listing in main screen
m153f   rst     $28
	DW      $2530           ; are we checking syntax?
	jr      z,m1547
	rst     $28
	DW      $1601           ; open channel if not
m1547   rst     $28
	DW      $0018           ; get character
	rst     $28
	DW      $2070           ; see if stream must be altered
	jr      c,m1567         ; move on if not
	rst     $28
	DW      $0018           ; get character
	cp      ';'
	jr      z,m155a         ; move on if ;
	cp      ','
	jr      nz,m1562        ; move on if not ,
m155a   rst     $28
	DW      $0020           ; get next character
m155d   call    m1121           ; get numeric expression
	jr      m156a           ; move on with line number to list from
m1562   rst     $28
	DW      $1ce6           ; else use zero
	jr      m156a
m1567   rst     $28
	DW      $1cde           ; fetch a numeric expression or use zero
m156a   call    m10b1           ; check for end of statement
m156d   rst     $28
	DW      $1825           ; use ROM 3 for actual listing operation
	ret

; PLAY command (enters here after syntax-checking)

m1571   di
	push    bc              ; save count of strings
	ld      de,$0037        ; $37 bytes required per string
	ld      hl,$003c        ; plus $3c bytes overhead
m1579   add     hl,de
	djnz    m1579
	ld      c,l
	ld      b,h             ; BC=workspace required
	rst     $28
	DW      $0030           ; make space
	di
	push    de
m1583   pop     iy              ; IY=start of space
	push    hl              ; stack HL=end of space+1
m1586   pop     ix
	ld      (iy+$10),$ff
m158c   ld      bc,$ffc9
	add     ix,bc           ; IX=start of next string parameter space
	ld      (ix+$03),$3c
	ld      (ix+$01),$ff
	ld      (ix+$04),$0f
	ld      (ix+$05),$05
	ld      (ix+$21),$00
	ld      (ix+$0a),$00
	ld      (ix+$0b),$00
	ld      (ix+$16),$ff
	ld      (ix+$17),$00
	ld      (ix+$18),$00
	rst     $28
	DW      $2bf1           ; get string from top of stack
	di
	ld      (ix+$06),e
	ld      (ix+$07),d      ; store address of string (twice)
	ld      (ix+$0c),e
	ld      (ix+$0d),d
	ex      de,hl
	add     hl,bc
	ld      (ix+$08),l
	ld      (ix+$09),h      ; store address of end-of-string+1
	pop     bc              ; restore count of strings
	push    bc
	dec     b
	ld      c,b
	ld      b,$00           ; BC=number of strings left on stack
	sla     c
	push    iy
	pop     hl
	add     hl,bc           ; HL=overheadspace+2*(string number-1)
	push    ix
	pop     bc
	ld      (hl),c
	inc     hl
	ld      (hl),b          ; store string parameter block address
	or      a
	rl      (iy+$10)        ; shift in 0 bit for each string
	pop     bc
	dec     b               ; decrement string count
	push    bc
	ld      (ix+$02),b      ; store string number (0...7) in parameters
	jr      nz,m158c        ; back for another string
	pop     bc              ; restore count of strings
	ld      (iy+$27),$1a
	ld      (iy+$28),$0b
	push    iy
	pop     hl
	ld      bc,$002b
	add     hl,bc
	ex      de,hl
	ld      hl,m161d
	ld      bc,$000d
	ldir                    ; copy FP routine in
	ld      d,$07
	ld      e,$f8
	call    m1a6a           ; output $f8 to AY register 7
	ld      d,$0b
	ld      e,$ff
	call    m1a6a           ; output $ff to AY register 11
	inc     d
	call    m1a6a           ; output $ff to AY register 12
	jr      m1669           ; move on

; FP routine used to calculate tempo values, executed in RAM with ROM 3
; paged in

m161d   rst     $28             ; engage FP-calculator
	defb    stk_ten         ; X,10
	defb    exchange        ; 10,X
	defb    division        ; 10/X
	defb    stk_data        ; 10/X,Y
	defb    $df,$75,$f4,$38,$75
	defb    division        ; 10/(X*Y)
	defb    end_calc
	ret

; Subroutine to check if BREAK is being pressed (exit with carry reset if so)

m162a   ld      a,$7f
	in      a,($fe)
	rra
	ret     c               ; exit with carry set if not pressed
	ld      a,$fe
	in      a,($fe)
	rra                     ; test other key & exit
	ret

; Subroutine to initialise string pointers to first string (m163b)

m1636   ld      bc,$0011
	jr      m163e
m163b   ld      bc,$0000
m163e   push    iy
	pop     hl
	add     hl,bc           ; get address of pointer to string
	ld      (iy+$23),l
	ld      (iy+$24),h      ; store address
	ld      a,(iy+$10)
	ld      (iy+$22),a      ; copy available strings byte
	ld      (iy+$21),$01    ; set string bit marker
	ret

; Subroutine to get address of current string parameter block in IX

m1653   ld      e,(hl)
	inc     hl
	ld      d,(hl)
	push    de
	pop     ix
	ret

; Subroutine to increment pointer to next string parameter block address

m165a   ld      l,(iy+$23)
	ld      h,(iy+$24)      ; get current parameter block pointer address
	inc     hl
	inc     hl              ; move to next
	ld      (iy+$23),l
	ld      (iy+$24),h      ; store
	ret

; More PLAY command

m1669   call    m163b           ; copy initial info
m166c   rr      (iy+$22)        ; rotate string counter
	jr      c,m1678         ; move on if no string
	call    m1653           ; get IX=address of current string parm block
	call    m1748           ; interpret string for standard parms
m1678   sla     (iy+$21)
	jr      c,m1683         ; move on if tried 8 strings
	call    m165a           ; increment pointer to string parms address
	jr      m166c           ; loop back
m1683   call    m1b7f           ; find shortest current notelength
	push    de
	call    m1b30           ; output next note from each string
	pop     de
m168b   ld      a,(iy+$10)
	cp      $ff
	jr      nz,m1697        ; move on unless no strings,or stop encountered
	call    m1a81           ; close down AY channels for this command
	ei
	ret                     ; exit
m1697   dec     de
	call    m1b64           ; pause
	call    m1baf           ; decrement note lengths & change notes if nec
	call    m1b7f           ; find shortest current notelength
	jr      m168b           ; loop back

; List of PLAY string parameters

m16a3   defm    "HZYXWUVMT)(NO!"

; Subroutine to get next character from string and increment string
; interpretation address (carry set if no char available)

m16b1   call    m1ad1           ; get next character
	ret     c               ; exit if end of string
	inc     (ix+$06)        ; increment string interpretation address
	ret     nz
	inc     (ix+$07)
	ret

; Subroutine to get a note from the string, returning semitone value A (or 0)
; If A=$80, a rest was found

m16bd   push    hl
	ld      c,$00           ; C=initial semitone value (natural)
m16c0   call    m16b1           ; get next char from string
	jr      c,m16cd         ; move on if none
	cp      '&'
	jr      nz,m16d8        ; move on if not a rest
	ld      a,$80           ; signal "rest"
m16cb   pop     hl
	ret
m16cd   ld      a,(iy+$21)
	or      (iy+$10)
	ld      (iy+$10),a      ; set string to not in use & exit
	jr      m16cb
m16d8   cp      '#'             ; test for sharp sign
	jr      nz,m16df
	inc     c               ; if so, increment semitone value & loop back
	jr      m16c0
m16df   cp      '$'             ; test for flat sign
	jr      nz,m16e6
	dec     c               ; if so, decrement semitone value & loop back
	jr      m16c0
m16e6   bit     5,a
	jr      nz,m16f0        ; move on if lowercase letter
	push    af
	ld      a,$0c
	add     a,c             ; for uppercase, add octave to semitone value
	ld      c,a
	pop     af
m16f0   and     $df             ; make uppercase
	sub     'A'
	jp      c,m1b10         ; error k if <A
	cp      $07
	jp      nc,m1b10        ; or if >G
	push    bc
m16fd   ld      b,$00
	ld      c,a
	ld      hl,m19e7
	add     hl,bc
	ld      a,(hl)          ; get note semitone value
	pop     bc
	add     a,c             ; add octave/sharp/flat value
	pop     hl
	ret

; Subroutine to get a numeric value from the string into BC (defaults to 0)

m1709   push    hl
	push    de
	ld      l,(ix+$06)
	ld      h,(ix+$07)      ; get string interpretation address
	ld      de,$0000        ; initial value 0
m1714   ld      a,(hl)          ; get next char
	cp      '0'
	jr      c,m1731
	cp      '9'+1
	jr      nc,m1731        ; move on if not a digit
	inc     hl
	push    hl
	call    m173c           ; multiply current value by 10
	sub     '0'
	ld      h,$00
	ld      l,a
	add     hl,de           ; add in digit
	jr      c,m172e         ; jump if overflow
	ex      de,hl
	pop     hl
	jr      m1714           ; back for more digits
m172e   jp      m1b08           ; error l - number too big
m1731   ld      (ix+$06),l
	ld      (ix+$07),h      ; replace updated interpretation address
	push    de
	pop     bc              ; BC=value
	pop     de              ; restore registers
	pop     hl
	ret

; Subroutine to multiply DE by 10

m173c   ld      hl,$0000        ; start with zero
	ld      b,$0a
m1741   add     hl,de           ; add DE 10 times
	jr      c,m172e         ; jump if overflow
	djnz    m1741
	ex      de,hl
	ret

; Subroutine to interpret a string

m1748   call    m162a           ; check for break
	jr      c,m1755         ; move on if not pressed
	call    m1a81           ; close down channels for this command
	ei                      ; re-enable interrupts
	call    m2ada
	defb    20              ; error L - BREAK into program
m1755   call    m16b1           ; get next character of string
	jp      c,m1990         ; move on if no more available
	call    m19de           ; search for char in parameter list
	ld      b,$00
	sla     c
	ld      hl,m19b8
	add     hl,bc           ; form pointer to routine address
	ld      e,(hl)
	inc     hl
	ld      d,(hl)
	ex      de,hl           ; HL=routine address
	call    m1770           ; call routine
	jr      m1748           ; loop back for more standard params
	ret                     ; exit when note length changed, or note found
m1770   jp      (hl)

; Parameter !: comment

m1771   call    m16b1           ; get next character from string
	jp      c,m198f         ; move on if end of string
	cp      '!'
	ret     z               ; exit if end of comment
	jr      m1771           ; loop back

; Parameter O: octave

m177c   call    m1709           ; get number from string
	ld      a,c
	cp      $09
	jp      nc,m1b00        ; error n if not 1-8
	sla     a
	sla     a
	ld      b,a
	sla     a
	add     a,b
	ld      (ix+$03),a      ; store base note number (12*octave)
	ret

; Parameter N: number separator

m1791   ret                     ; ignore

; Parameter (: start of repeat section

m1792   ld      a,(ix+$0b)      ; get current bracket depth
	inc     a               ; increment
	cp      $05
	jp      z,m1b18         ; if depth now 5, cause error d
	ld      (ix+$0b),a      ; store new depth
	ld      de,$000c        ; offset for bracket addresses
	call    m1813           ; get pointer to next bracket address down
	ld      a,(ix+$06)
	ld      (hl),a
	inc     hl
	ld      a,(ix+$07)
	ld      (hl),a          ; store address to repeat from
	ret

; Parameter ): end of repeat section

m17ae   ld      a,(ix+$16)      ; get number of )s encountered so far
	ld      de,$0017        ; offset for close bracket addresses
	or      a
	jp      m,m17dc         ; move on if none so far
	call    m1813           ; get address of current
	ld      a,(ix+$06)
	cp      (hl)
	jr      nz,m17dc
	inc     hl
	ld      a,(ix+$07)
	cp      (hl)
	jr      nz,m17dc        ; move on if not the same
	dec     (ix+$16)        ; decrement close bracket depth
	ld      a,(ix+$16)
	or      a
	ret     p               ; exit if still positive
	bit     0,(ix+$0a)
	ret     z               ; exit if not infinite repeat
	ld      (ix+$16),$00    ; set no close brackets
	xor     a
	jr      m17f7
m17dc   ld      a,(ix+$16)
	inc     a               ; increment close bracket depth
	cp      $05
	jp      z,m1b18         ; error d if depth of 5
	ld      (ix+$16),a      ; restore depth
	call    m1813           ; get pointer to next close bracket address
	ld      a,(ix+$06)
	ld      (hl),a
	inc     hl
	ld      a,(ix+$07)
	ld      (hl),a          ; store address to repeat to
	ld      a,(ix+$0b)      ; get current open bracket depth
m17f7   ld      de,$000c
	call    m1813           ; get pointer to address (or string start)
	ld      a,(hl)
	ld      (ix+$06),a
	inc     hl
	ld      a,(hl)
	ld      (ix+$07),a      ; reset interpretation address to correct point
	dec     (ix+$0b)        ; decrement open bracket depth
	ret     p
	ld      (ix+$0b),$00    ; reset to zero if no open brackets
	set     0,(ix+$0a)      ; set "infinite repeat"
	ret

; Subroutine to get HL=Ath word entry after IX+DE
; Used to find address for bracket address entries

m1813   push    ix
	pop     hl
	add     hl,de           ; add offset to string area
	ld      b,$00
	ld      c,a
	sla     c
	add     hl,bc           ; add offset to Ath word
	ret

; Parameter T: tempo

m181e   call    m1709           ; get number from string
	ld      a,b
	or      a
	jp      nz,m1b00        ; error n if >255
	ld      a,c
	cp      $3c
	jp      c,m1b00         ; error n if <60
	cp      $f1
	jp      nc,m1b00        ; error n if >240
	ld      a,(ix+$02)
	or      a
	ret     nz              ; ignore unless in first string
	ld      b,$00
	push    bc
	pop     hl
	add     hl,hl
	add     hl,hl
	push    hl
	pop     bc              ; BC=tempo*4
	push    iy
	rst     $28
	DW      $2d2b           ; stack BC on calculator stack
	di
	pop     iy
	push    iy
	push    iy
	pop     hl
	ld      bc,$002b        ; offset to FP calculation routine
	add     hl,bc
	ld      iy,ERR_NR
	push    hl              ; stack FP routine address
	ld      hl,m1864
	ld      (RETADDR),hl    ; set up return address
	ld      hl,REGNUOY
	ex      (sp),hl
	push    hl
	push    af
	push    hl
	jp      STOO            ; call FP calculator - TOS=10/(tempo*4*val)
m1864   di
	rst     $28
	DW      $2da2           ; get value to BC
	di
	pop     iy
	ld      (iy+$27),c
	ld      (iy+$28),b      ; store tempo value
	ret

; Parameter M: channel

m1872   call    m1709           ; get number from string
	ld      a,c
	cp      $40
	jp      nc,m1b00        ; error n if >63
	cpl
	ld      e,a
	ld      d,$07
	call    m1a6a           ; output channel complement to AY register 7
	ret

; Parameter V: volume level

m1883   call    m1709           ; get number from string
	ld      a,c
	cp      $10
	jp      nc,m1b00        ; error n if >15
	ld      (ix+$04),a      ; store volume level
	ld      e,(ix+$02)
	ld      a,$08
	add     a,e
	ld      d,a             ; AY register=channel+8 (channel=0..7)
	ld      e,c
	call    m1a6a           ; output volume level to register
	ret

; Parameter U: volume effect in a string

m189b   ld      e,(ix+$02)
	ld      a,$08
	add     a,e
	ld      d,a             ; AY register=channel+8 (channel=0..7)
	ld      e,$1f
	ld      (ix+$04),e      ; store volume effect marker
	ret

; Parameter W: volume effect

m18a8   call    m1709           ; get number from string
	ld      a,c
	cp      $08
	jp      nc,m1b00        ; error n if >7
	ld      b,$00
	ld      hl,m19d6
	add     hl,bc
	ld      a,(hl)          ; get envelope byte
	ld      (iy+$29),a      ; store it
	ret

; Parameter X: volume duration

m18bc   call    m1709           ; get number from string
	ld      d,$0b
	ld      e,c
	call    m1a6a           ; output duration to AY registers 11
	inc     d
	ld      e,b
	call    m1a6a           ; and 12
	ret

; Parameter Y: MIDI channel

m18cb   call    m1709           ; get number from string
	ld      a,c
	dec     a               ; decrement
	jp      m,m1b00         ; error n if was 0
	cp      $10
	jp      nc,m1b00        ; error n if >15
	ld      (ix+$01),a      ; store channel
	ret

; Parameter Z: MIDI programming code

m18dc   call    m1709           ; get number from string
	ld      a,c
	call    m1d91           ; output code to MIDI
	ret

; Parameter H: stop PLAY command

m18e4   ld      (iy+$10),$ff    ; signal "no strings in use"
	ret

; Notes and other parameters

m18e9   call    m1a07           ; is char a digit? (ie note length)
	jp      c,m196f         ; move on if not
	call    m199a           ; get HL=pointer to note lengths for string
	call    m19a2           ; save in overhead area
	xor     a
	ld      (ix+$21),a      ; zero number of tied notes
	call    m1ab6           ; decrement interpretation address
	call    m1709           ; get number from string
	ld      a,c
	or      a
	jp      z,m1b00         ; error n if <1
	cp      $0d
	jp      nc,m1b00        ; or >12
	cp      $0a
	jr      c,m1920         ; move on unless dealing with triplets
	call    m19ee           ; get note length value
	call    m1962           ; increment number of tied notes
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; store note length for first note
m1916   call    m1962           ; increment number of tied notes
	inc     hl
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; store note length for next tied note
	inc     hl
	jr      m1926
m1920   ld      (ix+$05),c      ; save new note length
	call    m19ee           ; get note length value
m1926   call    m1962           ; increment number of tied notes
m1929   call    m1ad1           ; test next character
	cp      '_'
	jr      nz,m195c        ; move on unless tieing notes
	call    m16b1           ; get the character
	call    m1709           ; get number from string
	ld      a,c
	cp      $0a
	jr      c,m194d         ; move on if not triplet
	push    hl
	push    de
	call    m19ee           ; get new note length value
	pop     hl
	add     hl,de
	ld      c,e
	ld      b,d             ; BC=old note length value+new note length val
	ex      de,hl           ; so does DE
	pop     hl              ; restore address to store
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; store value
	ld      e,c
	ld      d,b
	jr      m1916           ; loop back
m194d   ld      (ix+$05),c      ; store new note length
	push    hl
	push    de
	call    m19ee           ; get note length value
	pop     hl
	add     hl,de
	ex      de,hl           ; DE=old note length val+new note length val
	pop     hl
	jp      m1929           ; loop back

; Store note length value & move on

m195c   ld      (hl),e
	inc     hl
	ld      (hl),d          ; store note length value
	jp      m198a           ; move on

; Subroutine to increment number of tied notes for a string

m1962   ld      a,(ix+$21)      ; get number of tied notes
	inc     a               ; increment
m1966   cp      $0b
	jp      z,m1b28         ; error o - too many tied notes
	ld      (ix+$21),a
	ret

; Notes and other parameters (continued)

m196f   call    m1ab6           ; decrement string interpretation pointer
	ld      (ix+$21),$01    ; set 1 tied note
	call    m199a           ; get pointer to note lengths for string
	call    m19a2           ; save in overhead area
	ld      c,(ix+$05)      ; get current note length
	push    hl
	call    m19ee           ; calc note length value
	pop     hl
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; store it
	jp      m198a
m198a   pop     hl              ; retrieve return address
m198b   inc     hl
	inc     hl              ; move on by two
	push    hl              ; restack
	ret                     ; return

; Subroutine to set current string to "finished"

m198f   pop     hl              ; discard return address
m1990   ld      a,(iy+$21)      ; get string mask bit
	or      (iy+$10)
	ld      (iy+$10),a      ; place into strings counter
	ret

; Subroutine to set HL=pointer to note lengths for current string

m199a   push    ix
	pop     hl
	ld      bc,$0022
	add     hl,bc
	ret

; Subroutine to save note lengths pointer of string in overhead area

m19a2   push    hl              ; save note lengths pointer
	push    iy
	pop     hl
	ld      bc,$0011
	add     hl,bc           ; HL=overhead area+$11
	ld      b,$00
	ld      c,(ix+$02)
m19af   sla     c
	add     hl,bc           ; HL=overhead area+$11+(string*2)
	pop     de
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; store note lengths pointer
	ex      de,hl           ; restore HL=note lengths pointer
	ret

; Table of routine addresses for elements of PLAY strings

m19b8   DW      m18e9           ; note or other parameter
	DW      m1771           ; Z
	DW      m177c           ; Y
	DW      m1791           ; X
	DW      m1792           ; W
	DW      m17ae           ; U
	DW      m181e           ; V
	DW      m1872           ; M
	DW      m1883           ; T
	DW      m189b           ; )
	DW      m18a8           ; (
	DW      m18bc           ; N
	DW      m18cb           ; O
	DW      m18dc           ; !
	DW      m18e4           ; H

; Table of waveforms for volume effects

m19d6   defb    $00,$04,$0b,$0d
	defb    $08,$0c,$0e,$0a

; Subroutine to search for string character A in parameter list
; Z set if found

m19de   ld      bc,$000f
	ld      hl,m16a3
	cpir
	ret

; Table of note semitone values

m19e7   defb    $09,$0b,$00,$02,$04,$05,$07

; Subroutine to get note length value (DE) for note length C (1-12)

m19ee   push    hl
	ld      b,$00
	ld      hl,m19fa        ; start of table
	add     hl,bc
	ld      d,$00
	ld      e,(hl)          ; DE=note length value
	pop     hl
	ret

; Table of note length values

m19fa   defb    $80
	defb    $06,$09,$0c,$12
	defb    $18,$24,$30,$48
	defb    $60,$04,$08,$10

; Subroutine to test if A is a digit (carry reset if so)

m1a07   cp      '0'
	ret     c
	cp      '9'+1
	ccf
	ret

; Subroutine to play note A through AY channel for current string

m1a0e   ld      c,a             ; save semitone value
	ld      a,(ix+$03)
	add     a,c             ; add in base note value
	cp      $80
	jp      nc,m1b20        ; error m if out of range
	ld      c,a             ; save note value
	ld      a,(ix+$02)
	or      a
	jr      nz,m1a2d        ; move on unless first string
	ld      a,c
	cpl
	and     $7f
	srl     a
m1a25   srl     a
	ld      d,$06
	ld      e,a
	call    m1a6a           ; output value to AY register 6
m1a2d   ld      (ix+$00),c      ; save last note value
	ld      a,(ix+$02)
	cp      $03
	ret     nc              ; exit unless outputting AY channel 0-2
	ld      hl,m1c84
	ld      b,$00
	ld      a,c
	sub     $15
	jr      nc,m1a45
	ld      de,$0fbf        ; lowest note possible
	jr      m1a4c
m1a45   ld      c,a
	sla     c               ; form offset into semitone table
	add     hl,bc
	ld      e,(hl)
	inc     hl
m1a4b   ld      d,(hl)          ; get DE=note value
m1a4c   ex      de,hl
	ld      d,(ix+$02)
	sla     d               ; AY register=0,2 or 4
	ld      e,l
	call    m1a6a           ; output low byte of note value
	inc     d               ; AY register=1,3 or 5
	ld      e,h
	call    m1a6a           ; output high byte of note value
	bit     4,(ix+$04)
	ret     z               ; exit unless envelope to be used
	ld      d,$0d
	ld      a,(iy+$29)
	ld      e,a
	call    m1a6a           ; output waveform number to AY register 13
	ret

; Subroutine to output value E to sound register D

m1a6a   push    bc
	ld      bc,$fffd
	out     (c),d           ; select register D
	ld      bc,$bffd
	out     (c),e           ; output value E
	pop     bc
	ret

; Subroutine to get value of AY register A

m1a77   push    bc
	ld      bc,$fffd
	out     (c),a           ; select register
	in      a,(c)           ; get value
	pop     bc
	ret

; Subroutine to close down AY channels associated with this PLAY command

m1a81   ld      d,$07
	ld      e,$ff
	call    m1a6a           ; output $ff to AY register 7
	ld      d,$08
	ld      e,$00
	call    m1a6a           ; output 0 to AY register 8
	inc     d
	call    m1a6a           ; output 0 to AY register 9
	inc     d
	call    m1a6a           ; output 0 to AY register 10
	call    m163b           ; initialise string pointer info
m1a9a   rr      (iy+$22)        ; test for string
	jr      c,m1aa6         ; move on if none
	call    m1653           ; get IX=address of string parameter block
	call    m1d7b           ; output terminator if MIDI channel
m1aa6   sla     (iy+$21)
	jr      c,m1ab1         ; move on when 8 strings tested for
	call    m165a           ; increment pointer to next string block
	jr      m1a9a           ; loop back
m1ab1   ld      iy,ERR_NR       ; reset IY to system variables
	ret                     ; done

; Subroutine to decrement string interpretation pointer (skipping white space)

m1ab6   push    hl
	push    de
	ld      l,(ix+$06)
	ld      h,(ix+$07)      ; get current pointer
m1abe   dec     hl              ; decrement
	ld      a,(hl)
	cp      $20
	jr      z,m1abe
	cp      $0d
	jr      z,m1abe         ; loop back while on white space
	ld      (ix+$06),l
	ld      (ix+$07),h      ; store updated pointer
	pop     de
	pop     hl
	ret

; Subroutine to get next character from string in A, skipping any white space
; Carry set on exit if end of string reached

m1ad1   push    hl
	push    de
	push    bc
	ld      l,(ix+$06)
	ld      h,(ix+$07)      ; get HL=string interpretation address
m1ada   ld      a,h
	cp      (ix+$09)        ; compare against string end address
	jr      nz,m1ae9
	ld      a,l
	cp      (ix+$08)
	jr      nz,m1ae9
	scf                     ; set carry if end of string
	jr      m1af3
m1ae9   ld      a,(hl)
	cp      ' '
	jr      z,m1af7         ; move to skip any spaces
	cp      $0d
	jr      z,m1af7         ; or CRs
	or      a               ; reset carry
m1af3   pop     bc
	pop     de
	pop     hl
m1af6   ret
m1af7   inc     hl              ; increment string interpretation address
	ld      (ix+$06),l
	ld      (ix+$07),h
	jr      m1ada           ; loop back

; Error routines for PLAY

m1b00   call    m1a81           ; close down
	ei
	call    m2ada
	defb    41              ; error n - Out of range
m1b08   call    m1a81           ; close down
	ei
	call    m2ada
	defb    39              ; error l - Number too big
m1b10   call    m1a81           ; close down
	ei
	call    m2ada
	defb    38              ; error k - Invalid note name
m1b18   call    m1a81           ; close down
	ei
	call    m2ada
	defb    31              ; error d - Too many brackets
m1b20   call    m1a81           ; close down
	ei
	call    m2ada
	defb    40              ; error m - Note out of range
m1b28   call    m1a81           ; close down
	ei
	call    m2ada
	defb    42              ; error o - Too many tied notes

; Subroutine to output next note from each string

m1b30   call    m163b           ; initialise string pointer info
m1b33   rr      (iy+$22)        ; test for next string
	jr      c,m1b5a         ; move on if not present
	call    m1653           ; get address of string parameter block to IX
	call    m16bd           ; get note from string
	cp      $80
	jr      z,m1b5a         ; move on if rest found
	call    m1a0e           ; calculate semitone & play if string 0-2
	ld      a,(ix+$02)
	cp      $03
	jr      nc,m1b57        ; move on if strings 3-7
	ld      d,$08
	add     a,d
	ld      d,a
	ld      e,(ix+$04)
	call    m1a6a           ; output volume level to AY register 8+channel
m1b57   call    m1d5c           ; output semitone to MIDI channels
m1b5a   sla     (iy+$21)
	ret     c               ; exit when 8 strings done
	call    m165a           ; get to next string parameter block
	jr      m1b33           ; loop back

; Subroutine to pause for current notelength (DE)

m1b64   push    hl
	ld      l,(iy+$27)
	ld      h,(iy+$28)      ; HL=tempo value
	ld      bc,$0064
	or      a
	sbc     hl,bc
	push    hl
	pop     bc
	pop     hl
m1b74   dec     bc
	ld      a,b
	or      c
	jr      nz,m1b74        ; timing delay
	dec     de
	ld      a,d
	or      e
	jr      nz,m1b64        ; loop DE times
	ret

; Subroutine to find shortest cuurent note across all strings

m1b7f   ld      de,$ffff        ; largest notelength so far (-1)
	call    m1636           ; initialise pointers to first string
m1b85   rr      (iy+$22)        ; test for next string
	jr      c,m1b9d         ; move on if not present
	push    de
	ld      e,(hl)
	inc     hl
	ld      d,(hl)          ; get notelength pointer
	ex      de,hl
	ld      e,(hl)
	inc     hl
	ld      d,(hl)          ; get note length
	push    de
	pop     hl
	pop     bc
	or      a
	sbc     hl,bc
	jr      c,m1b9d         ; move on if already found smaller
	push    bc
	pop     de              ; keep current
m1b9d   sla     (iy+$21)        ; shift string bit marker
	jr      c,m1ba8         ; move on if done 8 strings
	call    m165a           ; increment pointer to next strings pointer
	jr      m1b85           ; loop back
m1ba8   ld      (iy+$25),e
	ld      (iy+$26),d      ; store shortest current note length
	ret

; Subroutine to decrement remaining note lengths for each string, changing
; notes if necessary

m1baf   xor     a
	ld      (iy+$2a),a      ; set no strings have changed notes
	call    m163b           ; initialise string pointers
m1bb6   rr      (iy+$22)
	jp      c,m1c48         ; move on if string not present
	call    m1653           ; get address of current string parameter block
	push    iy
	pop     hl
	ld      bc,$0011
	add     hl,bc
	ld      b,$00
	ld      c,(ix+$02)
	sla     c
	add     hl,bc
	ld      e,(hl)
	inc     hl
	ld      d,(hl)
	ex      de,hl
	push    hl
	ld      e,(hl)
	inc     hl
	ld      d,(hl)
	ex      de,hl           ; HL=notelength for this string
	ld      e,(iy+$25)
	ld      d,(iy+$26)      ; DE=length to play
	or      a
	sbc     hl,de
	ex      de,hl
	pop     hl
	jr      z,m1bea         ; move on if same length
	ld      (hl),e
	inc     hl
	ld      (hl),d          ; else store remaining length
	jr      m1c48           ; and move on
m1bea   ld      a,(ix+$02)
	cp      $03
	jr      nc,m1bfa        ; move on if MIDI channel
	ld      d,$08
	add     a,d
	ld      d,a             ; select AY register 8+channel
	ld      e,$00
	call    m1a6a           ; output 0 to register
m1bfa   call    m1d7b           ; output terminator if MIDI channel
	push    ix
	pop     hl
	ld      bc,$0021
	add     hl,bc
	dec     (hl)            ; decrement number of tied notes
	jr      nz,m1c14        ; move on if still some left
	call    m1748           ; interpret string for parameters
	ld      a,(iy+$21)
	and     (iy+$10)
	jr      nz,m1c48        ; move on if string no longer in use
	jr      m1c2b           ; move on
m1c14   push    iy
	pop     hl
	ld      bc,$0011
	add     hl,bc
	ld      b,$00
	ld      c,(ix+$02)
	sla     c
	add     hl,bc
	ld      e,(hl)
	inc     hl
	ld      d,(hl)
	inc     de
	inc     de
	ld      (hl),d
	dec     hl
	ld      (hl),e          ; store pointer to next tied note length
m1c2b   call    m16bd           ; get note from string
	ld      c,a
	ld      a,(iy+$21)
	and     (iy+$10)
	jr      nz,m1c48        ; move on if string no longer in use
	ld      a,c
	cp      $80
	jr      z,m1c48         ; move on if rest found
	call    m1a0e           ; play note through channel
	ld      a,(iy+$21)
	or      (iy+$2a)
	ld      (iy+$2a),a      ; signal "note changed for this string"
m1c48   sla     (iy+$21)
	jr      c,m1c54         ; move on if no more strings
	call    m165a           ; get to next string parameter block
	jp      m1bb6           ; loop back
m1c54   ld      de,$0001
	call    m1b64           ; pause
	call    m163b           ; initialise pointers to first string
m1c5d   rr      (iy+$2a)
	jr      nc,m1c7a        ; move on if note didn't change
	call    m1653           ; get pointer to string parameter block
	ld      a,(ix+$02)
	cp      $03
	jr      nc,m1c77        ; move on if MIDI channel
	ld      d,$08
	add     a,d
	ld      d,a
	ld      e,(ix+$04)
	call    m1a6a           ; output volume to AY register 8+channel
m1c77   call    m1d5c           ; output semitone to MIDI channel
m1c7a   sla     (iy+$21)
	ret     c               ; exit if 8 strings done
	call    m165a           ; move to next string parameter block
	jr      m1c5d           ; loop back

; The semitone table of note values

m1c84   DW      $0fbf,$0edc,$0e07,$0d3d
	DW      $0c7f,$0bcc,$0b22,$0a82
	DW      $09eb,$095d,$08d6,$0857
	DW      $07df,$076e,$0703,$069f
	DW      $0640,$05e6,$0591,$0541
	DW      $04f6,$04ae,$046b,$042c
	DW      $03f0,$03b7,$0382,$034f
	DW      $0320,$02f3,$02c8,$02a1
	DW      $027b,$0257,$0236,$0216
	DW      $01f8,$01dc,$01c1,$01a8
	DW      $0190,$0179,$0164,$0150
	DW      $013d,$012c,$011b,$010b
	DW      $00fc,$00ee,$00e0,$00d4
	DW      $00c8,$00bd,$00b2,$00a8
	DW      $009f,$0096,$008d,$0085
	DW      $007e,$0077,$0070,$006a
	DW      $0064,$005e,$0059,$0054
	DW      $004f,$004b,$0047,$0043
	DW      $003f,$003b,$0038,$0035
	DW      $0032,$002f,$002d,$002a
	DW      $0028,$0025,$0023,$0021
	DW      $001f,$001e,$001c,$001a
	DW      $0019,$0018,$0016,$0015
	DW      $0014,$0013,$0012,$0011
	DW      $0010,$000f,$000e,$000d
	DW      $000c,$000c,$000b,$000b
	DW      $000a,$0009,$0009,$0008

; Subroutine to output a semitone if a MIDI channel

m1d5c   ld      a,(ix+$01)
	or      a
	ret     m               ; exit if not a MIDI channel
	or      $90
	call    m1d91           ; output channel selector to MIDI
	ld      a,(ix+$00)
	call    m1d91           ; output last semitone value
	ld      a,(ix+$04)
	res     4,a             ; ignore waveform flag
	sla     a
	sla     a
	sla     a
	call    m1d91           ; output volume to MIDI
	ret

; Subroutine to output terminator to MIDI channel

m1d7b   ld      a,(ix+$01)
	or      a
	ret     m               ; exit if not a MIDI channel
	or      $80
	call    m1d91           ; output channel selector to MIDI
	ld      a,(ix+$00)
	call    m1d91           ; output semitone to MIDI
	ld      a,$40
	call    m1d91           ; output terminator to MIDI
	ret

; Subroutine to output a value (A) to the MIDI port (uses AUX)

m1d91   ld      l,a             ; save value
	ld      bc,$fffd
	ld      a,$0e
	out     (c),a           ; select AY register 14 (RS232/AUX)
	ld      bc,$bffd
	ld      a,$fa
	out     (c),a           ; output data low to AUX
	ld      e,$03
m1da2   dec     e
	jr      nz,m1da2        ; delay loop
	nop
	nop
	nop
	nop
	ld      a,l
	ld      d,$08           ; 8 bits to output
m1dac   rra                     ; get next LSB of value to carry
	ld      l,a
	jp      nc,m1db7
	ld      a,$fe
	out     (c),a           ; if set, output data high to AUX
	jr      m1dbd
m1db7   ld      a,$fa
	out     (c),a           ; if reset, output data low to AUX
	jr      m1dbd
m1dbd   ld      e,$02
m1dbf   dec     e
	jr      nz,m1dbf        ; delay loop
	nop
	add     a,$00
	ld      a,l
	dec     d
	jr      nz,m1dac        ; loop back for more bits
	nop
	nop
	add     a,$00
	nop
	nop
	ld      a,$fe
	out     (c),a           ; output data high to register
	ld      e,$06
m1dd5   dec     e
	jr      nz,m1dd5        ; delay loop
	ret

; Unused code for a FORMAT "P";n command, used in same way as FORMAT LINE n

m1dd9
      IF garry
	rst     $20
	rst     $28
	DW      $1c82           ; get a string expression
	rst     $18
	cp      ','
	jp      nz, m1125
	rst     $20             ; get next char
	rst     $28
	DW      $1c82           ; get numeric expression
	rst     $18
	ld      hl, FLAGS3
	res     6, (hl)
	cp      ','
	jr      nz, m1df7
	set     6, (hl)
	rst     $20
	rst     $28
	DW      $1c82           ; get a string expression
m1df7   call    m10b1
	ld      a, 1
	rst     $28
	DW      $1601
	jp      m37fe
      ELSE
	rst     $28
	DW      $0018           ; get character at CH_ADD
	rst     $28
	DW      $1c8c           ; get a string expression
	bit     7,(iy+$01)
	jr      z,m1df9         ; move on if just syntax-checking
	rst     $28
	DW      $2bf1           ; get string from stack
	ld      a,c
	dec     a
	or      b
	jr      z,m1df1         ; if length 1, move on
	call    m2ada
	defb    36              ; else error j - invalid baud rate
m1df1   ld      a,(de)
	and     $df             ; get string char & make uppercase
	cp      'P'
	jp      nz,m1125        ; nonsense in BASIC error if not "P"
m1df9   ld      hl,(CH_ADD)
	ld      a,(hl)
	cp      ';'
	jp      nz,m1125        ; nonsense in BASIC error if next char not ";"
      ENDIF
m1e02   rst     $28
	DW      $0020           ; get next char & continue into FORMAT LINE

; The FORMAT LINE command

m1e05   rst     $28
	DW      $1c82           ; get numeric expression
	bit     7,(iy+$01)
	jr      z,m1e15         ; move on if syntax-checking
	rst     $28
	DW      $1e99           ; get value to BC
	ld      (BAUD),BC       ; set BAUD rate
m1e15   rst     $28
	DW      $0018           ; get next char
	cp      $0d
	jr      z,m1e21         ; move on if end-of-line
	cp      ':'
	jp      nz,m1125        ; error if not end-of-statement
m1e21   call    m10b1           ; check for end-of-statement
	ld      bc,(BAUD)
	ld      a,b
	or      c
	jr      nz,m1e30        ; move on if baud rate not zero
	call    m2ada
	defb    $25             ; else error "invalid baud rate"
m1e30   ld      hl,m1e50        ; baud rate table
m1e33   ld      e,(hl)
	inc     hl
	ld      d,(hl)          ; get next baud rate
	inc     hl
	ex      de,hl
	ld      a,h
	cp      $25
	jr      nc,m1e47        ; move on if end of table
	and     a
	sbc     hl,bc
	jr      nc,m1e47        ; move on if >= required rate
	ex      de,hl
	inc     hl              ; skip timing constant
	inc     hl
	jr      m1e33           ; loop back for next
m1e47   ex      de,hl
	ld      e,(hl)
	inc     hl
	ld      d,(hl)          ; get appropriate timing constant
	ld      (BAUD),de       ; save in BAUD
	ret

; The baud rate table

m1e50   DW      $0032,$0aa5     ; 50
	DW      $006e,$04d4     ; 110
	DW      $012c,$01c3     ; 300
	DW      $0258,$00e0     ; 600
	DW      $04b0,$006e     ; 1200
	DW      $0960,$0036     ; 2400
	DW      $12c0,$0019     ; 4800
	DW      $2580,$000b     ; 9600

; Printer input channel routine

m1e70   ld      hl,FLAGS3
	bit     3,(hl)
	jp      z,m1e85         ; move on if using Centronics
	ld      hl,SERFL
	ld      a,(hl)
	and     a
	jr      z,m1e89         ; move on if no RS232 character waiting
	ld      (hl),$00        ; reset SERFL flag
	inc     hl
	ld      a,(hl)          ; and get character
	scf
	ret
m1e85   rst     $28
	DW      $15c4           ; invalid I/O device error
	ret
m1e89   call    m2af9           ; test for BREAK
      IF v41
	ld      hl,FLAGS2
	bit     6,(hl)
	jr      z,m1e8c
	jp      m1f94
      ENDIF
m1e8c   di
	exx
	ld      de,(BAUD)       ; DE=BAUD
	ld      hl,(BAUD)
	srl     h
	rr      l               ; HL=BAUD/2
	or      a
	ld      b,$fa           ; B=timing constant
	exx
	ld      c,$fd
	ld      d,$ff
	ld      e,$bf
	ld      b,d
	ld      a,$0e
	out     (c),a           ; select AY register 14
	in      a,(c)           ; get RS232/AUX value
	or      $f0
	and     $fb             ; set CTS low
	ld      b,e
	out     (c),a           ; output CTS low
	ld      h,a             ; save RS232/AUX value with CTS low
m1eb2   ld      b,d
	in      a,(c)           ; get RS232/AUX value
	and     $80
	jr      z,m1ec2         ; move on if TXD was low (ie data ready)
m1eb9   exx
	dec     b               ; decrement timer
	exx
	jr      nz,m1eb2        ; loop back
	xor     a               ; carry reset for no data
	push    af
	jr      m1efb           ; move on if no data received
m1ec2   in      a,(c)
	and     $80
	jr      nz,m1eb9        ; back if TXD high
	in      a,(c)
	and     $80
	jr      nz,m1eb9        ; back if TXD high
	exx
	ld      bc,$fffd
	ld      a,$80           ; A'=char to build (carry will be set when
	ex      af,af'          ; all 8 bits have been read)
m1ed5   add     hl,de
	nop
	nop
	nop
	nop
m1eda   dec     hl
	ld      a,h
	or      l
	jr      nz,m1eda        ; baud rate timing loop
	in      a,(c)           ; get RS232/AUX data
	and     $80             ; mask data bit
	jp      z,m1eef         ; move on if zero
	ex      af,af'
	scf
	rra                     ; rotate a 1 bit in
	jr      c,m1ef8         ; move on if byte complete
	ex      af,af'
	jp      m1ed5           ; loop back for more bits
m1eef   ex      af,af'
	or      a
	rra                     ; rotate a 0 bit in
	jr      c,m1ef8         ; move on if byte complete
	ex      af,af'
	jp      m1ed5           ; loop back for more bits
m1ef8   scf                     ; carry set for data received
	push    af
	exx
m1efb   ld      a,h
	or      $04
	ld      b,e
	out     (c),a           ; set RS232 CTS high
	exx
	ld      h,d
	ld      l,e             ; HL=BAUD
	ld      bc,$0007
	or      a
	sbc     hl,bc
m1f0a   dec     hl
	ld      a,h
	or      l
	jr      nz,m1f0a        ; timing loop
	ld      bc,$fffd
	add     hl,de
	add     hl,de
	add     hl,de
m1f15   in      a,(c)
	and     $80
	jr      z,m1f23         ; move on if TXD low (2nd byte available)
	dec     hl
	ld      a,h
	or      l
	jr      nz,m1f15        ; timing loop
	pop     af              ; restore value
	ei
	ret                     ; exit
m1f23   in      a,(c)
	and     $80
	jr      nz,m1f15        ; move back if TXD high
	in      a,(c)
	and     $80
	jr      nz,m1f15        ; move back if TXD high
	ld      h,d
	ld      l,e
	ld      bc,$0002
	srl     h
	rr      l
	or      a
	sbc     hl,bc
	ld      bc,$fffd
	ld      a,$80           ; prepare 2nd byte in A'
	ex      af,af'
m1f41   nop
	nop
	nop
	nop
	add     hl,de
m1f46   dec     hl
	ld      a,h
	or      l
	jr      nz,m1f46        ; timing loop
	in      a,(c)
	and     $80             ; test bit
	jp      z,m1f5b         ; move on if zero
	ex      af,af'
	scf
	rra                     ; rotate a 1 bit in
	jr      c,m1f64         ; move on if byte complete
	ex      af,af'
	jp      m1f41           ; back for more bits
m1f5b   ex      af,af'
	or      a
	rra                     ; rotate a 0 bit in
	jr      c,m1f64         ; move on if byte complete
	ex      af,af'
	jp      m1f41           ; back for more bits
m1f64   ld      hl,SERFL
	ld      (hl),$01        ; flag "2nd byte available"
	inc     hl
	ld      (hl),a          ; store 2nd byte
	pop     af              ; restore the 1st byte
	ei
	ret                     ; done

m1f94 IF v41
	di
	exx
	ld      de,(BAUD)       ; DE=BAUD
	ld      hl,(BAUD)
	srl     h
	rr      l               ; HL=BAUD/2
	or      a
	ld      b,$fa           ; B=timing constant
	exx
	ld      c,$fd
	ld      d,$ff
	ld      e,$bf
	ld      b,d
	ld      a,$0e
	out     (c),a           ; select AY register 14
	in      a,(c)           ; get RS232/AUX value
	or      $f0
	and     $fd             ; set CTS low
	ld      b,e
	out     (c),a           ; output CTS low
	ld      h,a             ; save RS232/AUX value with CTS low
x1eb2   ld      b,d
	in      a,(c)           ; get RS232/AUX value
	and     $10
	jr      z,x1ec2         ; move on if TXD was low (ie data ready)
x1eb9   exx
	dec     b               ; decrement timer
	exx
	jr      nz,x1eb2        ; loop back
	xor     a               ; carry reset for no data
	push    af
	jr      x1efb           ; move on if no data received
x1ec2   in      a,(c)
	and     $10
	jr      nz,x1eb9        ; back if TXD high
	in      a,(c)
	and     $10
	jr      nz,x1eb9        ; back if TXD high
	exx
	ld      bc,$fffd
	ld      a,$80           ; A'=char to build (carry will be set when
	ex      af,af'          ; all 8 bits have been read)
x1ed5   add     hl,de
	nop
	nop
	nop
	nop
x1eda   dec     hl
	ld      a,h
	or      l
	jr      nz,x1eda        ; baud rate timing loop
	in      a,(c)           ; get RS232/AUX data
	and     $10             ; mask data bit
	jp      z,x1eef         ; move on if zero
	ex      af,af'
	scf
	rra                     ; rotate a 1 bit in
	jr      c,x1ef8         ; move on if byte complete
	ex      af,af'
	jp      x1ed5           ; loop back for more bits
x1eef   ex      af,af'
	or      a
	rra                     ; rotate a 0 bit in
	jr      c,x1ef8         ; move on if byte complete
	ex      af,af'
	jp      x1ed5           ; loop back for more bits
x1ef8   scf                     ; carry set for data received
	push    af
	exx
x1efb   ld      a,h
	or      $02
	ld      b,e
	out     (c),a           ; set RS232 CTS high
	exx
	ld      h,d
	ld      l,e             ; HL=BAUD
	ld      bc,$0007
	or      a
	sbc     hl,bc
x1f0a   dec     hl
	ld      a,h
	or      l
	jr      nz,x1f0a        ; timing loop
	ld      bc,$fffd
	add     hl,de
	add     hl,de
	add     hl,de
x1f15   in      a,(c)
	and     $10
	jr      z,x1f23         ; move on if TXD low (2nd byte available)
	dec     hl
	ld      a,h
	or      l
	jr      nz,x1f15        ; timing loop
	pop     af              ; restore value
	ei
	ret                     ; exit
x1f23   in      a,(c)
	and     $10
	jr      nz,x1f15        ; move back if TXD high
	in      a,(c)
	and     $10
	jr      nz,x1f15        ; move back if TXD high
	ld      h,d
	ld      l,e
	ld      bc,$0002
	srl     h
	rr      l
	or      a
	sbc     hl,bc
	ld      bc,$fffd
	ld      a,$80           ; prepare 2nd byte in A'
	ex      af,af'
x1f41   nop
	nop
	nop
	nop
	add     hl,de
x1f46   dec     hl
	ld      a,h
	or      l
	jr      nz,x1f46        ; timing loop
	in      a,(c)
	and     $10             ; test bit
	jp      z,x1f5b         ; move on if zero
	ex      af,af'
	scf
	rra                     ; rotate a 1 bit in
	jr      c,x1f64         ; move on if byte complete
	ex      af,af'
	jp      x1f41           ; back for more bits
x1f5b   ex      af,af'
	or      a
	rra                     ; rotate a 0 bit in
	jr      c,x1f64         ; move on if byte complete
	ex      af,af'
	jp      x1f41           ; back for more bits
x1f64   ld      hl,SERFL
	ld      (hl),$01        ; flag "2nd byte available"
	inc     hl
	ld      (hl),a          ; store 2nd byte
	pop     af              ; restore the 1st byte
	ei
	ret                     ; done
      ENDIF

; Printer output channel routine

m1f6e   push    hl
	ld      hl,FLAGS3
	bit     2,(hl)
	pop     hl
	jp      z,m2051         ; go to output if pure binary channel
	push    af
	ld      a,(TVPARS)
	or      a
	jr      z,m1f8e         ; move on if no inline parameters expected
	dec     a
	ld      (TVPARS),a      ; decrement # parameters
	jr      nz,m1f89        ; move on if more still needed
	pop     af
	jp      m2020           ; move on
m1f89   pop     af
	ld      (TVDATA+1),a    ; save first parameter & exit
	ret
m1f8e   pop     af
	cp      $a3
	jr      c,m1fa0         ; move on unless BASIC token
	ld      hl,(RETADDR)
	push    hl              ; save RETADDR
	rst     $28
	DW      $0b52           ; output tokens using ROM 3
	pop     hl
	ld      (RETADDR),hl    ; restore RETADDR
	scf
	ret
m1fa0   ld      hl,FLAGS
	res     0,(hl)          ; reset "outputting space" flag
	cp      ' '
	jr      nz,m1fab
	set     0,(hl)          ; set "outputting space" flag
m1fab   cp      $7f
	jr      c,m1fb1
	ld      a,'?'           ; substitute ? for graphics
m1fb1   cp      ' '
	jr      c,m1fcc         ; move on for control codes
	push    af
	ld      hl,COL
	inc     (hl)            ; increment column
	ld      a,(WIDTH)
	cp      (hl)
	jr      nc,m1fc8        ; if within width, move on to print
	call    m1fd0           ; output CRLF
	ld      a,$01
	ld      (COL),a         ; set first column
m1fc8   pop     af
	jp      m2051           ; output character
m1fcc   cp      $0d
	jr      nz,m1fde        ; move on unless CR
m1fd0   xor     a
	ld      (COL),a         ; reset column counter
	ld      a,$0d
	call    m2051           ; output CRLF
	ld      a,$0a
	jp      m2051           ; & exit
m1fde   cp      $06
	jr      nz,m2001        ; move on unless PRINT comma
	ld      bc,(COL)        ; B=WIDTH, C=COL
	ld      e,$00
m1fe8   inc     e               ; increment COL & E
	inc     c
	ld      a,c
	cp      b
	jr      z,m1ff6         ; if end of line, go to do E spaces
m1fee   sub     $08
	jr      z,m1ff6         ; or if at a tab stop
	jr      nc,m1fee
	jr      m1fe8           ; loop back until reach a tab stop or eol
m1ff6   push    de
	ld      a,' '
	call    m1f6e           ; output a space
	pop     de
	dec     e
	ret     z
	jr      m1ff6           ; loop back for more
m2001   cp      $16
	jr      z,m200e         ; move on for AT (2 inline codes)
	cp      $17
      IF garry
	jr      z,m2017
      ELSE
	jr      z,m200e         ; move on for TAB (*BUG* 2 inline codes)
      ENDIF
	cp      $10
	ret     c               ; exit for codes 0-15
	jr      m2017           ; move on for colour codes (1 inline code)
m200e   ld      (TVDATA),a      ; store control code
	ld      a,$02
	ld      (TVPARS),a      ; & number of codes required
	ret
m2017   ld      (TVDATA),a      ; store control code
      IF garry
	ld      a,$01
      ELSE
	ld      a,$02           ; *BUG* should be 1
      ENDIF
	ld      (TVPARS),a      ; & number of codes required
	ret

; Here, we deal with inline parameters

m2020   ld      d,a             ; save last parameter
	ld      a,(TVDATA)      ; get control code
	cp      $16
	jr      z,m2030         ; move on for AT
	cp      $17
	ccf
	ret     nz              ; ignore other codes except TAB
	ld      a,(TVDATA+1)    ; use first parameter as column
	ld      d,a
m2030   ld      a,(WIDTH)       ; get width
	cp      d
	jr      z,m2038
	jr      nc,m203e
m2038   ld      b,a
	ld      a,d
	sub     b               ; reduce column by width until in range
	ld      d,a
	jr      m2030
m203e   ld      a,d
	or      a
	jp      z,m1fd0         ; for column 0, do CRLF
m2043   ld      a,(COL)
	cp      d
	ret     z               ; exit if at right column
	push    de
	ld      a,' '
	call    m1f6e           ; output a space
	pop     de
	jr      m2043           ; loop back

; Subroutine to output a character to the printer (Centronics or RS232)

m2051   push    hl
	ld      hl,FLAGS3
	bit     3,(hl)
	pop     hl
	jp      z,m20a8         ; move on if print output is centronics

      IF v41
	ld      hl,FLAGS2
	bit     6,(hl)
	jr      z,m205c
	jp      x216d
      ENDIF

m205c   push    af              ; save character
	ld      c,$fd
	ld      d,$ff
	ld      e,$bf
	ld      b,d
	ld      a,$0e
	out     (c),a           ; select AY register 14
m2067   call    m2af9           ; test for BREAK
	in      a,(c)           ; read RS232/AUX
	and     $40
	jr      nz,m2067        ; loop until DTR low
	ld      hl,(BAUD)
	ld      de,$0002
	or      a
	sbc     hl,de
	ex      de,hl           ; DE=BAUD-2
	pop     af              ; restore character
	cpl                     ; invert it
	scf                     ; set carry for initial bit
	ld      b,$0b           ; 11 bits to output
m207f   di                      ; disable interrupts
m2080   push    bc              ; save registers
	push    af
	ld      a,$fe
	ld      h,d             ; HL=BAUD-2
	ld      l,e
	ld      bc,$bffd
	jp      nc,m2092        ; move on to output a one bit
	and     $f7             ; mask RS232 RXD off
	out     (c),a           ; output zero bit to RS232
	jr      m2098
m2092   or      $08             ; set RS232 RXD on
	out     (c),a           ; output one bit to RS232
	jr      m2098
m2098   dec     hl
	ld      a,h
	or      l
	jr      nz,m2098        ; timing loop for baud rate
	nop                     ; more timing values
	nop
	nop
	pop     af              ; restore registers
	pop     bc
	or      a               ; clear carry (for stop bits)
	rra                     ; rotate next bit
	djnz    m2080           ; loop back for more
	ei
	ret

    IF v41
x216d   push    af
	ld      c,$fd
	ld      d,$ff
	ld      e,$bf
	ld      b,d
	ld      a,$0e
	out     (c),a
x2179   call    m2af9
	in      a,(c)
	and     $20
	jr      nz,x2179
	ld      hl,(BAUD)
	ld      de,$0002
	or      a
	sbc     hl,de
	ex      de,hl
	pop     af
	cpl
	scf
	ld      b,$0b
	di
x2192   push    bc
	push    af
	ld      a,$fe
	ld      h,d
	ld      l,e
	ld      bc,$bffd
	jp      nc,x21a4
	and     $fe
	out     (c),a
	jr      x21aa
x21a4   or      $01
	out     (c),a
	jr      x21aa
x21aa   dec     hl
	ld      a,h
	or      l
	jr      nz,x21aa
	nop
	nop
	nop
	pop     af
	pop     bc
	or      a
	rra
	djnz    x2192
	ei
	ret
m20a8   push    af              ; save character
	ld      bc,$1ffd
	ld      a,(BANK678)
	set     4,a
	di
	out     (c),a           ; output strobe
	ld      (BANK678),a
	ei
x2217   call    m2af9
	ld      bc,$0ffd
	in      a,(c)
	bit     0,a
	jr      nz,x2217        ; wait for printer ready
	pop     af
	out     (c),a           ; output strobe
	di
	ld      bc,$1ffd
	ld      a,(BANK678)
	res     4,a
	out     (c),a           ; output strobe
	set     4,a
	out     (c),a           ; output strobe
	ld      (BANK678),a
	ei
	scf
	ret
    ELSE
m20a8   push    af              ; save character
m20a9   call    m2af9           ; test for BREAK
	ld      bc,$0ffd
	in      a,(c)
	bit     0,a
	jr      nz,m20a9        ; wait for printer ready
	pop     af
	out     (c),a           ; output character
	di
m20b9   ld      bc,$1ffd
	ld      a,(BANK678)
      IF garry
	and     $ef
	out     (c),a           ; output strobe
	or      $10
	ld      (BANK678),a
	out     (c),a           ; output strobe
      ELSE
	xor     $10             ; toggle strobe bit
	ld      (BANK678),a
	out     (c),a           ; output strobe
	bit     4,a
	jr      z,m20b9         ; loop back to finish with strobe high
      ENDIF
	ei
	scf
	ret
	ret
    ENDIF

; The COPY (to printer) command routine

m20ce   bit     7,(iy+$01)
	ret     z               ; exit if syntax-checking
	ld      a,(BANK678)
	ld      bc,$1ffd
	set     4,a
	di                      ; disable interrupts
	ld      (BANK678),a
	out     (c),a           ; set strobe high
	ei
	ld      hl,YLOC
	ld      (hl),$2b        ; set Y location to 43 (4 bits on each row)
m20e7   ld      hl,m216b
	call    m2151           ; output 120dpi line command
	call    m2107           ; output raster image
	ld      hl,m2172
	call    m2151           ; output linefeed
	ld      hl,YLOC
	xor     a
	cp      (hl)
	jr      z,m2100
	dec     (hl)
	jr      m20e7           ; loop back for more lines
m2100   ld      hl,m2174
	call    m2151           ; reset linespacing and done
	ret

; Subroutine to output a raster line for a non-expanded copy

m2107   ld      hl,XLOC
	ld      (hl),$ff        ; set XLOC
m210c   call    m2118           ; output a pixel's width
	ld      hl,XLOC
	xor     a
	cp      (hl)
	ret     z               ; exit if done line
	dec     (hl)
	jr      m210c           ; back for next pixel width

; Subroutine to output a pixel's width of a non-expanded copy

m2118   ld      de,$c000        ; D=pixel position mask
	ld      bc,(XLOC)       ; C=x-position, B=y-position
	scf
	rl      b
	scf
	rl      b               ; B=top row of required bits
	ld      a,c
	cpl
	ld      c,a             ; start at left of screen
	xor     a               ; initialise raster byte
	push    af
	push    de
	push    bc
m212c   call    m215f           ; get pixel state
	pop     bc
	pop     de
	ld      e,$00
	jr      z,m2136         ; set E=0 for no pixel
	ld      e,d             ; or E=mask for pixel
m2136   pop     af
	or      e               ; combine pixel into raster byte
	push    af
	dec     b               ; decrement Y position
	srl     d
	srl     d               ; shift mask right twice (4 pixels per row)
	push    de
	push    bc
	jr      nc,m212c        ; loop back if more pixels to get
	pop     bc
	pop     de
	pop     af
	ld      b,$03
m2147   push    bc
	push    af
	call    m2051           ; output raster byte
	pop     af
	pop     bc
	djnz    m2147           ; loop back for 3 passes
	ret

; Subroutine to output a counted string at HL to the printer

m2151   ld      b,(hl)          ; get count
	inc     hl
m2153   ld      a,(hl)          ; get next char
	push    hl
	push    bc
	call    m2051           ; output char to printer
	pop     bc
	pop     hl
	inc     hl
	djnz    m2153           ; loop back for more
	ret

; Subroutine to check pixel at B=y,C=x
; On exit, Z is reset if pixel is ink, set if pixel is paper

m215f   rst     $28
	DW      $22aa           ; get address of pixel in HL
	ld      b,a
	inc     b               ; B=counter to get required pixel
	xor     a               ; zero A
	scf                     ; set carry
m2166   rra                     ; rotate bit into position
m2167   djnz    m2166
	and     (hl)            ; mask against screen byte
	ret

; The line header for a non-expanded copy

m216b   defb    $06             ; 6 bytes
	defb    $1b,'1'         ; select 7/72" linespacing
	defb    $1b,'L',$00,$03 ; print 768 dots in 120dpi

; The line terminator for a non-expanded copy

m2172   defb    $01             ; 1 byte
	defb    $0a             ; linefeed

; The terminator for a non-expanded copy

m2174   defb    $02             ; 2 bytes
	defb    $1b,'2'         ; select 1/6" linespacing

; The PRINT & LPRINT commands (enter at m2177 for LPRINT, m217b for PRINT)

m2177   ld      a,$03           ; use stream 3 for LPRINT
	jr      m217d
m217b   ld      a,$02           ; use stream 2 for PRINT
m217d   rst     $28
	DW      $2530           ; are we syntax-checking?
	jr      z,m2185
	rst     $28
	DW      $1601           ; open channel if not
m2185   rst     $28
	DW      $0d4d           ; set temporary colours
	rst     $28
	DW      $1fdf           ; use ROM 3 for command routine
	call    m10b1           ; check for end-of-statement
	ret

; The INPUT command

m218f   rst     $28
	DW      $2530
	jr      z,m219c         ; move on if syntax-checking
	ld      a,$01
m2196   rst     $28
	DW      $1601           ; open channel to stream 1
	rst     $28
	DW      $0d6e           ; clear lower screen
m219c   ld      (iy+$02),$01    ; set DF_SZ to 1
	rst     $28
	DW      $20c1           ; deal with the input items
	call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $20a0           ; use ROM 3 for actual routine
	ret

; The COPY command

m21aa   rst     $18
	cp      $0d
	jp      z,m20ce         ; go to do printer copy if end of line
	cp      ':'
	jp      z,m20ce         ; or end of statement
	cp      $b9
	jp      z,m3328         ; go to do expanded copy if COPY EXP
      IF garry
	defs    5
      ELSE
	cp      $f9
	jp      z,m35c4         ; move on if COPY RANDOMIZE
      ENDIF
	rst     $28
	DW      $1c8c           ; get a string expression
	rst     $28
	DW      $0018           ; get character
m21c5   cp      $cc
	jr      z,m21cd         ; move on if found TO
	call    m2ada
	defb    $0b             ; error C - nonsense in BASIC
m21cd   rst     $28
	DW      $0020           ; get next char
m21d0   cp      $aa
	jp      z,m2237         ; move on if COPY f$ TO SCREEN$
	cp      $a3
	jp      z,m2257         ; move on if COPY f$ TO SPECTRUM FORMAT
	cp      $e0
	jp      z,m2237         ; move on if COPY f$ TO LPRINT
	rst     $28
	DW      $1c8c           ; get a string expression
	call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2bf1           ; fetch last value from calculator stack
	ld      a,b
	or      c               ; check length of second string
	jr      nz,m21f0
	call    m2ada
	defb    $2c             ; error "Bad filename"
m21f0   inc     de
	ld      a,(de)          ; check 2nd char of 2nd string
	dec     de
	cp      ':'
	jr      nz,m21fb        ; move on if not specifying a drive
	ld      a,(de)
	and     $df             ; convert drive letter to uppercase
	ld      (de),a
m21fb   ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy 2nd string to page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; terminate 2nd filename with $ff
	inc     de              ; increment pointer after 2nd filename
	call    m2b64           ; page in normal memory
	push    de              ; save pointer
	rst     $28
	DW      $2bf1           ; fetch value from calculator stack
	ld      a,b
	or      c
	jr      nz,m2218        ; check length of first string
	call    m2ada
	defb    $2c             ; error "Bad filename"
m2218   inc     de
	ld      a,(de)          ; check 2nd char of first string
	dec     de
	cp      ':'
	jr      nz,m2223        ; move on if not specifying a drive
	ld      a,(de)
	and     $df             ; convert drive letter to uppercase
	ld      (de),a
m2223   pop     hl              ; restore address in page 7
	ex      de,hl
	call    m3f63           ; copy 1st filename to page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; terminate 1st filename with $ff
	call    m2b64           ; page in normal memory
	xor     a               ; signal "copying to a file"
	scf
	call    m2ba3           ; do the copy
	ret

; The COPY...TO SCREEN$/LPRINT commands

m2237   push    af              ; save keyword
	rst     $28
	DW      $0020           ; get next char
m223b   call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2bf1           ; get string
	ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy into page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
	call    m2b64           ; page in normal memory
	pop     af              ; restore keyword as destination flag
	and     a               ; reset Z flag
	call    m2ba3           ; copy the file
	ret

; The COPY....TO SPECTRUM FORMAT command

m2257   rst     $28
	DW      $0020           ; get next char
m225a   cp      $d0             ; check for FORMAT
	jr      z,m2262
	call    m2ada
	defb    $0b             ; nonsense in BASIC if not
m2262   rst     $28
	DW      $0020           ; get to next char
m2265   call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2bf1           ; get string
	ld      hl,tmp_fspec
	ex      de,hl
	call    m3f63           ; copy into page 7
	call    m2b89           ; page in DOS workspace
	ld      a,$ff
	ld      (de),a          ; add terminator
	call    m2b64           ; page in normal memory
	xor     a
	call    m2ba3           ; copy the file
	ret

; The NEW command

m2280   di
	call    m3e80
	DW      $01b0           ; use routine in ROM 0

; The CIRCLE command

m2286   rst     $18             ; get current char
	cp      ','
	jr      nz,m22c3        ; error C if not comma
	rst     $20             ; get next char
	rst     $28
	DW      $1c82           ; get numeric expression
	call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $232d           ; use ROM 3 for actual routine
	ret

; The DRAW command

m2296   rst     $18             ; get current char
	cp      ','
	jr      z,m22a2         ; move on if comma
	call    m10b1           ; check for end-of-statement
	rst     $28
	DW      $2477           ; use ROM 3 to draw line
	ret
m22a2   rst     $20             ; get next char
	rst     $28
	DW      $1c82           ; get numeric expression
	call    m10b1           ; check for end of statement
	rst     $28
	DW      $2394           ; use ROM 3 to draw curve
	ret

; The DIM command

m22ad   rst     $28
	DW      $28b2           ; search variables area
	jr      nz,m22c3        ; move on if error
	rst     $28
	DW      $2530
	jr      nz,m22bf        ; move on if runtime
	res     6,c             ; test string syntax as if numeric
	rst     $28
	DW      $2996           ; check syntax of parenthesised expression
	call    m10b1           ; check for end-of-statement
m22bf   rst     $28
	DW      $2c15           ; use ROM 3 for actual command
	ret
m22c3   call    m2ada
	defb    $0b             ; error C - nonsense in BASIC


; Subroutine to clear whole display unless unnecessary

m22c7   bit     0,(iy+$30)      ; check FLAGS2
	ret     z               ; exit if not necessay
	rst     $28
	DW      $0daf           ; cls
	ret

; Subroutine to evaluate an expression for the calculator, & set the
; result to be used by the next calculation

m22d0   ld      hl,$fffe
	ld      (PPC),hl        ; set statement -2
	res     7,(iy+$01)      ; signal "syntax checking"
	call    m2368           ; set interpreter to start of line
	rst     $28
	DW      $24fb           ; evaluate an expression
	bit     6,(iy+$01)
	jr      z,m2312         ; move on if value not numeric
	rst     $18
	cp      $0d
	jr      nz,m2312        ; or if next character isn't end-of-line
	set     7,(iy+$01)      ; signal "executing"
	call    m2368           ; set interpreter to start of line
	ld      hl,m25cb
	ld      (SYNRET),hl     ; set up error return address
	rst     $28
	DW      $24fb           ; evaluate an expression
	bit     6,(iy+$01)
	jr      z,m2312         ; move on if value not numeric
	ld      de,LASTV
	ld      hl,(STKEND)
	ld      bc,$0005
	or      a
	sbc     hl,bc
	ldir                    ; copy result into LASTV variable
	jp      m2316
m2312   call    m2ada
	defb    25              ; error Q - parameter error
m2316   ld      a,$0d
	call    m2347           ; do a newline
	ld      bc,$0001
	rst     $28
	DW      $0030           ; make a byte in the workspace
m2321   ld      (K_CUR),hl      ; save address of cursor
	push    hl
	ld      hl,(CURCHL)     ; get address of current channel information
	push    hl
	ld      a,$ff
	rst     $28
	DW      $1601           ; open channel to stream -1
	rst     $28
	DW      $2de3           ; print the result value
	pop     hl
	rst     $28
	DW      $1615           ; restore old channel
	pop     de
	ld      hl,(K_CUR)      ; get new cursor address
	and     a
	sbc     hl,de           ; HL=# of result chars
m233c   ld      a,(de)
	call    m2347           ; "type" each result character
	inc     de
	dec     hl
	ld      a,h
	or      l
	jr      nz,m233c        ; loop back for more
	ret

; Subroutine to "do" a key (A) using ROM 0's editing keys

m2347   push    hl
	push    de
	call    m2b89           ; page in DOS workspace
	ld      hl,ed_flags
	res     3,(hl)          ; ???
	push    af
	ld      a,$02
	rst     $28
	DW      $1601           ; open channel to stream 2
	pop     af
	call    m3e80
      IF v41
	DW      $071b           ; "do" the key
      ELSE
	DW      $0716           ; "do" the key
      ENDIF
	ld      hl,ed_flags
	res     3,(hl)          ; ???
	call    m2b64           ; page in normal memory
	pop     de
	pop     hl
	ret

; Subroutine to set interpreter to entered line, with A=first char

m2368   ld      hl,(E_LINE)
	dec     hl
	ld      (CH_ADD),hl     ; CH_ADD=E_LINE-1
	rst     $20             ; get next char
	ret

; Subroutine to determine if line is a single LET statement (Z set if so)

m2371   call    m2368           ; get first char in E_LINE
	cp      $f1
	ret     nz              ; exit unless LET
	ld      hl,(CH_ADD)
m237a   ld      a,(hl)
	inc     hl
	cp      $0d
	ret     z               ; exit when end of line found (with Z set)
	cp      ':'
	jr      nz,m237a
	or      a
	ret                     ; or when end of statement found (with Z reset)

; Subroutine to check if character is a binary operator (Z set if so)

m2385   ld      b,a             ; save char
	ld      hl,m2397        ; list of operators
m2389   ld      a,(hl)          ; get next
	inc     hl
	or      a
	jr      z,m2393         ; if end of list, exit with Z reset
	cp      b
	jr      nz,m2389        ; loop back if no match
	ld      a,b             ; restore char
	ret                     ; exit with Z set
m2393   or      $ff             ; reset Z
	ld      a,b             ; restore character
	ret

; List of valid binary operators for numeric calculations

m2397   defm    "+-*/^=><"
	defb    $c7,$c8,$c9     ; <=,>=,<>
	defb    $c5,$c6         ; OR,AND
	defb    0

; Subroutine to check if a character is a valid function (Z set if so)

m23a5   cp      $a5
	jr      c,m23b7         ; move on if before RND
	cp      $c4
	jr      nc,m23b7        ; or after NOT
	cp      $ac
	jr      z,m23b7         ; or if AT
	cp      $ad
	jr      z,m23b7         ; or if TAB
	cp      a               ; set Z for valid functions
	ret
m23b7   cp      $a5             ; reset Z
	ret

; Subroutine to check if character is start of a value

m23ba   ld      b,a             ; save character
	or      $20             ; make lowercase
	cp      'a'
	jr      c,m23c7
	cp      'z'+1
	jr      nc,m23c7
	cp      a               ; set Z if character is a letter
	ret
m23c7   ld      a,b
	cp      '.'
	ret     z               ; exit with Z set if "."
	call    m23e4           ; check for digits
	jr      nz,m23e1        ; if not, junk character & exit
m23d0   rst     $20             ; get next char
	call    m23e4
	jr      z,m23d0         ; loop back while still digits
	cp      '.'
	ret     z               ; exit with Z set if "."
	cp      'E'
	ret     z               ; or "E"
	cp      'e'
	ret     z               ; or "e"
	jr      m2385           ; else check for a binary operator
m23e1   or      $ff             ; junk character & reset Z
	ret

; Subroutine to check if A is a digit (Z set if so)

m23e4   cp      '0'
	jr      c,m23ee
	cp      '9'+1
	jr      nc,m23ee
	cp      a               ; set Z if char is a digit
	ret
m23ee   cp      '0'             ; reset Z otherwise
	ret

; The PLAY command

m23f1   ld      b,$00           ; string counter
	rst     $18             ; get char
m23f4   push    bc
	rst     $28
	DW      $1c8c           ; get a string
	pop     bc
	inc     b               ; increment counter
	cp      ','
	jr      nz,m2401        ; move on if no more strings
	rst     $20             ; get next char
	jr      m23f4           ; loop back
m2401   ld      a,b
	cp      $09
	jr      c,m240a         ; up to 9 strings allowed
	call    m2ada
	defb    $2b             ; error "Too many tied notes"
m240a   call    m10b1           ; check for end-of-statement
	jp      m1571           ; go to execute command

; Subroutine called from ROM 0 to initialise DOS & check the status
; of drives on the system, displaying information to the user.

; aca inicializa +3dos
m2410   call    m2b89           ; page in DOS workspace (page 7)
      IF garry
	call    m32b6
	ld      hl,FLAGS3
	bit     7,(hl)
	jr      nz,m2427        ; move on if DOS already initialised
	set     7,(hl)          ; signal "DOS initialised"
	call    m3f00
	DW      $00a3
	ld      ($df9d), a
m2427   call    m3f00
	DW      $0100
	call    m32ee

	IF alternative

	ld	a,16		; colores para linea 2 de la barra de
	rst	$10		; estado, no imprimo ningún msg tipo "physical
	ld	a,definklin_lw	; drives", no tiene sentido para mi por que
	rst	$10		; es obvio lo que significan las letras

	ELSE

	ld      hl, m368c
	call    m24b5

	ENDIF

m242a   ld      a,$ff
	ld      hl, $0002       ; standard ALERT routine in ROM 2
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_MESSAGE
	call    m32ee           ; restore TSTACK
	ld      hl,FLAGS3
	res     4,(hl)          ; signal "disk interface not present"
      ELSE
	ld      hl,FLAGS3
	bit     7,(hl)
	jr      nz,m242a        ; move on if DOS already initialised
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_INITIALISE
	call    m32ee           ; restore TSTACK
	ld      hl,FLAGS3
	set     7,(hl)          ; signal "DOS initialised"
m242a   ld      a,$ff
	ld      hl,$244e        ; standard ALERT routine in ROM 2
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_MESSAGE
	call    m32ee           ; restore TSTACK
	call    m2b64           ; page in page 0
	ld      hl,m24be
	call    m24b5           ; display "Drive"
	ld      hl,FLAGS3
	res     4,(hl)          ; signal "disk interface not present"
	call    m2b89           ; page in DOS workspace
      ENDIF
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DD_INTERFACE
	call    m32ee           ; restore TSTACK
      IF garry
	ld      a, $30
	jr      nc, m2488
      ELSE
	call    m2b64           ; page in page 0
	jr      c,m2463         ; move on if interface present
	ld      hl,m24c4
	call    m24b5           ; display " M:" if no interface
	jr      m24ae           ; move on
m2463   ld      a,'A'           ; set "A:" as default load/save drive
	ld      (LODDRV),a
	ld      (SAVDRV),a
      ENDIF
	ld      hl,FLAGS3
	set     4,(hl)          ; signal "disk interface present"
	res     5,(hl)          ; signal "no drive B:"
      IF garry=0
	call    m2b89           ; page in DOS workspace
      ENDIF
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DD_ASK_1
	call    m32ee           ; restore TSTACK
      IF garry=0
	call    m2b64           ; page in page 0
      ENDIF
	jr      c,m24a3         ; move on if drive B exists
	ld      c,$00
      IF garry
	ld      hl, 5
      ELSE
	ld      hl, n2455
	call    m2b89           ; page in DOS workspace
      ENDIF
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_MAP_B       ; map drive B: to unit 0
	call    m32ee           ; restore TSTACK

	IF garry
		ld      a, $31
		jr      m2488
	ELSE
		call    m2b64           ; page in page 0
		ld      hl,m24c8
		call    m24b5           ; display " A:"
		jr      m24ae           ; move on
	ENDIF

m24a3   ld      hl,FLAGS3
	set     5,(hl)          ; signal "drive B: present"

	IF garry

		ld      a, $32
m2488		rst     $10
		ld      hl, m369e
		call    m24b5
		ld      a, ($df9d)
		add     a, $30
		rst     $10
		ld      hl, m36a8
		call    m24b5
		ld      hl, $e2a0
		ld      c, $41
		ld      b, $10
m24a2   	ld      a, (hl)
		inc     hl
		or      (hl)
		inc     hl
		jr      z, m24b9
		ld      a, (LODDRV)
		cp      c
		ld      e, 0
		jr      nz, m24b2
		ld      e, 1

m24b2

		IF alternative
   		ld      a, $14		; prefiero video inverso para
					; mostrar la letra de la unidad
					; por defecto por que en un +3
					; real se ve mejor por composite
		ELSE
		ld      a, $13		; video bright
		ENDIF

		rst     $10

		ld      a, e
		rst     $10
		ld      a, c
		rst     $10

m24b9   	inc     c
		djnz    m24a2

		IF alternative
			call	rest_ink
		ELSE	
			call    m2b64           ; page in normal memory
		ENDIF

		ret

	ELSE

		ld	hl,m24d4
		call    m24b5           ; display "s A: and B:"
m24ae   	ld      hl,m24e4
		call    m24b5           ; display " available."
		ret
	
	ENDIF

; Subroutine to print a zero-terminated string

m24b5   ld      a,(hl)          ; get next char
	or      a
	ret     z               ; exit if zero

	IF garry
		rst     $10
	ELSE
		rst     $28
		DW      $0010	; print it
	ENDIF

	inc     hl
	jr      m24b5           ; back for more

; Drives present messages
    IF garry
n24c7	cp      $b5
	jr      nz, m24cc
	rst     $20
m24cc   call    m10b1
	jp      m0a96
	defs    30
    ELSE
      IF spanish
m24be   defm    "Unidades disponibles:  ", 0
m24c4   defm    "M", 0
m24c8   defm    "A y M", 0
m24d4   defm    "A, B y M", 0
m24e4   defm    ".", 0
      ELSE
m24be   defm    "Drive", 0
m24c4   defm    " M:", 0
m24c8   defm    "s A: and M:", 0
m24d4   defm    "s A:, B: and M:", 0
m24e4   defm    " available.", 0
      ENDIF
    ENDIF
; Subroutine used to execute a command line or evaluate an expression

m24f0   ld      (iy+$00),$ff    ; clear error
	ld      (iy+$31),$02    ; set lower screen to 2 lines
	ld      hl,ONERR
	push    hl
	ld      (ERR_SP),sp     ; set up error stack
	ld      hl,m2560
	ld      (SYNRET),hl     ; set error return address
	call    m2368           ; set interpretation address & get first char
	call    m23a5           ; test for function
m250c   jp      z,m22d0         ; if so, evaluate the expression
	cp      '('
	jp      z,m22d0         ; or if bracketed expression
	cp      '-'
	jp      z,m22d0         ; or a unary operator (+ or -)
	cp      '+'
	jp      z,m22d0
	call    m23ba           ; check for start of a value (var or number)
	jp      z,m22d0         ; if so, evaluate the expression
	call    m2b89           ; page in DOS workspace
	ld      a,(process)     ; get current process
	call    m2b64           ; page in normal memory
	cp      $04             ; is it the calculator?
m252f   jp      nz,m0fbf        ; if not, execute it
	call    m2371           ; is line a single LET statement?
	jp      z,m0fbf         ; if so, execute it
	pop     hl              ; unstack ONERR address
	ret                     ; exit

m253a
      IF v41
	ei
      ENDIF
; The +3-specific error-handling routine
; ONERR jumps here

	call    m2b89           ; page in DOS workspace
	ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file 0
	call    m32ee           ; restore TSTACK
	jr      c,m2559         ; move on if no error
	ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_ABANDON     ; abandon file 0
	call    m32ee           ; restore TSTACK
m2559   call    m2b64           ; page back normal memory
	ld      hl,(SYNRET)
	jp      (hl)            ; return to syntax return address

; This is one of the syntax return addresses, used when entering a line
; as a direct command

m2560   bit     7,(iy+$00)
	jr      nz,m2567
	ret                     ; exit if error in line
m2567   ld      hl,(E_LINE)
	ld      (CH_ADD),hl     ; reset CH_ADD to editing line
	rst     $28
	DW      $19fb           ; get line number of editing line
	ld      a,b
	or      c
	jp      nz,m268e        ; move on if it exists, to add to program
	rst     $18             ; get character
	cp      $0d
	ret     z               ; exit if empty line
	call    m22c7           ; clear display if necessary
	bit     6,(iy+$02)
	jr      nz,m2585
	rst     $28
	DW      $0d6e           ; clear lower screen if necessary
m2585   res     6,(iy+$02)      ; signal "lower screen clear"
	call    m2b89           ; page in DOS workspace
	ld      hl,ed_flags
	bit     6,(hl)          ; ???
	jr      nz,m259e
	inc     hl
	ld      a,(hl)          ; ???
	cp      $00
	jr      nz,m259e
	call    m3e80
	DW      l1a8e
m259e   call    m2b64           ; page in normal memory
	ld      hl,TV_FLAG
	res     3,(hl)          ; signal "mode hasn't changed"
	ld      a,$19
	sub     (iy+$4f)
	ld      (SCR_CT),a      ; set appropriate scroll count
	set     7,(iy+$01)      ; signal "execution"
	ld      (iy+$0a),$01    ; jump to statement 1
	ld      hl, n3e00
	push    hl              ; stack GOSUB stack end marker
	ld      hl,ONERR
	push    hl              ; stack error address
	ld      (ERR_SP),sp     ; reset ERR_SP
	ld      hl,m25cb
	ld      (SYNRET),hl     ; store execution error handler address
	jp      m1048           ; execute immediate command

; This is one of the syntax return addresses, used during execution

m25cb   ld      sp,(RAMTOP)
	inc     sp              ; clear return stack
	ld      hl,TSTACK
	ld      (OLDSP),hl      ; set OLDSP to temporary stack area
      IF garry
	nop
      ELSE
	halt                    ; wait for an interrupt
      ENDIF
	res     5,(iy+$01)      ; signal no key available
	ld      a,(ERR_NR)
	inc     a               ; A=error code
m25df   push    af              ; save error code
	ld      hl,$0000
	ld      (iy+$37),h      ; clear FLAGX
	ld      (iy+$26),h      ; clear high byte of X_PTR
	ld      (DEFADD),hl     ; clear DEFADD
	ld      hl,$0001
	ld      (STRMS+6),hl    ; reset stream 0
	rst     $28
	DW      $16b0           ; clear editing areas and calculator etc
	res     5,(iy+$37)      ; ???
	rst     $28
	DW      $0d6e           ; clear lower screen
	set     5,(iy+$02)      ; signal "clear lower screen after keystroke"
	pop     af              ; get back error code
	ld      b,a             ; save it
	cp      $0a
	jr      c,m2614         ; move on if 0-9
	cp      $1d
	jr      c,m2612         ; move on if A-R
	cp      $2c
	jr      nc,m261a        ; move on if +3DOS error
	add     a,$14           ; else convert for errors a-o
	jr      m2614
m2612   add     a,$07           ; convert to code to letter
m2614   rst     $28
	DW      $15ef           ; output error character (0-9 or A-R or a-o)
	ld      a,' '
	rst     $10             ; output space
m261a   ld      a,b             ; get back error code
	cp      $1d
	jr      c,m2631         ; move on if old 48K Spectrum error
	sub     $1d
	ld      b,$00
	ld      c,a
	ld      hl,m2705
	add     hl,bc
	add     hl,bc           ; HL points to address of error message
	ld      e,(hl)
	inc     hl
	ld      d,(hl)          ; DE=error message address
	call    m2ace           ; output it
	jr      m2637
m2631   ld      de,$1391        ; base address of ROM 3 message table
	rst     $28
	DW      $0c0a           ; output 48K Spectrum error message
m2637   xor     a
	ld      de,$1536
	rst     $28
	DW      $0c0a           ; output "comma" message
	ld      bc,(PPC)        ; get error line number
	rst     $28
	DW      $1a1b           ; output it
	ld      a,':'
	rst     $10             ; output ":"
	ld      c,(iy+$0d)      ; get error statement number
	ld      b,$00
	rst     $28
	DW      $1a1b           ; output it
	rst     $28
	DW      $1097           ; clear editing area/workspace
	ld      a,(ERR_NR)
	inc     a
	jr      z,m2674         ; move on if error "OK"
	cp      $09
	jr      z,m2661         ; move on if error "9 - STOP statement"
	cp      $15
	jr      nz,m2664        ; move on if not "L - BREAK into program"
m2661   inc     (iy+$0d)        ; increment statement for CONTINUE
m2664   ld      bc,$0003
	ld      de,OSPCC
	ld      hl,NSPPC
	bit     7,(hl)
	jr      z,m2672
	add     hl,bc
m2672   lddr                    ; copy line/statement to CONTINUE at
m2674   ld      (iy+$0a),$ff    ; clear NSPPC
	res     3,(iy+$01)      ; signal "K" mode
	ld      hl,FLAGS3
	res     0,(hl)          ; ???
	call    m3e80
	DW      l067b
m2686   ld      a,$10           ; error G - no room for line
	ld      bc,$0000
	jp      m25df           ; loop back

; Routine to ???

m268e   ld      (E_PPC),bc      ; ???
	call    m2b89           ; page in DOS workspace
	ld      a,b
	or      c
	jr      z,m26a1
	ld      (E_PPC),bc      ; ???
	ld      ($ec08),bc
m26a1   call    m2b64           ; page in normal memory
	ld      hl,(CH_ADD)
	ex      de,hl
	ld      hl,m2686        ; error return address (no room)
	push    hl
	ld      hl,(WORKSP)
	scf
	sbc     hl,de
	push    hl              ; HL=line length
	ld      h,b
	ld      l,c
	rst     $28
	DW      $196e           ; get address of line in program
	jr      nz,m26c0        ; if line not in program yet, move on
	rst     $28
	DW      $19b8           ; get address of next line
	rst     $28
	DW      $19e8           ; delete the existing line
m26c0   pop     bc              ; restore line length
	ld      a,c
	dec     a
	or      b
	jr      nz,m26db        ; move on if no line body (just deleting)
	call    m2b89           ; page in DOS workspace
	push    hl
	ld      hl,(E_PPC)
	call    m3e80
	DW      l1418
	ld      (E_PPC),hl
	pop     hl
	call    m2b64           ; page in normal memory
	jr      m2703
m26db   push    bc
	inc     bc
	inc     bc
	inc     bc
	inc     bc
	dec     hl
	ld      de,(PROG)
	push    de
m26e6   rst     $28
	DW      $1655           ; make space for ???
	pop     hl
m26ea   ld      (PROG),hl
	pop     bc
	push    bc
	inc     de
	ld      hl,(WORKSP)
	dec     hl
	dec     hl
	lddr
	ld      hl,(E_PPC)
	ex      de,hl
	pop     bc
	ld      (hl),b
	dec     hl
	ld      (hl),c
	dec     hl
	ld      (hl),e
	dec     hl
	ld      (hl),d
m2703   pop     af              ; ???
	ret

; Table of error message addresses

m2705   DW      m276d
	DW      m2778
	DW      m2787
	DW      m2791
	DW      m27a2
	DW      m27b5
	DW      m27c1
	DW      m27c1
	DW      m27d4
	DW      m27e2
	DW      m27f3
	DW      m2804
	DW      m2812
	DW      m2823
	DW      m282f
	DW      m2842
	DW      m2842
	DW      m284e
	DW      m285c
	DW      m286b
	DW      m2879
	DW      m288c
	DW      m289d
	DW      m28a6
	DW      m28b4
	DW      m28c5
	DW      m28d2
	DW      m28e5
	DW      m28fd
	DW      m290b
	DW      m2913
	DW      m291f
	DW      m2933
	DW      m293f
	DW      m294e
	DW      m2965
	DW      m296e
	DW      m297c
	DW      m2983
	DW      m2997
	DW      m29af
	DW      m29c1
	DW      m29d6
	DW      m29e6
	DW      m29f7
	DW      m2a0f
	DW      m2a29
	DW      m2a42
	DW      m2a59
	DW      m2a74
	DW      m2a8a
	DW      m2a97
      IF garry
	DW      m36fb
	DW      m370c
	DW      m3724
	DW      m3733
	DW      m3741
	DW      m3759
	DW      m376d
	DW      m3781
	DW      m378d
	DW      m379e
	defs    18
      ENDIF

      IF v41 || spanish
	DW      m2aa8
	DW      m2ac1
      ENDIF

; The +3 BASIC and +3DOS error messages
      IF spanish
m276d   defm    "Error en MERG", 'E'+$80
m2778   defm    "FICHERO INCORRECT", 'O'+$80
m2787   defm    "Error en COD", 'E'+$80
m2791   defm    "EXCESO DE PARENTESI", 'S'+$80
m27a2   defm    "YA EXISTE EL FICHER", 'O'+$80
m27b5   defm    "NOMBRE NO VALID", 'O'+$80
m27c1   defm    "NO EXISTE ESE FICHER", 'O'+$80
m27d4   defm    "DIPOSITIVO NO VALID", 'O'+$80
m27e2   defm    "VELOCIDAD NO VALID", 'A'+$80
m27f3   defm    "NOTA NO VALID", 'A'+$80
m2804   defm    "NUMERO MUY GRAND", 'E'+$80
m2812   defm    "NOTA FUERA DE MARGE", 'N'+$80
m2823   defm    "FUERA DE MARGE", 'N'+$80
m282f   defm    "DEMASIADAS NOTA", 'S'+$80
m2842   defm    "NOMBRE INCORRECT", 'O'+$80
m284e   defm    "PARAMETRO INCORRECT", 'O'+$80
m285c   defm    "UNIDAD NO ENCONTRAD", 'A'+$80
m286b   defm    "FICHERO NO ENCONTRAD", 'O'+$80
m2879   defm    "YA EXISTE EL FICHER", 'O'+$80
m288c   defm    "FIN DE FICHER", 'O'+$80
m289d   defm    "DISCO LLEN", 'O'+$80
m28a6   defm    "DIRECTORIO LLEN", 'O'+$80
m28b4   defm    "FICHERO SOLO LECTUR", 'A'+$80
m28c5   defm    "FICHERO NO ABIERT", 'O'+$80
m28d2   defm    "FICHERO YA EN US", 'O'+$80
m28e5   defm    "SINTAXIS INCORRECT", 'A'+$80
m28fd   defm    "FALTA SECCIO", 'N'+$80
m290b   defm    "FALTA CACH", 'E'+$80
m2913   defm    "FICH. DEMASIADO GRAND", 'E'+$80
m291f   defm    "DISCO NO DE ARRANQU", 'E'+$80
m2933   defm    "UNIDAD YA EN US", 'O'+$80
m293f   defm    "UNIDAD NO PREPARAD", 'A'+$80
m294e   defm    "DISCO PROTEGID", 'O'+$80
m2965   defm    "FALLO DE BUSQUED", 'A'+$80
m296e   defm    "ERROR DE DATO", 'S'+$80
m297c   defm    "SIN DATO", 'S'+$80
m2983   defm    "SIN MARCA DIRECCIONE", 'S'+$80
m2997   defm    "FORMATO NO RECONOCID", 'O'+$80
m29af   defm    "ERROR DESCONOCID", 'O'+$80
m29c1   defm    "DISCO CAMBIAD", 'O'+$80
m29d6   defm    "SOPORTE NO ADECUAD", 'O'+$80
m29e6   defm    "ATRIBUTO NO VALID", 'O'+$80
m29f7   defm    "COPIA DE/HACIA CINT", 'A'+$80
m2a0f   defm    "DESTINO POLIVALENT", 'E'+$80
m2a29   defm    "DESTINO NO ES UNIDA", 'D'+$80
m2a42   defm    "UNIDAD B AUSENT", 'E'+$80
m2a59   defm    "+2A NO ADMITE FORMA", 'T'+$80
m2a74   defm    "LA UNIDAD NO ES A NI ", 'B'+$80
m2a8a   defm    "UNIDAD INCORRECT", 'A'+$80
m2a97   defm    "ERROR DE LONGITU", 'D'+$80
      ELSE
m276d   defm    "MERGE erro", 'r'+$80
m2778   defm    "Wrong file typ", 'e'+$80
m2787   defm    "CODE erro", 'r'+$80
m2791   defm    "Too many bracket", 's'+$80
m27a2   defm    "File already exist", 's'+$80
m27b5   defm    "Invalid nam", 'e'+$80
m27c1   defm    "File does not exis", 't'+$80
m27d4   defm    "Invalid devic", 'e'+$80
m27e2   defm    "Invalid baud rat", 'e'+$80
m27f3   defm    "Invalid note nam", 'e'+$80
m2804   defm    "Number too bi", 'g'+$80
m2812   defm    "Note out of rang", 'e'+$80
m2823   defm    "Out of rang", 'e'+$80
m282f   defm    "Too many tied note", 's'+$80
m2842   defm    "Bad filenam", 'e'+$80
m284e   defm    "Bad parameter", 's'+$80
m285c   defm    "Drive not foun", 'd'+$80
m286b   defm    "File not foun", 'd'+$80
m2879   defm    "File already exist", 's'+$80
m288c   defm    "End of file foun", 'd'+$80
m289d   defm    "Disk ful", 'l'+$80
m28a6   defm    "Directory ful", 'l'+$80
m28b4   defm    "File is read onl", 'y'+$80
m28c5   defm    "File not ope", 'n'+$80
m28d2   defm    "File already in us", 'e'+$80
m28e5   defm    "No rename between drive", 's'+$80
m28fd   defm    "Missing exten", 't'+$80
m290b   defm    "Uncache", 'd'+$80
m2913   defm    "File too bi", 'g'+$80
m291f   defm    "Disk is not bootabl", 'e'+$80
m2933   defm    "Drive in us", 'e'+$80
m293f   defm    "Drive not read", 'y'+$80
m294e   defm    "Disk is write protecte", 'd'+$80
m2965   defm    "Seek fai", 'l'+$80
m296e   defm    "CRC data erro", 'r'+$80
m297c   defm    "No dat", 'a'+$80
m2983   defm    "Missing address mar", 'k'+$80
m2997   defm    "Unrecognised disk forma", 't'+$80
m29af   defm    "Unknown disk erro", 'r'+$80
m29c1   defm    "Disk has been change", 'd'+$80
m29d6   defm    "Unsuitable medi", 'a'+$80
m29e6   defm    "Invalid attribut", 'e'+$80
m29f7   defm    "Cannot copy to/from tap", 'e'+$80
m2a0f   defm    "Destination cannot be wil", 'd'+$80
m2a29   defm    "Destination must be driv", 'e'+$80
m2a42   defm    "Drive B: is not presen", 't'+$80
m2a59   defm    "+2A does not support forma", 't'+$80
m2a74   defm    "Drive must be A: or B", ':'+$80
m2a8a   defm    "Invalid driv", 'e'+$80
m2a97   defm    "Code length erro", 'r'+$80
      ENDIF

      	IF garry=0
m2aa8   defm    "You should never see thi", 's'+$80
m2ac1   defm    "Hello there !"
	ENDIF

; Subroutine to output an error message (terminated by
; a byte with bit 7 set)
; Enter with DE=message address

m2ace   ld      a,(de)          ; get next char
	and     $7f             ; mask bit 7
m2ad1   push    de
	rst     $10             ; output
	pop     de
	ld      a,(de)
m2ad5   inc     de
	add     a,a             ; check bit 7
	jr      nc,m2ace        ; loop back if not set
	ret

; The Error Handling routine
; Enter here with inline error code-1

m2ada   pop     hl              ; get address of error code
	ld      sp,(ERR_SP)     ; reset SP
	ld      a,(hl)
	ld      (RAMERR),a      ; store error number-1
	inc     a               ; get error code
	cp      $1e
	jr      nc,m2aeb        ; move on if a +3-specific error
m2ae8   rst     $28
	DW      RAMRST          ; else call ROM 3 error handler
m2aeb   dec     a
	ld      (iy+$00),a      ; save code in ERR_NR
m2aef   ld      hl,(CH_ADD)     ; get address at which error occurred
	ld      (X_PTR),hl      ; save it
	rst     $28
	DW      $16c5           ; clear calculator stack
	ret                     ; exit to error address

; Subroutine to test the BREAK key
; Terminates with error L - BREAK into program if so

m2af9   ld      a,$7f
	in      a,($fe)
	rra
	ret     c               ; exit if SPACE not pressed
	ld      a,$fe
	in      a,($fe)
	rra
	ret     c               ; or if CAPS not pressed
	call    m2ada
	defb    $14             ; error L

; Subroutine to execute routine at HL, returning to m3a1b in order to
; return control to ROM 3
; It is provided to allow serial input into 48K BASIC, but looks buggy
      IF garry
m2b09   rst     $18
	cp      $23
	jr      z, m2b14
	call    m10b1
	jp      m1266
m2b14   call    m2b35
	ld      b, 0
m2b19   call    m3f00
	DW      $0062
	push    de
	push    hl
	pop     bc
	rst     $28
	DW      $2d2b
	pop     bc
	rst     $28
	DW      $2d2b
	ld      de, m2b56
	ld      bc, 10
	call    m14f0
	rst     $28
	DW      $2aff
	ret
m2b35   rst     $20
	rst     $28
	DW      $1c82
	rst     $18
	cp      ','
	jp      nz, m22c3
	rst     $20
	rst     $28
	DW      $1c1f
	bit     6, (iy+$01)
	jp      z, m22c3
	pop     hl
	call    m10b1
	push    hl
	rst     $28
	DW      $1e94
	rst     $28
	DW      $1601
	ret
m2b56   rst     $28
	DW      $4034
	ld      b, c
	nop
	nop
	inc     b
	rrca
	defb    $38
	ret
	defs    4
      ELSE
m2b09   ei
	ex      af,af'
	ld      de,m3a1b
	push    de              ; stack return address to paging routine
	res     3,(iy+$02)      ; set "mode unchanged"
	push    hl
	ld      hl,(ERR_SP)
	ld      e,(hl)
	inc     hl
	ld      d,(hl)          ; DE=current error return address
	and     a
	ld      hl,$107f        ; test against ROM 3 editing error address
	sbc     hl,de
m2b20   jr      nz,m2b5a        ; move on if not
m2b22   pop     hl
	ld      sp,(ERR_SP)
	pop     de              ; drop two addresses from error return stack
	pop     de
	ld      (ERR_SP),de
m2b2d   push    hl              ; save routine address
	ld      de,m2b33        ; address to return to here
	push    de
m2b32   jp      (hl)            ; execute routine
m2b33   jr      c,m2b3e         ; if no error, move on
	jr      z,m2b3b         ; if no character, move on
m2b37   call    m2ada
	defb    7               ; error 8 ???
m2b3b   pop     hl
m2b3c   jr      m2b2d           ; call the routine again
m2b3e   cp      $0d
	jr      z,m2b50         ; if character was CR, move on
	ld      hl,(RETADDR)
	push    hl
	rst     $28
	DW      $0f85           ; else add a character to current input line
	pop     hl
	ld      (RETADDR),hl
	pop     hl
	jr      m2b2d           ; loop back to get another character
m2b50   pop     hl              ; discard routine address
	ld      a,(BANKM)
	or      $10             ; select ROM 3
	push    af
	jp      m3a1b           ; go to exit
m2b5a   pop     hl
	ld      de,m2b60        ; address to return to here
	push    de
	jp      (hl)            ; execute routine
m2b60   ret     c               ; exit if no errors
	ret     z
	jr      m2b37           ; else go to act on them
      ENDIF

; Subroutine to page in normal memory (page 0) and swap SP with OLDSP

m2b64:  ex      af,af'          ; save AF
	ld      a,$10
	di
	call    m2b7e           ; page in page 0
	pop     af              ; AF=return address
	ld      (TARGET),hl     ; save HL
	ld      hl,(OLDSP)
	ld      (OLDSP),sp
	ld      sp,hl           ; SP now swapped with OLDSP
	ei
	ld      hl,(TARGET)     ; restore HL
	push    af              ; restack return address
	ex      af,af'          ; restore AF
	ret

; Subroutine to page in a RAM/ROM/screen combination (in A)

m2b7e   push    bc
	ld      bc,$7ffd
	out     (c),a           ; page it in
	ld      (BANKM),a
	pop     bc
	ret

; Subroutine to page in DOS workspace (page 7) and swap SP with OLDSP

m2b89   ex      af,af'          ; save AF
	di
	pop     af              ; AF=return address
	ld      (TARGET),hl     ; save HL
	ld      hl,(OLDSP)
	ld      (OLDSP),sp
	ld      sp,hl           ; SP swapped with OLDSP
	ld      hl,(TARGET)     ; restore HL
	push    af              ; push back return address
	ld      a,$17
	call    m2b7e           ; page in page 7
	ei
	ex      af,af'          ; restore AF
	ret

; Subroutine to copy a file
; Enter with A=destination flag
; A=$00 - file
;  =$e0 - printer
;  =$aa - screen
; Z flag set if A=$00
; C flag reset if copying TO SPECTRUM FORMAT

m2ba3   call    m2b89           ; page in DOS workspace
	ld      (dst_dev),a     ; save destination flag
	push    af
	jr      z,m2bb2         ; move on if copying to a file
	cp      $e0             ; copy to LPRINT?
	ld      a,$03           ; use stream 3
	jr      z,m2bb4
m2bb2   ld      a,$02           ; use stream 2
m2bb4   call    m2b64           ; page in normal memory
	rst     $28
	DW      $1601           ; open channel to stream
	call    m2b89           ; page in DOS workspace
	pop     af              ; restore destination flag
	jr      z,m2c1e         ; move on if copying to another file
	ld      hl,tmp_fspec    ; stored filename address
	ld      bc,$0001        ; use file number 0,exclusive-read
	ld      de,$0001        ; open action - error if doesn't exist
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; open file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error opening
m2bd7   ld      b,$00           ; file 0
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_BYTE_READ   ; read a byte
	call    m32ee           ; restore TSTACK
	jr      c,m2bed         ; move on if no error
	cp      $19
	jp      nz,m3219        ; move on if not end-of-file error
	jr      m2c0a           ; end of file, so move on to close file
m2bed   ld      a,(dst_dev)     ; check destination flag
	cp      $aa
	ld      a,c             ; A=byte from file
	jr      nz,m2bff        ; move on unless copying to screen
	cp      $0d
	jr      z,m2bff         ; okay to output CRs
	cp      $20
	jr      nc,m2bff
	ld      a,$20           ; but replace other control chars with spaces
m2bff   call    m2b64           ; page in normal memory
	rst     $28
	DW      $0010           ; output byte
m2c05   call    m2b89           ; page in DOS workspace
	jr      m2bd7           ; back for more
m2c0a   ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error closing
	call    m2b64           ; page in normal memory
	ret                     ; done

; This part of the copy routine copies a file to another file

m2c1e   push    af              ; save destination flag
	ld      hl,tmp_fspec
	ld      (dst_add),hl    ; store address of destination filespec
	ld      de,dst_file
	call    m3109           ; copy filespec, error if too long
	push    hl              ; save address of source filespec
	ld      a,$ff
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_DRIVE   ; set default drive
	call    m32ee           ; restore TSTACK
	ld      (dst_drv),a     ; use default drive for destination
	ld      (src_drv),a     ; use default drive for source
	ld      hl,dst_file
	call    m30f0           ; is destination drive specified?
	jr      nz,m2c4d        ; move on if not
	ld      de,dst_drv
	call    m30e3           ; place drive letter at dst_drv
m2c4d   pop     hl              ; restore address of source filespec
	pop     af              ; restore destination flag
	jp      nc,m3123        ; move on if copying TO SPECTRUM FORMAT
	ld      (src_add),hl    ; save address of source filespec
	ld      de,src_file
	call    m3109           ; copy filespec, error if too long
	ld      hl,src_file
	call    m30f0           ; is source drive specified?
	jr      nz,m2c69        ; move on if not
	ld      de,src_drv
	call    m30e3           ; place drive letter at src_drv
m2c69   ld      (SCR_CT),a      ; zeroise scroll count
	ld      a,$0d
	rst     $10             ; output CR
	xor     a
	ld      (wild),a        ; clear "wild" flag
	ld      (copied),a      ; zero # files copied
	ld      (dst_dev),a     ; destination is a file
	ld      hl,dst_file
	call    m30b6           ; check if destination wild
	ld      a,(wild)
	or      a
	jr      z,m2c8c
	call    m2b64           ; if so, page in normal memory
	call    m2ada           ; and cause error "destination cannot be wild"
	defb    $49
m2c8c   ld      hl,m3283
	ld      de,tmp_file
	ld      bc,$000e
	ldir                    ; copy temporary filename
	ld      hl,src_file
	call    m30b6           ; check if source wild
	ld      a,(wild)
	or      a
	jr      nz,m2ca9        ; move on if so
	call    m2d58           ; copy a single file
	jp      m2d26           ; finish up
m2ca9   ld      hl,(dst_add)    ; get address of dest filespec
	call    m30f0           ; get past drive
	ld      a,$ff
	cp      (hl)
	jr      z,m2cbb         ; move on if just drive
	call    m2b64           ; page in normal memory
	call    m2ada
	defb    $4a             ; else error "destination must be drive"
m2cbb   ld      hl,wld_next
	xor     a
	ld      b,$0d
m2cc1   ld      (hl),a          ; zeroise directory entry 1
	inc     hl
	djnz    m2cc1
m2cc5   ld      hl,wld_next
	ld      de,cat_spec
	ld      bc,$000d
	ldir                    ; and zeroise directory entry 0
	ld      hl,(src_add)
	ld      bc,$0200        ; 1 entry required, include system files
	ld      de,cat_spec
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CATALOG     ; get next filename
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      hl,dst_dev
	ld      a,(hl)
	or      a
	jr      nz,m2cf7        ; move on if not copying first file
	inc     a
	ld      (hl),a          ; set "first file copied" flag
	ld      a,$17
	dec     b
	jp      z,m3219         ; cause error "File not found" if none
	inc     b
m2cf7   dec     b               ; B=0 if no more matches
	jr      z,m2d26         ; move to finish if done
	ld      hl,src_file
	call    m30f0           ; get past drive of source
	ex      de,hl
	ld      hl,wld_next     ; address of found entry
	ld      b,$08
	call    m30d9           ; copy filename part into source
	ld      hl,wld_next+8   ; get to extension of found entry
	ld      a,'.'
	ld      (de),a          ; insert "."
	inc     de
	ld      b,$03
	call    m30d9           ; copy extension part into source
	ld      a,$ff
	ld      (de),a          ; insert terminator
	ld      hl,dst_file
	call    m30f0           ; get past drive name in dest
	ld      (hl),$ff        ; insert terminator
	call    m2d58           ; copy a file
	jp      m2cc5           ; loop back for more

; Copy file routines - end part

m2d26   ld      hl,tmp_file
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_DELETE      ; delete temp file
	call    m32ee           ; restore TSTACK
	ld      a,(copied)
	dec     a               ; A=# files copied-1
	ld      hl,m3291        ; "1 file copied" message
	jr      z,m2d4c         ; move on if 1
	inc     a
	ld      l,a
	ld      h,$00           ; HL=# files copied
	ld      a,$0d
	rst     $10             ; output CR
	ld      e,' '
	call    m0800           ; output #
	ld      hl,m32a5        ; "files copied" message
m2d4c   call    m3268           ; output message
	ld      a,$17
	ld      (SCR_CT),a      ; set scroll count
	call    m2b64           ; page in normal memory
	ret                     ; done!

; Subroutine to copy a single file

m2d58   ld      hl,dst_file     ; dest filespec
	ld      de,src_file     ; source filespec
m2d5e   ld      a,(de)
	cp      (hl)            ; compare filespecs
	jr      nz,m2d72        ; move on if different
	ld      a,$ff
	cp      (hl)
	jr      nz,m2d6e
	call    m2b64           ; page in normal memory
	call    m2ada
	defb    $30             ; error if filespecs the same
m2d6e   inc     hl              ; increment pointers
	inc     de
	jr      m2d5e           ; loop back
m2d72   ld      hl,dst_file
	call    m30f0           ; move past drive specifier in dest filespec
	ld      a,(hl)
	cp      $ff
	jr      nz,m2d94        ; move on if a destination filename specified
	ld      hl,dst_file
	call    m30f0
	push    hl              ; store address after drive specifier
	ld      hl,src_file
	call    m30f0           ; get to filename of source
	pop     de
m2d8b   ld      a,(hl)
	ld      (de),a          ; copy filename to destination
	inc     a
	jr      z,m2d94         ; move on when done
	inc     de
	inc     hl
	jr      m2d8b           ; loop back
m2d94   xor     a
	ld      (copy_ram),a    ; signal "copy via M:"
	ld      a,'M'
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_FREE_SPACE  ; find free space on drive M:
	call    m32ee           ; restore TSTACK
      IF garry
	jp      nc,m2dc7
      ELSE
	jp      nc,m3219        ; move on if error
      ENDIF
	ld      a,h
	or      a
	jr      z,m2daf
	ld      hl,$003f        ; use max 63K on drive M:
m2daf   ld      a,l
	cp      $40
	jr      c,m2db7
	ld      hl,$003f        ; use max 63K on drive M:
m2db7   ld      h,l
	ld      l,$00
	add     hl,hl
	add     hl,hl
	ld      (free_m),hl     ; store free bytes on drive M:
	ld      de,$0800
	or      a
	sbc     hl,de
	jr      nc,m2dd1        ; move on if >=2K free
m2dc7   ld      a,$ff
	ld      (SCR_CT),a      ; set scroll count
	ld      a,$01
	ld      (copy_ram),a    ; signal "copy via RAM"
m2dd1   xor     a
	ld      (dst_open),a    ; signal no temporary file open
	ld      (eof),a         ; signal not EOF
	ld      hl,dst_file
	call    m30f0           ; get past drive of dest
	ld      a,(hl)
	cp      $ff
	jp      nz,m2e5d        ; if dest filename specified, jump on
	ld      hl,src_file
	call    m30f0           ; get past drive of source
	ld      a,(hl)
m2deb   cp      $ff
	jp      nz,m2e5d        ; if source filename specified, jump on
	ld      a,(dst_drv)     ; check destination drive
	cp      'M'
	jp      z,m2e5d         ; move on if M: (will fail on attempted copy)
	ld      a,(src_drv)     ; check source drive
	cp      'M'
	jp      z,m2e5d         ; move on if M: (will fail on attempted copy)
	ld      a,'A'           ; by this stage, we must be copy A:<-->B:
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_REF_XDPB    ; get XDPB for drive A:
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      c,0
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DD_LOGIN        ; login disk in drive A:
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	or      a
	ld      a,$06
	jp      nz,m3219        ; cause error 6 if not a standard +3 disk
	ld      a,(src_drv)     ; get source drive letter
	ld      bc,$0001
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPENDRIVE   ; open source drive as exclusive-read file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,(dst_drv)     ; get dest drive letter
	ld      bc,$0102
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPENDRIVE   ; open dest drive as exclusive-write file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,$01
	ld      (dst_open),a    ; signal temporary file open
	ld      a,(copy_ram)
	or      a
	jp      z,m2f44         ; copy via M: if >=2K free on drive M:
	jp      m2ecd           ; else copy via RAM
m2e5d   ld      hl,src_file     ; source name
	ld      a,$ff
	ld      (SCR_CT),a      ; set max scroll count
	push    hl
	push    hl
	call    m3268           ; display filespec
	pop     de
	ex      de,hl
	or      a
	sbc     hl,de
	ld      de,$0011
	add     hl,de
	ld      b,l             ; B=# spaces required
m2e74   push    bc
	ld      a,' '
	rst     $10             ; output a space
	pop     bc
	djnz    m2e74           ; loop back
	pop     hl
	ld      a,(dst_drv)     ; get dest drive letter
	or      $20             ; make lowercase
	cp      'm'
	jr      z,m2e95         ; move on if copying to M:
	ld      a,(copy_ram)
	or      a
	jr      nz,m2e95        ; or if >=2K free on M:
	ld      a,(src_drv)     ; get source drive letter
	or      $20
	cp      'm'
	jp      nz,m2f2d        ; if not copying from M:, move on
m2e95   ld      hl,src_file     ; source filename
	ld      bc,$0001        ; file 0,excl read
	ld      de,$0002        ; must be openable
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      hl,dst_file     ; dest filename
	push    hl
	call    m3268           ; display filename
	pop     hl
	ld      bc,$0102        ; file 1, exc write
	ld      de,$0204        ; create new file
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,$01
	ld      (dst_open),a    ; signal temporary file open

; Subroutine to copy everything from file 0 to file 1, via a 2K area
; in page 0 (bug: this should be page 7!)
      IF v41 || garry
m2ecd   ld      bc,$0007        ; file 0, page 0 (oops, should be page 7!)
      ELSE
m2ecd   ld      bc,$0000        ; file 0, page 0 (oops, should be page 7!)
      ENDIF
	ld      de,$0800        ; 2K to read
	ld      hl,tmp_buff     ; address to read
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_READ        ; read bytes
	call    m32ee           ; restore TSTACK
	jr      c,m2f08         ; move on if no error
	cp      $19
	jp      nz,m3219        ; if error not end-of-file, cause error
	ld      a,$01
	ld      (eof),a         ; signal end-of-file reached
m2eed   push    de              ; save # unread bytes
	ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file 0
	call    m32ee           ; restore TSTACK
	pop     de
	jp      nc,m3219        ; move on if error
	ld      hl,$0800
	or      a
	sbc     hl,de
	ex      de,hl           ; DE=number of bytes read
	jr      m2f0b           ; move on
m2f08   ld      de,$0800        ; DE=2048 bytes read
m2f0b   ld      a,e
	or      d
	jr      z,m2f23         ; if no bytes read, move on
	ld      hl,tmp_buff
      IF v41 || garry
	ld      bc,$0107
      ELSE
	ld      bc,$0100
      ENDIF
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_WRITE       ; write bytes to file 1 from page 0 (oops)
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
m2f23   ld      a,(eof)
	or      a
	jp      z,m2ecd         ; loop back if not end-of-file
	jp      m309e           ; close file 1 and exit

; Continuation of copy command, where M: is involved

m2f2d   ld      hl,src_file     ; source filename
	ld      bc,$0001        ; file 0, excl read
	ld      de,$0002        ; must be openable
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error

; Subroutine to copy everything from file 0 to file 1, via a temporary
; file in drive M:
; Each 2K is read via RAM in page 0 - this should be page 7 (oops!)

m2f44   ld      hl,tmp_file     ; temporary filename
	ld      bc,$0203        ; file 2, exclusive read-write mode
	ld      de,$0204        ; open & create actions
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; open temporary file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      hl,$0000
	ld      (tmp_bytes),hl  ; zero # bytes copied to temp file
      IF v41 || garry
m2f61   ld      bc,$0007
      ELSE
m2f61   ld      bc,$0000
      ENDIF
	ld      de,$0800
	ld      hl,tmp_buff
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_READ        ; read 2K into RAM
	call    m32ee           ; restore TSTACK
	jr      c,m2f9c         ; move on if no error
	cp      $19
	jp      nz,m3219        ; cause error if it wasn't end-of-file
	ld      a,$01
	ld      (eof),a         ; signal end-of-file reached
	push    de              ; save # unread bytes
	ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file 0
	call    m32ee           ; restore TSTACK
	pop     de
	jp      nc,m3219        ; move on if error
	ld      hl,$0800
	or      a
	sbc     hl,de
	ex      de,hl           ; DE=# bytes read
	jr      m2f9f
m2f9c   ld      de,$0800        ; DE=2048 bytes read
m2f9f   ld      a,e
	or      d
	jr      z,m2fb9         ; move on if no bytes read
	push    de
	ld      hl,tmp_buff
      IF v41 || garry
	ld      bc,$0207
      ELSE
	ld      bc,$0200
      ENDIF
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_WRITE       ; write bytes to temporary file
	call    m32ee           ; restore TSTACK
	pop     de
	jp      nc,m3219        ; move on if error
m2fb9   ld      hl,(tmp_bytes)
	add     hl,de
	ld      (tmp_bytes),hl  ; update number of bytes copied to temp file
	ld      de,$0800
	add     hl,de
	ex      de,hl
	ld      hl,(free_m)
	ld      a,(eof)
	or      a
	jr      nz,m2fd2        ; move on if end-of-file reached
	sbc     hl,de
	jr      nc,m2f61        ; loop back if temp file can take 2K more
m2fd2   ld      a,(src_drv)
	and     $df             ; get source drive (capitalised)
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_FLUSH       ; flush for source drive
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      b,$02
	ld      hl,$0000
	ld      e,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_SET_POSITION ; get to start of temp file
	call    m32ee           ; restore TSTACK
	ld      a,(dst_open)
	or      a
	jr      nz,m301e        ; move on if dst_file contains spec of temp file
	ld      hl,dst_file
	push    hl
	call    m3268           ; else display filespec
	pop     hl
	ld      bc,$0102
	ld      de,$0204
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; open file 1 in exclusive-write mode
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,$01
	ld      (dst_open),a    ; signal dest file is open
m301e   ld      hl,tmp_buff
	ld      de,$0800
      IF v41 || garry
	ld      bc,$0207
      ELSE
	ld      bc,$0200
      ENDIF
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_READ        ; read 2K from temp file
	call    m32ee           ; restore TSTACK
	ld      hl,$0800        ; HL=$0800 bytes read
	jr      c,m3042         ; move on if no error
	cp      $19
	jp      nz,m3219        ; cause non-EOF errors
	ld      hl,$0800
	or      a
	sbc     hl,de           ; HL=# bytes read
m3042   ex      de,hl
      IF v41 || garry
	ld      bc,$0107
      ELSE
	ld      bc,$0100
      ENDIF
	ld      hl,tmp_buff
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_WRITE       ; write bytes to file 1
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      hl,(tmp_bytes)
	ld      de,$0800
	or      a
	sbc     hl,de
	jr      c,m3069         ; move on if temp file empty
	ld      a,h
	or      l
	ld      (tmp_bytes),hl  ; update bytes left in temp file
	jr      nz,m301e        ; loop back to copy more
m3069   ld      b,$02
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close temp file
	call    m32ee           ; restore TSTACK
	ld      a,(dst_drv)
	and     $df             ; get dest drive (capitalised)
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_FLUSH       ; flush dest drive
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,(eof)
	or      a
	jp      z,m2f44         ; loop back if not EOF
	ld      hl,tmp_file
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_DELETE      ; delete temp file
	call    m32ee           ; restore TSTACK

; Enter here if copying via 2K area in RAM

m309e   ld      b,$01
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close file 1
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,$0d
	rst     $10             ; output CR
	ld      hl,copied
	inc     (hl)            ; increment # files copied
	ret

; Subroutine to check whether filespec at HL is wild
; Causes error if filespec longer than $11 (inc terminator)

m30b6   ld      b,$11
	ld      a,(hl)
	cp      '?'
	jr      nz,m30c4        ; move on if not ? wildcard
	push    af
	ld      a,$01
	ld      (wild),a        ; set wildcard flag
	pop     af
m30c4   cp      '*'
	jr      nz,m30cf        ; move on if not * wildcard
	push    af
	ld      a,$01
	ld      (wild),a        ; set wildcard flag
	pop     af
m30cf   inc     hl              ; increment pointer
	inc     a
	ret     z               ; exit if done
	djnz    m30b6           ; loop back
	ld      a,$14
	jp      m3219           ; cause bad filename error if too long

; Subroutine to copy up to B chars from HL to DE, stopping at first space

m30d9   ld      a,(hl)          ; get next char
	cp      ' '
	ret     z               ; exit if space
	ld      (de),a          ; copy char
	inc     hl              ; increment pointers
	inc     de
	djnz    m30d9           ; loop back
	ret

; Subroutine to get a drive letter from a filespec & place it
; in the address at DE. HL points past the colon of the filespec

m30e3   dec     hl
	dec     hl
	ld      a,(hl)          ; get character before colon
	or      $20             ; make lowercase
	cp      'a'
	ret     c               ; exit if < 'a'
	cp      '{'
	ret     nc              ; or if > 'z'
	ld      (de),a          ; store drive letter
	ret

; Subroutine to check if filespec includes drive specification
; On entry, HL=address of filespec
; On exit, Z flag set if drive specified, and HL points to
; start of filename after colon.

m30f0   push    hl
	pop     de              ; copy address of filename to DE
	ld      a,(hl)          ; get first char
	inc     a
	jr      z,m3103         ; move to exit if end of filename
	ld      b,$03           ; check first 3 chars
m30f8   ld      a,(hl)
	cp      ':'             ; is char a ':' ?
	jr      z,m3107         ; move on if so
	inc     a
	jr      z,m3103         ; exit if end of filename
	inc     hl
	djnz    m30f8           ; back for more
m3103   or      $ff             ; reset Z flag - no drive specified
	ex      de,hl           ; HL=start of filename
	ret
m3107   inc     hl              ; HL=start of filename after drive spec
	ret                     ; exit with Z set

; Subroutine to copy a $ff-terminated filename from HL to DE
; If max length of $11 (inc terminator) exceeded, cause error

m3109   ld      b,$11           ; allow 17 characters in a filename
	ld      a,(hl)
	ld      (de),a          ; copy filename
	inc     hl              ; increment pointers
	inc     de
	inc     a               ; test for end of filename
	jr      z,m3119         ; exit if found
	djnz    m3109           ; loop back
	ld      a,$14
	jp      m3219           ; cause +3DOS error $14, "Bad filename"
m3119   ret


; Subroutine to clear screen and open channel to stream 2

m311a   rst     $28
	DW      $0d6b           ; cls
	ld      a,$02
	rst     $28
	DW      $1601           ; open channel to stream 2
	ret

; Routine to copy files to spectrum format

m3123   xor     a
	ld      (wild),a        ; no wildcard
	ld      (dst_open),a    ; dest not open file
	ld      hl,dst_file
	call    m30b6           ; is dest filespec wild?
	ld      a,(wild)
	or      a
	jr      z,m313d         ; move on if not
	call    m2b64           ; page in normal memory
	call    m2ada
	defb    $49             ; else error "destination cannot be wild"
m313d   ld      hl,dst_file
	ld      b,$12
m3142   ld      a,(hl)
	cp      '.'             ; has file got extension?
	inc     hl
	jr      z,m314b         ; move on if so
	inc     a
	jr      nz,m3142
m314b   dec     hl
	ex      de,hl
	ld      hl,m3214
	ld      bc,$0004        ; length 4 misses terminator (oops!)
	ldir                    ; copy ".HED" extension
	ld      hl,(dst_add)
	ld      bc,$0001        ; file 0, exclusive read
	ld      de,$0001
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; open source file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      hl,dst_file     ; dest filename
	ld      bc,$0102        ; file 1, exclusive write
	ld      de,$0104
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_OPEN        ; open dest file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,$01
	ld      (dst_open),a    ; signal dest open
	ld      hl,$0000
	ld      (tmp_bytes),hl  ; signal 0 bytes copied
m318e   ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_BYTE_READ   ; read a byte
	call    m32ee           ; restore TSTACK
	jr      c,m31a4         ; move on if no error
	cp      $19
	jp      nz,m3219        ; cause non-EOF error
	jr      z,m31bd         ; move on
m31a4   ld      b,$01
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_BYTE_WRITE  ; write byte
	call    m32ee           ; restore TSTACK
	ld      hl,(tmp_bytes)
	inc     hl              ; update bytes copied
	ld      (tmp_bytes),hl
	jr      c,m318e         ; loop back if no error
	jp      m3219           ; cause error
m31bd   ld      b,$00
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close source file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	ld      a,(dst_drv)
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_FLUSH       ; flush dest drive
	call    m32ee           ; restore TSTACK
	ld      b,$01
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_REF_HEAD    ; point at header data for dest file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219
	ld      a,$03
	ld      (ix+$00),a      ; set CODE type
	ld      hl,(tmp_bytes)
	ld      (ix+$01),l
	ld      (ix+$02),h      ; set length
	xor     a
	ld      (ix+$03),a      ; set load address to zero
	ld      (ix+$04),a
	ld      b,$01
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; close dest file
	call    m32ee           ; restore TSTACK
	jp      nc,m3219        ; move on if error
	call    m2b64           ; page in normal memory
	ret                     ; done

m3214   defm    ".HED", $ff

; Routine to close files 0-2, delete temporary files and
; generate the +3DOS error held in A

m3219   push    af              ; save +3DOS error code
	ld      b,$03           ; three files
m321c   push    bc              ; stack counter
	dec     b               ; decrement counter
	push    bc              ; stack file number
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_CLOSE       ; try to close file B
	call    m32ee           ; restore TSTACK
	pop     bc              ; restore file number
	jr      c,m3238         ; move on if closed okay
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_ABANDON     ; else abandon it
	call    m32ee           ; restore TSTACK
m3238   pop     bc
	djnz    m321c           ; back for other files
	ld      a,$0d
	rst     $10             ; new line on screen
	ld      a,(dst_open)
	or      a
	jr      z,m3252         ; move on if no temporary file created
	ld      hl,dst_file     ; HL=address of temporary filename
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_DELETE      ; delete temporary file
	call    m32ee           ; restore TSTACK
m3252   ld      hl,tmp_file
	call    m32b6           ; save TSTACK in page 7
	call    m3f00
	DW      DOS_DELETE      ; delete other temporary file
	call    m32ee           ; restore TSTACK
	pop     af              ; restore +3DOS error code
	call    m2b64           ; page in normal memory
      IF v41
	ld      a,$02
	rst     $28
	DW      $1601
      ENDIF
	call    m0e9a           ; cause +3DOS error
	defb    $ff

; Subroutine to display filename/message at HL

m3268   ld      a,(hl)          ; get next char
	inc     hl
	or      a
	ret     z               ; exit if null
	cp      $ff
	ret     z               ; or $ff
	and     $7f
	rst     $10             ; display character
	jr      m3268           ; loop back


; Subroutine to get a key (apparently unused)

m3274   ld      hl,FLAGS
	res     5,(hl)          ; set "no key"
m3279   bit     5,(hl)
	jr      z,m3279         ; loop until key available
	res     5,(hl)          ; set "no key"
	ld      a,(LAST_K)      ; get it
	ret

; Temporary filespec, used in COPY

m3283   defm    "M:VAXNSUZ.$$$", $ff

; Files copied messages
    IF garry
	defs    37
    ELSE
      IF spanish
m3291   defm    $0d, "  1 fichero copiado.", $0d, $0d, 0
m32a5   defm    " ficheros copiados.", $0d, $0d, 0
      ELSE
m3291   defm    $0d, "  1 file copied.", $0d, $0d, 0
m32a5   defm    " files copied.", $0d, $0d, 0
      ENDIF
    ENDIF
; Subroutine to copy TSTACK to a temporary area in page 7, and
; reset SP to use whole of TSTACK again

m32b6   di
	ld      (tmp_hl),hl     ; save HL
	push    af
	pop     hl
	ld      (tmp_af),hl     ; save AF
	ld      (tmp_de),de     ; save DE
	ld      (tmp_bc),bc     ; save BC
	ld      hl,TSTACK
	ld      de,tmp_stack
	IF v41
		ld      bc,$0083
	ELSE
		ld      bc,$0084
	ENDIF
	lddr                    ; copy TSTACK area into page 7
	pop     bc              ; BC=return address
	ld      (tmp_sp),sp     ; save SP
	ld      hl,TSTACK
	ld      sp,hl           ; set SP back to top of TSTACK
	push    bc              ; restack return address
	ld      bc,(tmp_bc)     ; restore BC
	ld      de,(tmp_de)     ; restore DE
	ld      hl,(tmp_af)
	push    hl
	pop     af              ; restore AF
	ld      hl,(tmp_hl)     ; restore HL
	ei
	ret

; Subroutine to restore TSTACK from where it's been saved in a temporary
; area in page 7

m32ee:  di
	ld      (tmp_hl),hl     ; save HL
	push    af
	pop     hl
	ld      (tmp_af),hl     ; save AF
	ld      (tmp_de),de     ; save DE
	ld      (tmp_bc),bc     ; save BC
	pop     hl
	ld      (tmp_ret),hl    ; save return address
	ld      hl,tmp_stack
	ld      de,TSTACK
      IF v41
	ld      bc,$0083
      ELSE
	ld      bc,$0084
      ENDIF
	lddr                    ; restore TSTACK from saved location
	ld      hl,(tmp_sp)
	ld      sp,hl           ; restore SP
	ld      hl,(tmp_ret)
	push    hl              ; restack return address
	ld      bc,(tmp_bc)     ; restore BC
	ld      de,(tmp_de)     ; restore DE
	ld      hl,(tmp_af)
	push    hl
	pop     af              ; restore AF
	ld      hl,(tmp_hl)     ; restore HL
	ei
	ret

; The COPY EXP command

m3328   xor     a
	call    m2b89           ; page in DOS workspace
	ld      (tmp_buff+5),a  ; flag "normal copy exp"
	call    m2b64           ; page in normal memory
	rst     $28
	DW      $0020           ; get next character
	cp      $dd
	jr      nz,x34be        ; move on if not INVERSE
	ld      a,$fc
	call    m2b89           ; page in DOS workspace
	ld      (tmp_buff+5),a  ; flag "inverse copy exp"
	call    m2b64           ; page in normal memory
	rst     $28
	DW      $0020           ; get to next char
x34be IF v41
	cp      $dc
	jr      nz,m3347
	ld      hl,FLAGS
	bit     7,(hl)
	jr      z,x34d9
	ld      hl,$5800
	ld      bc,$0300
x34cf   ld      a,(hl)
	or      $40
	ld      (hl),a
	inc     hl
	dec     bc
	ld      a,b
	or      c
	jr      nz,x34cf
x34d9   rst     $28
	DW      $0020           ; get to next char
      ENDIF
m3347   call    m10b1           ; check for end-of-statement
	ld      a,(BANK678)
	ld      bc,$1ffd
	set     4,a             ; set strobe high
	di
	ld      (BANK678),a
	out     (c),a           ; output strobe
	ei
	call    m2b89           ; page in DOS workspace
	di
	ld      a,$1b           ; set DUMPLF/216" linespacing
	call    m33b9
	ld      a,'3'
	call    m33b9
	ld      hl,DUMPLF
	ld      a,(hl)
	call    m33b9
	ld      hl,$401f        ; address of top right corner of display
	ld      e,$20           ; number of chars per line
m3373   push    hl
	ld      d,$01           ; start with bit 0 pixel
m3376   push    de
	push    hl
	ld      hl,m34bf
	call    m33c5           ; output raster line header
	pop     hl
	pop     de
	push    hl
m3381   call    m33d1           ; output raster data for next two pixels
	ld      a,h
	and     $07
	inc     h               ; get to next pixel line down
	cp      $07
	jr      nz,m3381        ; loop back if still in same character line
	ld      a,h
	sub     $08             ; back to top line of a character
	ld      h,a
	ld      a,l
	add     a,$20           ; move to next character line down
	ld      l,a
	jr      nc,m3381        ; loop back if same screen third
	ld      a,h
	add     a,$08           ; increment screen third
	ld      h,a
	cp      $58
	jr      nz,m3381        ; loop back if all thirds not done
	pop     hl              ; restore top of screen address
	sla     d
	sla     d               ; shift left two pixels
	jr      nc,m3376        ; loop back if within same char
	pop     hl              ; restore top of screen address
	dec     hl              ; previous character
	dec     e
	jr      nz,m3373        ; loop back if not finished
	ld      a,$1b           ; reset printer
	call    m33b9
	ld      a,'@'
	call    m33b9
	ei
	call    m2b64           ; page in normal memory
	ret                     ; done

; Subroutine to page in normal memory, output a character to the
; printer, and page back DOS workspace

m33b9   ei
	call    m2b64           ; page in normal memory
	call    m2051           ; output char to printer
	call    m2b89           ; page in DOS workspace
	di
	ret

; Subroutine to output a $ff-terminated string to the printer

m33c5   ld      a,(hl)          ; get next char
	cp      $ff
	ret     z               ; exit if $ff
	push    hl
	call    m33b9           ; output char
	pop     hl
	inc     hl
	jr      m33c5           ; loop back

; Subroutine to output 4 raster bytes for the next 2 pixels

m33d1   push    af              ; save registers
	push    hl
	push    de
	push    hl
	call    m3402           ; E=attribute for address in HL
	pop     hl
	call    m3412           ; clear 4-byte buffer
	call    m343f           ; copy appropriate pattern for pixel to buffer
	call    m341f           ; shift pattern left 3 bits
	sla     d               ; shift pixel number
	call    m343f           ; merge in pattern for next pixel
	call    m3432           ; shift patterns left 2 bits
	ld      b,$04           ; get ready to output 4 raster bytes
	ld      hl,tmp_buff
m33ef   ld      a,(hl)          ; get pattern
	push    bc
	push    hl
	ld      hl,tmp_buff+5
	xor     (hl)            ; invert if required
	call    m33b9           ; output byte
	pop     hl
	pop     bc
	inc     hl
	djnz    m33ef           ; loop back
	pop     de
	pop     hl
	pop     af
	ret

; Subroutine to get attribute byte in E for screen address in HL

m3402   push    af
	ld      a,h
	and     $18
	srl     a
	srl     a
	srl     a
	or      $58             ; address attribs
	ld      h,a
	ld      e,(hl)          ; get attrib
	pop     af
	ret

; Subroutine to clear a 4-byte area at tmp_buff

m3412   push    hl
	ld      hl,tmp_buff
	ld      b,$04
m3418   ld      (hl),$00        ; clear the buffer
	inc     hl
	djnz    m3418
	pop     hl
	ret

; Subroutine to shift patterns in buffer left 3 bits

m341f   push    hl
	push    bc
	ld      hl,tmp_buff
	ld      b,$04
m3426   sla     (hl)            ; shift left
	sla     (hl)
	sla     (hl)
	inc     hl
	djnz    m3426
	pop     bc
	pop     hl
	ret

; Subroutine to shift patterns in buffer left 2 bits

m3432   ld      hl,tmp_buff
	ld      b,$04
m3437   sla     (hl)            ; shift left
	sla     (hl)
	inc     hl
	djnz    m3437
	ret

; Subroutine to merge required pattern for pixel into buffer at tmp_buff

m343f   push    de              ; save registers
	push    hl
	ld      a,d
	and     (hl)            ; mask required pixel
	ld      a,e             ; A=attribute
	jr      nz,m344c        ; move on if need ink
	srl     a               ; shift paper colour to ink position
	srl     a
	srl     a
m344c   and     $07             ; mask off ink/paper colour as required
	bit     6,e             ; check BRIGHT
	jr      z,m3454
	or      $08             ; add 8 if bright
m3454   ld      hl,m346f        ; address of colour offsets table
	ld      d,$00
	ld      e,a
	add     hl,de
	ld      e,(hl)          ; DE=offset into pattern table
	ld      hl,m347f
	add     hl,de           ; HL=required pattern address
	ld      b,$04
	ld      de,tmp_buff
m3465   ld      a,(de)
	or      (hl)
	ld      (de),a          ; merge pattern into buffer
	inc     hl
	inc     de
	djnz    m3465
	pop     hl              ; restore registers
	pop     de
	ret

; Table of offsets into following pattern table

m346f   defb    $00,$04,$08,$0c
	defb    $10,$14,$18,$1c
	defb    $20,$24,$28,$2c
	defb    $30,$34,$38,$3c

; Pattern table for expanded copy

m347f   defb    $07,$07,$07,$07 ; black
	defb    $07,$05,$07,$07 ; blue
	defb    $03,$07,$06,$07 ; red
	defb    $07,$03,$06,$03 ; magenta
	defb    $06,$03,$06,$03 ; green
	defb    $06,$05,$02,$05 ; cyan
	defb    $02,$05,$02,$05 ; yellow
	defb    $01,$06,$03,$04 ; white
	defb    $07,$07,$07,$07 ; black
	defb    $05,$02,$03,$04 ; bright blue
	defb    $06,$01,$02,$01 ; bright red
	defb    $01,$04,$02,$04 ; bright magenta
	defb    $04,$00,$04,$01 ; bright green
	defb    $01,$00,$04,$00 ; bright cyan
	defb    $00,$02,$00,$00 ; bright yellow
	defb    $00,$00,$00,$00 ; bright white

; Raster line header for expanded copy

m34bf   defb    $0d,$0a         ; CRLF
	defb    $1b,'L',$00,$03 ; 768 bytes in 120dpi mode
	defb    $ff

; CAT "T:" routine

m34c6   ld      bc,$0011
	rst     $28
	DW      $0030           ; make space for tape header
	push    de
	pop     ix              ; IX=address of space
m34cf   ld      a,$0d
	rst     $28
	DW      $0010           ; output CR
m34d4   ld      a,$7f
	in      a,($fe)
	rra
	jr      c,m34e3         ; move on if BREAK not pressed
	ld      a,$fe
	in      a,($fe)
	rra
	jr      c,m34e3         ; move on if BREAK not pressed
	ret                     ; done
m34e3   ld      a,$00
	ld      de,$0011
	scf
	push    ix
	rst     $28
	DW      $0556           ; read a header
	pop     ix
	jr      nc,m34d4        ; loop back if failed
	push    ix
	ld      a,$22
	rst     $28
	DW      $0010           ; output quote
	ld      b,$0a           ; name length 10
m34fb   ld      a,(ix+$01)
	rst     $28
	DW      $0010           ; output next byte
	inc     ix
	djnz    m34fb           ; loop back
	pop     ix
	ld      hl,m35a1
	call    m3591           ; output quote and space
	ld      a,(ix+$00)      ; get file type
	cp      $00
	jr      nz,m3537        ; move on if not program
	ld      a,(ix+$0e)
	cp      $80
	jr      z,m352f         ; move on if no auto-run line number
	ld      hl,m35be
	call    m3591           ; display "LINE" message
	ld      c,(ix+$0d)
	ld      b,(ix+$0e)
	call    m359a           ; output line number
	ld      a,' '
	rst     $28
	DW      $0010           ; output space
m352f   ld      hl,m35a4
	call    m3591           ; output "BASIC" message
	jr      m34cf           ; loop back
m3537   cp      $01
	jr      nz,m3554        ; move on if not number array
	ld      hl,m35ad
	call    m3591           ; output "DATA" message
	ld      a,(ix+$0e)
	and     $7f
	or      $40
	rst     $28
	DW      $0010           ; output variable name
	ld      hl,m35b9+1
	call    m3591           ; output "()" message
	jp      m34cf           ; loop back
m3554   cp      $02
	jr      nz,m3571        ; move on if not character array
	ld      hl,m35ad
	call    m3591           ; output "DATA" message
	ld      a,(ix+$0e)
	and     $7f
	or      $40
	rst     $28
	DW      $0010           ; output variable name
	ld      hl,m35b9
	call    m3591           ; output "$()" message
	jp      m34cf           ; loop back
m3571   ld      hl,m35b3
	call    m3591           ; output "CODE" message
	ld      c,(ix+$0d)
	ld      b,(ix+$0e)
	call    m359a           ; output load address
	ld      a,','
	rst     $28
	DW      $0010           ; output comma
	ld      c,(ix+$0b)
	ld      b,(ix+$0c)
	call    m359a           ; output length
	jp      m34cf           ; loop back

; Subroutine to output a null-terminated string

m3591   ld      a,(hl)          ; get next char
	or      a
	ret     z               ; exit if null
	rst     $28
	DW      $0010           ; output char
	inc     hl
	jr      m3591           ; loop back

; Subroutine to output number in BC

m359a   rst     $28
	DW      $2d2b           ; stack number on calculator
	rst     $28
	DW      $2de3           ; output number
	ret

; Messages for tape catalogs

m35a1   defm    $22, " ", $00
m35a4   defm    "(BASIC) ", 0
m35ad   defm    "DATA ", 0
m35b3   defm    "CODE ", 0
m35b9   defm    "$() ", 0
m35be   defm    "LINE ", 0

  IF garry
	rst     $28
	DW      $2bf1
	push    bc
	push    de
	rst     $28
	DW      $1e94
	pop     de
	pop     bc
	call    m3f00
	DW      $0056
m35d3   jp      nc, m0edb
	ret
m35d7   rst     $28
	DW      $1e94
	call    l3f00
	DW      $0059
	jr      m35d3
	rst     $18
	cp      '#'
	jr      z, m35f0
	rst     $28
	DW      $1c82
	call    m10b1
	rst     $28
	DW      $1e67
	ret
m35f0   call    m111c
	call    m10b1
	ld      de, m04d5
	ld      bc, 13
	call    m14f0
	rst     $28
	DW      $1e94
	rst     $28
	DW      $1601
	rst     $28
	DW      $2da2
	push    bc
	rst     $28
	DW      $2da2
	push    bc
	pop     hl
	pop     de
	ld      b, 1
	call    m3f00
	DW      $0062
	ret

; ####################################################################################################################

is_tap:		xor	a
		ld	(tapeloader_stat1),a
		dec	de			;veo si termina en .tap
		ld	a,(de)
		and	$df
		cp	'P'			;me baso en que termine solo con la letra "P" por ".TAP"
		jr	nz,notap		;si no es otra cosa no lo considero como ".TAP" solo
						;espero que el usuario sea prolijo y haya proporcionado
		ld	bc,$0000		;un nombre de archivo en el argumento del tipo "*.TAP"
		ld	(save_POS_FILE+0),bc
		ld	(save_POS_FILE+2),bc
		
		ld	b,$06
		call	m3f00
		DW  	DOS_ABANDON
		
		ld	bc,$0601        	; abro el archivo lectura exc. con handle: 06
		ld	a,c
		ld	(tapeloader_stat1),a	; 1=en pag 7 me indicará que estoy cargando desde cinta
		ld	de,$0002		; error si no existe + puntero en 0
		ld	hl,tmp_file
		call	m3f00
		DW  	DOS_OPEN        	; open file
		jr	c,ok_opentap		; salto adelante si todo Ok

		call    m2b64           	; page in normal memory
		call    m0e9a           	; cause DOS error
		defb    $ff

ok_opentap:	ld	a,(LODDRV)
		ld	(tapeloader_stat2),a
		ld	a,'T'
		ld      (LODDRV), a		; cambio a T: el "drive" por defecto

		call    m32ee			; esta es la forma de retornar
		call    m2b64			; al basic
		ret

notap:		ld      hl, tmp_file		; trato de continuar como si "aqui no paso nada"
		ret

; ####################################################################################################################

	defs    103 - ($-is_tap)

	IF alternative

rest_ink:	ld	hl,strver
		call    m24b5
		ld	a,$10			; *2
		rst	$10			; *1
		ld	a,defink		; *2
		rst	$10			; *1
		jp	m2b64			; *3 - page in normal memory
		defs	3			; *9

	ELSE

m368c:	   	defm	"Physical drives: ", 0	; *18

	ENDIF

m369e   defm    " floppy, ", 0
m36a8	defm    " IDE"

	IF alternative

		defm	", ",0,0

strver:		defm	", v"
		defb	($30 + VMAYOR)
		defb	"."
		defb	($30 + VMINOR)
		defb	($30 + VPATCH)
		defm	VRPATCH
		defs	5

	ELSE

		defm	"Logical drives: ", 0

	ENDIF

m36bd   defm    "Really format hard disk (Y/N)?", 0
m36dc   defm    "Really delete partition (Y/N)?", 0
m36fb   defm    "Invalid partitio", 'n'+$80
m370c   defm    "Partition already exist", 's'+$80
m3724   defm    "Not implemente", 'd'+$80
m3733   defm    "Partition ope", 'n'+$80
m3741   defm    "Out of partition handle", 's'+$80
m3759   defm    "Not a swap partitio", 'n'+$80
m376d   defm    "Drive already mappe", 'd'+$80
m3781   defm    "Out of XDPB", 's'+$80
m378d   defm    "No swap partitio", 'n'+$80
m379e   defm    "Invalid devic", 'e'+$80

	defs    $31

	;CORREGIDO MANUALMENTE DE ACA PARA ABAJO

m37dd   call    m24b5
	ld      hl, FLAGS
	res     5, (hl)
m37e5   bit     5, (hl)
	jr      z, m37e5
	res     5, (hl)
	ld      a, (LAST_K)
	and     $df
	cp      'N'
	jr      z, m37f8
	cp      'Y'
	jr      nz, m37e5
m37f8   push    af
	rst     $28
	DW      $0d6e
	pop     af
	ret

m37fe   rst     $28
	DW      $0d6e
	ld      hl, m36bd
	call    m37dd
	push    af
	ld      bc, 0
	ld      a, (FLAGS3)
	bit     6, a
	jr      z, m3815
	rst     $28
	DW      $1e99
m3815   push    bc
	rst     $28
	DW      $1e99
	push    bc
	rst     $28
	DW      $1e94
	ld      c, a
	pop     hl
	pop     de
	pop     af
	cp      'N'
	ret     z
m3824   push    hl
	push    bc
	push    de
	ld      b, 7
	ld      hl, $ed11
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $01a2
	cp	$42
	scf
	ccf
	jp	nz,m395a
	ld      ix, $ed11
	ld      h, (ix+$03)
	ld      l, (ix+$06)
	ld      e, (ix+$01)
	inc	ixh
	ld	d, (ix+$00)
	call    m32ee
	call    m2b64
	ex      (sp), hl
	and     a
	sbc     hl, de
	jp      nc, m38c7
	add     hl, de
	ld      a, h
	or      l
	jr      z, m3862
	ex      de, hl
	pop     hl
	set     7, h
	jr      m3863
m3862   pop     hl
m3863   pop     bc
	ld      a, c
	pop     bc
	push    de
	pop     ix
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00b2
	call    m32ee
	call    m2b64
	ret     c
m387b   call    m0ecb
	rst     $38
	rst     $18
	cp      $e4
	jr      z, m388e
	cp      $b9
	jr      z, m388e
	call    m10b1
	jp      m2280
m388e   push    af
	rst     $20
	rst     $28
	DW      $1c8c
	rst     $18
	cp      ','
	jp      nz, m1125
m3899   rst     $20
	rst     $28
	DW      $1c82
	call    m10b1
	rst     $28
	DW      $1e99
	push    bc
	call    m3965
	jp      nz, m398a
	pop     hl
	pop     af
	cp      $b9
	ld      a, 2
	ld      bc, 17
	jr      z, m38ba
	ld      a, 3
	ld      bc, 17
m38ba   call    m2b89
	ld      ($efa8), a
	push    hl
	and     a
	sbc     hl, bc
	pop     hl
	jr      c, m38ce
m38c7   call    m2b64
	call    m2ada
	ld      a, (bc)
m38ce   ld      a, h
	or      l
	jr      z, m38c7
	push    de
	push    hl
	ld      hl, m3d8b
	ld      de, $efb8
	ld      bc, $1d
	ldir
	pop     hl
	push    hl
	ld      d, l
	ld      e, 0
	ld      a, l
	cp      5
	jr      c, m3903
	cp      9
	jr      c, m38fa
	srl     d
	rr      e
	ld      bc, $3f06
	ld      h, 3
	ld      a, $c0
	jr      m390c
m38fa   ld      bc, $1f05
	ld      h, 1
	ld      a, $f0
	jr      m390c
m3903   sla     d
	ld      bc, $0f04
	ld      h, 0
	ld      a, $ff
m390c   ld      ($efba), bc
	ld      ($efc1), a
	ld      a, h
	ld      ($efbc), a
	dec     de
	ld      b, l
	xor     a
m391a   add     a, $10
	djnz    m391a
	and     a
	jr      nz, m3925
	dec     a
	ld      de, $07f7
m3925   ld      ($efca), a
	ld      ($efbd), de
	pop     hl
	add     hl, hl
	add     hl, hl
	add     hl, hl
	dec     hl
	ld      ($efb0), hl
	ld      a, $ff
	ld      ($efaf), a
	pop     af
	push    af
	ld      hl, $ef98
	call    m32b6
	call    m3f00
	DW      $00b8
	call    m32ee
	jr      nc, m395d
	pop     af
	ld      l, $e5
	ld      ix, $0020
	call    m32b6
	call    m3f00
	DW      $00bb
m395a   call    m32ee
m395d   call    m2b64
	ret     c
	call    m0ecb
	rst     $38
m3965   rst     $28
	DW      $2bf1
	ld      a, b
	or      c
	jr      nz, m3970
	call    m2ada
	inc     l
m3970   inc     de
	ld      a, (de)
	dec     de
	cp      '>'
	ld      a, 0
	jr      nz, m398e
m3979   ld      a, (de)
	inc     de
	inc     de
	dec     bc
	dec     bc
	sub     '0'
	jr      z, m398e
	cp      1
	jr      z, m398e
	ld      d, a
	cp      5
	ret     c
m398a   call    m2ada
	ld      e, c
m398e   push    af
	ld      b, 0
	ld      a, c
	cp      $11
	jr      c, m3998
	ld      a, $10
m3998   ex      de, hl
	ld      de, $ef98
	call    m3f63
	di
	ld      a, (BANKM)
	or      7
	ld      bc, $7ffd
	out     (c), a
	ex      de, hl
	ld      d, $10
m39ad   ld      (hl), $20
	inc     hl
	dec     d
	jr      nz, m39ad
	ld      a, (BANKM)
	out     (c), a
	ei
	pop     de
	ret
m39bb   rst     $18
	cp      $eb
	jp      z, m3df2
	cp      $bf
	jr      z, m39dc
	cp      $df
	jr      z, m3a1f
	cp      $c4
	jr      z, m3a3d
	cp      $cc
	jp      nz, m1125
	rst     $20
	rst     $28
	DW      $1c8c
	call    m10b1
	jp      m04e5
m39dc   rst     $20
	rst     $28
	DW      $1c8c
	call    m3a81
	call    m10b1
	call    m3965
	ld      a, d
	push    af
	jr      nz, m3a03
	ld      hl, $ef98
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00b5
	call    m32ee
	call    m2b64
	jr      nc, m3a1b
m3a03   push    bc
	call    m3a8e
	pop     bc
	pop     af
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00f1
	call    m32ee
	call    m2b64
	ret     c
m3a1b   call    m0ecb
	rst     $38
m3a1f   rst     $20
	call    m3a81
	call    m10b1
	call    m3a8e
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00f4
	call    m32ee
	call    m2b64
	jr      nc, m3a1b
	ret
m3a3d   rst     $20
	call    m10b1
	ld      hl, m36dc
	call    m37dd
	push    af
	call    m3965
	jp      nz, m398a
	pop     af
	cp      'N'
	ret     z
	ld      a, d
	push    af
	ld      hl, $ef98
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00b5
	call    m32ee
	call    m2b64
	jr      nc, m3a7d
	pop     af
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00be
	call    m32ee
	call    m2b64
	ret     c
m3a7d   call    m0ecb
	rst     $38
m3a81   rst     $18
	ld      hl, FLAGS3
	res     6, (hl)
	cp      $b5
	ret     nz
	set     6, (hl)
	rst     $20
	ret
m3a8e   rst     $28
	pop     af
	dec     hl
	dec     bc
	dec     bc
	ld      a, b
	or      c
	jr      nz, m3aab
	inc     de
	ld      a, (de)
	cp      ':'
	jr      nz, m3aab
	dec     de
	ld      a, (de)
	and     $df
	cp      'A'
	jr      c, m3aab
	cp      'Q'
	jr      nc, m3aab
	ld      l, a
	ret
m3aab   call    m2ada
	ld      c, (hl)
cmdspec rst     $18
	cp      $ab		; ATTR
	ld      bc, $1ff
	jr      z, m3b08
	cp      $d9		; INK
	ld      bc, $0107
	jr      z, m3b08
	cp      $da		; PAPER
	ld      bc, $0407
	jr      z, m3b08
	cp      $db		; FLASH
	ld      bc, $0801
	jr      z, m3b08
	cp      $dc		; BRIGHT
	ld      bc, $0701
	jr      z, m3b08
	cp      $0d		; ENTER
	jr      z, m3adb
	cp      ':'
	jr      nz, m3ae1
m3adb   call    m10b1
	jp      m1465
m3ae1   rst     $28
	DW      $1c8c
	call    m10b1
	rst     $28
	DW      $2bf1
	ex      de, hl
	ld      de, tmp_file
	call    m3f63
	call    m2b89
	ld      a, $ff
	ld      (de), a
	
; #############################################################################        
	;ld      hl, tmp_file
	call	is_tap
; #############################################################################

	call    m32b6
	call    m3f00
	DW      $00fd
	call    m32ee
	jp      m3d21

;CAMBIA/STORE ATRIBUTOS
m3b08   push    bc	
	rst     $20
	rst     $28
	DW      $1c82
	call    m3a81
	call    m10b1
	rst     $28
	DW      $1e94
	pop     bc
	ld      d, a
	ld      a, c
	cp      d
	jr      nc, m3b20
	call    m2ada
	inc     de
m3b20   dec     b
	jr      z, m3b29
	rlc     d
	rlc     c
	jr      m3b20
m3b29   call    m2b89
	ld      a, c
	cpl
	ld      c, a
	ld      a, (ed_ATTR_T)
	and     c
	or      d
	ld      (ed_ATTR_T), a
	ld      (ed_ATTR_P), a
	call    m2b64
	ld      hl, FLAGS3
	bit     6, (hl)
	ret     z
	ld      h, a
	xor     a
	jp      m3e5c

m3b48   cp      $ad		; TAB
	jr      z, m3b64
	cp      $b5		; ASN
	jp      z, m3da8
	call    m2ada
	dec     bc
n3b4d:	cp      $b9		; EXP
	jp      z, m062b
	cp      $b5		; ASN
	jp      z, m3da8
	cp      $ad		; TAB
	jp      nz, m05f8
m3b64   rst     $20
	cp      $b9		; EXP
	jr      nz, m3b6f
	rst     $20
	ld      hl, FLAGS3
	set     6, (hl)
m3b6f   call    m10b1
	call    m2b89
	ld      b, 2
m3b77   push    bc
	ld      a, 2
	sub     b
	ld      de, 0
	push    de
	push    af
	ld      hl, m3d48
	call    m07d7
	pop     af
	push    af
	ld      c, a
	add     a, $30
	call    m07cf
	ld      b, 7
	ld      hl, $ed11
	call    m32b6
	call    m3f00
	DW      $01a2
	call    m32ee
	cp	$42
	scf
	ccf
	jr      nz, m3c06
	ld      ix, $ed11
	ld      hl, m3d77
	call    m07d7
	ld      l, (ix+$01)
	inc	ixh
	ld      h, (ix+$00)
	dec	ixh
	ld      e, $ff
	call    m07df
	ld      a, '/'
	call    m07cf
	ld      h, 0
	ld      l, (ix+$03)
	ld      e, $ff
	call    m07df
	ld      a, '/'
	call    m07cf
	ld      h, 0
	ld      l, (ix+$06)
	ld      e, $ff
	call    m07df
	ld      a, ')'
	call    m07cf
	ld      a, 13
	call    m07cf
m3bdd   ld      bc, 0
m3be0   pop     af
	push    af
	ld      hl, $ef98
	call    m32b6
	call    m3f00
	DW      $00c4
	call    m32ee
	jp      nc, m3d15
	ld      ix, $ef98
	ld      a, (ix+$10)
	cp      0
	jr      nz, m3c0e
	pop     af
	pop     de
	inc     de
	push    de
	push    af
	jp      m3cf9
m3c06   ld      hl, m3d7a
	call    m07d7
	jr      m3bdd
m3c0e   push    bc
	ld      hl, $ef98
	ld      e, $10
m3c14   ld      a, (hl)
	inc     hl
	and     a
	jr      nz, m3c1b
	ld      a, $7e
m3c1b   call    m07cf
	dec     e
	jr      nz, m3c14
	ld      a, $20
	call    m07cf
	ld      l, (ix+$18)
	ld      h, (ix+$19)
	ld      e, (ix+$1a)
	ld      a, e
	or      h
	jr      nz, m3c46
	ld      h, l
	ld      l, (ix+$17)
	inc     hl
	srl     h
	rr      l
	ld      e, $20
	call    m07df
	ld      hl, m3d2b
	jr      m3c63
m3c46   xor     a
	srl     e
	rr      h
	rr      l
	rra
	srl     e
	rr      h
	rr      l
	rra
	srl     e
	rr      h
	rr      l
	ld      e, $20
	call    m07df
	ld      hl, m3d27
m3c63   call    m07d7
	ld      a, (ix+$10)
	cp      1
	ld      hl, m005e
	jr      z, m3c8f
	cp      2
	ld      hl, m3d52
	jr      z, m3c8f
	cp      3
	ld      hl, m3d57
	jr      z, m3c8f
	cp      $fe
	ld      hl, m3d5d
	jr      z, m3c8f
	cp      $ff
	ld      hl, m3d63
	jr      z, m3c8f
	ld      hl, m3d68
m3c8f   call    m07d7
	ld      a, (ix+$10)
	cp      3
	jr      nz, m3ca7
	ld      a, (ix+$3c)
	and     a
	jr      z, m3ca7
	call    m07cf
	ld      a, ':'
	call    m07cf
m3ca7   ld      a, 13
	call    m07cf
	ld      hl, FLAGS3
	bit     6, (hl)
	jr      z, m3cf8
	ld      hl, m0056
	call    m07d7
	ld      l, (ix+$11)
	ld      h, (ix+$12)
	ld      e, $ff
	call    m07df
	ld      a, ','
	call    m07cf
	ld      l, (ix+$13)
	ld      h, 0
	ld      e, $ff
	call    m07df
	ld      hl, m3d70
	call    m07d7
	ld      l, (ix+$14)
	ld      h, (ix+$15)
	ld      e, $ff
	call    m07df
	ld      a, ','
	call    m07cf
	ld      l, (ix+$16)
	ld      h, 0
	ld      e, $ff
	call    m07df
	ld      a, 13
	call    m07cf
m3cf8   pop     bc
m3cf9   inc     bc
	ld      a, b
	or      c
	jp      nz, m3be0
m3cff   pop     af
	pop     hl
	ld      e, $ff
	call    m07df
	ld      hl, m3d2e
	call    m07d7
m3d0c   pop     bc
	dec     b
	jp      nz, m3b77
	call    m2b64
	ret
m3d15   cp      $38
	jr      z, m3cff
	cp      $16
	jr      nz, m3d21
	pop     af
	pop     hl
	jr      m3d0c
m3d21   call    m2b64
	jp      m387b
m3d27   defm    "Mb ", 0
m3d2b   defm    "K ", 0
m3d2e   defm    " free partition entries", 13, 13, 0
m3d48   defm    "IDE unit ", 0
m3d52   defm    "swap", 0
m3d57   defm    "data ", 0
m3d5d   defm    "*BAD*", 0
m3d63   defm    "FREE", 0
m3d68   defm    "unknown", 0
m3d70   defm    " End: ", 0
m3d77   defm    " (", 0
m3d7a   defm    " (not detected)", 13, 0
m3d8b   defb    0, 2, 0, 0, 0, 0, 0, $ff, 1, 0, 0, 0, $80, 0
	defb    0, 2, 3, 0, 0, $80, 0, 0, 2, 0, 0, 0, 0, 0, 0
m3da8   rst     $20
	call    m10b1
	call    m2b89
	ld      b, $10
	ld      l, 'A'
m3db3   push    bc
	push    hl
	ld      bc, $ef98
	call    m32b6
	call    m3f00
	DW      $00f7
	call    m32ee
	jp      nc, m3d21
	jr      z, m3de9
	pop     hl
	push    hl
	ld      a, l
	call    m07cf
	ld      a, ':'
	call    m07cf
	ld      a, ' '
	call    m07cf
	ld      hl, $ef98
	ld      b, $12
m3ddd   ld      a, (hl)
	call    m07cf
	inc     hl
	djnz    m3ddd
	ld      a, 13
	call    m07cf
m3de9   pop     hl
	pop     bc
	inc     l
	djnz    m3db3
	call    m2b64
	ret
m3df2   rst     $20
	rst     $28
	DW      $1c8c
	call    m10b1
	rst     $28
	DW      $2bf1
	ld      b, 0
	ld      a, c
	and     a
	jr      nz, m3e06
	call    m2ada
	inc     l
m3e06   cp      $10
	jr      c, m3e0c
	ld      c, $10
m3e0c   push    bc
	ex      de, hl
	ld      de, tmp_file
	call    m3f63
	call    m3965
	ld      a, d
	jp      nz, m398a
	push    af
	ld      hl, $ef98
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00b5
	call    m32ee
	call    m2b64
	jr      nc, m3e58
	pop     af
	pop     de
	call    m2b89
	ld      hl, tmp_file
	add     hl, de
	ld      d, a
	ld      a, $11
	sub     e
m3e3f   ld      (hl), $20
	inc     hl
	dec     a
	jr      nz, m3e3f
	ld      a, d
	ld      hl, tmp_file
	call    m32b6
	call    m3f00
	DW      $00c1
	call    m32ee
	call    m2b64
	ret     c
m3e58   call    m0ecb
	rst     $38

m3e5c   ld      b, a
	ld      c, a
	ld      l, 8
	call    m2b89
	call    m32b6
	call    m3f00
	DW      $00d6
	ld      a, (ATTR_P)
	ld      h, a
	ld      l, 9
	xor     a
	ld      b, a
	ld      c, a
	call    m3f00
	DW      $00d6
	call    m32ee
	call    m2b64
	ret
  
ELSE

; The "COPY RANDOMIZE" command
; This is a silly command

m35c4   ld      c,$40           ; loop timing values
m35c6   ld      b,$00
m35c8   ld      a,$fe
	in      a,($fe)
	bit     3,a
	jr      nz,m35dc        ; move on if "C" not pressed
	ld      a,$bf
	in      a,($fe)
	bit     3,a
	jr      nz,m35dc        ; move on if "J" not pressed
	bit     1,a             ; move on if "L" not pressed
	jr      z,m35e5         ; go to silly routine if "CJL" held
m35dc   djnz    m35c8           ; loop back
	dec     c
	jr      nz,m35c6        ; loop back
	call    m2ada
	defb    $0b             ; nonsense in BASIC error
m35e5
      IF v41 || spanish
	ld      ix,$2000
      ENDIF
	xor     a
	out     ($fe),a         ; black border
	ld      hl,$5800
	ld      de,$5801
	ld      bc,$02ff
	ld      (hl),$00
	ldir                    ; black screen
	ld      hl,$4000
	ld      de,$4001
	ld      bc,$17ff
	ld      (hl),$ff
	ldir                    ; fill screen with ink
	ld      c,$07           ; base ink colour
m3604   ld      a,c
	and     $07
	jr      nz,m360c
	inc     c               ; increment base ink colour
	jr      m3604
m360c   ld      d,$58           ; high byte of start of attribs
	ld      hl,m3631        ; table of attrib offsets
m3611   ld      e,(hl)
	push    af
      IF v41 || spanish
	ld      a,$fb
	in      a,($fe)
	rra
	jr      c,x37c5
	pop     af
	ld      hl,$5800
	ld      de,$5801
	ld      bc,$02ff
	ld      (hl),$38
	ldir
	call    m2ada
	rst     $38
x37c5   ld      a,$ef
	in      a,($fe)
	rra
	jr      c,x37ce
	inc     ix
x37ce   ld      a,$df
	in      a,($fe)
	rra
	jr      c,m3613
	dec     ix
      ENDIF
m3613   ld      a,e
	or      a
	jr      nz,m3624        ; if not end of third, move on
	inc     d               ; get to next third
	ld      a,d
	cp      $5b
	jr      nz,m3621        ; move on if still in attribs area
	pop     af
	inc     c               ; increment base ink colour
      IF v41 || spanish
	push    af
	push    bc
	push    ix
	pop     bc
x37e8   dec     bc
	ld      a,b
	or      c
	jr      nz,x37e8
	pop     bc
	pop     af
      ENDIF
	jr      m3604           ; loop back
m3621   pop     af
	jr      m362e           ; go to loop back for another offset
m3624   pop     af
	and     $07
	or      a
	jr      nz,m362c        ; is ink colour 0 or 8?
	ld      a,$07           ; set to white if so
m362c   ld      (de),a          ; set attribute
	dec     a               ; decrement ink colour
m362e   inc     hl              ; increment table pointer
	jr      m3611           ; loop back

; The table of attribute positions for the COPY RANDOMIZE routine

m3631   defb    $21,$41,$61,$81
	defb    $a1,$c1,$e1,$82
	defb    $83,$84,$25,$45
	defb    $65,$85,$a5,$c5
	defb    $e5,$27,$47,$67
	defb    $87,$a7,$c7,$e7
	defb    $28,$29,$2a,$2b
	defb    $88,$89,$8a,$e8
	defb    $e9,$ea,$eb,$2d
	defb    $4d,$6d,$8d,$ad
	defb    $cd,$ed,$ee,$ef
	defb    $f0,$f1,$33,$53
	defb    $73,$93,$b3,$d3
	defb    $f3,$f4,$f5
	defb    $f6,$f7,$59,$79
	defb    $99,$b9,$d9,$3a
	defb    $3b,$3c,$3d,$fa
	defb    $fb,$fc,$fd,$5e
	defb    $7e,$9e,$be,$de
	defb    $00              ; end of first screen third
	defb    $21,$22,$23,$24
	defb    $25,$43,$63,$83
	defb    $a3,$c3,$e3,$27
	defb    $47,$67,$87,$a7
	defb    $c7,$e7,$88,$89
	defb    $8a,$2b,$4b,$6b
	defb    $8b,$ab,$cb,$eb
	defb    $2d,$4d,$6d,$8d
	defb    $ad,$cd,$ed,$2e
	defb    $2f,$30,$31,$8e
	defb    $8f,$90,$ee,$ef
	defb    $f0,$f1,$33,$53
	defb    $73,$93,$b3,$d3
	defb    $f3,$34,$35
	defb    $36,$94,$95,$96
	defb    $57,$77,$b5,$d6
	defb    $f7,$39,$59,$79
	defb    $99,$b9,$d9,$f9
	defb    $3a,$3b,$3c,$3d
	defb    $9a,$9b,$9c,$fa
	defb    $fb,$fc,$fd,$00 ; end of second screen third
	defb    $20,$40,$60,$80
	defb    $a0,$c0,$e0,$22
	defb    $24,$44,$64,$84
	defb    $a4,$c4,$e4,$45
	defb    $66,$47,$28,$48
	defb    $68,$88,$a8,$c8
	defb    $e8,$4c,$6c,$8c
	defb    $ac,$cc,$ec,$2d
	defb    $2e,$2f,$8d,$8e
	defb    $8f,$50,$70,$90
	defb    $b0,$d0,$f0,$94
	defb    $95,$96,$97,$98
	defb    $56,$76,$b6,$d6
	defb    $5a,$3b,$3c,$3d
	defb    $5e,$7e,$9d,$9c
	defb    $be,$de,$fd,$fc
	defb    $fb,$da,$00     ; end of table

m3713   defs    $19
    IF v41
      IF spanish
m372c   defs    $00bf
      ELSE
m372c   defs    $0104
      ENDIF
    ELSE
      IF spanish
m372c   defs    $0250
      ELSE
m372c   defs    $02d4
      ENDIF
    ENDIF

; The printer input (m3a00) and output (m3a05) routines
; This is a copy of a routine in ROM 3 which switches in this ROM,
; at which point it takes over

m3a00   ld      hl,m3d03        ; printer input routine
	jr      m3a08
m3a05   ld      hl,m3d06        ; printer output routine
m3a08   ex      af,af'
	ld      bc,$1ffd
	ld      a,(BANK678)
	push    af
	and     $fb             ; select ROM 1
	di
	ld      (BANK678),a
	out     (c),a
	jp      m3d00           ; this ROM takes over at this point
m3a1b   ex      af,af'
	pop     af
	ld      bc,$1ffd
	di
	ld      (BANK678),a     ; previous value
	out     (c),a           ; at this point, control passes back to ROM 3
	ei
	ex      af,af'
	ret

	defs    $02d7

m3d00   jp      m2b09           ; call the required routine
m3d03   jp      m1e70           ; jump to input routine
m3d06   jp      m1f6e           ; jump to output routine

	defs    $0177
  ENDIF

; Subroutine to call a subroutine in ROM 0
; The subroutine address is inline after the call to this routine

m3e80   ld      (OLDHL),hl      ; save HL in OLDHL
	ld      (OLDBC),bc      ; save BC in OLDBC
	push    af
	pop     hl
	ld      (OLDAF),hl      ; save AF in OLDAF
	ex      (sp),hl         ; HL=address of inline address
	ld      c,(hl)
	inc     hl
	ld      b,(hl)          ; BC=inline ROM 0 address
	inc     hl
	ex      (sp),hl         ; stack return address
	push    bc
	pop     hl              ; HL=routine address in ROM 0
	ld      a,(BANKM)
	and     $ef
	di
	ld      (BANKM),a
	ld      bc,$7ffd
	out     (c),a           ; page in ROM 0

; The rest of the routine continues at $3ea2 in ROM 0
; The following is a continuation of a mirrored routine in ROM 0 for
; calling this ROM

m3ea2   ei
	ld      bc,$3eb5
	push    bc              ; stack return add to swap back ROMs
	push    hl              ; stack routine address
	ld      hl,(OLDAF)
	push    hl
	pop     af              ; restore AF
	ld      bc,(OLDBC)      ; restore BC
	ld      hl,(OLDHL)      ; restore HL
	ret                     ; execute routine in this ROM

; This part is the routine which returns control to ROM 0

m3eb5   push    af              ; save AF & BC
	push    bc
	ld      a,(BANKM)
	and     $ef
	di
	ld      (BANKM),a
	ld      bc,$7ffd
	out     (c),a           ; page back ROM 0

; The rest of the routine continues at $3ec5 in ROM 0
; The following is a continuation of a mirrored routine in ROM 0 for
; returning to this ROM

m3ec5   ei
	pop     bc              ; restore registers
	pop     af
	ret                     ; return

      IF garry
m3291   defm    $0d, "  1 file copied.", $0d, $0d, 0
m32a5   defm    " files copied.", $0d, $0d, 0
	defs    $12
      ELSE
	defs    $37
      ENDIF
; Subroutine to call a subroutine in ROM 2
; The subroutine address is inline after the call to this routine
; This routine is duplicated in ROMs 0 & 2, so that when we start switching
; (first to ROM 0, then to ROM 2) there is no problem.

m3f00:  ld      (OLDHL),hl      ; save HL,BC and AF
	ld      (OLDBC),bc
	push    af
	pop     hl
	ld      (OLDAF),hl
	ex      (sp),hl
	ld      c,(hl)
	inc     hl
	ld      b,(hl)          ; BC=inline address
	inc     hl
	ex      (sp),hl         ; restack updated return address
      IF garry
	ld      hl, m3f42
	push    hl
      ENDIF
	push    bc
	pop     hl              ; HL=address to call in ROM
	ld      a,(BANKM)
	and     $ef
	di
	ld      (BANKM),a
	ld      bc,$7ffd
	out     (c),a           ; page in ROM 0
	ld      a,(BANK678)
	or      $04
	ld      (BANK678),a
m3f2a   ld      bc,$1ffd
	out     (c),a           ; page in ROM 2
	ei
      IF garry=0
	ld      bc,m3f42
	push    bc              ; stack routine address to return to ROM 1
      ENDIF
	push    hl              ; stack routine address to call in ROM 2
	ld      hl,(OLDAF)      ; restore registers
	push    hl
	pop     af
	ld      bc,(OLDBC)
	ld      hl,(OLDHL)
	ret                     ; exit to routine

; This part of the routine returns control to ROM 1

m3f42   push    bc              ; save registers
	push    af
	ld      a,(BANK678)
	and     $fb
	di
	ld      (BANK678),a
	ld      bc,$1ffd
	out     (c),a           ; page in ROM 0
	ld      a,(BANKM)
	or      $10
	ld      (BANKM),a
	ld      bc,$7ffd
	out     (c),a           ; page in ROM 1
	ei
	pop     af              ; restore registers
	pop     bc
	ret                     ; done!

; Subroutine to copy a block of memory from HL in page 0 to
; DE in page 7 (length BC bytes)

m3f63   di                      ; ensure interrupts disabled
	exx
	ld      bc,$7ffd        ; BC'=paging port
	exx
m3f69   exx
	ld      a,$10
	out     (c),a           ; page in page 0
	exx
	ld      a,(hl)
	ex      af,af'          ; get A'=byte from page 0
	exx
	ld      a,$17
	out     (c),a           ; page in page 7
	exx
	ex      af,af'
	ld      (de),a          ; store byte from page 0 in page 7
	inc     hl              ; increment addresses
	inc     de
	dec     bc              ; decrement counter
	ld      a,b
	or      c
	jr      nz,m3f69        ; loop back for more
	ld      a,(BANKM)
	ld      bc,$7ffd
	out     (c),a           ; page in previous memory
	ei                      ; enable interrupts
	ret

; Subroutine to copy a block of memory from HL in page 7 to
; DE in page 0 (length BC bytes)

m3f8a   di
	exx
	ld      bc,$7ffd        ; BC'=paging port
	exx
m3f90   exx
	ld      a,$17
	out     (c),a           ; page in page 7
	exx
	ld      a,(hl)
	ex      af,af'          ; A'=byte from page 7
	exx
	ld      a,$10
	out     (c),a           ; page in page 0
	exx
	ex      af,af'
	ld      (de),a          ; store byte in page 0
	inc     hl              ; increment addresses
	inc     de
	dec     bc              ; decrement pointers
	ld      a,b
	or      c
	jr      nz,m3f90        ; loop back for more
	ld      a,(BANKM)
	ld      bc,$7ffd
	out     (c),a           ; page in previous memory
	ei                      ; enable interrupts
	ret
  IF garry
m07c9   defm    "K free", 13, 0
m07d1   defm    "No files found", 13, 0
merase  defm    "Erase ", 0
myn     defm    " ? (Y/N)", 0
	defs    28
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff
  ELSE
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	defb    $ff,$ff,$ff,$ff,$ff,$ff
    IF v41
      IF spanish
	defb    $8e
      ELSE
	defb    $12
      ENDIF
    ELSE
      IF spanish
	defb    $3c
      ELSE
	defb    $72
      ENDIF
    ENDIF
  ENDIF

; ----------------------------------------------------------------------------------------------------

; ============================
; PLAY command data structures
; ============================
;
; During execution of the PLAY command, an area of memory $3c bytes long plus
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
; +01     1       $ff, or MIDI channel 0-15
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

