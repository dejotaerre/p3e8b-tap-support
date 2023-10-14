;--------------------------------------------------
; Additional system variables used in the +3
;--------------------------------------------------

; System variables and data structures in DOS workspace (page 7)

; Alternate screen $C000-$DAFF
; Also used by +3 BASIC commands for temporary storage

	DEFINE		src_add		$C000		; (2) address of source filespec
	DEFINE		dst_add		$C002		; (2) address of dest filespec
	DEFINE		src_drv		$C004		; (1) source drive
	DEFINE		dst_drv		$C005		; (1) dest drive
	DEFINE		eof		$C006		; (1) EOF flag
	DEFINE		free_m		$C007		; (2) space free on M: for copying
	DEFINE		copy_ram	$C009		; (1) flag: true if copy via RAM, not M:
	DEFINE		dst_open	$C00A		; (1) flag: true if dst_file is open
	DEFINE		wild		$C00B		; (1) flag: true for wildcards in filespec
	DEFINE		dst_dev		$C00C		; (1) dest dev:$00=file,$e0=printer,$aa=screen
	DEFINE		tmp_bytes	$C00D		; (2) #bytes copied to temporary file
	DEFINE		copied		$C00F		; (1) #files copied
	DEFINE		dst_file	$C010		; (18) dest filespec ($ff-term)
	DEFINE		src_file	$C022		; (18) source filespec ($ff-term)
	DEFINE		cat_spec	$C034		; (13) filespec to search catalog from
	DEFINE		wld_next	$C041		; (13) next filename formed from wild spec
	DEFINE		tmp_file	$C04E		; (18) temp filespec ($ff-term)

; +3DOS permanent structures & variables

	DEFINE		pg_buffer	$DB00		; ($20) buffer for copying between pages
	DEFINE		rt_alert	$DB20		; (2) ALERT routine address
	DEFINE		al_resp		$DB22		; (7) ALERT response string
	DEFINE		al_mess		$DB29		; ($77) message for ALERT routine
	DEFINE		fcbs		$DBA0		; ($380) FCBs ($38 bytes each for $10 files)
	DEFINE		sysfcb0		$DF20		; ($38) system FCB 0
	DEFINE		sysfcb1		$DF58		; ($38) system FCB 1
	DEFINE		filerecs	$DF90		; (3) #recs in file (during open)
	DEFINE		def_user	$DF93		; (1) default user area
	DEFINE		def_drv		$DF94		; (1) default drive
	DEFINE		extchg		$DF95		; (1) extents changed in operation flag
	DEFINE		att_clr		$DF96		; (1) attributes to clear
	DEFINE		att_set		$DF97		; (1) attributes to set
	DEFINE		cat_buff	$DF98		; (2) address of catalog buffer
	DEFINE		cat_filt	$DF9A		; (1) catalog filter
	DEFINE		cat_size	$DF9B		; (1) catalog buffer size (in entries)
	DEFINE		cat_ents	$DF9C		; (1) number of completed catalog entries

	; $df9d-$df9f unused (3 bytes)

	DEFINE		rt_dirent	$DFA0		; (2) routine to call for every dir entry
	DEFINE		direntry	$DFA2		; ($20) directory entry buffer

	; $dfc2-$dfcf unused (14 bytes)
	; Not advisable to use, due to bug in datestamp checking routine

	DEFINE		rw_page		$DFD0		; (1) page to read/write to
	DEFINE		rw_add		$DFD1		; (2) address to read/write to

	; $dfd3-$dfdf unused (13 bytes)

	DEFINE		bcbs		$DFE0		; ($b0) BCBs ($0b bytes each for $10 buffers)
	DEFINE		cache7		$E090		; ($200) Page 7 cache buffer (always exists)
	DEFINE		cachenum	$E290		; (1) number of cache buffers
	DEFINE		cachefst	$E291		; (1) first cache buffer number
	DEFINE		bcb_inuse	$E292		; (2) inuse BCB chain
	DEFINE		bcb_free	$E294		; (2) free BCB chain

	; $e296-$e29f unused (10 bytes)

	DEFINE		xdpb_ptrs	$E2A0		; ($20) pointers to XDPBs (or 0) for A: to P:
	DEFINE		xdpb_a		$E2C0		; ($30) XDPB for drive A:
	DEFINE		chksm_a		$E2F0		; ($10) checksum vector for drive A:
	DEFINE		alloc_a		$E300		; ($2d) allocation bitmap for drive A:
	DEFINE		xdpb_b		$E32D		; ($30) XDPB for drive B:
	DEFINE		chksm_b		$E35D		; ($10) checksum vector for drive B:
	DEFINE		alloc_b		$E36D		; ($2d) allocation bitmap for drive B:
	DEFINE		xdpb_m		$E39A		; ($30) XDPB for drive M:
	DEFINE		alloc_m		$E3CA		; ($20) allocation bitmap for drive M:
	DEFINE		unit0		$E3EA		; (1) drive mapped to unit 0
	DEFINE		rt_chgdsk	$E3EB		; (2) CHANGE_DISK routine address
	DEFINE		rt_temp		$E3ED		; (2) address of subroutine (temporary)
	DEFINE		spec_m		$E3EF		; (8) disk spec for drive M:

	; $e3f7-$e3ff unused (9 bytes)

	DEFINE		ddl_parms	$E400		; ($19) parameters in calls to DD_L_READ etc
	DEFINE		rt_encode	$E419		; (2) ENCODE routine address

	; $e41a-$e41f unused (6 bytes)

	DEFINE		equipment	$E420		; (8) equipment info for FD units 0 to 3
							; Byte 0: bits 0..1=side info (0=unknown,1/2=#sides)
							; Byte 0: bits 2..3=track info (0=unknown,1/2=single/double)
							; Byte 0: bit 6 set if head position known
							; Byte 1: track under head
	DEFINE		tm_mtron	$E428		; (1) motor on time
	DEFINE		tm_mtroff	$E429		; (1) motor off time
	DEFINE		tm_wroff	$E42A		; (1) write off time
	DEFINE		tm_hdset	$E42B		; (1) head settle time
	DEFINE		tm_step		$E42C		; (1) step rate
	DEFINE		retry_cnt	$E42D		; (1) retry count

	; $e42e-$e42f unused (2 bytes)

	DEFINE		fdc_res		$E430		; (8) FDC results buffer

	; $e438-$e5ff unused (456 bytes)

	DEFINE		timeout		$E600		; (1) current disk motor timeout

	; $e601 unused (1 byte)

; From this point, "unused" status is not 100% certain, due to limited
; knowledge of the Editor ROM

; Temporary storage used when switching ROMs etc

	DEFINE		tmp_sp		$E602		; (2) temporary SP store
	DEFINE		tmp_ret		$E604		; (2) temporary return address store
	DEFINE		tmp_af		$E606		; (2) temporary AF store
	DEFINE		tmp_hl		$E608		; (2) temporary HL store
	DEFINE		tmp_de		$E60A		; (2) temporary DE store
	DEFINE		tmp_bc		$E60C		; (2) temporary BC store

	; $e60e-$e77b unused (366 bytes)

	DEFINE		tmp_stack	$E7ff		; ($84) temporary TSTACK store
							; from $e77c-$e7ff

	; $e800-$ebff unused (1024 bytes)

; Editor variables

	DEFINE		men_high	$EC0C		; (1) highlighted menu line
	DEFINE		ed_flags	$EC0D		; (1) - bit 1 set when processing menu
	DEFINE		process		$EC0E		; (1) process: $07 Loader, $04 Calculator
	DEFINE		ed_ATTR_P	$EC0F		; (1) editor/saved ATTR_P
	DEFINE		ed_MASK_P	$EC10		; (1) editor/saved MASK_P
	DEFINE		ed_ATTR_T	$EC11		; (1) editor/saved ATTR_T
	DEFINE		ed_MASK_T	$EC12		; (1) editor/saved MASK_T
	DEFINE		ed_P_FLAG	$EC13		; (1) editor/saved P_FLAG
	DEFINE		ed_N_ROWS	$EC15		;

	; $ec20-$ecff unused (224 bytes)

; Temporary buffers/storage

	DEFINE		tmp_fspec	$ED01		; (??) temporary filespec workspace
	DEFINE		tmp_buff	$ED11		; (2048) temporary buffer for FORMAT/COPY
							; *BUG* means COPY uses page 0 instead of 7
	; $f511-$f6e9 unused (473 bytes)

	DEFINE		men_rout	$F6EA		; (2) address of menu routines table
	DEFINE		men_text	$F6EC		; (2) address of menu text
	DEFINE		ed_area		$F6F3		; (1) Edit area info - Number of rows in the editing area.
	DEFINE		row_below	$F6F5		; (1) Number of rows held in the Below-Screen Line Edit Buffer.

	; $f700-$f8ff unused (512 bytes)

	DEFINE		nr_above	$F9DB		; (1) Number of rows held in the Above-Screen Line Edit Buffer.

	; $fa00-$fbff unused (512 bytes)

	DEFINE		ign_space	$FC9E		; (1) flag: if set, ignore leading space
	DEFINE		line_add	$FC9F		; (2) 0 or add of line data
	DEFINE		ascii_add	$FCA1		; (2) 0 or add of ASCII expanded token/number
	DEFINE		ascii_txt	$FCA3		; (??) bit-7 terminated ASCII text

	; IX normally set to $fd98, so this area unknown exactly

	DEFINE		edit_top	$FD99		; (1) top screen line of editing area

	DEFINE		curs_cols	$FD9E		; (1) cursor colours
	DEFINE		curs_attr	$FD9F		; (1) saved attribute under cursor
	DEFINE		chkword		$FDA0		; (??) word to check if token

	; $fe00-$ffff used to load bootsector by +3DOS ROM
;--------------------------------------------------
;
	; ADICIONO variableas PARA MI EXTENSION DE CARGA DE ARCHIVOS TAPs

	DEFINE		tmp_workmem		$8000	; idem para contener rutinas temp y stack
	DEFINE		save_buffer		$c100	; creo que +3DOS no usa esto (1K)
	DEFINE		tmp_buffertap		$c600	; tmp en donde vuelco lo que lea desde disco
	DEFINE		CONFRAM			$E800	; guardo el nro de pagina del 4to seg que
							; habia al llamarse la rutina LOAD de ROM3
	DEFINE		tmp_BANKM		$E801	; guardo una copia aqui de lo que habia en BANKM
	DEFINE		tmp_BANK678		$E802	; idem pero para BANK678
	DEFINE		save_STACK		$E803	; (2) guardo temporalmente SP aqui
	DEFINE		tapl_stat1		$E805	; (1) multiproposito tapeloader 1
	DEFINE		tapl_stat2		$E806	; (1) multiproposito tapeloader 2
	DEFINE		save_REGI		$E807	; (1) respaldo aqui el registro I
	DEFINE		save_INTERR		$E808	; (1) respaldo aqui el estado de las interrupciones
	DEFINE		buffer_ADDR		$E809	; (2) direccion del buffer por la que voy
	DEFINE		buffer_COUNT		$E80B	; (2) cuenta regresiva para vaciar el buffer
	DEFINE		end_LOAD		$E80D	; (1) poner a 1 para terminar la carga y salir por error
	DEFINE		EXCEPCIONES		$E80E	; (1) indique llamar a funciones alternativas en la funcion
							; que llega del buffer
	DEFINE		S_PFILE		$E80F	; (4) guarda temporalmente la posicion del puntero del archivo #6
	DEFINE		save_rt_alert		$E813	; (2) swapeo acá la dir de la rutina alert. para desactivarla 
							; mientras dure la carga desde "cinta"

	DEFINE		buff_tapsize		5*1024	; si uso valores mayores o no funciona, o queda corrupto
