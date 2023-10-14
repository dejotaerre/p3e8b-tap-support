; VARIABLES DEL SISTEMA ESPECIFICAS PARA +3DOS QUE VAN
; EN LO QUE ANTES ERA EL BUFFER DE IMPRESORA

		DEFINE		SWAP		$5B00
		DEFINE		STOO		$5B10
		DEFINE		YOUNGER		$5B21
		DEFINE		REGNUOY		$5B2A
		DEFINE		ONERR		$5B3A
		DEFINE		OLDHL		$5B52
		DEFINE		OLDBC		$5B54
		DEFINE		OLDAF		$5B56
		DEFINE		TARGET		$5B58
		DEFINE		RETADDR		$5B5A
		DEFINE		BANKM		$5B5C
		DEFINE		RAMRST		$5B5D
		DEFINE		RAMERR		$5B5E
		DEFINE		BAUD		$5B5F
		DEFINE		SERFL		$5B61
		DEFINE		COL		$5B63
		DEFINE		WIDTH		$5B64
		DEFINE		TVPARS		$5B65
		DEFINE		FLAGS3		$5B66
		DEFINE		BANK678		$5B67
		DEFINE		XLOC		$5B68
		DEFINE		YLOC		$5B69
		DEFINE		OLDSP		$5B6A
		DEFINE		SYNRET		$5B6C
		DEFINE		LASTV		$5B6E
		DEFINE		RC_LINE		$5B73
		DEFINE		RC_START	$5B75
		DEFINE		RC_STEP		$5B77
		DEFINE		LODDRV		$5B79
		DEFINE		SAVDRV		$5B7A
		DEFINE		DUMPLF		$5B7B
		DEFINE		STRIP1		$5B7C
		DEFINE		STRIP2		$5B84
		DEFINE		TSTACK		$5BFF

; PUERTOS DE SWITCHEO DE MEMORIA
PBANKM:		EQU		$7FFD			; PUERTO CONF. DE PAGINAS RAM (1)
PBANK678:	EQU		$1FFD			; PUERTO CONF. DE PAGINAS RAM (2)