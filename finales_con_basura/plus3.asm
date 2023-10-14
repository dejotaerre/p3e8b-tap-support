/*
Los fuentes que estoy usando los descargué desde aquí
http://sourceforge.net/p/emuscriptoria/code/HEAD/tree/plusROMs/

No he podido contactarme con Garry Lancaster solicitándole los fuentes de todas
las versiones del proyecto +3e, para con solo el fin de modificarlas con mi 
"extensión" de carga de TAPs.

Una cosa si he podido notar de las diferentes versiones de ROMs, las ROMs 0 y 3
son binariamente iguales en cualquier version de interface IDE

Las ROMS 1 y 2 son en parte desensamble de los archivos bajados de emuscriptoria y
parte de desensambles mios ayudándome del maravilloso DZ80 (esto fue en épocas de MS-DOS)

Todo se ha realizado con muchisimo trabajo y horas de dedicación, hasta que tuve
en mis manos unos fuentes operativos para llevar a cabo las modificaciones que
tenía en mente

** IMPORTANTE:
** LA ROM 2 CORRESPONDE A LA DEL INTERFACE SIMPLE DE 8 BITS **
** DEBIDO QUE A LAS VERSIONES PARA LAS OTRAS INTERFACES NO ESTAN DISPONIBLES **

Por otro lado el inconveniente con estos fuentes es que solo funcionan con
spanish=0, v41=0, garry=1, cualquier otra combinación no funciona, es decir:
(faltan labels, defs con valores incorrectos, etc...)

--------------------------------------------------------------------------------------

Por favor si alguien dispone de los fuentes de Garry Lancaster en todas las versiones
de interfaces IDE, quisiera saberlo para incorporarles mi "extension" de carga de TAPs
en todas las versiones posibles, de lo contrario solo estará disponible para el
interface simple de 8 bits.

*/
	DEFINE	garry		1	; ensambla las modificaciones esenciales para +3e
					; dicho de otro modo: ensambla "las roms +3e"
					; (tengo la sensación que estos fuentes fueron
					; usados para crear roms en inglés o español,
					; con o sin +3e, con versión 4.0 o 4.1 etc,
					; solo modificando constantes, pero no pude hacerme
					; de los fuentes correctos)

	DEFINE	spanish		0	; ¡solo sirve 0! si pones 1 se hizo un trabajo espantoso en
					; estos desensambles ya que termina faltando labels, y en
					; caso de solucionarlo se genera un binario de mas de 16K

	DEFINE	v41		0	; si garry es 1, v41 debe quedar en 0 (también
					; anda como la mierda como con spanish=1)
					;
					; DEFINITIVA Y AL FINAL DE TODO...
					; la única configuración operable es:
					; garry       = 1
					; v41         = 0
					; spanish     = 0
					; alternative = 0 o 1

	DEFINE	alternative	1	; En 1 si prefiero mis modificaciones (aparte de
					; la carga de TAPs) son en esencia "cosméticas" de
					; colores, con mensajes minimalistas e históricamente
					; mas "correctos" en el booteo, y con el sonido original
					; en la selección de las opciones de los menues.
					;
					; (estos son gustos personales pero se puede desactivar
					; esta constante, o bien poner alt_colors en 0 y
					; modificar los parámetros en el ELSE del
					; IF alt_colors de mas abajo)

	DEFINE	VMAYOR		1	; estas ROMS de +3e corresponden a la versión 1.38 de GL
	DEFINE	VMINOR		3	; me resultó imposible conseguir versiones mas recientes
	DEFINE	VPATCH		8	; en este caso es la version 1.38 + "t" para indicar que
	DEFINE	VRPATCH		"t"	; permite cargar TAPs con SPECTRUM "archivo.tap" + LOAD ""

; ==================================================================================================

	DEFINE	alt_colors	1	; esquema de colores para el modo alternative

; ==================================================================================================

	IF alternative

		IF alt_colors

			DEFINE	defattr		%00001101 ; a mi me gusta fondo azul y tinta cyan
			DEFINE	defborder	%00000001 ; borde azul
			DEFINE	definklin_lw	%00000111 ; tinta para linea 2 barra de estado
			DEFINE	defink		%00000101 ; tinta luego de la inicialización
							  ; con estos colores sugiero:
							  ; "SPECTRUM ATTR 13 ASN" par conservar
							  ; los mismos colores en la inicialización

		ELSE

			; estos serían los valores para el clásico fondo/borde blanco con
			; tinta negra

			DEFINE	defattr		%00111000 ; fondo blanco en negro
			DEFINE	defborder	%00000111 ; borde blanco
			DEFINE	definklin_lw	%00000000 ; tinta para linea 2 barra de estado
			DEFINE	defink		%00000000 ; tinta luego de la inicialización
							  ; con estos colores sugiero:
							  ; "SPECTRUM ATTR 56 ASN" par conservar
							  ; los mismos colores en la inicialización

		ENDIF

	ELSE

		DEFINE	defattr		%00111000 ; el estandar con fondo blanco, tinta
		DEFINE	defborder	%00000111 ; negra, y con borde blanco
		DEFINE	definklin_lw	%00000000 ; (no se usa para alternative=0)
		DEFINE	defink		%00000000 ; (no se usa para alternative=0)

	ENDIF

; ==================================================================================================

	INCLUDE "sysvar48.asm"			; variables del sistema para ROM 48K
	INCLUDE "sysvarp3.asm"			; variables del sistema para +3DOS
	INCLUDE "sysvarp7.asm"			; variables +3e página 7

; ==================================================================================================

	INCLUDE	"plus3ROM0.asm"			; ROM DEL EDITOR ROM 
	INCLUDE	"plus3ROM1.asm"			; ROM DEL SYNTAX ROM
	INCLUDE	"plus3ROM2.asm"			; ROM DEL +3DOS
	INCLUDE	"plus3ROM3.asm"			; ROM 48k ROM

; ==================================================================================================