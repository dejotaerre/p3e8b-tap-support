/*
====================================================================================================
Los fuentes que estoy usando los descargué desde aquí:
http://sourceforge.net/p/emuscriptoria/code/HEAD/tree/plusROMs/
(y fueron "beautificados" con TABs sólidos a 8 columnas)

No he podido contactarme con Garry Lancaster solicitándole los fuentes
de todas las versiones del proyecto +3e, solo con el fin de modificarlas
agregando mi "extensión" de carga de archivos TAPs.

Una cosa si he podido notar de las diferentes versiones de ROMs, y es que las
ROMs 0 y 3 son binariamente iguales en cualquier version de interface IDE

Las ROMS 1 y 2 son en parte desensamble de los archivos bajados de emuscriptoria y
parte de desensambles mios ayudándome del maravilloso DZ80

Se ha realizado con muchísimo trabajo y horas de dedicación, hasta que tuve
en mis manos unos fuentes operativos para llevar a cabo las modificaciones que
tenía en mente

*******************************************************************************
** IMPORTANTE:                                                               **
** LA ROM 2 CORRESPONDE A LA DEL INTERFACE SIMPLE DE 8 BITS DEBIDO QUE LOS   **
** FUENTES DE LAS VERSIONES PARA LOS OTROS INTERFACES (ej: DivMMC) NO ESTAN  **
** DISPONIBLES, AUN ASÍ ES POSIBLE QUE TODO FUNCIONE BIEN INTERCAMBIANDO EL  **
** BINARIO DE LA ROM2 POR LAS DE OTRAS VERSIONES, YA QUE MI EXTENSION PARA   **
** CARGA DE TAPS LO UNICO QUE HACE ES ABRIR, CARGAR, Y CERRAR ARCHIVOS       **
** (aunque esto no lo he probado)                                            **
*******************************************************************************

Por otro lado el inconveniente con estos fuentes es que solo funcionan con
spanish=0, v41=0, garry=1, cualquier otra combinación no funciona, es decir:
(falta de labels, defs con valores incorrectos, resultados diferentes a 16K,
y larguísimo etc...)

Así que en base a esto decidí quitar toda referencia a spanish=0, v41=0, garry=1
y solo quedarme con la versión con la que quiero trabajar, es decir:
ROMS +3e de 8 bits en inglés

====================================================================================================

Por favor si alguien dispone de los fuentes de Garry Lancaster en todas las versiones
de interfaces IDE, quisiera saberlo para incorporarles mi "extension" de carga de TAPs
en todas las versiones posibles, de lo contrario solo estará disponible de momento
para el interface simple de 8 bits.

====================================================================================================
*/

; ==================================================================================================

	DEFINE	VMAYOR		1	; estas ROMS de +3e corresponden a la versión 1.38 de GL
	DEFINE	VMINOR		3	; me resultó imposible conseguir versiones mas recientes
	DEFINE	VPATCH		8	; en este caso es la version 1.38 + "t" para indicar que
	DEFINE	VRPATCH		"t"	; permite cargar TAPs con SPECTRUM "archivo.tap" + LOAD ""

; ==================================================================================================

	DEFINE	alternative	1	; En 1 si prefiero mis modificaciones (aparte de
					; la carga de TAPs) que son en esencia "cosméticas" de
					; colores, y con mensajes de inicio minimalistas e
					; históricamente mas "correctos" en el booteo, además del
					; sonido original en la selección de las opciones de menues
					;
					; (estos son gustos personales pero se puede desactivar
					; esta constante, o bien poner alt_colors en 0 y
					; modificar los parámetros en el ELSE del
					; IF alt_colors de mas abajo)

	DEFINE	alt_colors	1	; esquema de colores para el modo alternativo

; ==================================================================================================

	IF alternative

		IF alt_colors

			DEFINE	defattr		%00001101 ; 1) fondo azul y tinta cyan
			DEFINE	defborder	%00000001 ; 2) pone borde azul
			DEFINE	definklin_lw	%00000111 ; 3) tinta blanca linea 2 barra de estado
			DEFINE	defink		%00000101 ; 4) tinta cyan luego de la iniciación

							  ; con estos colores sugiero:
							  ; "SPECTRUM ATTR 13 ASN" par conservar
							  ; los mismos colores en la inicialización

		ELSE

			; estos serían los valores para el clásico
			; fondo/borde blanco con tinta negra

			DEFINE	defattr		%00111000 ; 1) fondo blanco tinta negra
			DEFINE	defborder	%00000111 ; 2) pone borde blanco
			DEFINE	definklin_lw	%00000000 ; 3) tinta negra linea 2 barra de estado
			DEFINE	defink		%00000000 ; 4) tinta negra luego de la iniciación

							  ; con estos colores sugiero:
							  ; "SPECTRUM ATTR 56 ASN" par conservar
							  ; los mismos colores en la inicialización

		ENDIF

	ELSE

		DEFINE	defattr		%00111000 ; el estandar con fondo blanco, tinta
		DEFINE	defborder	%00000111 ; negra, y con borde blanco (ídem alt_colors=0)
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
; DJr - oct/2023