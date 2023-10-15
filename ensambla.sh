#!/bin/bash

#===========================================================================================
#vemos si tenemos lo necesario

if ! command -v sjasmplus >/dev/null 2>&1; then

		echo
		echo "SJASMPLUS no está instalado en el sistema, no se puede ensamblar"
		echo "(https://github.com/z00m128/sjasmplus.git)"
		echo
		exit 1

fi

echo

rm -f *.rom

sjasmplus --nologo --lstlab --lst=plus3.lst plus3.asm

if [ $? -ne 0 ]; then
	echo
	echo "*************************"
	echo "* ¡Falló el ensamblado! *"
	echo "*************************"
	echo
	exit 1
fi

echo

#--plus3disk file
#--snapshot file
#--tape file

romprefix=p3t_

# ROMs de 64K por chip para quemar en EEPROM DOBLE ROM en sócalo 1 y 2
# con la versión en inglés de las ROMs +3 v4.1
cat ./roms_standar_p3/P3-41A.ROM ${romprefix}rom0.rom ${romprefix}rom1.rom > ROM-A.rom
cat ./roms_standar_p3/P3-41B.ROM ${romprefix}rom2.rom ${romprefix}rom3.rom > ROM-B.rom

# copio las roms generadas hacia mi otro proyecto "+3E File Selector"
# (si existe la carpeta) y es para usar con FUSE para sus testeos con
# dicho proyecto
if [ -d "../p3e-file-selector" ]; then
	cp -v -f p3t_rom?.rom ../p3e-file-selector
	echo
fi

# creo una imágen de disquete con archivos para testeo en el emulador
cd bin
rm -f ../test.dsk
echo "new -f PCW3 ../test.dsk"		>  makedsk
echo "open -f PCW3 ../test.dsk"		>> makedsk
echo "mput -f -b taps/*.tap"		>> makedsk
echo "mput -f -b z80s/PROFANAT.Z80" >> makedsk
echo "mput -f -b z80s/RENEGAD2.Z80" >> makedsk
echo "exit"							>> makedsk
./cpcxfs < makedsk
rm makedsk
cd ..

if ! command -v fuse >/dev/null 2>&1; then

	echo
	echo
	echo "ADVERTENCIA: no encuentro el emulador FUSE, así que no podrás probar"
	echo "el resultado, puede buscar en https://fuse-emulator.sourceforge.net/"
	echo "o bien descargarlo desde tu gestor de paquetes favorito"
	echo

else

	#fusexec="fuse-sdl"
	fusexec="fuse"

	#--graphics-filter tv4x 
	# argumentos para fuse con el propósito de testeo rápido
	exec_args="--machine plus3 \
	--simpleide \
	--multiface3 \
	--plus3disk ./test.dsk \
	--rom-plus3-0 ./p3t_rom0.rom --rom-plus3-1 ./p3t_rom1.rom \
	--rom-plus3-2 ./p3t_rom2.rom --rom-plus3-3 ./p3t_rom3.rom \
	--simpleide-masterfile ./+3e8bits.hdf \
	--graphics-filter tv4x \
	--pal-tv2x \
	--drive-plus3a-type 'Double-sided 80 track' --drive-plus3b-type 'Double-sided 80 track'"

	comando="${fusexec} ${exec_args}"

	echo 
	echo
	echo "***********************************************************************************************"
	echo EJECUTANDO:
	echo $fusexec $exec_args
	echo "***********************************************************************************************"

	eval $comando > /dev/null 2>&1

fi

# me muestro el espacio libre que queda en los "trozos" sin programación en las 4 ROMs de 16K
echo
echo "Espacio libre en ROMs bajo las etiquetas FREE_ROMx_n:"
echo
echo ROM0:
cat plus3.lst | grep "R0_FREE_." | grep --color=never "0x" | sort -k2
echo
echo ROM1:
cat plus3.lst | grep "R1_FREE_." | grep --color=never "0x" | sort -k2
echo
echo ROM2:
cat plus3.lst | grep "R2_FREE_." | grep --color=never "0x" | sort -k2
echo
echo ROM3:
cat plus3.lst | grep "R3_FREE_." | grep --color=never "0x" | sort -k2
echo

# limpieza
#rm *.rom
#rm *.lst
#rm *.dsk

exit 0