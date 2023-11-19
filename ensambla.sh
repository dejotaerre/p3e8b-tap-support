#!/bin/bash

# OCT-2023 - ADICIONADO SOPORTE PARA ARM (es decir: Raspberry PI)

#===========================================================================================

arquitectura=$(uname -m)

if [ "$arquitectura" = "x86_64" ]; then
    cpcfscmd=cpcxfs
    sjasmcmd=sjasmplus
elif [ "$arquitectura" = "i686" ]; then
    cpcfscmd=cpcxfs
    sjasmcmd=sjasmplus
elif [ "$arquitectura" = "armv7l" ]; then
    cpcfscmd=cpcxfs_arm
    sjasmcmd=sjasmplus_arm
elif [ "$arquitectura" = "aarch64" ]; then
    cpcfscmd=cpcxfs_arm
    sjasmcmd=sjasmplus_arm
else
    echo "Arquitectura $arquitectura no compatible con este script"
    exit 1
fi

echo

rm -f *.rom

./bin/$sjasmcmd --lstlab --lst=plus3.lst plus3.asm

if [ $? -ne 0 ]; then
	echo
	echo "*************************"
	echo "* ¡Falló el ensamblado! *"
	echo "*************************"
	echo
	exit 1
fi

echo

romprefix=p3t_

# ROMs de 32K por cada chip para quemar en EPROMS en sócalos 1 y 2
cat ${romprefix}rom0.rom ${romprefix}rom1.rom > 32K_ROM-A.rom
cat ${romprefix}rom2.rom ${romprefix}rom3.rom > 32K_ROM-B.rom

# ROMs de 64K por chip para quemar en EPROMs de 64K para sócalos 1 y 2
# con la versión en inglés de las ROMs +3 v4.1 en un +3 con switch de doble banco
cat ./roms_standar_p3/P3-41A.ROM ${romprefix}rom0.rom ${romprefix}rom1.rom > 64K_ROM-A.rom
cat ./roms_standar_p3/P3-41B.ROM ${romprefix}rom2.rom ${romprefix}rom3.rom > 64K_ROM-B.rom

# copio las roms generadas hacia mi otro proyecto "+3E File Selector"
# (si detecto que existe la carpeta) y es para usar con FUSE para sus
# testeos en dicho proyecto

if [ -d "../p3e-file-selector" ]; then
	cp -v -f p3t_rom?.rom ../p3e-file-selector
	echo
fi

# creo una imágen de disquete con archivos para testeo
# (ignoro por que "mput" no funciona bien con redirecciones de archivos,
# ej: "<", por eso lo hago archivo por arhivo)

cd bin
rm -f ../test.dsk

echo "new -f PCW3 ../test.dsk"  >  makedsk
echo "open -f PCW3 ../test.dsk" >> makedsk
echo "cd ./dskfiles" >> makedsk
echo "put -b 1942.tap" >> makedsk
echo "put -b abusimb.tap" >> makedsk
echo "put -b beyplc.tap" >> makedsk
echo "put -b bumpy.tap" >> makedsk
echo "put -b vixen.tap" >> makedsk
echo "put -b wecleman.tap" >> makedsk
echo "put -b PROFANAT.Z80" >> makedsk
echo "put -b RENEGAD2.Z80" >> makedsk
echo "exit" >> makedsk

cp test.dsk

./${cpcfscmd} < makedsk

rm makedsk
cd ..

if ! command -v fuse >/dev/null 2>&1; then

	echo
	echo -e "\e[1;31mADVERTENCIA:\e[0m no encuentro el emulador FUSE, así que no podrás probar"
	echo "el resultado, puedes buscar en https://fuse-emulator.sourceforge.net/"
	echo "o bien instalarlo con tu gestor de paquetes favorito"
	echo
	echo "Sin embargo, tus nuevas ROMs fueron ensambladas con los nombres"
	echo "${romprefix}rom0.rom"
	echo "${romprefix}rom1.rom"
	echo "${romprefix}rom2.rom"
	echo "${romprefix}rom3.rom"
	echo "para que puedas quemarlas en EPROMs"
	echo "***********************************************************************************************"

else

	#fusexec="fuse-sdl"
	fusexec="fuse"

	# genero los comandos para crear una imagen de disquete para testeos en el emulador
	# para ejecutar y testear las ROMs
	exec_args="--machine plus3 \
--simpleide \
--multiface3 \
--rom-plus3-0 ./p3t_rom0.rom --rom-plus3-1 ./p3t_rom1.rom \
--rom-plus3-2 ./p3t_rom2.rom --rom-plus3-3 ./p3t_rom3.rom \
--simpleide-masterfile ./+3e8bits.hdf \
--graphics-filter tv4x \
--pal-tv2x \
--drive-plus3a-type \"Double-sided 80 track\" \
--drive-plus3b-type \"Double-sided 80 track\" \
--plus3disk ./test.dsk"

	echo 
	echo

	echo "***********************************************************************************************"
	echo EJECUTANDO:
	echo $fusexec $exec_args
	echo "***********************************************************************************************"

	excmd="${fusexec} ${exec_args}"
	eval $excmd > /dev/null 2>&1

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
rm *.lst
rm *.dsk

exit 0
