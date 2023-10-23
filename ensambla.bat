@echo off
echo.

del *.rom > nul

.\bin\sjasmplus.exe --lstlab --lst=plus3.lst plus3.asm

if errorlevel 1 (
	echo.
	echo "**************************"
	echo "* Ensamblado incorrecto! *"
	echo "**************************"
	echo.
	exit 1
)

REM ROMs de 32K por cada chip para quemar en EPROMS en sócalos 1 y 2
REM con la versión en inglés de las ROMs +3 v4.1
copy /b p3t_rom0.rom + p3t_rom1.rom 32K_ROM-A.rom
copy /b p3t_rom2.rom + p3t_rom3.rom 32K_ROM-B.rom

REM ROMs de 64K por cada chip para quemar en EPROMS "DOBLE" ROM en sócalos 1 y 2
REM con la versión en inglés de las ROMs +3 v4.1
copy /b roms_standar_p3\P3-41A.ROM + p3t_rom0.rom + p3t_rom1.rom 64K_ROM-A.rom
copy /b roms_standar_p3\P3-41B.ROM + p3t_rom2.rom + p3t_rom3.rom 64K_ROM-B.rom

if exist ..\p3e-file-selector (
	copy /Y p3t_rom?.rom ..\p3e-file-selector
)

cd bin

REM genero los comandos para crear una imagen de disquete para testeos en el emulador
echo new -f PCW3 ..\test.dsk >  makedsk
echo open -f PCW3 ..\test.dsk >> makedsk
echo mput -f -b taps\*.tap >> makedsk
echo mput -f -b z80s\PROFANAT.Z80 >> makedsk
echo mput -f -b z80s\RENEGAD2.Z80 >> makedsk
echo exit >> makedsk

del ..\test.dsk
cpcxfsw < makedsk
del makedsk

rem Hago una variable de entorno con los parámetros necesarios
rem para ejecutar y testear las ROMs
set fuse_opciones=--machine plus3
set fuse_opciones=%fuse_opciones% --simpleide
set fuse_opciones=%fuse_opciones% --multiface3
set fuse_opciones=%fuse_opciones% --plus3disk test.dsk
set fuse_opciones=%fuse_opciones% --rom-plus3-0 ..\p3t_rom0.rom
set fuse_opciones=%fuse_opciones% --rom-plus3-1 ..\p3t_rom1.rom
set fuse_opciones=%fuse_opciones% --rom-plus3-2 ..\p3t_rom2.rom
set fuse_opciones=%fuse_opciones% --rom-plus3-3 ..\p3t_rom3.rom
set fuse_opciones=%fuse_opciones% --simpleide-masterfile +3e8bits.hdf
set fuse_opciones=%fuse_opciones% --graphics-filter tv4x
set fuse_opciones=%fuse_opciones% --pal-tv2x
set fuse_opciones=%fuse_opciones% --rom-multiface3 ..\fusew\roms\mf3.rom
set fuse_opciones=%fuse_opciones% --drive-plus3a-type "Double-sided 80 track"
set fuse_opciones=%fuse_opciones% --drive-plus3b-type "Double-sided 80 track"

set fuse_exec=fusew\fuse

echo.
echo.
echo "***********************************************************************************************"
echo EJECUTANDO:
echo %fuse_exec% %fuse_opciones%
echo "***********************************************************************************************"

cd ..

%fuse_exec% %fuse_opciones%

rem LIMPIEZA DE ARCHIVOS NO NECESARIOS LUEGO DEL TESTING
del *.lst
del *.dsk

exit 0
