@echo off

del *.rom

./bin/sjasmplus.exe --lstlab --lst=plus3.lst plus3.asm

if errorlevel 1 (
	echo.
	echo "**************************"
	echo "* Ensamblado incorrecto! *"
	echo "**************************"
	echo.
	exit 1
)

REM ROMs de 64K por cada chip para quemar en EEPROMS DOBLE ROM en sócalo 1 y 2
REM con la versión en inglés de las ROMs +3 v4.1
copy /b roms_standar_p3\P3-41A.ROM + p3t_rom0.rom + p3t_rom1.rom p3t_rom_a.rom
copy /b roms_standar_p3\P3-41B.ROM + p3t_rom2.rom + p3t_rom3.rom p3t_rom_b.rom

if exist ../p3e-file-selector (
	copy /Y p3t_rom?.rom ..\p3e-file-selector
)

cd bin

echo new -f PCW3 ..\test.dsk >  makedsk
echo open -f PCW3 ..\test.dsk >> makedsk
echo mput -f -b taps\*.tap >> makedsk
echo mput -f -b z80s\PROFANAT.Z80 >> makedsk
echo mput -f -b z80s\RENEGAD2.Z80 >> makedsk
echo exit >> makedsk

del ..\test.dsk
cpcxfsw < makedsk
del makedsk

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
rem set fuse_opciones=%fuse_opciones% --rom-multiface3 ..\fusew\mf3.rom
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
rem del *.lst
rem del *.dsk

exit 0