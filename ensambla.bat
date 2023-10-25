@echo off
echo.

REM limpieza de anteriores pruebas
del *.rom > nul

REM ensamblo
.\bin\sjasmplus.exe --lstlab --lst=plus3.lst plus3.asm

if errorlevel 1 goto err_ensamblado
goto next_copy

:err_ensamblado

echo.
echo "**************************"
echo "* Ensamblado incorrecto! *"
echo "**************************"
echo.
goto fin

:next_copy

REM ROMs de 32K por cada chip para quemar en EPROMS para los sócalos 1 y 2
copy /b p3t_rom0.rom + p3t_rom1.rom 32K_ROM-A.rom > nul
copy /b p3t_rom2.rom + p3t_rom3.rom 32K_ROM-B.rom > nul

REM ROMs de 64K por cada chip para quemar en EPROMS "DOBLE" ROM para los sócalos 1 y 2
REM con la versión en inglés de las ROMs +3 v4.1
copy /b roms_standar_p3\P3-41A.ROM + p3t_rom0.rom + p3t_rom1.rom 64K_ROM-A.rom > nul
copy /b roms_standar_p3\P3-41B.ROM + p3t_rom2.rom + p3t_rom3.rom 64K_ROM-B.rom > nul

REM Si existe mi otro proyecto P3FSEL copio las ROMs en su carpeta, ya que las uso
REM allí también para propósitos de testeo con el emulador FUSE de dicha utilidad
if exist ..\p3e-file-selector goto copy_p3efs_1
if not exist ..\p3e-file-selector-main goto next_testrun

echo.
echo "Copiando ROMS en mi proyecto P3FSEL"
echo.
copy /Y p3t_rom?.rom ..\p3e-file-selector-main > nul
goto next_testrun

:copy_p3efs_1
echo.
echo "Copiando ROMS en mi proyecto P3FSEL"
echo.
copy /Y p3t_rom?.rom ..\p3e-file-selector > nul

:next_testrun
cd bin

REM genero los comandos para crear una imagen de disquete DSK para testeos en el emulador
echo new -f PCW3 ..\test.dsk >  makedsk
echo open -f PCW3 ..\test.dsk >> makedsk
echo mput -f -b taps\*.tap >> makedsk
echo mput -f -b z80s\PROFANAT.Z80 >> makedsk
echo mput -f -b z80s\RENEGAD2.Z80 >> makedsk
echo exit >> makedsk

if exist ..\test.dsk del ..\test.dsk

REM USO CPCXFS para crear un DSK de testeo, está compilado para Windows 32bits, si vas
REM a usar este BAT para testear en MSDOS vas a tener que descargar sus fuentes y 
REM compilarlo, sin contar que tendrás que hacer mismo con SJASMPLUS
cpcxfsw < makedsk
if exist makedsk del makedsk

REM Hago una variable de entorno con los parámetros necesarios
REM para ejecutar y testear las ROMs
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

REM LIMPIEZA DE ARCHIVOS NO NECESARIOS LUEGO DEL TESTING
if exist plus3.lst del plus3.lst
if exist test.dsk del test.dsk

:fin
echo.
pause