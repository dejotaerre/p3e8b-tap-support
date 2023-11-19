# p3e8b-tap-support
ZX Spectrum +3E 8-BITS w/TAP support

Para el ensamblado se requiere usar línea de comandos, en caso Linux es "ensambla.sh" y en caso de windows es "ensambla.bat"

Se requiere en el caso de Linux tener ya instalado SJASMPLUS, en caso de windows ya está incluido en la carpeta ./bin

Si tu intención es solo ensamblar las ROMs para quemarlas en EPROMS ignora los siguientes párrafos

Si estás depurando y/o quieres darles una probada primero, se requiere del emulador FUSE en Linux, en cambio para windows ya hay una "pre-instalación" en la carpeta ./fusew

Si quieres usar otro emulador necesitarás uno que soporte interface "+3e 8bits", pero adicionalmente deberás adaptar "ensambla.sh" o "ensambla.bat" según tu plataforma.

También se hace uso del programa CPCXFS para manipular archivos DSK, los cuales ya se encuentran compilados en la carpeta ./bin tanto sea para Linux en formato x86-64, así como ARM (léase Raspberry PI), pero también el ejecutable para Windows