@ECHO OFF

:: This is a batch file to call admb.bat.
:: Put it in a directory guaranteed to be on the path.
:: Edit the two paths below to indicate locations for ADMB and the C/C++ compiler.
:: The path will be changed temporarily while running this command.

SETLOCAL

:: ***** EDIT THESE TWO PATHS *****

set ADMB_HOME=C:\Utils\ADMB
set CC_PATH=C:\Utils\Rtools\MinGW\bin

:: ADD TWO DIRECTORIES (TEMPORARILY) TO THE PATH

set ADMB_PATH=%ADMB_HOME%\bin
set Path=.;%ADMB_PATH%;%CC_PATH%;%Path%

Call %ADMB_PATH%\admb.bat %1 %2 %3 %4 %5 %6 %7 %8
