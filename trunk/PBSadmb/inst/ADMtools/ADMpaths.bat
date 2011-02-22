@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1 )

:: EDIT THE FOLLOWING PATHS BELOW
set ADMB_HOME=C:\Temp\ADMB
set MINGW_HOME=C:\Temp\MinGW
set TEXT_EDIT=C:\Apps\UltraEdit\uedit32.exe

rem Paths dependent on those above 
rem Note: using "" delimiters disable the ADMB scripts
set ADMB_BIN=%ADMB_HOME%\bin
set MINGW_BIN=%MINGW_HOME%\bin


