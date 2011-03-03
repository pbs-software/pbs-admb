@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1 )

:: EDIT THE FOLLOWING PATHS BELOW
set ADMB_HOME=C:\Utils\ADMB10-gcc450
set MINGW_HOME=C:\Utils\gcc450
set TEXT_EDIT=C:\Utils\UltraEdit\uedit32.exe

rem Paths dependent on those above 
rem Note: using "" delimiters disable the ADMB scripts
set ADMB_BIN=%ADMB_HOME%\bin
set MINGW_BIN=%MINGW_HOME%\bin


