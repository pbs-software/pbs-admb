@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1 )

if "%1"=="" (
  echo ERROR - you must specify a model
  echo example: %0% vonb
  goto end )

SET ADM_NO_PAUSE=1
call ADMcheck.bat

if not defined ADM_ERROR (
	call adlink.bat %1 %2 %3 %4 %5
)
:end

