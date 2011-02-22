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
	if "%2"=="" (
		tpl2cpp vonb
	) else (
		if "%2"=="-r" (
			tpl2rem "%1" 
		) else (
			echo ERROR - the only option is '-r'
			echo %0 vonb -r
			goto end
		)
	) )

:end

