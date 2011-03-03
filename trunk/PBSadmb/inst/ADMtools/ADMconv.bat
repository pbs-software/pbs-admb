@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1 )

SET ADM_NO_PAUSE=1

call ADMcheck.bat

if not defined ADM_ERROR (
	if "%2"=="" (
		tpl2cpp %1
	) else (
		if "%1"=="-r" (
			tpl2rem "%2" 
		) else (
			echo ERROR - the only valid option is '-r'
			echo Example - %0 -r %2
			goto end
		)
	) )

:end
