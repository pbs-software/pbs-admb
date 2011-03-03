@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1 )

SET ADM_NO_PAUSE=1
call ADMcheck.bat

if not defined ADM_ERROR (
	call adcomp.bat %1 %2 %3 %4 %5
)
:end
