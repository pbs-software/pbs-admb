@ECHO OFF
if not defined ADM_SETLOCAL (
	SETLOCAL
	SET ADM_SETLOCAL=1
)

call ADMpaths.bat

if not exist "%ADMB_HOME%" (
	ECHO Cannot find ADMB Home - check ADMB_HOME "%ADMB_HOME%"
	set ADM_ERROR=1
	)
if not exist "%ADMB_BIN%\admb.bat" (
	ECHO Cannot find admb.bat - check ADMB_BIN "%ADMB_BIN%"
	set ADM_ERROR=1
	)
if not exist "%MINGW_BIN%\g++.exe" (
	ECHO Cannot find g++.exe - check MINGW_BIN "%MINGW_BIN%"
	set ADM_ERROR=1
	)
if not exist "%TEXT_EDIT%" (
	ECHO Cannot find editor - check TEXT_EDIT "%TEXT_EDIT%"
	set ADM_ERROR=1
	)

set Path=%ADMB_BIN%;%MINGW_BIN%
set TMPDIR=%TMP%

if not defined ADM_ERROR (
	echo All program paths look good
)
if defined ADM_ERROR (
	pause
)
