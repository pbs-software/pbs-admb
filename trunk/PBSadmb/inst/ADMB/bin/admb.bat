@echo off
 
rem Command line script for ADMB

setlocal

if ""  NEQ ""%4 (echo syntax: admb [-s] [-re] name) else (
  if "" NEQ ""%3 (tpl2rem %3 > tmp_admb && myccsre %3 && mylinksre %3) else (
  if ""==""%2 (tpl2cpp %1 > tmp_admb && mycc %1 && mylink %1)  else (
    if /i %1==-re (tpl2rem %2 > tmp_admb && myccre %2 && mylinkre %2) else (
    if /i %1==-s (tpl2cpp -bounds %2 > tmp_admb && myccs %2 && mylinks %2) else (
		echo syntax: admb [-s] [-re] name )))))	

endlocal
echo on


