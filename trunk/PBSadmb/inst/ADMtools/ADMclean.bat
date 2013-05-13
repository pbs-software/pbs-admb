@Echo Off
Echo Clean most ADMB files, leaving originals (.tpl,.dat,.pin)

if {%1}=={} goto Lister
if %1 EQU yes goto Deleter

:Lister
Rem List files
  if exist *.bak dir/b *.bak
  if exist *.bar dir/b *.bar
  if exist *.b0? dir/b *.b0?
  if exist *.cor dir/b *.cor
  if exist *.cov dir/b *.cov
  if exist *.cpp dir/b *.cpp
  if exist *.ecm dir/b *.ecm
  if exist *.eva dir/b *.eva
  Rem if exist *.exe dir/b *.exe
  if exist *.hes dir/b *.hes
  if exist *.hst dir/b *.hst
  if exist *.htp dir/b *.htp
  if exist *.log dir/b *.log
  if exist *.mc? dir/b *.mc?
  if exist *.o   dir/b *.o
  if exist *.obj dir/b *.obj
  if exist *.out dir/b *.out
  if exist *.par dir/b *.par
  if exist *.psv dir/b *.psv
  if exist *.p0? dir/b *.p0?
  if exist *.r0? dir/b *.r0?
  if exist *.rep dir/b *.rep
  if exist *.run dir/b *.run
  if exist *.std dir/b *.std
  if exist *.swp dir/b *.swp
  if exist *.sym dir/b *.sym
  if exist *.tmp dir/b *.tmp
  if exist admodel.dep dir/b admodel.dep
  if exist eigv.rpt dir/b eigv.rpt
  if exist sims. dir/b sims.
  if exist variance. dir/b variance.
  if exist tmp_admb. dir/b tmp_admb.

Set /P ADMclean=Delete all the above files? [y/n] 
if /I not "%ADMclean%"=="y" goto Exit

:Deleter
Rem Delete files
  if exist *.bak del *.bak
  if exist *.bar del *.bar
  if exist *.b0? del *.b0?
  if exist *.cor del *.cor
  if exist *.cov del *.cov
  if exist *.cpp del *.cpp
  if exist *.ecm del *.ecm
  if exist *.eva del *.eva
  Rem if exist *.exe del *.exe
  if exist *.hes del *.hes
  if exist *.hst del *.hst
  if exist *.htp del *.htp
  if exist *.log del *.log
  if exist *.mc? del *.mc?
  if exist *.o   del *.o
  if exist *.obj del *.obj
  if exist *.out del *.out
  if exist *.par del *.par
  if exist *.psv del *.psv
  if exist *.p0? del *.p0?
  if exist *.r0? del *.r0?
  if exist *.rep del *.rep
  if exist *.run del *.run
  if exist *.std del *.std
  if exist *.swp del *.swp
  if exist *.sym del *.sym
  if exist *.tmp del *.tmp
  if exist admodel.dep del admodel.dep
  if exist eigv.rpt del eigv.rpt
  if exist sims. del sims.
  if exist variance. del variance.
  if exist tmp_admb. del tmp_admb.
  Echo ***** Files Deleted *****

:Exit
