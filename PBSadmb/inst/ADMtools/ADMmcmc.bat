@Echo Off
  if !%1==! Goto Exit
  if !%2==! Goto Exit
  if !%3==! Goto Exit

Echo Run %1.exe with %2 simulations, saved every %3 steps
  %1.exe -mcmc %2 -mcsave %3 > %1.mc.log
  %1.exe -mceval > %1.mc.dat 
  goto End

:Exit
  echo ERROR - Specify: (1) a package name, (2) # simulations, and (3) sampling frequency
  echo example: %0 POP 100000 1000

:End