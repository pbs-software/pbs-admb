\name{installADMB}
\alias{installADMB}
\title{ Install windows ADMB and MinGW binaries }
\description{
  Only applicable for Windows: Downloads and installs the windows ADMB and MinGW binaries.

  A user interface is displayed which allows to select to install either ADMB and/or MinGW
  for either 32bit or 64bit versions of Windows. Default installation directories are suggested
  for installation; however, a user may choose to install the binaries elsewhere.

  A file "pathconfig.txt" is saved in the PBSadmb library (under R), which keeps track of the
  most recently installed locations.
}
\usage{
installADMB()
}
\keyword{file}
