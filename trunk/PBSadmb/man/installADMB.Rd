\name{installADMB}
\alias{installADMB}
\title{ Install windows admb binary (for gcc) }
\description{
  Only applicable for Windows: Downloads and installs the windows ADMB binary for gcc.
  ADMB is installed under PBSadmb's library directory under R.
}
\usage{
installADMB( arch = "32", skip.warning = FALSE )
}
\arguments{
  \item{arch}{which version to install: "32" or "64" bit versions}
  \item{skip.warning}{logical: if true, don't issue a warning when installing a 64bit version from a 32bit version of R}
}
\keyword{file}
