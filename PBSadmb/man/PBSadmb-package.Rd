\name{PBSadmb}
\docType{package}
\alias{PBSadmb}
\alias{PBSadmb-package}
\title{PBS ADMB Package}

\description{
The R package \pkg{PBSadmb} gives complete R support to the external program
\href{https://www.admb-project.org/}{AD Model Builder}, released into the public
domain in 2009. The program offers users a remarkably efficient tool for 
estimating parameters and their uncertainty, based on complex nonlinear 
statistical models.

In a standard ADMB installation, users would interact with the program via a
DOS command shell (in Windows) or a bash shell (in Linux or Mac OS X). 
The package \pkg{PBSadmb} makes it possible to interact entirely from an R
console, as a common interface for all operating systems. A single R script can
encapsulate commands to ADMB, as well as all analyses that follow. 
The package includes protocols for writing code to make the integration 
between R and ADMB almost seamless.

\pkg{PBSadmb} also provides a Graphical User Interface (GUI) that facilitates
the steps required for a complete ADMB analysis. Both new and experienced users
can use the GUI for tutorial and educational purposes.

You can obtain \href{https://CRAN.R-project.org/package=PBSadmb}{PBSadmb} from the
Comprehensive R Archive Network (\href{https://CRAN.R-project.org}{CRAN}).
Always use the current version of \pkg{PBSadmb} with the most recent version of
\href{https://CRAN.R-project.org/package=PBSmodelling}{PBSmodelling},
another package available from CRAN. 

\pkg{PBSadmb} represents just one of a series of R packages developed at the 
Pacific Biological Station 
(\href{https://www.pac.dfo-mpo.gc.ca/science/facilities-installations/index-eng.html#pbs}{PBS})
in Nanaimo, British Columbia. A more advanced version of \pkg{PBSadmb} might be
available on GitHub under \href{https://github.com/pbs-software}{PBS Software}
in the repository \href{https://github.com/pbs-software/pbs-admb}{pbs-admb}.
Regardless, a user can install the GitHub version using:
\preformatted{
     devtools::install_github("pbs-software/pbs-admb/PBSadmb")
}
Note: not every revision has been checked for CRAN worthiness.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC

  Maintainer:\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-25}
}
\keyword{package}
\concept{ADMB}

