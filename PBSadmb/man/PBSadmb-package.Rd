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

You can obtain \pkg{PBSadmb} from the
\href{https://CRAN.R-project.org/package=PBSadmb}{Comprehensive R Archive Network}.
Always use the current version of \pkg{PBSadmb} with the most recent version of
\pkg{PBSmodelling}, another package available from CRAN. 

\pkg{PBSadmb} represents just one of a series of R packages developed at the 
\href{https://www.pac.dfo-mpo.gc.ca/science/facilities-installations/index-eng.html#pbs}{Pacific Biological Station}
in Nanaimo, British Columbia. A more advanced version of \pkg{PBSadmb} might be
available on \href{https://github.com/pbs-software}{GitHub}. Any evolving
package (Windows binary and source tarball) is built after using CRAN's rigorous
\code{R CMD check --as-cran} routine (on a 64-bit Windows 10 system) and posted to
\href{https://drive.google.com/open?id=0B2Bkic2Qu5LGOGx1WkRySVYxNFU}{Google Drive}.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC

  Maintainer:\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-11-10}
}

\keyword{package}
