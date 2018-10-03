\name{admb}
\alias{admb}
\title{
 Start the PBSadmb GUI for ADMB
}
\description{
  Start up the PBSadmb GUI for controlling and running AD Model Builder.
}
\usage{
admb(prefix="", wdf="admbWin.txt", pathfile="ADpaths.txt")
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{wdf}{string name of the \emph{window description file} that creates the GUI.}
  \item{pathfile}{string name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, and \code{editor}.}
}
\details{
  The \code{pathfile} has now replaced the \code{optfile}, which no longer 
  exists as an argument. The \code{pathfile} identifies valid paths, which are
  passed to the options manager.
  Additionally, the user can change whichever path settings s/he wishes in the GUI.

  The package still recognizes the file \code{ADopts.txt}, which has been 
  demoted to a back-up file that will be saved only when the user pushes the
  Save button on the first tab in the GUI.
  If \code{ADopts.txt} exists and the options file \code{.PBSadmb} is not
  available (i.e. \code{NULL}), then \code{ADopts.txt} will be used on start up
  to populate the GUI.
}
\note{
  On UNIX systems the MinGW compiler and UNIX tools are readily available; 
  therefore, only the \code{admbpath} (path to ADMB's home directory)
  and the path to a text editor are needed.
}
\section{Warning}{
  Do not call your \code{pathfile="ADopts.txt"} unless you want to tempt fate.
}
\author{
  \href{mailto:schnutej-dfo@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-09-28}
}
\seealso{ 
  \code{\link[PBSadmb]{setADpath}}, \code{\link[PBSadmb]{readADopts}}, 
  \code{\link[PBSadmb]{readADpaths}}, \code{\link[PBSadmb]{makeAD}}
}
\keyword{IO}

