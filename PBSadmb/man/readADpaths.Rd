\name{readADpaths}
\alias{readADpaths}
\alias{saveADpaths}
\alias{.win.readADpaths}
\alias{.win.saveADpaths}
\title{
  Read ADMB Paths From a Text File
}
\description{
  Read or save ADMB paths (\code{admbpath}, \code{gccpath}, \code{msysbin}, \code{editor}) from 
  or to a simple, 2-column text file where the first column gives the R variable
  name and the second column specifies the path (enclosed by double quotation marks \dQuote{}).
}
\usage{
readADpaths(pathfile)
saveADpaths(pathfile)
.win.readADpaths(winName="PBSadmb")
.win.saveADpaths(winName="PBSadmb")
}
\arguments{
  \item{pathfile}{string name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, \code{msysbin}, and \code{editor}.}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  The simplest way to pass valid paths to the options manager in \pkg{PBSadmb}
  is to supply a text file in the working directory.
  This file can be constructed easily using any text editor. An example might look like:
\preformatted{
admbpath "C:/Apps/admb_13.2"
gccpath  "C:/Apps/R/Rtools44/x86_64-w64-mingw32.static.posix"
msysbin  "C:/Apps/R/Rtools44/usr/bin"
editor   "C:/Apps/UltraEdit/Uedit32.exe"
}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  On UNIX systems the MinGW compiler and UNIX tools are readily available; 
  therefore, only the \code{admbpath} (path to ADMB's home directory)
  and the path to a text editor are needed.

  The user can supply any number of paths in a \code{pathfile}, which are passed to 
  the options manager; however, only the above four paths are used at present.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{readADopts}},
  \code{\link[PBSadmb]{convAD}},
  \code{\link[PBSadmb]{compAD}},
  \code{\link[PBSadmb]{linkAD}},
  \code{\link[PBSadmb]{makeAD}}
}
\keyword{data}
\keyword{list}
\concept{options}
