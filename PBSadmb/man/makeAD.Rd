\name{makeAD}
\alias{makeAD}
\alias{.win.makeAD}
\alias{.setPath}
\title{
  Make an Executable Binary File from a C File
}
\description{
  Essentially a wrapper function that calls in sequence: \code{convAD},
  \code{compAD}, and \code{linkAD}.
}
\usage{
makeAD(prefix, raneff=FALSE, safe=TRUE, dll=FALSE, debug=FALSE, 
   logfile=TRUE, add=TRUE, verbose=TRUE, pathfile=NULL)

.win.makeAD(winName="PBSadmb")
.setPath(pathfile)
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'}).}
  \item{raneff}{\code{logical} -- if \code{TRUE}, use the random effects model, otherwise use the normal model}
  \item{safe}{\code{logical} -- if \code{TRUE}, use safe mode with bounds checking on all
    array objects, otherwise use optimized mode for fastest execution.}
  \item{dll}{\code{logical} -- if \code{TRUE}, create dll (rather than executable)}
  \item{debug}{\code{logical} -- if \code{TRUE}, compile with debug symbols}
  \item{logfile}{\code{logical} -- if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{\code{logical} -- if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{\code{logical} -- if \code{TRUE}, report the shell call an its messages to the R console.}
  \item{pathfile}{\code{character} -- name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, and \code{editor}.}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  This function uses the C++ comiler declared in \code{.PBSadmb}. If \code{logfile=TRUE},
  any errors will appear in \code{'prefix'.log}. If \code{verbose=TRUE},
  they will appear in the R console.
}
\value{
  Returns nothing. The three functions called by \code{makeAD} each 
  return the shell call and its messages.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.makeAD}.
  
  Internally to \code{makeAD}, \code{convAD}, \code{compAD}, and \code{linkAD}, 
  \code{.setPath(pathfile)} sets the temporary environment path to run ADMB.

  The optional \code{pathfile} is offered for use in command (non-GUI) functions. 
  Users can easily create this file in a text editor.
  Note that on UNIX systems, only the \code{admbpath} (actually the path to ADMB's home directory)
  and text editor are used because the MinGW compiler (\code{gccpath}) and UNIX tools are already recognised.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{convAD}},
  \code{\link[PBSadmb]{compAD}},
  \code{\link[PBSadmb]{linkAD}},
  \code{\link[PBSadmb]{runAD}},
  \code{\link[PBSadmb]{readADpaths}}
}
\keyword{programming}
\keyword{interface}
\concept{ADMB}

