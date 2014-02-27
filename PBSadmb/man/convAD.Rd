\name{convAD}
\alias{convAD}
\title{ Convert TPL Code to CPP Code }
\description{
  Convert code in \code{'prefix'.tpl} to C++ code in \code{'prefix'.cpp}.
}
\usage{
convAD(prefix, raneff=FALSE, safe=TRUE, dll=FALSE, debug=FALSE, 
   logfile=TRUE, add=TRUE, verbose=TRUE, pathfile=NULL)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{raneff}{logical: if \code{TRUE}, use the random effects model executable
    \code{tpl2rem.exe}, otherwise use the normal model executable \code{tpl2cpp.exe}.}
  \item{safe}{logical: if \code{TRUE}, use safe mode with bounds checking on all
    array objects, otherwise use optimized mode for fastest execution.}
  \item{dll}{create dll (rather than executable)}
  \item{debug}{compile with debug symbols}
  \item{logfile}{logical: if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{logical: if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{logical: if \code{TRUE}, report the shell call an its messages to the R console.}
  \item{pathfile}{string name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, and \code{editor}.}
}
\details{
  This function invokes the ADMB command \code{tpl2cpp.exe} or \code{tpl2rem.exe},
  if \code{raneff} is \code{FALSE} or \code{TRUE} respectively. If \code{logfile=TRUE},
  any errors will appear in \code{'prefix'.log}. If \code{verbose=TRUE},
  they will appear in R console.
}
\value{
  Invisibly returns the shell call and its messages.
}
\author{ Jon T. Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC. }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.convAD}.

  The optional \code{pathfile} is offered for use in command (non-GUI) functions. 
  Users can easily create this file in a text editor.
  Note that on UNIX systems, only the \code{admbpath} (actually the path to ADMB's home directory)
  and text editor are used because the MinGW compiler (\code{gccpath}) and UNIX tools are already recognised.
}
\seealso{
  \code{\link[PBSadmb]{compAD}}, \code{\link[PBSadmb]{linkAD}}, 
  \code{\link[PBSadmb]{makeAD}}, \code{\link[PBSadmb]{readADpaths}}
}
\keyword{programming}
\keyword{interface}
