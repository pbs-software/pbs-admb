\name{linkAD}
\alias{linkAD}
\title{ Link Object Files to Make an Executable }
\description{
  Links the binary object file \code{'prefix'.o} to the ADMB libraries and 
  produces the executable file \code{'prefix'.exe}.
}
\usage{
linkAD(prefix, raneff=FALSE, safe=TRUE, dll=FALSE, debug=FALSE, 
   logfile=TRUE, add=TRUE, verbose=TRUE, pathfile=NULL)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{raneff}{logical: use the random effects model, otherwise use the normal model.}
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
  This function uses the C++ comiler declared in \code{.PBSadmb}. If \code{logfile=TRUE},
  any errors will appear in \code{'prefix'.log}. If \code{verbose=TRUE},
  they will appear in the R console.
}
\value{
  Invisibly returns the shell call and its messages.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2015-01-27}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.linkAD}.

  The optional \code{pathfile} is offered for use in command (non-GUI) functions. 
  Users can easily create this file in a text editor.
  Note that on UNIX systems, only the \code{admbpath} (actually the path to ADMB's home directory)
  and text editor are used because the MinGW compiler (\code{gccpath}) and UNIX tools are already recognised.
}
\seealso{
  \code{\link[PBSadmb]{convAD}}, \code{\link[PBSadmb]{compAD}}, 
  \code{\link[PBSadmb]{makeAD}}, \code{\link[PBSadmb]{readADpaths}}
}
\keyword{programming}
\keyword{interface}
