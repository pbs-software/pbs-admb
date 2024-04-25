\name{compAD}
\alias{compAD}
\alias{.win.compAD}
\title{
  Compile C Code
}
\description{
  Compile C++ code in \code{'prefix'.cpp} to create a binary object
  file \code{'prefix'.o}.
}
\usage{
compAD(prefix, raneff=FALSE, safe=TRUE, dll=FALSE, debug=FALSE, 
   logfile=TRUE, add=TRUE, verbose=TRUE, pathfile=NULL)

.win.compAD(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'}).}
  \item{raneff}{\code{logical} -- if \code{TRUE}, use the random effects model, otherwise use the normal model
    (currently does not influence the compile stage, but the argument is preserved
    here for future development).}
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
  Invisibly returns the shell call and its messages.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-23}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.compAD}.

  The optional \code{pathfile} is offered for use in command (non-GUI) functions. 
  Users can easily create this file in a text editor.
  Note that on UNIX systems, only the \code{admbpath} (actually the path to ADMB's home directory)
  and text editor are used because the MinGW compiler (\code{gccpath}) and UNIX tools are already recognised.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{convAD}},
  \code{\link[PBSadmb]{linkAD}},
  \code{\link[PBSadmb]{makeAD}},
  \code{\link[PBSadmb]{readADpaths}}
}
\keyword{programming}
\keyword{interface}
\concept{ADMB}

