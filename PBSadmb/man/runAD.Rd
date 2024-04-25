\name{runAD}
\alias{runAD}
\alias{.win.runAD}
\title{
  Run an Executable Binary File
}
\description{
  Run the executable binary file \code{'prefix'.exe} that was created by 
  \code{makeAD}.
}
\usage{
runAD(prefix, argvec="", logfile=TRUE, add=TRUE, verbose=TRUE)

.win.runAD(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'}).}
  \item{argvec}{\code{character} -- scalar/vector of arguments appropriate for the executable
    \code{'prefix'.exe}.}
  \item{logfile}{\code{logical} -- if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{\code{logical} -- if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{\code{logical} -- if \code{TRUE}, report the shell call an its messages to the R console.}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  This function typically reads the two files \code{'prefix'.dat} and \code{'prefix'.pin},
  although in same cases one or both of these files may not be necessary.
  
  If \code{logfile=TRUE}, output (including error messages, if any) will appear in
  \code{'prefix'.log}. If \code{verbose=TRUE}, it will appear in the R console.
}
\value{
  Invisibly returns the results of the shell call.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-23}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.runAD}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{runMC}},
  \code{\link[PBSadmb]{makeAD}},
  \code{\link[PBSadmb]{cleanAD}}
}
\keyword{programming}
\keyword{interface}
\concept{ADMB}

