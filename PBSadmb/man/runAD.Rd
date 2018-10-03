\name{runAD}
\alias{runAD}
\title{ Run an Executable Binary File }
\description{
  Run the executable binary file \code{'prefix'.exe} that was created by 
  \code{makeAD}.
}
\usage{
runAD(prefix, argvec="", logfile=TRUE, add=TRUE, verbose=TRUE)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{argvec}{string scalar/vector of arguments appropriate for the executable
    \code{'prefix'.exe}.}
  \item{logfile}{logical: if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{logical: if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{logical: if \code{TRUE}, report the shell call an its messages to the R console.}
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
  \href{mailto:schnutej-dfo@shaw.ca}{Jon T. Schnute}, Scientist Emeritus\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2009-09-10}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.runAD}.
}
\seealso{
  \code{\link{runMC}}, \code{\link{makeAD}}, \code{\link{cleanAD}}
}
\keyword{programming}
\keyword{interface}
