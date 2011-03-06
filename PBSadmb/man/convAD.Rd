\name{convAD}
\alias{convAD}
\title{ Convert TPL Code to CPP Code }
\description{
  Convert code in \code{'prefix'.tpl} to C++ code in \code{'prefix'.cpp}.
}
\usage{
convAD(prefix, raneff=FALSE, safe=TRUE, dll=FALSE, debug=FALSE, logfile=TRUE, add=TRUE, verbose=TRUE)
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
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.convAD}.
}
\seealso{ \code{\link{compAD}}, \code{\link{linkAD}}, \code{\link{makeAD}} }
\keyword{programming}
\keyword{interface}
