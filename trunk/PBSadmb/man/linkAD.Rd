\name{linkAD}
\alias{linkAD}
\title{ Link Object Files to Make an Executable }
\description{
  Links the binary object file \code{'prefix'.o} to the ADMB libraries and 
  produces the executable file \code{'prefix'.exe}.
}
\usage{
linkAD(prefix, raneff=FALSE, safe=TRUE, logfile=TRUE, add=TRUE, 
       verbose=TRUE, comp="GCC")
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{raneff}{logical: use the random effects model, otherwise use the normal model.}
  \item{safe}{logical: if \code{TRUE}, use safe mode with bounds checking on all
    array objects, otherwise use optimized mode for fastest execution.}
  \item{logfile}{logical: if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{logical: if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{logical: if \code{TRUE}, report the shell call an its messages to the R console.}
  \item{comp}{string: compiler to use - "GCC" is only currently supported}
}
\details{
  This function uses the C++ comiler declared in \code{.ADopts}. If \code{logfile=TRUE},
  any errors will appear in \code{'prefix'.log}. If \code{verbose=TRUE},
  they will appear in the R console.
}
\value{
  Invisibly returns the shell call and its messages.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.linkAD}.
}
\seealso{ \code{\link{convAD}}, \code{\link{compAD}}, \code{\link{makeAD}} }
\keyword{programming}
\keyword{interface}
