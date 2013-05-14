\name{compAD}
\alias{compAD}
\title{ Compile C Code }
\description{
  Compile C++ code in \code{'prefix'.cpp} to create a binary object
  file \code{'prefix'.o}.
}
\usage{
compAD(prefix, raneff=FALSE, safe=TRUE, dll=FALSE, debug=FALSE, 
       logfile=TRUE, add=TRUE, verbose=TRUE)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{raneff}{logical: use the random effects model, otherwise use the normal model
    (currently does not influence the compile stage, but the argument is preserved
    here for future development).}
  \item{safe}{logical: if \code{TRUE}, use safe mode with bounds checking on all
    array objects, otherwise use optimized mode for fastest execution.}
  \item{dll}{create dll (rather than executable)}
  \item{debug}{compile with debug symbols}
  \item{logfile}{logical: if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{logical: if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{logical: if \code{TRUE}, report the shell call an its messages to the R console.}
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
  A wrapper function that can be called from a GUI exists as \code{.win.compAD}.
}
\seealso{ \code{\link{convAD}}, \code{\link{linkAD}}, \code{\link{makeAD}} }
\keyword{programming}
\keyword{interface}
