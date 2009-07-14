\name{showADargs}
\alias{showADargs}
\title{ Show All Arguments for an ADMB Executable }
\description{
  Show all arguments available for an ADMB executable in the 
  default text editor.
}
\usage{
showADargs(prefix, ed=TRUE)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{ed}{logical: if \code{TRUE}, write the ADMB arguments to a file and 
    view them with the text editor, else display the arguments on the R console.}
}
\value{
  Invisibly returns the argument list.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.showADargs}.
}
\seealso{
  \code{\link{editADfile}}, \code{\link{runAD}}
}
\keyword{file}

