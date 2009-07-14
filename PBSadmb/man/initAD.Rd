\name{initAD}
\alias{initAD}
\title{ Inititialize the ADMB Options List }
\description{
  Checks for te specified ADMB options file and if available
  reads it into R's memory.
}
\usage{
initAD(optfile="ADopts.txt")
}
\arguments{
  \item{optfile}{string name of an options file in the user's working directory.}
}
\value{
  Returns a Boolean value, \code{TRUE} if the file was available and 
  read into R's memory.
}
\details{
..This function also creates the variable \code{.ADopts} in the global environment.
  Use \code{checkADopts} to ensure that the paths in \code{.ADopts} make sense.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.initAD}.
}
\seealso{ \code{\link{readADopts}}, \code{\link{makeADopts}},\code{\link{writeADopts}},
  \code{\link{checkADopts}}
}
\keyword{ IO }
\keyword{ list }
