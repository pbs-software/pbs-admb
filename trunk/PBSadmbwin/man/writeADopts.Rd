\name{writeADopts}
\alias{writeADopts}
\title{ Writes the ADMB Options List from Memory to a File}
\description{
  Writes the global ADMB options list to a file in 'PBS' format 
  (see \code{PBSmodelling::writeList}).
}
\usage{
writeADopts(opts=.ADopts, optfile="ADopts.txt")
}
\arguments{
  \item{opts}{the global options list object \code{.ADopts}.}
  \item{optfile}{string name of the intended output file.}
}
\value{
  Returns \code{opts} invisibly. Writes the options list object to an ASCII file.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.writeADopts}.
}
\seealso{ \code{\link{initAD}}, \code{\link{makeADopts}}, \code{\link{readADopts}} }
\keyword{ data }
\keyword{ list }
