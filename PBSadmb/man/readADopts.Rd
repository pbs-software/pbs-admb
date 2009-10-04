\name{readADopts}
\alias{readADopts}
\title{ Reads an ADMB Options List into Memory From a File}
\description{
  Reads ADMB options into a global, hidden list object 
  called \code{.ADopts} from an ASCII text file using \cr
  \code{PBSmodelling::readList}).
}
\usage{
readADopts(optfile="ADopts.txt")
}
\arguments{
  \item{optfile}{string name of an ASCII text file containing ADMB 
    options information.}
}
\value{
  No values returned. Reads the ADMB options into the list object \code{.ADopts}.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.readADopts}.
}
\seealso{ \code{\link{makeADopts}},\code{\link{writeADopts}} }
\keyword{ data }
\keyword{ list }
