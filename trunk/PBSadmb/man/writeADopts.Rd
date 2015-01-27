\name{writeADopts}
\alias{writeADopts}
\title{
  Writes the ADMB Options List from Memory to a File
}
\description{
  Writes the global ADMB options list to a file using the function
  \code{PBSmodelling::saveOptions}.
}
\usage{
writeADopts(optfile="ADopts.txt")
}
\arguments{
  \item{optfile}{string name of the intended output file.}
}
\value{
  Returns \code{opts} invisibly. Writes the options list object to an ASCII file.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\note{
  A wrapper function that can be called from a GUI exists as 
  \code{.win.writeADopts} (\emph{not currently used}).
}
\seealso{
  \code{\link[PBSadmb]{readADopts}}, \code{\link[PBSadmb]{setADMBpath}},
  \code{\link[PBSmodelling]{saveOptions}}
}
\keyword{ data }
\keyword{ list }
