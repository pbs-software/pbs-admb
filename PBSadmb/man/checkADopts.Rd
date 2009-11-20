\name{checkADopts}
\alias{checkADopts}
\title{ Check ADMB Options for Link Integrity}
\description{
  Check that \code{.ADopts} has all required components and
  that links point to actual files on the hard drive.
}
\usage{
checkADopts(opts=getOptions(.PBSadmb), check=c("admpath","gccpath","editor"),
            warn=TRUE, popup=FALSE)
}
\arguments{
  \item{opts}{ADMB options values.}
  \item{check}{components of \code{.ADopts} to check.}
  \item{warn}{logical: if \code{TRUE}, print the results of the check to the R console.}
  \item{popup}{logical: if \code{TRUE}, display program location problems in a popup GUI.}
}
\value{
  Boolean value where \code{TRUE} indicates all programs were located in the 
  specified directories and \code{FALSE} if at least one program cannot be found.
  The returned Boolean scalar has two attributes: \cr
  \code{warn} - named list of test results, and \cr
  \code{message} - named vector of test results.
}
\author{ Rowan Haigh, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.checkADopts}.
}
\seealso{ \code{\link{makeADopts}},\code{\link{readADopts}} }
\keyword{ data }
\keyword{ list }
