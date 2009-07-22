\name{editADfile}
\alias{editADfile}
\title{ Edit a File }
\description{
  Edit a file using the text editor specified in \code{.ADopts}.
}
\usage{
editADfile(fname)
}
\arguments{
  \item{fname}{string name of file in current working directory
    (or elsewhere if path delimited by \code{/} or \code{\\}).}
}
\value{
  Returns Boolean: \code{TRUE} if file exists, \code{FALSE} if it does not.
}
\note{
  This function explicitly uses the editor chosen for PBSadmb. PBSmodelling
  has another function \code{openFile} that uses Windows file associations
  or an application specified with \code{setPBSext}.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\seealso{
  \code{\link{editAD}}, \code{\link{makeADopts}}
}
\keyword{file}

