\name{editADfile}
\alias{editADfile}
\title{Edit an ADMB File}
\description{
  Edit an ADMB file using the text editor specified in \code{.PBSadmb}.
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
  This function explicitly uses the editor chosen for \pkg{PBSadmb}. 
  \pkg{PBSmodelling} has another function \code{openFile} that uses Windows
  file associations or an application specified with \code{setPBSext}.
}
\author{Jon T. Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC}
\seealso{
  \code{\link{editAD}}, \code{\link{setADMBpath}}
}
\keyword{file}

