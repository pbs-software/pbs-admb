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
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2018-09-28}
}
\seealso{
  \code{\link{editAD}}, \code{\link{setADpath}}
}
\keyword{file}

