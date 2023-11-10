\name{editAD}
\alias{editAD}
\title{ Edit ADMB Files }
\description{
  Edit files associated with specified prefix and suffixes.
}
\usage{
editAD(prefix, suffix=c(".tpl",".cpp",".log"))
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{suffix}{string scalar/vector specifying one or more suffixes.}
}
\value{
  Invisibly returns Boolean vector with elements \code{TRUE} if files exist, 
  \code{FALSE} if they do not.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2018-09-28}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.editAD}.
  
  This function explicitly uses the editor chosen for PBSadmb. PBSmodelling
  has another function \code{openFile} that uses Windows file associations
  or an application specified with \code{setPBSext}.
}
\seealso{
  \code{\link{editADfile}}, \code{\link{setADpath}}
}
\keyword{file}

