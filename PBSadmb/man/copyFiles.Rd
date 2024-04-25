\name{copyFiles}
\alias{copyFiles}
\title{ Copy System Files }
\description{
  Copy files with specified prefixes and suffixes from one location to another.
}
\usage{
copyFiles(prefix, suffix=NULL, srcdir=getwd(), dstdir=getwd(), ask=TRUE)
}
\arguments{
  \item{prefix}{\code{character} -- scalar/vector of potential file prefixes. }
  \item{suffix}{\code{character} -- scalar/vector of potential file suffixes. }
  \item{srcdir}{\code{character} -- source directory from which to copy files. }
  \item{dstdir}{\code{character} -- destination directory to copy files to. }
  \item{ask}{\code{logical} -- if \code{TRUE}, popup boxes will prompt the user
    for every instance that a file will be overwritten. }
}
\details{
  This function uses R's \code{list.files} and \code{file.copy} functions.
  The pattern recognition tends not to work when given the wildcard character
  \code{*}; however, the user may use this character, and the code will interpret it.
}
\value{
  Invisibly returns a Boolean vector with names of files that have been copied or not.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{editAD}}
}
\keyword{utilities}
\keyword{IO}
