\name{copyFiles}
\alias{copyFiles}
\title{ Copy System Files }
\description{
  Copy files with specified prefixes and suffixes from one location to another.
}
\usage{
copyFiles(prefix, suffix=NULL, dir0=getwd(), dir1=getwd(), ask=TRUE)
}
\arguments{
  \item{prefix}{ string scalar/vector of potential file prefixes. }
  \item{suffix}{ string scalar/vector of potential file suffixes. }
  \item{dir0}{ source directory from which to copy files. }
  \item{dir1}{ destination directory to copy files to. }
  \item{ask}{ logical: if \code{TRUE}, popup boxes will prompt the user
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
\author{ Rowan Haigh, Pacific Biological Station, Nanaimo, BC }
\seealso{ \code{\link{editAD}} }
\keyword{ utilities }
\keyword{ IO }
