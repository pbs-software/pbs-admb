\name{editAD}
\alias{editAD}
\alias{editADfile}
\alias{.win.editAD}
\alias{.win.editPLT}
\title{ Edit ADMB Files }
\description{
  Edit files associated with specified prefix and suffixes.
}
\usage{
editAD(prefix, suffix=c(".tpl",".cpp",".log"))
editADfile(fname)
.win.editAD(winName="PBSadmb")
.win.editPLT()
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'})}
  \item{suffix}{\code{character} -- scalar/vector specifying one or more suffixes}
  \item{fname}{\code{character} -- name of file in current working directory
    (or elsewhere if path delimited by \code{/} or \code{\\})}
  \item{winName}{\code{character} -- name of GUI window}
}
\value{
  Invisibly returns Boolean vector with elements \code{TRUE} if files exist, 
  \code{FALSE} if they do not.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.editAD}.
  
  This function explicitly uses the editor chosen for PBSadmb. PBSmodelling
  has another function \code{openFile} that uses Windows file associations
  or an application specified with \code{setPBSext}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{setADpath}},
  \code{\link[PBSadmb]{copyFiles}}
}
\keyword{file}
\keyword{IO}

