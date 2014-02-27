\name{readADpaths}
\alias{readADpaths}
\alias{saveADpaths}
\title{
  Read ADMB Paths From a Text File
}
\description{
  Read or save ADMB paths (\code{admbpath}, \code{gccpath}, \code{editor}) from 
  or to a simple, 2-column text file where the first column gives the R variable
  name and the second column specifies the path (enclosed by double quotation marks \dQuote{}).
}
\usage{
readADpaths(pathfile)
saveADpaths(pathfile)
}
\arguments{
  \item{pathfile}{string name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, and \code{editor}.}
}
\details{
  The simplest way to pass valid paths to the options manager in \pkg{PBSadmb} is to supply a text file in the working directory.
  This file can be constructed easily using any text editor. An example might look like:

  \preformatted{
  admbpath "C:/admb"
  gccpath  "C:/mingw"
  editor   "C:/Windows/System32/notepad.exe"
  }
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\note{
  On UNIX systems the MinGW compiler and UNIX tools are readily available; 
  therefore, only the \code{admbpath} (path to ADMB's home directory)
  and the path to a text editor are needed.

  The user can supply any number of paths in a \code{pathfile}, which are passed to 
  the options manager; however, only the above three paths are used at present.
}
\seealso{
  \code{\link[PBSadmb]{readADopts}},
  \code{\link[PBSadmb]{convAD}}, \code{\link[PBSadmb]{compAD}},
  \code{\link[PBSadmb]{linkAD}}, \code{\link[PBSadmb]{makeAD}}
}
\keyword{ data }
\keyword{ list }
