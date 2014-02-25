\name{readADpaths}
\alias{readADpaths}
\title{
  Read ADMB Paths From a Text File
}
\description{
  Read ADMB paths (admbpath, gccpath, editor) from a simple, 2-column text file 
  where the first column givens the R variable name and the second colum specifies
  the path (within double quotation marks \dQuote{}).
}
\usage{
readADpaths(pathfile)
}
\arguments{
  \item{pathfile}{string name of simple 2-column text file that details the relevant paths -- admbpath, gccpath, editor.}
}
\details{
  If a user is running \pkg{PBSadmb} functions from a command line only, the simplest
  way to supply the appropriate paths is via a text file that can be constructed easily by 
  the user in the project's working directory. An example might look like:

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
  Note that on UNIX systems, only the \code{admbpath} (actually the path to ADMB's home directory)
  and text editor are used because the MinGW compiler (\code{gccpath}) and UNIX tools are already recognised.
  
  Aside: The user can supply any number of paths to R variables and these are passed to 
  the options manager; however, at present only the above three paths are utilised.
}
\seealso{
  \code{\link[PBSadmb]{readADopts}},
  \code{\link[PBSadmb]{convAD}}, \code{\link[PBSadmb]{compAD}}
  \code{\link[PBSadmb]{linkAD}}, \code{\link[PBSadmb]{makeAD}}
}
\keyword{ data }
\keyword{ list }
