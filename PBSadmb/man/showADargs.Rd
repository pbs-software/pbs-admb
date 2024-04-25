\name{showADargs}
\alias{showADargs}
\alias{.win.showADargs}
\alias{.win.showGUIargs}
\title{
  Show All Arguments for ADMB Executable
}
\description{
  Show all arguments available for an ADMB executable in the 
  default text editor.
}
\usage{
showADargs(prefix, ed=TRUE)
.win.showADargs(winName="PBSadmb")
.win.showGUIargs(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{"vonb"})}
  \item{ed}{\code{logical} -- if \code{TRUE}, write the ADMB arguments to a file and 
    view them with the text editor, else display the arguments on the R console}
  \item{winName}{\code{character} -- name of GUI window}
}
\value{
  Invisibly returns the argument list.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.showADargs}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{editADfile}},
  \code{\link[PBSadmb]{runAD}}
}
\keyword{file}
\concept{ADMB}

