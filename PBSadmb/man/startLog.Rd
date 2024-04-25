\name{startLog}
\alias{startLog}
\alias{.win.startLog}
\title{
  Start a Log File
}
\description{
  Start a log file by removing any previous version and 
  appending header information.
}
\usage{
startLog(prefix)
.win.startLog(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'})}
  \item{winName}{\code{character} -- name of GUI window}
}
\value{
  No explicit value returned. Writes header lines into a log file 
  \code{'prefix'.log}.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.startLog}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{appendLog}},
  \code{\link[PBSadmb]{editAD}}
}
\keyword{file}
\keyword{IO}
