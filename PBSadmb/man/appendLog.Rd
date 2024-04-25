\name{appendLog}
\alias{appendLog}
\alias{.win.appendLog}
\title{
  Append Data to Log File
}
\description{
  Append summary information or output to a previously created log file.
}
\usage{
appendLog(prefix, lines)
.win.appendLog(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'})}
  \item{lines}{\code{character} -- data to append to \code{'prefix'.log})}
  \item{winName}{\code{character} -- name of GUI window}
}
\value{
  No explicit value returned. Appends data into a log file 
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
  A wrapper function that can be called from a GUI exists as \code{.win.appendLog}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{startLog}},
  \code{\link[PBSadmb]{editAD}}
}
\keyword{file}
\keyword{IO}
