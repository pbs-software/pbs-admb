\name{startLog}
\alias{startLog}
\title{ Start a Log File }
\description{
  Start a log file by removing any previous version and 
  appending header information.
}
\usage{
startLog(prefix)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
}
\value{
  No explicit value reurned. Writes header lines into a log file 
  \code{'prefix'.log}.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.startLog}.
}
\seealso{ \code{\link{appendLog}}, \code{\link{editADfile}} }
\keyword{file}
