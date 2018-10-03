\name{appendLog}
\alias{appendLog}
\title{ Append Data to Log File }
\description{
  Append summary information or output to a previously created log file.
}
\usage{
appendLog(prefix, lines)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{lines}{data to append to \code{'prefix'.log}).}
}
\value{
  No explicit value reurned. Appends data into a log file 
  \code{'prefix'.log}.
}
\author{
  \href{mailto:schnutej-dfo@shaw.ca}{Jon T. Schnute}, Scientist Emeritus\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2009-09-10}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.appendLog}.
}
\seealso{ \code{\link{startLog}}, \code{\link{editADfile}} }
\keyword{file}
