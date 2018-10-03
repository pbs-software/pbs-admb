\name{cleanAD}
\alias{cleanAD}
\title{ Clean ADMB-Generated Files from the Working Directory }
\description{
  Detects files in the working directory with the specified \code{prefix} and
  removes them all save those with the suffix \code{.tpl}, \code{.dat}, and \code{.pin}.
}
\usage{
cleanAD(prefix)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
}
\details{
  Aside from potential garbage files with the specified \code{prefix}, other 
  files associated with ADMB are detected. Also files \code{*.tmp} and \code{*.bak}
  are displayed. Calling \code{cleanAD} invokes the hidden function \code{.cleanUp},
  which creates a GUI menu of the potential garbage files. The user can select whichever
  files s/he wishes for disposal.
}
\value{
  Returns nothing. Invokes a GUI menu of potential garbage files.
}
\author{
  \href{mailto:schnutej-dfo@shaw.ca}{Jon T. Schnute}, Scientist Emeritus\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2009-09-10}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.cleanAD}.
}
\seealso{ \code{\link{makeAD}}, \code{\link{runAD}}, \code{\link{readRep}} }
\keyword{manip}

