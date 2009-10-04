\name{admb}
\alias{admb}
\title{ Start the PBS ADMB GUI }
\description{
  Start up the PBS GUI for running ADMB.
}
\usage{
admb(prefix="", wdf="admbWin.txt", optfile="ADopts.txt")
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{wdf}{string name of the \emph{window description file} that creates the GUI.}
  \item{optfile}{string name of options file (usually in user's working directory).}
}

\author{ Rowan Haigh, Pacific Biological Station, Nanaimo BC, Canada }
\seealso{ \code{\link{makeADopts}} }
\keyword{ IO }

