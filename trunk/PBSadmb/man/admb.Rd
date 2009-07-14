\name{admb}
\alias{admb}
\title{ Start the PBS ADMB GUI }
\description{
  Start up the PBS GUI for running ADMB.
}
\usage{
admb(prefix="", pkg="PBSadmb", wdf="admbWin.txt", 
     optfile="ADopts.txt", hnam=NULL)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{pkg}{string name of package (redundant but may allow flexibility in future).}
  \item{wdf}{string name of the \emph{window description file} that creates the GUI.}
  \item{optfile}{string name of options file (usually in user's working directory).}
  \item{hnam}{string name of history file (not currently used, no history widget).}
}

\author{ Rowan Haigh, Pacific Biological Station, Nanaimo BC, Canada }
\seealso{ \code{\link{initAD}}, \code{\link{makeADopts}} }
\keyword{ IO }

