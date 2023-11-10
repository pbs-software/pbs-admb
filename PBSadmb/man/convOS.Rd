\name{convOS}
\alias{convOS}
\title{
  Convert Text Files to Default OS Format
}
\description{
  Convert text files to the default format of the operating system.
}
\usage{
convOS(inam, onam = inam, path = getwd() )
}
\arguments{
  \item{inam}{string vector of names specifying files to be converted to the format of the operating system.}
  \item{onam}{string vector of name specifying the output files (the default overwrites the input file).}
  \item{path}{string specifying the path where the input files are located (defaults to current working directory).}
}
\value{
  Text file(s) formatted in accordance with standards of the operating system.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2009-12-04}
}
\note{
  This function essentially executes a \code{readLines} command followed by a call to \code{writeLines}.
}
\seealso{
  \code{\link{copyFiles}}, \code{.addQuotes}
}
\keyword{character}
\keyword{file}

