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
  \item{inam}{\code{character} -- vector of names specifying files to be converted to the format of the operating system}
  \item{onam}{\code{character} -- vector of name specifying the output files (the default overwrites the input file)}
  \item{path}{\code{character} -- specifying the path where the input files are located (defaults to current working directory)}
}
\value{
  Text file(s) formatted in accordance with standards of the operating system.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  This function essentially executes a \code{readLines} command followed by a call to \code{writeLines}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{copyFiles}},
  \code{\link[PBSadmb]{.addQuotes}}
}
\keyword{character}
\keyword{file}

