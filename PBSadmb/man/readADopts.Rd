\name{readADopts}
\alias{readADopts}
\alias{writeADopts}
\alias{.initOptions}
\title{
  Read/Write ADMB Options List From/To File
}
\description{
  Read ADMB options list into memory from a file.\cr
  Write ADMB options list from memory into a file.
}
\usage{
readADopts(optfile="ADopts.txt")
writeADopts(optfile="ADopts.txt")
.initOptions()
}
\arguments{
  \item{optfile}{\code{character} -- name of an ASCII text file containing ADMB 
    options information}
}
\details{
  Create a \code{PBSoptions} class object called \code{.PBSadmb} (stored in the
  package's working environment \code{PBSadmbEnv}) and read in ADMB options 
  from an ASCII text file using a \code{load} function that ultimately calls
  \code{PBSmodelling::readList}.

  Writes the global ADMB options list to a file using the function
  \code{PBSmodelling::saveOptions}.

  Dot function \code{'.initOptions'} basically calls \code{'readADopts'} if
  it's the first time (i.e., no options are set in the object \code{'.PBSadmb'}
  in the environment \code{'.PBSadmbEnv'}).
}
\value{
  \code{readADopts} -- no values returned; reads the ADMB options into the list object \code{'.PBSadmb'}.\cr
  \code{writeADopts} -- returns \code{opts} invisibly; writes the options list object to an ASCII file.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{atget}},
  \code{\link[PBSadmb]{readADpaths}},
  \code{\link[PBSadmb]{setADpath}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{getOptions}},
  \code{\link[PBSmodelling]{loadOptions}}
}
\keyword{data}
\keyword{list}
\concept{options}
