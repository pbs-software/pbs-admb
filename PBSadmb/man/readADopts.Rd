\name{readADopts}
\alias{readADopts}
\title{
  Read an ADMB Options List into Memory From a File
}
\description{
  Create a \code{PBSoptions} class object called \code{.PBSadmb} (stored in the
  package's working environment \code{PBSadmbEnv}) and read in ADMB options 
  from an ASCII text file using a \code{load} function that ultimately calls
  \code{PBSmodelling::readList}.
}
\usage{
  readADopts(optfile="ADopts.txt")
}
\arguments{
  \item{optfile}{string name of an ASCII text file containing ADMB 
    options information.}
}
\value{
  No values returned. Reads the ADMB options into the list object \code{.PBSadmb}.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\note{
  A wrapper function that can be called from a GUI exists as 
  \code{.win.readADopts} (\emph{not currently used}).
}
\seealso{
  \code{\link[PBSadmb]{setADMBpath}}, \code{\link[PBSadmb]{readADpaths}},
  \code{\link[PBSadmb]{writeADopts}}, \code{\link[PBSmodelling]{loadOptions}}
}
\keyword{ data }
\keyword{ list }
