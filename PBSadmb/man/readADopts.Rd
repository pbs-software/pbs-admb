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
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC\cr
  Simon Fraser University (Burnaby BC), Vancouver Island University (Nanaimo BC)\cr
  Last modified \code{Rd: 2019-03-14}
}
\note{
  A wrapper function that can be called from a GUI exists as 
  \code{.win.readADopts} (\emph{not currently used}).
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{setADpath}},
  \code{\link[PBSadmb]{readADpaths}},
  \code{\link[PBSadmb]{writeADopts}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{loadOptions}}
}
\keyword{ data }
\keyword{ list }
