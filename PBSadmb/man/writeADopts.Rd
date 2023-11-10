\name{writeADopts}
\alias{writeADopts}
\title{
  Writes the ADMB Options List from Memory to a File
}
\description{
  Writes the global ADMB options list to a file using the function
  \code{PBSmodelling::saveOptions}.
}
\usage{
writeADopts(optfile="ADopts.txt")
}
\arguments{
  \item{optfile}{\code{character} -- name of the intended output file.}
}
\value{
  Returns \code{opts} invisibly. Writes the options list object to an ASCII file.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC\cr
  Simon Fraser University (Burnaby BC), Vancouver Island University (Nanaimo BC)\cr
  Last modified \code{Rd: 2019-03-14}
}
\note{
  A wrapper function that can be called from a GUI exists as 
  \code{.win.writeADopts} (\emph{not currently used}).
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{atget}},
  \code{\link[PBSadmb]{readADopts}},
  \code{\link[PBSadmb]{setADpath}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{getOptions}},
  \code{setOptions} in \code{\link[PBSmodelling]{getOptions}},
  \code{saveOptions} in \code{\link[PBSmodelling]{loadOptions}}
}
\keyword{ data }
\keyword{ list }
