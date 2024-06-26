\name{readRep}
\alias{readRep}
\alias{.win.readRep}
\alias{.win.viewRep}
\title{
  Read an ADMB Report into R Memory
}
\description{
  Import ADMB-generated report files into R's memory using the names of the 
  report files to name the R-objects.
}
\usage{
readRep(prefix, suffix=c(".cor",".rep",".std",".mc.dat"), global=FALSE)

.win.readRep(winName="PBSadmb")
.win.viewRep(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'})}
  \item{suffix}{\code{character} -- scalar/vector specifying one or more suffixes}
  \item{global}{\code{logical} -- if \code{TRUE}, save the imported reports as objects to 
    global environment using the same names as the report files}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  If the report object is one of \code{c(".cor", ".std", ".mc.dat")}, the
  report object is a data frame, otherwise it is a string vector. Multiple 
  report objects are returned as a list of objects. A single report object is 
  returned as the object itself.

  This function attempts to detect the file format from a number of possibilities.
  For example, if the file has the special format recognized by PBSmodelling,
  then the function returns a list with named components. The example \code{vonb}
  included with this package shows how to write the template to get consistent
  variable names between ADMB and R. See the User's Guide for complete details.
}
\value{
  Invisibly returns the list of report objects. If only one report is 
  imported, a single report object is returned.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.readRep}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{editAD}}
}
\keyword{file}
\keyword{manip}

