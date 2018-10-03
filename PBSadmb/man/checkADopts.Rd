\name{checkADopts}
\alias{checkADopts}
\title{ Check ADMB Options for Path Integrity}
\description{
  Check that the options object \code{.PBSadmb} has all required components
  and that paths point to actual files on the hard drive.
}
\usage{
checkADopts(opts=getOptions(atcall(.PBSadmb)),
   check=c("admbpath","gccpath","msysbin","editor"), 
   warn=TRUE, popup=FALSE, verify=TRUE)
}
\arguments{
  \item{opts}{\code{list} -- ADMB options values.}
  \item{check}{\code{character} -- components (directory paths) of \code{.PBSadmb} to check.}
  \item{warn}{\code{logical} -- if \code{TRUE}, print the results of the check to the R console.}
  \item{popup}{\code{logical} -- if \code{TRUE}, display program location problems in a popup GUI.}
  \item{verify}{\code{logical} -- if \code{TRUE}, then the \sQuote{Verify} button in the GUI was pressed.
    This only affects the message generation \sQuote{All programs found}. Command line call does not depend on GUI.}
}
\value{
  Boolean value where \code{TRUE} indicates all programs were located in the 
  specified directories and \code{FALSE} if at least one program cannot be found.
  The returned Boolean scalar has two attributes: \cr
  \code{warn} -- named list of test results, and \cr
  \code{message} -- named vector of test results.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-10-03}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.checkADopts}.
}
\seealso{ \code{\link[PBSadmb]{setADpath}},\code{\link[PBSadmb]{readADopts}} }
\keyword{ data }
\keyword{ list }
