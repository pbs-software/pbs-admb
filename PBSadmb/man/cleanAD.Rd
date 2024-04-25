\name{cleanAD}
\alias{cleanAD}
\alias{.cleanUp}
\alias{.cleanUpAgain}
\alias{.cleanWD}
\alias{.doCleanAD}
\alias{.win.cleanAD}
\alias{.win.findClean}
\title{
  Clean ADMB-Generated Files from Working Directory
}
\description{
  Detect files in the working directory with the specified \code{prefix} and
  removes them all, save those with the suffix \code{.tpl}, \code{.dat}, and \code{.pin}.
}
\usage{
cleanAD(prefix)
.cleanUp(prefix, suffix, files)
.cleanUpAgain(winName="cleanWindow")
.cleanWD(files)
.doCleanAD(winName="cleanWindow")
.win.cleanAD(winName="PBSadmb")
.win.findClean(winName="cleanWindow")
}
\arguments{
  \item{prefix}{\code{character} -- prefix of the ADMB project (e.g., \code{'vonb'})}
  \item{suffix}{\code{character} -- vector of suffixes/extensions for files to be removed}
  \item{files}{\code{character} -- vector of explicit file names for removal}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  Aside from potential garbage files with the specified \code{'prefix'}, other 
  files associated with ADMB are detected. Also files \code{'*.tmp'} and \code{'*.bak'}
  are displayed. Calling \code{'cleanAD'} invokes the hidden function \code{'.cleanUp'},
  which creates a GUI menu of the potential garbage files. The user can select whichever
  files desired for disposal.
}
\value{
  Returns nothing. Invokes a GUI menu of potential garbage files.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-25}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{'.win.cleanAD'}.
  
  \code{.cleanUp} -- Anisa Egeli's \code{PBSmodelling::cleanProj} function modified for flexibility.\cr
  \code{.cleanWD} -- clean all potential garbage files; Anisa Egeli's function \code{PBSmodelling::cleanWD}.\cr
  \code{.doCleanAD} -- Anisa Egeli's \code{PBSmodelling:::.doClean} function modified for file names only.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{makeAD}},
  \code{\link[PBSadmb]{runAD}},
  \code{\link[PBSadmb]{readRep}}
}
\keyword{manip}
\keyword{utilities}

