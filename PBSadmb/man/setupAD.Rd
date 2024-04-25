\name{setupAD}
\alias{setupAD}
\title{
  Set Up Paths for PBSadmb
}
\description{
  Set up path information by reading from a \code{pathfile} 
  (default = \code{ADpaths.txt}) and checking that certain executable
  files exist.
}
\usage{
setupAD(pathfile)
}
\arguments{
  \item{pathfile}{\code{character} -- name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, and \code{editor}.}
}
\details{
  This program is useful primarily for console-based function calls. 
  It sets up the background options for \pkg{PBSadmb} functions (\code{convAD},
  \code{compAD}, \code{linkAD}) by reading paths from a file and checking
  to make sure that they are valid (i.e., contain certain exectuable files like 
  \code{tpl2cpp} and \code{g++}). The options are store in a PBSoptions class 
  objects called \code{.PBSadmb} in the temporary environment \code{.PBSadmbEnv}.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-23}
}
\note{
  To access the options manager in the temporary working environment, use the \pkg{PBSadmb}
  accessor functions (\code{atget}, \code{atput}, \code{atcall}, \code{atprint}). For eample:

  \code{atprint(.PBSadmb)}

  On UNIX systems the MinGW compiler and UNIX tools are readily available; 
  therefore, only the \code{admbpath} (path to ADMB's home directory)
  and the path to a text editor are needed.
}
\seealso{ 
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{readADpaths}},
  \code{\link[PBSadmb]{checkADopts}},
  \code{\link[PBSadmb]{makeAD}}
}
\keyword{ manip }

