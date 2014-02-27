\name{admb}
\alias{admb}
\title{
 Start the PBSadmb GUI for ADMB
}
\description{
  Start up the PBSadmb GUI for controlling and running AD Model Builder.
}
\usage{
admb(prefix="", wdf="admbWin.txt", optfile="ADopts.txt", pathfile="ADpaths.txt")
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{wdf}{string name of the \emph{window description file} that creates the GUI.}
  \item{optfile}{string name of options file (usually in user's working directory). 
    This argument will likely be deprecated in some future version of the package.}
  \item{pathfile}{string name of 2-column text file that details the 
    relevant paths for the R variables \code{admbpath}, \code{gccpath}, and \code{editor}.}
}
\details{
  The \code{optfile} is being phased out and replaced by the \code{pathfile} for 
  idetifying valid paths, which are passed to the options manager.
  Additionally, the user can change whichever path settings s/he wishes in the GUI.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC 
}
\note{
  On UNIX systems the MinGW compiler and UNIX tools are readily available; 
  therefore, only the \code{admbpath} (path to ADMB's home directory)
  and the path to a text editor are needed.
}
\seealso{ 
  \code{\link[PBSadmb]{setADMBPath}}, \code{\link[PBSadmb]{readADopts}}, 
  \code{\link[PBSadmb]{readADpaths}}, \code{\link[PBSadmb]{makeAD}}
}
\keyword{ IO }

