\name{admb}
\alias{admb}
\title{
 Start the PBSadmb GUI for ADMB
}
\description{
  Start up the PBSadmb GUI for controlling and running AD Model Builder.
}
\usage{
admb(prefix="", wdf="admbWin.txt", optfile="ADopts.txt", pathfile)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{wdf}{string name of the \emph{window description file} that creates the GUI.}
  \item{optfile}{string name of options file (usually in user's working directory).}
  \item{pathfile}{string name of simple 2-column text file that details the relevant paths -- admbpath, gccpath, editor.}
}
\details{
  If a valid \code{pathfile} is specified, the directory paths in this file 
  will supercede paths specified in a valid \code{optfile} upon start-up.
  After that, the user can change whichever setup settings s/he wishes.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC 
}
\note{
  Note that on UNIX systems, only the \code{admbpath} (actually the path to ADMB's home directory)
  and text editor are used because the MinGW compiler (\code{gccpath}) and UNIX tools are already recognised.
}
\seealso{ 
  \code{\link[PBSadmb]{setADMBPath}}, \code{\link[PBSadmb]{readADopts}}, 
  \code{\link[PBSadmb]{readADpaths}}, \code{\link[PBSadmb]{makeAD}}
}
\keyword{ IO }

