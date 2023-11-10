\name{setADpath}
\alias{setADpath}
\alias{setADver}
\title{ Create ADMB Options List }
\description{
  Creates an options list object detailing the pathways to the ADMB home directory,
  the GCC home directory, the MSYS (Unix utilities) bin directoy, and the user's preferred text editor.
  Also keeps track of software versions for ADMB and GCC.
}
\usage{
setADpath(admbpath, gccpath, msysbin, editor)
setADver(admbver, gccver)
}
\arguments{
  \item{admbpath}{\code{character} -- explicit path to the user's ADMB home directory.}
  \item{gccpath}{\code{character} -- explicit path to the user's GCC home directory.}
  \item{msysbin}{\code{character} -- explicit path to the user's MSYS bin directory (binary executables and libraries).}
  \item{editor}{\code{character} -- explicit path and program to use for editing text.}
  \item{admbver}{\code{character} -- version number of ADMB software.}
  \item{gccver}{\code{character} -- version number of g++ software.}
}
\value{
  Creates a global, hidden list object called \code{.PBSadmb},
  located in the temporary environment \code{.PBSadmbEnv}. 
  Use the functions \code{atget}, \code{atput}, \code{atcall}, and 
  \code{atprint} to get, put, call, and print the object \code{.PBSadmb}.
  The function \code{alisp} lists all the objects in the \code{.PBSadmbEnv}
  environment.
}
\note{
  These functions replace \code{makeADopts}. The old control file called 
  \code{ADopts.txt} is retained as a backup file system, which is accessed on 
  R session start-up and first call to the function \code{admb} to initialize
  the contents of the options manager \code{.PBSadmb} and the GUI. However, if a
  path file (e.g., \code{ADpaths.txt}) exists, the paths in this file will 
  override those taken from \code{ADopts.txt}. 

  Additionally, the \sQuote{Verify} button always consults the admb version file 
  (if it exists) and the g++ executable to collect version information. 
  If not available, version information is set to an empty string.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer, Victoria BC\cr
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2018-09-28}
}
\seealso{ \code{\link{readADopts}},  \code{\link{writeADopts}} }
\keyword{ data }
\keyword{ list }
