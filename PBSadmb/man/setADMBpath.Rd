\name{setADMBpath}
\alias{setADMBpath}
\alias{setADMBver}
\title{ Creates the ADMB Options List }
\description{
  Creates an options list object detailing the pathways to the ADMB directory,
  the GCC bin, and the user's preferred text editor. 
  Also keeps track of software versions for ADMB and GCC.
}
\usage{
setADMBpath(admbpath, gccpath, editor)
setADMBver(admbver, gccver)
}
\arguments{
  \item{admbpath}{explicit path to the user's ADMB directory.}
  \item{gccpath}{explicit path to the user's GCC bin (C-compiler) directory.}
  \item{editor}{explicit path and program to use for editing text.}
  \item{admbver}{version number of ADMB software.}
  \item{gccver}{version number of g++ software.}
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

  Additionally, the Verify button always consults the admb version file 
  (if it exists) and the g++ executable to collect version information. 
  If not available, version information is set to an empty string.
}
\author{Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC}
\seealso{ \code{\link{readADopts}},  \code{\link{writeADopts}} }
\keyword{ data }
\keyword{ list }
