\name{setADMBPath}
\alias{setADMBPath}
\alias{setADMBVer}
\title{ Creates the ADMB Options List }
\description{
  Creates a global list object detailing the pathways to the ADMB directory, the GCC bin, 
  and the user's preferred text editor. Also keeps track of software versions for ADMB and GCC.
}
\usage{
setADMBPath(admbpath, gccpath, editor)
setADMBVer(admbver, gccver)
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
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\seealso{ \code{\link{makeADopts}},  \code{\link{writeADopts}} }
\keyword{ data }
\keyword{ list }
