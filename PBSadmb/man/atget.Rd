\name{atget}
\alias{atget}
\alias{atcall}
\alias{atprint}
\alias{atput}
\alias{alisp}
\title{
  Get/Print Objects From or Put Objects Into Temporary Work Environment
}
\description{
  These functions are wrappers to the PBSmodelling accessor functions that
  get/print objects from or put objects into a temporary work environment,
  in this case \code{.PBSadmbEnv}. Working objects include \code{PBSadmb},
  which acts as a storage object for some of the functions, and \code{.PBSadmb},
  which controls the options for the user's project.
}
\usage{
atget(...)
atcall(...)
atprint(...)
atput(...)
alisp(...)
}
\arguments{
  \item{...}{For \code{atget} through to \code{atput}, the only free argument is: \cr
    \code{x} -- name (with or without quotes) of an object to retrieve or store 
    in the temporary environment; cannot be represented by a variable. \cr
    Fixed arguments: \code{penv = parent.frame(), tenv = .PBSadmbEnv} \cr
    See \code{\link[PBSmodelling]{tget}} for additional information.

    For \code{alisp}, there is only one fixed argument: \cr
    \code{pos = .PBSadmbEnv} \cr
    All other arguments are available -- see \code{\link[PBSmodelling]{lisp}} }
}
\details{
  These accessor functions were developed as a response to the CRAN
  repository policy statement: \dQuote{Packages should not modify the
  global environment (user's workspace).}
  
}
\note{
  Additional wrapper functions to access functions in \code{.PBSadmbEnv}
  are named with the prefix \code{.win} (none at the moment).
}
\value{
  Objects are retrieved from or sent to the temporary working 
  environment to/from the place where the function(s) are called.
  Additionally, \code{atcall} invisibly returns the object without 
  transferring, which is useful when the object is a function that the
  user may wish to call, for example, \code{atcall(myfunc)()}, or as 
  arguments in other functions.
}
\references{
  CRAN Repository Policy: 
  \url{https://cran.r-project.org/web/packages/policies.html}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2018-09-28}
}
\seealso{
\code{\link[PBSmodelling]{tget}} and \code{\link[PBSmodelling]{lisp}} in \pkg{PBSmodelling}
}
\keyword{manip}
\keyword{environment}

