\name{dot-funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.addQuotes}
\alias{.asIs}
\alias{.callSys}
\alias{.changeWD}
\alias{.changeWDEnter}
\alias{.chooseCols}
\alias{.getDirName}
\alias{.load.prefix.droplist}
\alias{.normPath}
\alias{.version}
\title{
  Miscellaneous Dot Functions
}
\description{
  An assortment of dot functions to facilitate running \pkg{PBSadmb}.
}
\usage{
.addQuotes(str)
.asIs(x)
.callSys(..., wait=TRUE)
.changeWD(wd)
.changeWDEnter()
.chooseCols(winName="PBSadmb")
.getDirName(path)
.load.prefix.droplist()
.normPath(path, winslash="\\\\", mustWork=FALSE)
.version(x)
}
\arguments{
  \item{str}{\code{character} -- string to which quotation marks are added}
  \item{x}{\code{numeric|character} -- number for \code{'.asIs'}; string for \code{'.version'}}
  \item{...}{\code{character} -- system command and options for \code{'shell'|'system'}}
  \item{wait}{\code{logical} -- if \code{TRUE}, wait until the system command has been executed}
  \item{wd}{\code{character} -- working directory}
  \item{winName}{\code{character} -- name of a GUI window}
  \item{path}{\code{character} -- path on the computing machine}
  \item{winslash}{\code{character} -- delimiters to use between folders|directories for pathways}
  \item{mustWork}{\code{logical} -- if \code{TRUE}, then pathway must be valid}
}
%- maybe also 'usage' for other objects documented here.
\details{
  Small utility functions that were demoted to a hidden status
  before CRAN required their documentation.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-23}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{admb}}\cr
In package \pkg{PBSmodelling}:\cr
  \code{getWinAct},
  \code{getWinVal},
  \code{tget}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{utilities}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{hidden}
% \concept{ ~cpt2 }
% Use only one concept per line.
