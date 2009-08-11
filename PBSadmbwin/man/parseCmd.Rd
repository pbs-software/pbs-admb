\name{parseCmd}
\Rdversion{1.1}
\alias{parseCmd}
\title{
  Parse the ADMB Command Line
}
\description{
  Parse the ADMB command line by selecting the appropriate line from
  \code{ADMBcmd} and substituting values for \code{prefix}, \code{admpath},
  and \code{gccpath} into the variables denoted by leading \code{@} characters.
}
\usage{
parseCmd(prefix, os=.Platform$OS, comp="GCC", index=1, admpath="", gccpath="")
}
\arguments{
  \item{prefix}{prefix for the ADMB project.}
  \item{os}{operating system}
  \item{comp}{C++ compiler description}
  \item{index}{index that indicates convert, compile or link with various options:
      safe or optimize, random effects or normal.}
  \item{admpath}{explicit path for the ADMB home directory.}
  \item{gccpath}{explicit path for the C++ bin directory.}
}
\value{
  Character string that is the ADMB command line suitable for the specified index
  (see \code{ADMBcmd} using the user's appropriate installation paths.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
\code{\link{ADMBcmd}}
}
\keyword{manip}

