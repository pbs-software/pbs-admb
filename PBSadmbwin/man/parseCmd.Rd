\name{parseCmd}
\Rdversion{1.1}
\alias{parseCmd}
\title{
  Parse an Indexed ADMB Command
}
\description{
  Parse an indexed ADMB command line for a specified \code{index}, operating
  system (\code{os}), and compiler (\code{comp}). The result depends on the
  project \code{prefix}, the path (\code{admpath}) to the ADMB home directory,
  and the path (\code{gccpath}) to the C++ compiler. Within the database,
  variables are denoted by leading \code{@} characters.
}
\usage{
parseCmd(prefix, index, os=.Platform$OS, comp="GCC", admpath="", gccpath="")
}
\arguments{
  \item{prefix}{prefix for the ADMB project.}
  \item{index}{index that indicates one of eight possibilities related to three
    processing steps (convert, compile, link) and options: safe or optimize,
    random effects or normal.}
  \item{os}{operating system}
  \item{comp}{C++ compiler description}
  \item{admpath}{explicit path for the ADMB home directory.}
  \item{gccpath}{explicit path for the C++ bin directory.}
}
\value{
  Character string, the ADMB command from \code{ADMBcmd} corresponding to
  the specified index, prefix, and system paths.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
\code{\link{ADMBcmd}}
}
\keyword{manip}

