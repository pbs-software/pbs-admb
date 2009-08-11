\name{ADMBcmd}
\Rdversion{1.1}
\alias{ADMBcmd}
\docType{data}
\title{
  ADMB Command Line Scripts
}
\description{
  Command line scripts for ADMB's convert, compile, and link routines.
}
\usage{data(ADMBcmd)}
\format{
  A data frame with 16 observations on the following 8 variables.
  \describe{
    \item{\code{OS}}{operating system}
    \item{\code{Comp}}{C++ compiler type}
    \item{\code{Index}}{index that indicates convert, compile or link with various options:
      safe or optimize, random effects or normal.}
    \item{\code{Step}}{description of step (convert, compile, or link)}
    \item{\code{Safe}}{logical: if \code{TRUE} use safe mode; if not, use optimixe mode.}
    \item{\code{RanEff}}{logical: if \code{TRUE} use randome effects model; if not, use normal model.}
    \item{\code{Command}}{the command line suitable for the combination of \code{Step},
      \code{Safe}, and \code{RanEff}.}
    \item{\code{Comment}}{comment about the cammand line.}
  }
}
\source{
  Jon T. Schnute, Pacific Biological Station, Nanaimo BC
}
\keyword{datasets}
