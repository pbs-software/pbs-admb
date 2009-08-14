\name{ADMBcmd}
\Rdversion{1.1}
\alias{ADMBcmd}
\docType{data}
\title{
  Database of ADMB Command Scripts
}
\description{
  Command scripts for ADMB's convert, compile, and link routines
}
\usage{data(ADMBcmd)}
\format{
  A data frame with the following 8 variables:
  \describe{
    \item{\code{OS}}{operating system}
    \item{\code{Comp}}{C++ compiler type}
    \item{\code{Index}}{index that indicates convert, compile or link with options:
      safe or optimize, random effects or normal.}
    \item{\code{Step}}{description of processing step (convert, compile, or link)}
    \item{\code{Safe}}{logical: if \code{TRUE} use safe mode; if not, use optimise mode.}
    \item{\code{RanEff}}{logical: if \code{TRUE} use random effects model; if not, use normal model.}
    \item{\code{Command}}{the command suitable for specified combination of \code{Step},
      \code{Safe}, and \code{RanEff}}
    \item{\code{Comment}}{comment about the command, if any}
  }
}
\details{
  This database represents a compilation of ADMB scripts for various 
  operating systems and compilers. A user's project normally starts 
  with a template file, named with a prefix to denote the project and 
  a standard suffix \code{.tpl}. This file must go through three 
  processing steps: conversion to C/C++ code, compilation by a 
  specified compiler, and linking with ADMB libraries.
  
  The reulting command depends on the operating system, compiler, 
  processing step, and two binary options (safe/optimized; 
  normal/random effects). In principle, the three processing steps and 
  two binary options give 3x2x2=12 possibilities. However, conversion 
  doesn't depend on the \dQuote{safe/optimized} choice, and compilation 
  doesn't depend on \dQuote{normal/random effects}. This reduction leaves
  only 8 possibilities, specified by an \code{index} in the range 
  1:8.
  
  A variable in a \code{Command} string is designated by the prefix 
  character \code{@}. We use this for convenient string substitution 
  by \code{parseCmd}, the function that translates database strings 
  into actual ADMB commands.
  
  The subdirectoy \code{.../ADMB/scripts} in the installed package 
  contains an Excel spreadsheet, used as the source file for this 
  database. Currently, our database is incomplete, and we heartily 
  encourage the ADMB community to make contributions for additional 
  operating systems and compilers.
}
\source{
Jon T. Schnute, Pacific Biological Station, Nanaimo BC
}
\seealso{ \code{\link{parseCmd}}
} 
\keyword{datasets}
