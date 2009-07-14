\name{plotMC}
\alias{plotMC}
\title{ Plot Results of MCMC Simulation }
\description{
  Plot results of an ADMB MCMC simulation using various plot methods.
}
\usage{
plotMC(prefix, act="pairs", pthin=1, useCols=NULL)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{act}{string scalar: action describing plot type (current choices:
    \code{"pairs"}, \code{"eggs"}, \code{"acf"}, \code{"trace"}, and
    \code{"dens"}).}
  \item{pthin}{numeric scalar indicating interval at which to collect records
    from the \code{.mc.dat} file for plotting.}
  \item{useCols}{logical vector indicating which columns of \code{.mc.dat} to plot.}
}
\author{ Rowan Haigh, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.plotMC}.
  Use the PBSadmb GUI to explore these plots easily.
}
\seealso{
  \code{\link{runMC}}, \code{\link{showADargs}}
}
\keyword{hplot}

