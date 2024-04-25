\name{plotMC}
\alias{plotMC}
\alias{.win.plotMC}
\title{ Plot Results of MCMC Simulation }
\description{
  Plot results of an ADMB MCMC simulation using various plot methods.
}
\usage{
plotMC(prefix, act="pairs", pthin=1, useCols=NULL)
.win.plotMC(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'})}
  \item{act}{\code{character} -- scalar: action describing plot type (current choices:
    \code{"pairs"}, \code{"eggs"}, \code{"acf"}, \code{"trace"}, and
    \code{"dens"})}
  \item{pthin}{\code{numeric} -- scalar indicating interval at which to collect records
    from the \code{.mc.dat} file for plotting}
  \item{useCols}{\code{logical} -- vector indicating which columns of \code{.mc.dat} to plot}
  \item{winName}{\code{character} -- name of GUI window}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-24}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.plotMC}.
  Use the PBSadmb GUI to explore these plots easily.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{runMC}},
  \code{\link[PBSadmb]{showADargs}}
}
\keyword{hplot}
\concept{MCMC}
