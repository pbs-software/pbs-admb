\name{runMC}
\alias{runMC}
\alias{.win.runMC}
\alias{.win.run}
\title{
  Run an Executable Binary File in MCMC Mode
}
\description{
  Run the executable binary file \code{'prefix'.exe}, created by 
  \code{makeAD}, to generate MCMC simulations.
}
\usage{
runMC(prefix, nsims=2000, nthin=20, outsuff=".mc.dat",
      logfile=FALSE, add=TRUE, verbose=TRUE)

.win.runMC(winName="PBSadmb")
.win.run(winName="PBSadmb")
}
\arguments{
  \item{prefix}{\code{character} -- name prefix of the ADMB project (e.g., \code{'vonb'}).}
  \item{nsims}{\code{numeric} -- scalar indicating number of MCMC simulations to perform.}
  \item{nthin}{\code{numeric} -- scalar indicating the sampling rate or thinning of the 
    \code{nsims} MCMC simulations to report.}
  \item{outsuff}{\code{character} -- name suffix of the MCMC output data file.}
  \item{logfile}{\code{logical} -- if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{\code{logical} -- if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{\code{logical} -- if \code{TRUE}, report the shell call an its messages to the R console.}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  This function runs \code{'prefix'.exe} twice, first with the arguments
  \code{-mcmc 'nsims' -mcsave 'nthin'} and second with the argument \code{-mceval}.
  By default, output goes to the file \code{'prefix'.mc.dat}, although a user can
  specify a different output suffix.
  
  To see this function in action, use the PBSadmb GUI with the example \code{vonb} or
  \code{simpleMC}.
}
\value{
  Invisibly returns the results of the shell call.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Scientist Emeritus, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-23}
}
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.runMC}.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{runAD}},
  \code{\link[PBSadmb]{makeAD}},
  \code{\link[PBSadmb]{cleanAD}}
}
\keyword{programming}
\keyword{interface}
\concept{MCMC}
