\name{runMC}
\alias{runMC}
\title{ Run an Executable Binary File in MCMC Mode }
\description{
  Run the executable binary file \code{'prefix'.exe}, created by 
  \code{makeAD}, to generate MCMC simulations.
}
\usage{
runMC(prefix, nsims=2000, nthin=20, outsuff=".mc.dat",
      logfile=FALSE, add=TRUE, verbose=TRUE)
}
\arguments{
  \item{prefix}{string name prefix of the ADMB project (e.g., \code{"vonb"}).}
  \item{nsims}{numeric scalar indicating number of MCMC simulations to perform.}
  \item{nthin}{numeric scalar indicating the sampling rate or thinning of the 
    \code{nsims} MCMC simulations to report.}
  \item{outsuff}{string name suffix of the MCMC output data file.}
  \item{logfile}{logical: if \code{TRUE}, create a log file of the messages from the shell call.}
  \item{add}{logical: if \code{TRUE}, append shell call messages to an exsiting log file.}
  \item{verbose}{logical: if \code{TRUE}, report the shell call an its messages to the R console.}
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
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.runMC}.
}
\seealso{
  \code{\link{runAD}}, \code{\link{makeAD}}, \code{\link{cleanAD}}
}
\keyword{programming}
\keyword{interface}
