\name{dot-win.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.win.appendLog}
\alias{.win.checkADopts}
\alias{.win.checkADpath}
\alias{.win.checkPrefix}
\alias{.win.cleanAD}
\alias{.win.compAD}
\alias{.win.convAD}
\alias{.win.editAD}
\alias{.win.editPLT}
\alias{.win.findClean}
\alias{.win.findTPL}
\alias{.win.linkAD}
\alias{.win.makeAD}
\alias{.win.plotMC}
\alias{.win.readADopts}
\alias{.win.readADpaths}
\alias{.win.readRep}
\alias{.win.run}
\alias{.win.runAD}
\alias{.win.runMC}
\alias{.win.saveADpaths}
\alias{.win.setADpath}
\alias{.win.setADver}
\alias{.win.showADargs}
\alias{.win.showGUIargs}
\alias{.win.startLog}
\alias{.win.suggestPath}
\alias{.win.viewCode}
\alias{.win.viewRep}
\alias{.win.writeADopts}
\title{
  GUI Windows Wrappers
}
\description{
  Wrapper functions to run existing or temporary functions from a GUI's function call.
}
\usage{
.win.appendLog(winName="PBSadmb")
.win.checkADopts(winName="PBSadmb")
.win.checkADpath(winName="PBSadmb")
.win.checkPrefix(winName="PBSadmb")
.win.cleanAD(winName="PBSadmb")
.win.compAD(winName="PBSadmb")
.win.convAD(winName="PBSadmb")
.win.editAD(winName="PBSadmb")
.win.editPLT()
.win.findClean(winName="cleanWindow")
.win.findTPL(suffix=".tpl",winName="PBSadmb")
.win.linkAD(winName="PBSadmb")
.win.makeAD(winName="PBSadmb")
.win.plotMC(winName="PBSadmb")
.win.readADopts(winName="PBSadmb")
.win.readADpaths(winName="PBSadmb")
.win.readRep(winName="PBSadmb")
.win.run(winName="PBSadmb")
.win.runAD(winName="PBSadmb")
.win.runMC(winName="PBSadmb")
.win.saveADpaths(winName="PBSadmb")
.win.setADpath(winName="PBSadmb")
.win.setADver(winName="PBSadmb")
.win.showADargs(winName="PBSadmb")
.win.showGUIargs(winName="PBSadmb")
.win.startLog(winName="PBSadmb")
.win.suggestPath(winName="PBSadmb")
.win.viewCode(pkg="PBSadmb")
.win.viewRep(winName="PBSadmb")
.win.writeADopts(winName="PBSadmb")
}
\arguments{
  \item{winName}{\code{character} -- name of a GUI window containing widgets}
  \item{suffix}{\code{character} -- file name suffix (e.g., \code{'.tpl'})}
  \item{pkg}{\code{character} -- name of R package}
}
%- maybe also 'usage' for other objects documented here.
\details{
  PBSmodelling Windows (Graphical User Interfaces or GUIs) call
  wrapper functions that call non-GUI functions in the package.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-11-09}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{appendLog}},
  \code{\link[PBSadmb]{checkADopts}},
  \code{\link[PBSadmb]{cleanAD}},
  \code{\link[PBSadmb]{compAD}},
  \code{\link[PBSadmb]{convAD}},
  \code{\link[PBSadmb]{editAD}},
  \code{\link[PBSadmb]{linkAD}},
  \code{\link[PBSadmb]{makeAD}},
  \code{\link[PBSadmb]{plotMC}},
  \code{\link[PBSadmb]{readADopts}},
  \code{\link[PBSadmb]{readADpaths}},
  \code{\link[PBSadmb]{readRep}},
  \code{\link[PBSadmb]{runAD}},
  \code{\link[PBSadmb]{runMC}},
  \code{\link[PBSadmb]{saveADpaths}},
  \code{\link[PBSadmb]{setADpath}},
  \code{\link[PBSadmb]{setADver}},
  \code{\link[PBSadmb]{showADargs}},
  \code{\link[PBSadmb]{startLog}},
  \code{\link[PBSadmb]{suggestPath}},
  \code{\link[PBSadmb]{writeADopts}}\cr
In \pkg{PBSmodelling}:\cr
  \code{getWinAct},
  \code{getWinVal},
  \code{tget}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{interface}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{GUI}
% \concept{ ~cpt2 }
% Use only one concept per line.
