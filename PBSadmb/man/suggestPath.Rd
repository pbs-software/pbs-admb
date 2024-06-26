\name{suggestPath}
\alias{suggestPath}
\alias{.win.suggestPath}
\title{
  Suggest Path to Specified Program
}
\description{
  Suggest a directory path from the system \code{PATH} where a 
  program specified by the user might be located.
}
\usage{
suggestPath(progs, ipath = NULL, file_ext = NULL)
.win.suggestPath(winName="PBSadmb")
}
\arguments{
  \item{progs}{\code{character} -- vector of program names without the extension 
    (assumes \code{'.exe'} in Windows); Unix programs do not have extensions}
  \item{ipath}{\code{character} -- specified by the user as the initial path (directory)
    to check before checking all other directories on the \code{PATH}}
  \item{file_ext}{\code{character} -- user can specify an alternative extension
    if the program does not end in \code{'.exe'}}
  \item{winName}{\code{character} -- name of GUI window}
}
\details{
  Determine whether the specified programs can be located on the user's system.
  
  A wrapper function called \code{.win.suggestPath} is used by
  the \pkg{PBSadmb} GUI to suggest paths for the ADMB home, 
  the Windows MinGW home, and an editor.
}
\value{
  Returns a logical vector where each element corresponds to a program searched. 
  If the element is \code{TRUE}, then the program was found on the path, 
  which is supplied as the name of the vector element.
  If the element is \code{FALSE}, the program may exist on the user's system, 
  but is not in any of the directories specified by the \code{PATH} 
  environment of the system.
  
  The returned vector has a list attribute where each item in the list
  corresponds to each element in the vector, and shows the results of
  the search for each of the directories.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-04-25}
}
\note{
  A wrapper function \code{'.win.suggestPath'} can be called from a GUI.
}
\seealso{
In package \pkg{PBSadmb}:\cr
  \code{\link[PBSadmb]{setADpath}}
}
\keyword{utilites}
\keyword{sysdata}


