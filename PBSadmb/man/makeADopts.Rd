\name{makeADopts}
\alias{makeADopts}
\title{ Creates the ADMB Options List }
\description{
  Creates a global list object detailing the pathways to the ADMB directory, the GCC bin, 
  and the user's preferred text editor.
}
\usage{
makeADopts(admpath, gccpath, editor)
}
\arguments{
  \item{admpath}{explicit path to the user's ADMB directory.}
  \item{gccpath}{explicit path to the user's GCC bin (C-compiler) directory.}
  \item{editor}{explicit path and program to use for editing text.}
}
\value{
  Creates a global, hidden list object called \code{.ADopts}.
}
\author{ Jon T. Schnute, Pacific Biological Station, Nanaimo BC, Canada }
\note{
  A wrapper function that can be called from a GUI exists as \code{.win.makeADopts}.
}
\seealso{ \code{\link{makeADopts}},  \code{\link{writeADopts}} }
\keyword{ data }
\keyword{ list }
