\name{write.lp}
\alias{write.lp}

\title{Write Linear Program}

\description{
Write an lpSolve linear program model object to a file.
}

\usage{
write.lp(lprec, filename, type = c("lp", "mps", "freemps"),
         use.names = c(TRUE, TRUE))
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{filename}{a character string containing the name of the output file.}
  \item{type}{either \code{"lp"}, \code{"mps"} or \code{"freemps"}. The type of file to output. Please see the reference for a description of these formats.}
  \item{use.names}{a logical vector of length two specifying repectively whether row and column names should be written into the file. If \code{TRUE} then the names are written into the file, otherwise lpSolve's internal names are used. This can be useful if the model names do not comply with the syntax of the chosen file type.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


