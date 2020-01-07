\name{resize.lp}
\alias{resize.lp}

\title{Resize LP}

\description{
Resize the data structures in an lpSolve linear program model object.
}

\usage{
resize.lp(lprec, nrow, ncol)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{nrow}{a single nonnegative integer value specifying the new number of rows for the lpSolve linear program model object.}
  \item{ncol}{a single nonnegative integer value specifying the new number of rows for the lpSolve linear program model object.}
}

\details{
If the new size of the model is smaller than the size of the current model, the excess rows and/or columns are deleted. If the new size is larger, no change will be immediately apparent. However, the internal structures of \code{lprec} will have been adjusted to accommodate the larger model. Efficiency of model building can be improved by calling this function before adding additional columns (for example).
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(4, 0)
resize.lp(lps.model, 4, 2)

x <- c(6,2,4,9)
add.column(lps.model, x)

y <- c(3,1,5)
ind <- c(1,2,4)
add.column(lps.model, y, ind)
}

\keyword{programming}


