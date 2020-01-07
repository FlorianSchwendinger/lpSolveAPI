\name{get.basis}
\alias{get.basis}

\title{Get Basis}

\description{
Retrieve the basis from a solved lpSolve linear program model object.
}

\usage{
get.basis(lprec, nonbasic = FALSE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{nonbasic}{a logical value. If \code{TRUE}, the nonbasic variables are returned as well.}
}

\value{
an integer vector containing the indices of the basic (and nonbasic if requested) variables. If an error occurs (for instance when calling \code{get.basis} on a model that has not yet been solved) a \code{NULL} value is returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(3, 3)
## build and solve model ##
get.basis(lps.model)
}

\keyword{programming}


