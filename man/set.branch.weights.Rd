\name{set.branch.weights}
\alias{set.branch.weights}

\title{Set Branch Weights}

\description{
Set weights on the variables in an lpSolve linear program model object.
}

\usage{
set.branch.weights(lprec, weights)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{weights}{a numeric vector with \code{n} elements (where \code{n} is the number of decision variables in \code{lprec}) containing the weights for the decision variables.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


