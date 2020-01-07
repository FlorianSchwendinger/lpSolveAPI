\name{get.constr.value}
\alias{get.constr.value}

\title{Get Constraint Value}

\description{
Retrieve constraint values from an lpSolve linear program model object.
}

\usage{
get.constr.value(lprec, side = c("rhs", "lhs"), constraints = 1:m)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{side}{either \code{"rhs"} (right-hand-side) or \code{"lhs"} (left-hand-side) - which constraint values to retrieve.}
  \item{constraints}{a numeric vector of unique values from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying the constraints for which the values will be retrieved.}
}

\value{
a numeric vector containing the constraint values.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


