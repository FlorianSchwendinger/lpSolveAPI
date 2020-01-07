\name{set.constr.value}
\alias{set.constr.value}

\title{Set Constraint Value}

\description{
Set constraint values in an lpSolve linear program model object.
}

\usage{
set.constr.value(lprec, rhs = NULL, lhs = NULL, constraints = 1:m)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{rhs}{a numeric vector the same length as \code{constraints} containing the right-hand-side values to be set.  If \code{NULL} no right-hand-side values are set.}
  \item{lhs}{a numeric vector the same length as \code{constraints} containing the left-hand-side values to be set.  If \code{NULL} no left-hand-side values are set.}
  \item{constraints}{a numeric vector of unique values from the set \{1, \dots, m\} (where m is the number of constraints in \code{lprec}) specifying which constraints to set.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{resize.lp}}}

\keyword{programming}


