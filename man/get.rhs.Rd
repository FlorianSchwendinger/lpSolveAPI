\name{get.rhs}
\alias{get.rhs}

\title{Get Right-Hand-Side}

\description{
Retrieve right-hand-side values from an lpSolve linear program model object.
}

\usage{
get.rhs(lprec, constraints = 1:m)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{constraints}{a numeric vector of unique values from the set \code{\{1, \dots, m\}} specifying the constraints for which the right-hand-side values should be retrieved.}
}

\value{
a numeric vector containing the right-hand-side values specified by \code{constraints}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(0, 3)
xt <- c(6,2,4)
add.constraint(lps.model, xt, "<=", 150)
xt <- c(1,1,6)
add.constraint(lps.model, xt, ">=", 0)
xt <- c(4,5,4)
add.constraint(lps.model, xt, "=", 40)
set.objfn(lps.model, c(-3,-4,-3))

get.rhs(lps.model)
}

\keyword{programming}


