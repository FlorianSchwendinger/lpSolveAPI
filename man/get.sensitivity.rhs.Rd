\name{get.sensitivity.rhs}
\alias{get.sensitivity.rhs}

\title{Get Sensitivity: Right-Hand-Side}

\description{
Retrieve the sensitivity of the constraints from a successfully solved lpSolve linear program model object.
}

\usage{
get.sensitivity.rhs(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\value{
a list with components

  \item{duals}{a numeric vector of length \code{m+n} (where \code{m} is the number of constraints in \code{m} and \code{n} is the number of decision variables in \code{lprec}) containing the values of the dual variables (reduced costs).}

  \item{dualsfrom}{a numeric vector of length \code{m+n} (where \code{m} is the number of constraints in \code{m} and \code{n} is the number of decision variables in \code{lprec}) containing the values of the lower limits on the dual variables.}

  \item{dualstill}{a numeric vector of length \code{m+n} (where \code{m} is the number of constraints in \code{m} and \code{n} is the number of decision variables in \code{lprec}) containing the values of the upper limits on the dual variables.}
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

solve(lps.model)
get.sensitivity.rhs(lps.model)
}

\keyword{programming}


