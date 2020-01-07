\name{get.sensitivity.obj}
\alias{get.sensitivity.obj}

\title{Get Sensitivity: Objective}

\description{
Retrieve the sensitivity of the objective function from a successfully solved lpSolve linear program model object.
}

\usage{
get.sensitivity.obj(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\value{
a list with components

  \item{objfrom}{a numeric vector of length \code{n} (where \code{n} is the number of decision variables in \code{lprec}) containing the values of the lower limits of the objective function.}

  \item{objtill}{a numeric vector of length \code{n} (where \code{n} is the number of decision variables in \code{lprec}) containing the values of the upper limits of the objective function.}
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
get.sensitivity.obj(lps.model)
}

\keyword{programming}


