\name{get.variables}
\alias{get.variables}

\title{Get Variables}

\description{
Retrieve the values of the decision variables from a successfully solved lpSolve linear program model object.
}

\usage{
get.variables(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\value{
a numeric vector containing the values of the decision variables corresponding to the optimal solution.
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
get.variables(lps.model)
}

\keyword{programming}


