\name{get.dual.solution}
\alias{get.dual.solution}

\title{Get Dual Solution}

\description{
Retrieve the values of the dual variables (the reduced costs) from a successfully solved lpSolve linear program model object.
}

\usage{
get.dual.solution(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\value{
a numeric vector containing the values of the dual variables. If an error occurs (for instance \code{lprec} has not been successfully solved) a \code{NULL} value is returned.
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
get.dual.solution(lps.model)
}

\keyword{programming}


