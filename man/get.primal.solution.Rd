\name{get.primal.solution}
\alias{get.primal.solution}

\title{Get Primal Solution}

\description{
Retrieve the values of the primal variables from a successfully solved lpSolve linear program model object.
}

\usage{
get.primal.solution(lprec, orig = FALSE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{orig}{a logical value. When presolve is active, the size of the lp may decrease during solve.  By default, the answer to this reduced problem is returned. Set this argument to \code{TRUE} to retrieve the solution to the original lp.}
}

\value{
a numeric vector containing the values of the primal variables. If an error occurs (for instance \code{lprec} has not been successfully solved) a \code{NULL} value is returned.
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
get.primal.solution(lps.model)
}

\keyword{programming}


