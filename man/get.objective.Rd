\name{get.objective}
\alias{get.objective}

\title{Get Objective}

\description{
Retrieve the value of the objective function from a successfully solved lpSolve linear program model object.
}

\usage{
get.objective(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\value{
a single numeric value containing the value of the objective function.
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
get.objective(lps.model)
}

\keyword{programming}


