\name{delete.constraint}
\alias{delete.constraint}

\title{Delete Constraint}

\description{
Delete a constraint from an lpSolve linear program model object.
}

\usage{
delete.constraint(lprec, constraints)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{constraints}{a numeric vector of unique values from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying which constraints should be deleted.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(0, 4)
set.objfn(lps.model, rep(1, 4))

xt <- c(6,2,4,9)
add.constraint(lps.model, xt, "<=", 50)

yt <- c(3,1,5)
ind <- c(1,2,4)
add.constraint(lps.model, yt, 2, 75, ind)

delete.constraint(lps.model, 1)
}

\keyword{programming}


