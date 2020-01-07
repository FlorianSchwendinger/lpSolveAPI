\name{set.bounds}
\alias{set.bounds}

\title{Set Bounds}

\description{
Set bounds on the decision variables in an lpSolve linear program model object.
}

\usage{
set.bounds(lprec, lower = NULL, upper = NULL, columns = 1:n)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{lower}{a numeric vector of lower bounds to be set on the decision variables specified in \code{columns}. If \code{NULL} the lower bounds are not changed.}
  \item{upper}{a numeric vector of upper bounds to be set on the decision variables specified in \code{columns}. If \code{NULL} the upper bounds are not changed.}
  \item{columns}{a numeric vector of values from the set \code{\{1, \dots, n\}} specifying the columns to have their bounds set. If \code{NULL} all columns are set.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(0, 4)

set.bounds(lps.model, lower = rep(-1.0, 4))
set.bounds(lps.model, upper = 1:4)

set.bounds(lps.model, lower = rep(0.0, 4), upper = rep(1.0, 4))
}

\keyword{programming}


