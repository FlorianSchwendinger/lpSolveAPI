\name{get.bounds}
\alias{get.bounds}

\title{Get Bounds}

\description{
Retrieve the bounds on the decision variables from an lpSolve linear program model object.
}

\usage{
get.bounds(lprec, columns = 1:n)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying the decision variables for which the bounds should be retrieved.}
}

\value{
  a list with components \code{lower} and \code{upper}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(3, 3)
get.bounds(lps.model)
}

\keyword{programming}


