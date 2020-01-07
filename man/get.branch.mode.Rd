\name{get.branch.mode}
\alias{get.branch.mode}

\title{Get Branch Mode}

\description{
Retrieve the branch mode for one or more decision variables from an lpSolve linear program model object.
}

\usage{
get.branch.mode(lprec, columns = 1:n, as.char = TRUE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying for which decision variables the branch modes should be retrieved. If \code{NULL} branch modes are retrieved for all the decision variables.}
  \item{as.char}{a logical value. If \code{TRUE} the branch mode is returned as a character string, otherwise the integer code used directly in lpSolve is returned.}
}

\value{
either a character vector or an integer vector containing the branch modes for the decision variables specified in \code{columns}. The possibilities are: \code{0 = "ceiling"}, \code{1 = "floor"} and \code{2 = "auto"}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(3, 3)
get.branch.mode(lps.model)
}

\keyword{programming}


