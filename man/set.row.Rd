\name{set.row}
\alias{set.row}

\title{Set Row}

\description{
Set a column in an lpSolve linear program model object.
}

\usage{
set.row(lprec, row, xt, indices)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{row}{a single numeric value from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying which column to set.}
  \item{xt}{a numeric vector containing the constraint coefficients (only the nonzero coefficients if \code{indices} is also given). The length of \code{xt} must be equal to the number of decision variables in \code{lprec} unless \code{indices} is provided.}
  \item{indices}{optional for sparse \code{xt}. A numeric vector the same length as \code{xt} of unique values from the set \{1, \dots, n\} where n is the number of decision variables in \code{lprec}; \code{xt[i]} is set in column \code{indices[i]} in the specified row.  The coefficients for the columns not in \code{indices} are set to zero.  This argument should be omitted when \code{length(xt) == n}.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(2, 4)
set.row(lps.model, 2, c(6,2,4,9))
set.row(lps.model, 1, c(3,1,5), indices = c(1,2,4))
}

\keyword{programming}

