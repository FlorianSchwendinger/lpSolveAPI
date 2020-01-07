\name{set.column}
\alias{set.column}

\title{Set Column}

\description{
Set a column in an lpSolve linear program model object.
}

\usage{
set.column(lprec, column, x, indices)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{column}{a single numeric value from the set \code{\{1, \dots, n\}} specifying which column to set.}
  \item{x}{a numeric vector containing the elements (only the nonzero elements if \code{indices} is also given) to be used in the added column. The length of \code{x} must be equal to the number of constraints in \code{lprec} unless \code{indices} is provided.}
  \item{indices}{optional for sparse \code{x}. A numeric vector the same length as \code{x} of unique values from the set \{0, \dots, m\} where m is the number of constraints in \code{lprec}; \code{x[i]} is set in constraint \code{indices[i]} in the specified column.  The coefficients for the constraints not in \code{indices} are set to zero.  In particular, index \code{0} is the objective function coefficient in the specified column and is set to zero by default. This argument should be omitted when \code{length(x) == m}.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(4, 2)
set.column(lps.model, 2, c(6,2,4,9))
set.column(lps.model, 1, c(3,1,5), indices = c(1,2,4))
}

\keyword{programming}


