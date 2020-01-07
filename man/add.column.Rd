\name{add.column}
\alias{add.column}

\title{Add Column}

\description{
Add a column to an lpSolve linear program model object.
}

\usage{
add.column(lprec, x, indices)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{x}{a numeric vector containing the elements (only the nonzero elements if the \code{indices} argument is also provided) of the column to be added. The length of \code{x} must be equal to the number of constraints in \code{lprec} unless \code{indices} is provided.}
  \item{indices}{optional for sparse \code{x}. A numeric vector the same length as \code{x} of unique values from the set \{0, \dots, m\} where m is the number of constraints in \code{lprec}; \code{x[i]} is entered into constraint \code{indices[i]} in the added column.  The coefficients for the constraints not in \code{indices} are set to zero.  In particular, index \code{0} is the objective function coefficient in the added column and is set to zero by default. This argument should be omitted when \code{length(x) == m}.}
}

\details{
This function adds an additional column to an lpSolve linear program model object.  If multiple columns are to be added, performance can be improved by calling \code{\link{resize.lp}} before adding the columns.
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{resize.lp}}}

\examples{
lps.model <- make.lp(4, 0)
add.column(lps.model, c(6,2,4,9))
add.column(lps.model, c(3,1,5), indices = c(1,2,4))
}

\keyword{programming}


