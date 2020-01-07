\name{add.constraint}
\alias{add.constraint}

\title{Add Constraint}

\description{
Add a constraint to an lpSolve linear program model object.
}

\usage{
add.constraint(lprec, xt, type = c("<=", "=", ">="), rhs, indices, lhs)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{xt}{a numeric vector containing the constraint coefficients (only the nonzero coefficients if \code{indices} is also given). The length of \code{xt} must be equal to the number of decision variables in \code{lprec} unless \code{indices} is provided.}
  \item{type}{a numeric or character value from the set \code{\{1 = "<=", 2 = ">=", 3 = "="\}} specifying the type of the constraint.}
  \item{rhs}{a single numeric value specifying the right-hand-side of the constraint.}
  \item{indices}{optional for sparse \code{xt}. A numeric vector the same length as \code{xt} of unique values from the set \{1, \dots, n\} where n is the number of decision variables in \code{lprec}; \code{xt[i]} is entered into column \code{indices[i]} in the added constraint.  The coefficients for the columns not in \code{indices} are set to zero.  This argument should be omitted when \code{length(xt) == n}.}
  \item{lhs}{optional. A single numeric value specifying the left-hand-side of the constraint.}
}

\details{
Specifying the objective function before adding constraints will improve the performance of this function.

The use of this function should be avoided when possible. Building a model column-by-column rather than row-by-row will be on the order of 50 times faster (building the model - not solving the model).
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(0, 4)
set.objfn(lps.model, rep(1, 4))
add.constraint(lps.model, c(6,2,4,9), "<=", 50)
add.constraint(lps.model, c(3,1,5), 2, 75, indices = c(1,2,4))
}

\keyword{programming}


