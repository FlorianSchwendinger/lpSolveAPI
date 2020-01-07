\name{set.objfn}
\alias{set.objfn}

\title{Set Objective Function}

\description{
Set the objective function in an lpSolve linear program model object.
}

\usage{
set.objfn(lprec, obj, indices)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{obj}{a numeric vector of length \code{n} (where n is the number of decision variables in \code{lprec}) containing the coefficients of the objective function. Alternatively, if \code{indices} is also provided, a numeric vector of the same length as \code{indices} containing only the nonzero coefficients.}
  \item{indices}{optional for sparse \code{obj}. A numeric vector the same length as \code{obj} of unique values from the set \{1, \dots, n\} where n is the number of decision variables in \code{lprec}; \code{obj[i]} is entered into column \code{indices[i]} in objective function.  The coefficients for the columns not in \code{indices} are set to zero.  This argument should be omitted when \code{length(obj) == n}.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{resize.lp}}}

\examples{
lps.model <- make.lp(2, 4)
set.objfn(lps.model, c(1,2,3,4))
set.objfn(lps.model, c(5,7,6), indices = c(1,2,4))
}

\keyword{programming}


