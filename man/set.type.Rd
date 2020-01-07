\name{set.type}
\alias{set.type}

\title{Set Type}

\description{
Set the type of a decision variable in an lpSolve linear program model object.
}

\usage{
set.type(lprec, columns, type = c("integer", "binary", "real"))
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying which variables are to have their type set.}
  \item{type}{either \code{"integer"}, \code{"binary"} or \code{"real"}. The decision variables in \code{columns} will have their type set to this value.}
}

\details{
A binary decision variable is simply an integer decision with an upper bound of one and a lower bound of zero. When \code{type = "binary"} the type of the decision variable will be set to \code{"integer"} and the bounds will be set to zero and one respectively.
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{set.type}}}

\examples{
lps.model <- make.lp(0, 3)
xt <- c(6,2,4)
add.constraint(lps.model, xt, "<=", 150)
xt <- c(1,1,6)
add.constraint(lps.model, xt, ">=", 0)
set.type(lps.model, 2, "binary")
xt <- c(4,5,4)
add.constraint(lps.model, xt, "=", 40)
set.type(lps.model, 3, "integer")
set.objfn(lps.model, c(-3,-4,-3))

get.type(lps.model)
}

\keyword{programming}


