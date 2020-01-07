\name{get.constr.type}
\alias{get.constr.type}

\title{Get Constraint Type}

\description{
Retrieve constraint types from an lpSolve linear program model object.
}

\usage{
get.constr.type(lprec, constraints = 1:m, as.char = TRUE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{constraints}{a numeric vector of unique values from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying the constraints for which the types will be retrieved.}
  \item{as.char}{a logical value. If \code{TRUE} the constraint type is returned as a character string, otherwise the integer code used internally by lpSolve is returned.}
}

\value{
either a character vector or an integer vector containing the types of the constraints specified in \code{constraints}. The possibilities are: \code{0 = "free"}, \code{1 = "<="}, \code{2 = ">="} and \code{3 = "="}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(0, 3)

xt <- c(6,2,4)
add.constraint(lps.model, xt, "<=", 15)

xt <- c(1,1,6)
add.constraint(lps.model, xt, ">=", 15)

xt <- c(4,5,4)
add.constraint(lps.model, xt, "=", 40)

get.constr.type(lps.model)
}

\keyword{programming}


