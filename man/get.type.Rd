\name{get.type}
\alias{get.type}

\title{Get Type}

\description{
Retrieve the type of a decision variable from an lpSolve linear program model object.
}

\usage{
get.type(lprec, columns = 1:n, as.char = TRUE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying the columns for which the type will be retrieved.}
  \item{as.char}{a logical value. If \code{TRUE} the constraint type is returned as a character string, otherwise the integer code used internally by lpSolve is returned.}
}

\details{
The function \code{\link{set.type}} can be used to set a decision variable as binary. A binary decision variable is simply an integer decision with an upper bound of one and a lower bound of zero - hence this function will report the type as integer.
}

\value{
either a character vector with elements from the set \code{\{"real", "integer"\}} indicating the type of each decision variable specified in \code{columns} or (if \code{as.char = FALSE}) a logical vector with \code{TRUE} elements corresponding to the integer decision variables specified in \code{columns}.
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


