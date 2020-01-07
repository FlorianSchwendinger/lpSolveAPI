\name{set.constr.type}
\alias{set.constr.type}

\title{Set Constraint Type}

\description{
Set constraint types in an lpSolve linear program model object.
}

\usage{
set.constr.type(lprec, types, constraints = 1:m)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{types}{either a numeric vector or a character vector containing elements from the set \code{\{1 = "<=", 2 = ">=", 3 = "="\}} specifying the types of constraints.  Additionally, the contraint type can be set to \emph{free} uinsg the code \code{0}.}
  \item{constraints}{a numeric vector of unique values from the set \{1, \dots, m\} (where m is the number of constraints in \code{lprec}) specifying which constraints to set.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{resize.lp}}}

\examples{
lps.model <- make.lp(4, 2)

x <- c(6,2,4,9)
set.column(lps.model, 2, x)

y <- c(3,1,5)
ind <- c(1,2,4)
set.column(lps.model, 1, y, ind)

set.constr.type(lps.model, rep("<=", 4))
}

\keyword{programming}


