\name{set.mat}
\alias{set.mat}

\title{Set Matrix Element}

\description{
Set the value of a single matrix element in an lpSolve linear program model object.
}

\usage{
set.mat(lprec, i, j, value)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{i}{a single numeric value from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying the row of the matrix.}
  \item{j}{a single numeric value from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying the column of the matrix.}
  \item{value}{a single numeric value.}
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

set.mat(lps.model, 3, 2, 4.5)
}

\keyword{programming}


