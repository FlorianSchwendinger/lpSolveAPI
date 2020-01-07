\name{get.column}
\alias{get.column}

\title{Get Column}

\description{
Retrieve a column from an lpSolve linear program model object.
}

\usage{
get.column(lprec, column)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{column}{a single numeric value from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying which column to retrieve.}
}

\value{
a list with elements \code{column} and \code{nzrow} such that \code{column[i]} contains the constraint coefficient in row \code{nzrow[i]}. Rows not present in \code{nzrow} have coefficient zero in \code{column}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(4, 0)

x <- c(6,2,4,9)
add.column(lps.model, x)

y <- c(3,1,5)
ind <- c(1,2,4)
add.column(lps.model, y, ind)

get.column(lps.model, 2)
}

\keyword{programming}


