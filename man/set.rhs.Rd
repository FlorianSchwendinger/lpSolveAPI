\name{set.rhs}
\alias{set.rhs}

\title{Set Right-Hand-Side}

\description{
Set elements on the right-hand-side of an lpSolve linear program model object.
}

\usage{
set.rhs(lprec, b, constraints = 1:m)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{b}{a numeric vector of length \code{m} (where m is the number of constraints in \code{lprec}) }
  \item{constraints}{a numeric vector containing unique elements from the set \{1, \dots, m\} (where m is the number of constraints in \code{lprec}) identifying the constraints for which to set the right-hand-side value.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(4, 2)
x <- c(6,2,4,9)
set.column(lps.model, 2, x)
y <- c(3,1,5)
ind <- c(1,2,4)
set.column(lps.model, 1, y, ind)

set.rhs(lps.model, c(10, 20, 40, 80))
}

\keyword{programming}


