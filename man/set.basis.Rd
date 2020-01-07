\name{set.basis}
\alias{set.basis}

\title{Set Basis}

\description{
Set the initial basis in an lpSolve linear program model object.
}

\usage{
set.basis(lprec, basis, nonbasic = FALSE, default = FALSE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{basis}{a numeric vector of unique values from the set \code{\{1, \dots, (m+n)\}} (where \code{m} is the number of constraints and \code{n} is the number of decision variables) specifying the initial basis. The values may be positive or negative where a negative value indicates that the variable is at its lower bound and positive value indicates that the variable is at its upper bound. If \code{nonbasic} is \code{FALSE} then the \code{n} basic variables must be provided. If \code{nonbasic} is \code{TRUE} then the nonbasic variables must be provided as well.}
  \item{nonbasic}{a logical value. If \code{TRUE} the nonbasic variables must be included in \code{basis} as well.}
  \item{default}{a logical value. If \code{TRUE} the default (all slack variable) basis is used. In this case, the value of \code{basis} and \code{nonbasic} are ignored.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


