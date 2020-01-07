\name{get.solutioncount}
\alias{get.solutioncount}

\title{Get Solution Count}

\description{
Computes the number of equal solutions in a successfully solved lpSolve linear program model object. This is only valid if there are integer, semi-continuous or SOS variables in the model so that the branch-and-bound algorithm is used. This count gives the number of solutions with the same optimal objective value.
}

\usage{
get.solutioncount(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\value{
a single integer value giving the number of solutions attaining the optimal objective value.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


