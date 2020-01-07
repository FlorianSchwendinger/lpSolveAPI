\name{get.total.iter}
\alias{get.total.iter}

\title{Get Total Iterations}

\description{
Retrieves the total number of iterations from a successfully solved lpSolve linear program model object.
}

\usage{
get.total.iter(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\details{
If \code{lprec} contains integer variables then this function returns the number of iterations to find a relaxed solution plus the number of iterations in the B&B process. If \code{lprec} contains no integer variables then this function returns the number of iterations to find a solution.
}

\value{
a single integer value giving the total number of iterations.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


