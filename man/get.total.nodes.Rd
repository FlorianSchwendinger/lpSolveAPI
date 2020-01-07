\name{get.total.nodes}
\alias{get.total.nodes}

\title{Get Total Nodes}

\description{
Retrieves the total number of nodes processed in the branch-and-bound algorithm from a successfully solved lpSolve linear program model object.
}

\usage{
get.total.nodes(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\details{
The output of this function is only applicable for models containing integer variables.
}

\value{
a single integer value giving the total number of nodes processed.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


