\name{delete.lp}
\alias{delete.lp}

\title{Delete Linear Program Model}

\description{
Free all the resources used by an lpSolve linear program model object and set the value of the external pointer to \code{NULL}.
}

\usage{
delete.lp(lprec)
}
 
\arguments{
  \item{lprec}{an lpSolve linear program model object.}
}

\details{
This function is used as a finalizer for lpSolve linear program model objects.
}

\value{
\code{lprec} is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


