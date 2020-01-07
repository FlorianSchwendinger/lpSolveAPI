\name{select.solution}
\alias{select.solution}

\title{Select Solution}

\description{
Select which solution is returned by the lpSolve accessor methods.
}

\usage{
select.solution(lprec, solution)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{solution}{optional. An integer between \code{1} and the number of optimal solutions to the model.}
}

\details{
When the branch and bound algorithm is used (i.e., when there are integer, semi-continuous or SOS variables in the model) there may be multiple optimal solutions.
}

\value{
a single integer value: the number of optimal solutions.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


