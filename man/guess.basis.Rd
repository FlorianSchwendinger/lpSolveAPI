\name{guess.basis}
\alias{guess.basis}

\title{Guess Basis}

\description{
Attempt to find a feasible basis corresponding to a user provided feasible point.
}

\usage{
guess.basis(lprec, guess)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{guess}{a numeric vector of length \code{n} (the number of decision variables in \code{lprec}) containing a feasible point.}
}

\value{
if successful, a numeric vector containing the indices of a starting basis. This vector is suitable for use with the \code{\link{set.basis}} function.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{set.basis}}}

\keyword{programming}


