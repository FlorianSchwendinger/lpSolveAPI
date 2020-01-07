\name{set.branch.mode}
\alias{set.branch.mode}

\title{Set Branch Mode}

\description{
Specify which branch to take first in the branch-and-bound algorithm for decision variables in an lpSolve linear program model object.
}

\usage{
set.branch.mode(lprec, columns, modes)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector containing values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying which columns to set the mode for.}
  \item{modes}{a character vector composed of the strings \code{\{"ceiling", "floor", "auto", "default"\}} giving the branch modes for the decision variables specified in \code{columns}.  Please see the reference for a description of these terms.}
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


