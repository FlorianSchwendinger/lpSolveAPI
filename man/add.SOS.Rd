\name{add.SOS}
\alias{add.SOS}

\title{Add A Special Ordered Set Constraint}

\description{
Add a Special Ordered Set (SOS) constraint to an lpSolve linear program model object.
}

\usage{
add.SOS(lprec, name, type, priority, columns, weights)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{name}{a character string specifying a name for the SOS constraint.}
  \item{type}{a positive integer specifying the type of the SOS constraint.}
  \item{priority}{an integer specifying the priority of the SOS constraint.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) indicating which decision variables belong to the special ordered set.}
  \item{weights}{a numeric vector the same length as \code{columns} specifying the variable weights.}
}

\value{
If the operation was successful: a single integer value containing the list index of the new special ordered set. A return value of \code{0} indicates an error.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


