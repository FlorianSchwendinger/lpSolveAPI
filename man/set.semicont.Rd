\name{set.semicont}
\alias{set.semicont}

\title{Set Semicontinuous}

\description{
Set a decision variable as semicontinuous in an lpSolve linear program model object.
}

\usage{
set.semicont(lprec, columns, sc = TRUE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying which variables should be made semicontinuous.}
  \item{sc}{a logical value. If \code{FALSE} the decision variables specified in \code{columns} will have their \code{kind} set to \code{standard}.}
}

\details{
Decision variables have both a type and a kind. The type is either \code{real} or \code{integer} and indicates the type of values the decision variable may take. The kind is one of \code{\{standard, semi-continuous, SOS\}}. Semi-continuous decision variables can take allowed values between their upper and lower bound as well as zero. Please see the link in the references for further details.
}

\value{
a \code{NULL} value is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


