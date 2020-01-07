\name{get.kind}
\alias{get.kind}

\title{Get Kind}

\description{
Retrieve the kind of a decision variable from an lpSolve linear program model object.
}

\usage{
get.kind(lprec, columns = 1:n)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying the columns for which the kind will be retrieved.}
}

\details{
Decision variables have both a type and a kind. The type is either \code{real} or \code{integer} and indicates the type of values the decision variable may take. The kind is one of \code{\{standard, semi-continuous, SOS\}}. Semi-continuous decision variables can take allowed values between their upper and lower bound as well as zero. Please see the link in the references for a discussion of special ordered set (SOS) constraints.
}

\value{
a character vector containing the kind of each decision variable specified in \code{columns}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(0, 3)
xt <- c(6,2,4)
add.constraint(lps.model, xt, "<=", 150)
xt <- c(1,1,6)
add.constraint(lps.model, xt, ">=", 0)
xt <- c(4,5,4)
add.constraint(lps.model, xt, "=", 40)
set.objfn(lps.model, c(-3,-4,-3))

get.kind(lps.model)
}

\keyword{programming}


