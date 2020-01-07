\name{solve.lpExtPtr}
\alias{solve.lpExtPtr}

\title{Solve a Linear Program}

\description{
Attempt to compute the optimal solution of an lpSolve linear program model object.
}

\usage{
\method{solve}{lpExtPtr}(a, b, ...)
}

\arguments{
  \item{a}{an lpSolve linear program model object.}
  \item{b}{this argument is ignored.}
  \item{...}{additional arguments are ignored.}
}

\value{
a single integer value containing the status code.
}

\section{Status Codes}{
  \tabular{rl}{
   0: \tab "optimal solution found" \cr
   1: \tab "the model is sub-optimal" \cr
   2: \tab "the model is infeasible" \cr
   3: \tab "the model is unbounded" \cr
   4: \tab "the model is degenerate" \cr
   5: \tab "numerical failure encountered" \cr
   6: \tab "process aborted" \cr
   7: \tab "timeout" \cr
   9: \tab "the model was solved by presolve" \cr
  10: \tab "the branch and bound routine failed" \cr
  11: \tab "the branch and bound was stopped because of a break-at-first or break-at-value" \cr
  12: \tab "a feasible branch and bound solution was found" \cr
  13: \tab "no feasible branch and bound solution was found" \cr
  }
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

solve(lps.model)
}

\keyword{programming}


