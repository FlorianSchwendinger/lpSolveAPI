\name{row.add.mode}
\alias{row.add.mode}

\title{Row Add Mode}

\description{
Switch to row entry mode.
}

\usage{
row.add.mode(lprec, state)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{state}{optional: either \code{"on"} or \code{"off"}. This argument should be provided only to switch row entry mode on or off.}
}

\details{
The best way to build a linear program model in lpSolve is column by column, hence row entry mode is turned off by default. If the model must be built by adding constraints, the performance of the \code{\link{add.constraint}} function can be greatly improved by turning row entry mode on.

There are several caveats associated with row entry mode. First, only use this function on lpSolve linear program models created by \code{\link{make.lp}}. Do not use this function on models read from a file. Second, add the objective function before adding the constraints. Third, do not call any other API functions while in row entry mode; no other data matrix access is allowed. After adding all the contraints, turn row entry mode off. Once turned off, you cannot switch back to row entry mode.
}

\value{
  \code{"on"} if row entry mode is on in \code{lprec}; otherwise \code{"off"}. 
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{add.constraint}}}

\keyword{programming}


