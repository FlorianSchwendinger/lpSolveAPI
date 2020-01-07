\name{lp.control}
\alias{lp.control}

\title{lpSolve Control Parameters}

\description{
Set control parameters in an lpSolve linear program model object.
}

\usage{
lp.control(lprec, ..., reset = FALSE)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{\dots}{control arguments to bet set in \code{lprec}.}
  \item{reset}{a logical value. If \code{TRUE} all control parameters are reset to their default values.}
}

\value{
a list containing all of the control parameters as set internally in \code{lprec}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\seealso{\code{\link{lp.control.options}}}

\keyword{programming}


