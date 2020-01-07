\name{dimnames.lpExtPtr}
\alias{dimnames.lpExtPtr}
\alias{dimnames<-.lpExtPtr}

\title{Dimnames of an Object}

\description{
Retrieve or set the dimnames in an lpSolve linear program model object.
}

\usage{
\method{dimnames}{lpExtPtr}(x)
\method{dimnames}{lpExtPtr}(x) <- value
}

\arguments{
  \item{x}{an lpSolve linear program model object.}
  \item{value}{a list containing two character vectors of lengths \code{m} and \code{n} specifying the row names and the column names for the lpSolve linear program model object.  The number of constraints in \code{x} is denoted by \code{m} and the number of decision variables by \code{n}.}
}

\value{
a list of two character vectors containing the row names and the column names in the lpSolve linear program model object.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\examples{
lps.model <- make.lp(4, 0)

x <- c(6,2,4,9)
add.column(lps.model, x)

y <- c(3,1,5)
ind <- c(1,2,4)
add.column(lps.model, y, ind)

dimnames(lps.model) <- list(c("alpha", "bravo", "charlie", "delta"),
                     c("whiskey", "tango"))
}

\keyword{programming}


