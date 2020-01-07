\name{read.lp}
\alias{read.lp}

\title{Read LP}

\description{
Read an lpSolve linear program model object from a file.
}

\usage{
read.lp(filename, type = c("lp", "mps", "freemps"), verbose = "neutral", 
        options)
}

\arguments{
  \item{filename}{a character string giving the name of the file which the linear programming model is to be read from.}
  \item{type}{the type of file provided in \code{filename}. If missing, \code{read.lp} will attempt to guess the file type by examining the file's suffix.}
  \item{verbose}{a character string controlling the level of error reporting. The default value \code{"neutral"} is no error reporting. Set to \code{"normal"} to enable error reporting or to \code{"full"} for a comprehensive parse log. See the \code{verbose} entry in \code{\link{lp.control.options}} for a complete description of this argument and its possible values.}
  \item{options}{a character vector of options for the (free)mps parser.  Possible values are

    \describe{
      \item{\code{free}}{Use the free MPS format even when \code{type = "mps"}. If not specified, the fixed MPS format is used. This option has no effect when \code{type = "freemps"} but will not result in an error or a warning.}

      \item{\code{ibm}}{Interpret integer variables without bounds as binary variables per the original IBM standard. By default, lp_solve interprets integer variables without bounds as having no upper bound.}

      \item{\code{negobjconst}}{Interpret the objective constant with an opposite sign. Some solvers interpret the objective constant as a value in the RHS and negate it when brought to the LHS. This option allows lp_solve to do this as well.}
    }
  }
}

\value{
an lpSolve linear program model object.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@me.com}}

\keyword{programming}


