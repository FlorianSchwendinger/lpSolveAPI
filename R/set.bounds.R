set.bounds <- function(lprec, lower = NULL, upper = NULL, columns = 1:n)
{
  n <- dim(lprec)[2]
  ncol <- length(columns)

  if(!is.null(lower)) {
    if(length(lower) != ncol)
      stop(sQuote("lower"), " must contain one element for each column",
           " in the model")

    .Call(RlpSolve_set_lowbo, lprec, as.integer(columns), as.double(lower))
  }

  if(!is.null(upper)) {
    if(length(upper) != ncol)
      stop(sQuote("upper"), " must contain one element for each column",
           " in the model")

    .Call(RlpSolve_set_upbo, lprec, as.integer(columns), as.double(upper))
  }

  invisible()
}


