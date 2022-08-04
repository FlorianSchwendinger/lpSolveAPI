add.column <- function(lprec, x, indices)
{
  if(missing(indices)) {
    if(length(x) != dim(lprec)[1])
      stop("the length of ", sQuote("x"), " is not equal to the number of ", 
           "constraints in the model")

    epsel <- .Call(RlpSolve_get_epsel, lprec)
    indices <- which(abs(x) > epsel)
    x <- x[indices]
  }

  if(length(x) != length(indices))
    stop(sQuote("x"), " and ", sQuote("indices"), " are not the same length")

  .Call(RlpSolve_add_columnex, lprec, as.double(x), as.integer(indices))

  invisible()
}


