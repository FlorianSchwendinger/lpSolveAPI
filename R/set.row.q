set.row <- function(lprec, row, xt, indices)
{
  if(missing(indices)) {
    if(length(xt) != dim(lprec)[2])
      stop("the length of ", sQuote("xt"), " is not equal to the number of ", 
           "decision variables in the model")

    epsel <- .Call(RlpSolve_get_epsel, lprec)
    indices <- which(abs(xt) > epsel)
    xt <- xt[indices]
  }

  if(length(xt) != length(indices))
    stop(sQuote("xt"), " and ", sQuote("indices"), " are not the same length")

  .Call(RlpSolve_set_rowex, lprec, as.integer(row), as.double(xt),
        as.integer(indices))

  invisible()
}


