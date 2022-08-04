resize.lp <- function(lprec, nrow, ncol)
{
  .Call(RlpSolve_resize_lp, lprec, as.integer(nrow), as.integer(ncol))

  invisible()
}


