set.semicont <- function(lprec, columns, sc = TRUE)
{
  .Call(RlpSolve_set_semicont, lprec, as.integer(columns), as.logical(sc))

  invisible()
}


