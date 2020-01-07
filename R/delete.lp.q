delete.lp <- function(lprec)
{
  .Call(RlpSolve_delete_lp, lprec)

  invisible(lprec)
}


