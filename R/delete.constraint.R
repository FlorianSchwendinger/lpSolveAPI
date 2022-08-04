delete.constraint <- function(lprec, constraints)
{
  .Call(RlpSolve_del_constraint, lprec, as.integer(constraints))

  invisible()
}


