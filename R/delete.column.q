delete.column <- function(lprec, columns)
{
  .Call(RlpSolve_del_column, lprec, as.integer(columns))

  invisible()
}


