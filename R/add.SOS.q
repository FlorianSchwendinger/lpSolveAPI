add.SOS <- function(lprec, name, type, priority, columns, weights)
{
  .Call(RlpSolve_add_SOS, lprec, as.character(name), as.integer(type),
        as.integer(priority), as.integer(columns), as.double(weights))

  invisible()
}


