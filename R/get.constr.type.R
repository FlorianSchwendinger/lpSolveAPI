get.constr.type <- function(lprec, constraints = 1:m, as.char = TRUE)
{
  m <- dim(lprec)[1]

  if(m < 1)
    constraints <- integer(0)

  types <- .Call(RlpSolve_get_constr_type, lprec, as.integer(constraints))

  if(as.char)
    types <- c("free", "<=", ">=", "=")[types+1]

  types
}


