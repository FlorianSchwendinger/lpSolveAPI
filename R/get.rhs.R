get.rhs <- function(lprec, constraints = 1:m)
{
  m <- dim(lprec)[1]
  get.constr.value(lprec, constraints = constraints)
}


