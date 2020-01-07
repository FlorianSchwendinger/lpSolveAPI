set.rhs <- function(lprec, b, constraints = 1:m)
{
  m <- dim(lprec)[1]
  set.constr.value(lprec, rhs = b, constraints = constraints)
}


