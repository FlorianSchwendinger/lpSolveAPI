get.basis <- function(lprec, nonbasic = FALSE)
{
  basis <- .Call(RlpSolve_get_basis, lprec, as.logical(nonbasic))

  if(basis[1] == 1)
    basis <- basis[-1]
  else
    basis <- NULL

  basis
}


