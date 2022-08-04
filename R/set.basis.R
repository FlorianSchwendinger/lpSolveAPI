set.basis <- function(lprec, basis, nonbasic = FALSE, default = FALSE)
{
  if(default) {
    .Call(RlpSolve_default_basis, lprec)
    return(invisible())
  }

  if(nonbasic)
    if(length(basis) != sum(dim(lprec)))
      stop("the length of ", sQuote("basis"), " must be the same as the number",
           " of columns and constraints in the model")

  else
    if(length(basis) != dim(lprec)[2])
      stop("the length of ", sQuote("basis"), " must be the same as the number",
           " of columns in the model")

  .Call(RlpSolve_set_basis, lprec, as.integer(c(0, basis)),
        as.logical(nonbasic))

  invisible()
}


