guess.basis <- function(lprec, guess)
{
  if(length(guess) != dim(lprec)[2])
    stop("the length of ", sQuote("guess"), " must be equal to the number",
         " of columns in the model")

  basis <- .Call(RlpSolve_guess_basis, lprec, as.double(c(0.0, guess)))

  if(basis[1] == 1)
    basis <- basis[-1]
  else
    basis <- NULL

  basis
}


