set.branch.weights <- function(lprec, weights)
{
  if(length(weights) != dim(lprec)[2])
    stop("the length of ", sQuote("weights"), " must be the same as the number",
         " of columns in the model")

  .Call(RlpSolve_set_var_weights, lprec, as.double(weights))

  invisible()
}


