set.objfn <- function(lprec, obj, indices)
{
  if(missing(indices)) {
    if(length(obj) != dim(lprec)[2])
      stop("the length of ", sQuote("obj"), " is not equal to the number of ", 
           "decision variables in the model")

    epsel <- .Call(RlpSolve_get_epsel, lprec)
    indices <- which(abs(obj) > epsel)
    obj <- obj[indices]
  }

  if(length(obj) != length(indices))
    stop(sQuote("obj"), " and ", sQuote("indices"), " are not the same length")

  .Call(RlpSolve_set_obj_fnex, lprec, as.double(obj), as.integer(indices))

  invisible()
}


