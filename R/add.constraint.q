add.constraint <- function(lprec, xt, type = c("<=", "=", ">="), rhs, indices,
                           lhs)
{
  if(missing(indices)) {
    if(length(xt) != dim(lprec)[2])
      stop("the length of ", sQuote("xt"), " is not equal to the number of ", 
           "decision variables in the model")

    epsel <- .Call(RlpSolve_get_epsel, lprec)
    indices <- which(abs(xt) > epsel)
    xt <- xt[indices]
  }

  if(length(xt) != length(indices))
    stop(sQuote("xt"), " and ", sQuote("indices"), " are not the same length")

  if(is.character(type)) {
    type <- match.arg(type)
    type <- match(type, c("<=", ">=", "="))
  }

  .Call(RlpSolve_add_constraintex, lprec, as.double(xt), as.integer(indices),
        as.integer(type), as.double(rhs))

  if(!missing(lhs)) {
    range <- abs(rhs - lhs)
    .Call(RlpSolve_set_rh_range, lprec, as.integer(dim(lprec)[1]),
          as.double(range))
  }

  invisible()
}


