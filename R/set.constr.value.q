set.constr.value <- function(lprec, rhs = NULL, lhs = NULL, constraints = 1:m)
{
  m <- dim(lprec)[1]

  if(!is.null(rhs)) {
    if(length(rhs) != length(constraints))
      stop(sQuote("rhs"), " and ", sQuote("constraints"),
           " are not the same length")

    .Call(RlpSolve_set_rh, lprec, as.integer(constraints), as.double(rhs))
  }

  if(!is.null(lhs)) {
    rhs <- get.rhs(lprec, constraints = constraints)

    if(length(lhs) != length(rhs))
      stop(sQuote("lhs"), " and ", sQuote("constraints"),
           " are not the same length")

    range <- abs(rhs - lhs)
    .Call(RlpSolve_set_rh_range, lprec, as.integer(constraints),
          as.double(range))
  }

  invisible()
}


