set.constr.type <- function(lprec, types, constraints = 1:m)
{
  m <- dim(lprec)[1]

  if(length(types) != length(constraints))
    stop(sQuote("types"), " and ", sQuote("constraints"),
         " are not the same length")

  if(is.character(types))
    types <- match(types, c("<=", ">=", "="), nomatch = 0)

  .Call(RlpSolve_set_constr_type, lprec, as.integer(constraints),
        as.integer(types))

  invisible()
}


