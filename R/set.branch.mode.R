set.branch.mode <- function(lprec, columns, modes)
{
  if(length(columns) != length(modes))
    stop(sQuote("columns"), " and ", sQuote("modes"),
         " must be the same length")

  if(is.character(modes)) {
    modes <- pmatch(modes, c("ceiling", "floor", "auto", "default"),
                    nomatch = NA)
    if(any(is.na(modes)))
      stop("invalid mode")
    else
      modes <- modes - 1
  }

  .Call(RlpSolve_set_var_branch, lprec, as.integer(columns), as.integer(modes))

  invisible()
}


