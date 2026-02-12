
# branch_mode_to_integer(c("ceiling", "floor", "auto", "default"))
# branch_mode_to_integer(rep(c("ceiling", "floor", "auto", "default"), 2))
# branch_mode_to_integer(c("A", "ceiling", "B"))
branch_mode_to_integer <- function(modes) {
  branch_modes <- c("ceiling", "floor", "auto", "default")
  modes_int <- match(modes, branch_modes, nomatch = NA)
  if (any(is.na(modes_int))) {
    emsg <- sprintf(
        "converting branch modes to integer allowed branch modes are '%s' got '%s'",
        deparse(branch_modes), deparse(unique(modes[is.na(modes_int)]))
    )
    stop(emsg)
  }
  return(modes_int - 1L)
}


set.branch.mode <- function(lprec, columns, modes) {
  if (length(columns) != length(modes)) {
    stop(sprintf("%s and %s must be the same length", sQuote("columns"), sQuote("modes")))
  }

  if (is.character(modes)) {
    modes <- branch_mode_to_integer(modes)
  }

  .Call(RlpSolve_set_var_branch, lprec, as.integer(columns), as.integer(modes))

  invisible()
}
