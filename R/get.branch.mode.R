get.branch.mode <- function(lprec, columns = 1:n, as.char = TRUE)
{
  n <- dim(lprec)[2]

  if(n < 1)
    columns <- integer(0)

  modes <- .Call(RlpSolve_get_var_branch, lprec, as.integer(columns))

  if(as.char)
    modes <- c("ceiling", "floor", "auto", "default")[1 + modes]

  modes
}


