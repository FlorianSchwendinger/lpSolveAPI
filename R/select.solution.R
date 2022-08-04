select.solution <- function(lprec, solution)
{
  n.solutions <- .Call(RlpSolve_get_solutioncount, lprec)

  if(!missing(solution)) {
    solution <- as.integer(solution)

    if(solution < 1 || solution > n.solutions)
      stop("the value of ", sQuote("solution"), " is not valid")

    .Call(RlpSolve_set_solutionlimit, lprec, solution)
  }

  n.solutions
}


