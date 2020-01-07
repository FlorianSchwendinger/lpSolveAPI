get.primal.solution <- function(lprec, orig = FALSE)
{
  if(orig)
    .Call(RlpSolve_get_var_primalresult, lprec)
  else
    .Call(RlpSolve_get_primal_solution, lprec)
}


