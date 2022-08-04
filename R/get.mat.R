get.mat <- function(lprec, i, j)
  .Call(RlpSolve_get_mat, lprec, as.integer(i), as.integer(j))


