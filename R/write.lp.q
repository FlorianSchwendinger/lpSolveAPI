write.lp <- function(lprec, filename, type = c("lp", "mps", "freemps"),
                     use.names = c(TRUE, TRUE))
{
  type <- match.arg(type)

  .Call(RlpSolve_set_use_names, lprec, as.logical(TRUE),
        as.logical(use.names[1]))
  .Call(RlpSolve_set_use_names, lprec, as.logical(FALSE),
        as.logical(use.names[2]))

  switch(type,
    "lp" = .Call(RlpSolve_write_lp, lprec, as.character(filename)),
    "mps" = .Call(RlpSolve_write_mps, lprec, as.character(filename)),
    "freemps" = .Call(RlpSolve_write_freemps, lprec, as.character(filename))
  )

  .Call(RlpSolve_set_use_names, lprec, as.logical(TRUE), as.logical(TRUE))
  .Call(RlpSolve_set_use_names, lprec, as.logical(FALSE), as.logical(TRUE))

  invisible()
}


