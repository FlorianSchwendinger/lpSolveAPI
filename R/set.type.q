set.type <- function(lprec, columns, type = c("integer", "binary", "real"))
{
  type <- match.arg(type)

  switch(type,
    integer = .Call(RlpSolve_set_int, lprec, as.integer(columns),
                    as.logical(TRUE)),
    binary = .Call(RlpSolve_set_binary, lprec, as.integer(columns),
                   as.logical(TRUE)),
    real = .Call(RlpSolve_set_int, lprec, as.integer(columns),
                 as.logical(FALSE))
  )

  invisible()
}


