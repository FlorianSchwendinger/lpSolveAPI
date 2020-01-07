dimnames.lpExtPtr <- function(x)
{
  lp.size <- dim(x)

  if(lp.size[1] >= 1)
    rowNames <- .Call(RlpSolve_get_row_names, x, as.integer(1:lp.size[1]))
  else
    rowNames <- character(0)

  if(lp.size[2] >= 1)
    colNames <- .Call(RlpSolve_get_col_names, x, as.integer(1:lp.size[2]))
  else
    colNames <- character(0)

  list(rowNames, colNames)
}


"dimnames<-.lpExtPtr" <- function(x, value)
{
  dx <- dim(x)

  if(length(value[[1]]) != dx[1])
    stop(length(value[[1]]), " names provided for " , dx[1], " rows")

  if(length(value[[2]]) != dx[2])
    stop(length(value[[2]]), " names provided for " , dx[2], " columns")

  if(dx[1] > 0)
    .Call(RlpSolve_set_row_names, x, as.integer(1:dx[1]),
          as.character(value[[1]]))

  if(dx[2] > 0)
    .Call(RlpSolve_set_col_names, x, as.integer(1:dx[2]),
          as.character(value[[2]]))

  x
}


