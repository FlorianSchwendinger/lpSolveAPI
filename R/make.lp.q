make.lp <- function(nrow = 0, ncol = 0, verbose = "neutral")
{
  ch <- c("neutral", "critical", "severe", "important", "normal", "detailed",
          "full")
  verbose <- match.arg(verbose, choices = ch)
  verbose <- match(verbose, table = ch) - 1

  lprec <- .Call(RlpSolve_make_lp, as.integer(nrow), as.integer(ncol))

  if(!is.null(lprec)) {
    .Call(RlpSolve_set_verbose, lprec, as.integer(verbose))
    reg.finalizer(lprec, delete.lp, TRUE)
    oldClass(lprec) <- "lpExtPtr"
  }

  lprec
}


