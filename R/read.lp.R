read.lp <- function(filename, type = c("lp", "mps", "freemps"),
                    verbose = "neutral", options)
{
  if(!file.exists(filename))
    stop(dQuote(filename), ": no such file")

  if(missing(type)) {
    type <- strsplit(basename(filename), split = ".", fixed = TRUE)[[1]]
    type <- casefold(type[length(type)])
    type <- match(type, c("lp", "mps", "freemps"), nomatch = -1)
    if(type < 0)
      stop("unable to determine file type - please use the ", sQuote("type"),
           " argument")
    else
      type <- c("lp", "mps", "freemps")[type]
  }

  else
    type <- match.arg(type)

  verbosities <- c("neutral", "critical", "severe", "important",
                   "normal", "detailed", "full")
  verbose <- match.arg(verbose, choices = verbosities)
  verbose <- match(verbose, table = verbosities) - 1

  if(!missing(options)) {
    if(type == "lp")
      warning(sQuote("options"), "argument ignored for type lp files")

    options <- unique(casefold(options))
    options <- match.arg(options, choices = c("free", "ibm", "negobjconst"),
                         several.ok = TRUE)
    mps.options <- c(free = 8, ibm = 16, negobjconst = 32)
    options <- sum(mps.options[options]) + verbose
  }
  
  else
    options <- verbose

  lprec <- switch(type,
    "lp" = .Call(RlpSolve_read_LP, as.character(filename), as.integer(verbose)),
    "mps" = .Call(RlpSolve_read_MPS, as.character(filename),
                  as.integer(options)),
    "freemps" = .Call(RlpSolve_read_freeMPS, as.character(filename),
                      as.integer(options))
  )

  if(is.null(lprec))
    stop("could not interpret ", basename(filename), " as an ", type, " file")

  else {
    reg.finalizer(lprec, delete.lp, TRUE)
    oldClass(lprec) <- "lpExtPtr"
  }

  lprec
}


