lp.control <- function(lprec, ..., reset = FALSE)
{
  if(reset)
    .Call(RlpSolve_reset_params, lprec)

  status <- list()
  dots <- list(...)
  dot.names <- names(dots)
  controls <- c("anti.degen", "basis.crash", "bb.depthlimit", "bb.floorfirst",
                "bb.rule", "break.at.first", "break.at.value",  "epslevel",
                "epsb", "epsd", "epsel", "epsint", "epsperturb", "epspivot",
                "improve", "infinite", "maxpivot", "mip.gap", "negrange",
                "obj.in.basis", "pivoting", "presolve", "scalelimit", "scaling",
                "sense", "simplextype", "timeout", "verbose")
  dot.names <- match.arg(dot.names, controls, several.ok = TRUE)

  for(dot.name in dot.names) {
    switch(dot.name,

      "anti.degen" = {
        anti.degen <- dots[[dot.name]]

        methods <- c("none", "fixedvars", "columncheck", "stalling",
                     "numfailure", "lostfeas", "infeasible", "dynamic",
                     "duringbb", "rhsperturb", "boundflip")
        anti.degen <- match.arg(anti.degen, methods, several.ok = TRUE)

        if(any(anti.degen == "none"))
          anti.degen <- 0
        else {
          idx <- 2^(0:9)
          names(idx) <- methods[-1]
          anti.degen <- sum(idx[anti.degen])
        }

        .Call(RlpSolve_set_anti_degen, lprec, as.integer(anti.degen))
      },

      "basis.crash" = {
        basis.crash <- dots[[dot.name]]

        methods <- c("none", "mostfeasible", "leastdegenerate")
        basis.crash <- match.arg(basis.crash, methods, several.ok = FALSE)
        idx <- c(0, 2, 3)
        names(idx) <- methods
        basis.crash <- idx[basis.crash]

        .Call(RlpSolve_set_basiscrash, lprec, as.integer(basis.crash))
      },

      "bb.depthlimit" = {
        bb.depthlimit <- dots[[dot.name]]
        .Call(RlpSolve_set_bb_depthlimit, lprec, as.integer(bb.depthlimit))
      },

      "bb.floorfirst" = {
        bb.floorfirst <- dots[[dot.name]]

        methods <- c("ceiling", "floor", "automatic")
        bb.floorfirst <- match.arg(bb.floorfirst, methods, several.ok = FALSE)
        idx <- c(0, 1, 2)
        names(idx) <- methods
        bb.floorfirst <- idx[bb.floorfirst]

        .Call(RlpSolve_set_bb_floorfirst, lprec, as.integer(bb.floorfirst))
      },

      "bb.rule" = {
        bb.rule <- dots[[dot.name]]

        rules <- c("first", "gap", "range", "fraction", "pseudocost",
                   "pseudononint", "pseudoratio")
        rule <- match.arg(bb.rule[1], rules, several.ok = FALSE)
        idx <- 0:6
        names(idx) <- rules
        rule <- idx[rule]

        bb.rule <- bb.rule[-1]
        if(length(bb.rule)) {
          all.values <- c("weightreverse", "branchreverse", "greedy",
                          "pseudocost", "depthfirst", "randomize", "gub",
                          "dynamic", "restart", "breadthfirst", "autoorder",
                          "rcostfixing", "stronginit")
          values <- match.arg(bb.rule, all.values, several.ok = TRUE)
          idx <- 2^(3:15)
          names(idx) <- all.values
          values <- idx[values]
        }
        else
          values <- double(0)

        bb.rule <- sum(c(rule, values))
        .Call(RlpSolve_set_bb_rule, lprec, as.integer(bb.rule))
      },

      "break.at.first" = {
        break.at.first <- dots[[dot.name]]
        .Call(RlpSolve_set_break_at_first, lprec, as.logical(break.at.first))
      },

      "break.at.value" = {
        break.at.value <- dots[[dot.name]]
        .Call(RlpSolve_set_break_at_value, lprec, as.double(break.at.value))
      },

      "epslevel" = {
        epslevel <- dots[[dot.name]]

        methods <- c("tight", "medium", "loose", "baggy")
        epslevel <- match.arg(epslevel, methods, several.ok = FALSE)
        idx <- 0:3
        names(idx) <- methods
        epslevel <- idx[epslevel]

        .Call(RlpSolve_set_epslevel, lprec, as.integer(epslevel))
      },

      "epsb" = {
        epsb <- dots[[dot.name]]
        .Call(RlpSolve_set_epsb, lprec, as.double(epsb))
      },

      "epsd" = {
        epsd <- dots[[dot.name]]
        .Call(RlpSolve_set_epsd, lprec, as.double(epsd))
      },

      "epsel" = {
        epsel <- dots[[dot.name]]
        .Call(RlpSolve_set_epsel, lprec, as.double(epsel))
      },

      "epsint" = {
        epsint <- dots[[dot.name]]
        .Call(RlpSolve_set_epsint, lprec, as.double(epsint))
      },

      "epsperturb" = {
        epsperturb <- dots[[dot.name]]
        .Call(RlpSolve_set_epsperturb, lprec, as.double(epsperturb))
      },

      "epspivot" = {
        epspivot <- dots[[dot.name]]
        .Call(RlpSolve_set_epspivot, lprec, as.double(epspivot))
      },

      "improve" = {
        improve <- dots[[dot.name]]
        methods <- c("none", "solution", "dualfeas", "thetagap", "bbsimplex")
        improve <- match.arg(improve, methods, several.ok = TRUE)

        if(any(improve == "none"))
          improve <- 0
        else {
          idx <- 2^(0:3)
          names(idx) <- methods[-1]
          improve <- sum(idx[improve])
        }

        .Call(RlpSolve_set_improve, lprec, as.integer(improve))
      },

      "infinite" = {
        infinite <- dots[[dot.name]]
        .Call(RlpSolve_set_infinite, lprec, as.double(infinite))
      },

      "maxpivot" = {
        maxpivot <- dots[[dot.name]]
        .Call(RlpSolve_set_maxpivot, lprec, as.integer(maxpivot))
      },

      "mip.gap" = {
        mip.gap <- dots[[dot.name]]

        if(length(mip.gap) != 2)
          mip.gap <- rep(mip.gap[1], 2)

        .Call(RlpSolve_set_mip_gap, lprec, as.logical(TRUE),
              as.double(mip.gap[1]))
        .Call(RlpSolve_set_mip_gap, lprec, as.logical(FALSE),
              as.double(mip.gap[2]))
      },

      "negrange" = {
        negrange <- dots[[dot.name]]
        .Call(RlpSolve_set_negrange, lprec, as.double(negrange))
      },

      "obj.in.basis" = {
        obj.in.basis <- dots[[dot.name]]
        .Call(RlpSolve_set_obj_in_basis, lprec, as.logical(obj.in.basis))
      },

      "pivoting" = {
        pivoting <- dots[[dot.name]]

        rules <- c("firstindex", "dantzig", "devex", "steepestedge")
        rule <- match.arg(pivoting[1], rules, several.ok = FALSE)
        idx <- 0:3
        names(idx) <- rules
        rule <- idx[rule]

        pivoting <- pivoting[-1]
        if(length(pivoting)) {
          all.modes <- c("primalfallback", "multiple", "partial", "adaptive",
                         "randomize", "autopartial", "loopleft",
                         "loopalternate", "harristwopass", "truenorminit")
          modes <- match.arg(pivoting, all.modes, several.ok = TRUE)
          idx <- c(4, 8, 16, 32, 128, 512, 1024, 2048, 4096, 16384)
          names(idx) <- all.modes
          modes <- idx[modes]
        }
        else
          modes <- double(0)

        pivoting <- sum(c(rule, modes))
        .Call(RlpSolve_set_pivoting, lprec, as.integer(pivoting))
      },

      "presolve" = {
        presolve <- dots[[dot.name]]

        methods <- c("none", "rows", "cols", "lindep", "sos", "reducemip",
                     "knapsack", "elimeq2", "impliedfree", "reducedgcd",
                     "probefix", "probereduce", "rowdominate", "coldominate",
                     "mergerows", "impliedslk", "colfixdual", "bounds", "duals",
                     "sensduals")
        presolve <- match.arg(presolve, methods, several.ok = TRUE)

        if(any(presolve == "none"))
          presolve <- 0
        else {
          idx <- c(2^(0:2), 2^(5:20))
          names(idx) <- methods[-1]
          presolve <- sum(idx[presolve])
        }

        loops <- .Call(RlpSolve_get_presolveloops, lprec)
        .Call(RlpSolve_set_presolve, lprec, as.integer(presolve),
              as.integer(loops))
      },

      "scalelimit" = {
        scalelimit <- dots[[dot.name]]
        .Call(RlpSolve_set_scalelimit, lprec, as.double(scalelimit))
      },

      "scaling" = {
        scaling <- dots[[dot.name]]

        types <- c("none", "extreme", "range", "mean", "geometric",
                   "curtisreid")
        type <- match.arg(scaling[1], types, several.ok = FALSE)

        if(any(type == "none"))
          scaling <- 0

        else {
          idx <- c(0, 1, 2, 3, 4, 7)
          names(idx) <- types
          type <- idx[type]

          scaling <- scaling[-1]
          if(length(scaling)) {
            all.modes <- c("quadratic", "logarithmic", "power2", "equilibrate",
                           "integers", "dynupdate", "rowsonly", "colsonly")
            modes <- match.arg(scaling, all.modes, several.ok = TRUE)
            idx <- 2^(3:10)
            names(idx) <- all.modes
            modes <- idx[modes]
          }
          else
            modes <- double(0)

          scaling <- sum(c(type, modes))
        }

        .Call(RlpSolve_set_scaling, lprec, as.integer(scaling))
      },

      "sense" = {
        sense <- dots[[dot.name]]
        sense <- match.arg(sense, c("minimize", "maximize"))
        sense <- sense == "maximize"
        .Call(RlpSolve_set_sense, lprec, as.logical(sense))
      },

      "simplextype" = {
        simplextype <- dots[[dot.name]]
        simplextype <- match.arg(simplextype, c("primal", "dual"),
                                 several.ok = TRUE)

        if(length(simplextype) != 2)
          simplextype <- rep(simplextype[1], 2)

        if(simplextype[1] == "primal" && simplextype[2] == "primal")
          simplextype <- 5

        else if(simplextype[1] == "primal" && simplextype[2] == "dual")
          simplextype <- 9

        else if(simplextype[1] == "dual" && simplextype[2] == "primal")
          simplextype <- 6

        else if(simplextype[1] == "dual" && simplextype[2] == "dual")
          simplextype <- 10

        .Call(RlpSolve_set_simplextype, lprec, as.integer(simplextype))
      },

      "timeout" = {
        timeout <- dots[[dot.name]]
        .Call(RlpSolve_set_timeout, lprec, as.integer(timeout))
      },

      "verbose" = {
        verbose <- dots[[dot.name]]
        ch <- c("neutral", "critical", "severe", "important", "normal",
                "detailed", "full")
        verbose <- match.arg(verbose, choices = ch)
        verbose <- match(verbose, table = ch) - 1

        .Call(RlpSolve_set_verbose, lprec, as.integer(verbose))
      }
    )
  }

  anti.degen <- .Call(RlpSolve_is_anti_degen, lprec,
                      as.integer(c(0,1,2,4,8,16,32,64,128,256,512)))
  anti.degen <- c("none", "fixedvars", "columncheck", "stalling", "numfailure",
                  "lostfeas", "infeasible", "dynamic", "duringbb", "rhsperturb",
                  "boundflip")[anti.degen]

  basis.crash <- .Call(RlpSolve_get_basiscrash, lprec)
  basis.crash <- c("none", "NOT USED", "mostfeasible",
                   "leastdegenerate")[1 + basis.crash]

  bb.depthlimit <- .Call(RlpSolve_get_bb_depthlimit, lprec)

  bb.floorfirst <- .Call(RlpSolve_get_bb_floorfirst, lprec)
  bb.floorfirst <- c("ceiling", "floor", "automatic")[1 + bb.floorfirst]

  bb.rule.index <- .Call(RlpSolve_get_bb_rule, lprec)
  bb.rule <- bb.rule.index %% 8
  bb.rule <- c("first", "gap", "range", "fraction", "pseudocost",
               "pseudononint", "pseudoratio", "user")[1 + bb.rule]

  bb.value.index <- integer(0)

  for(i in 15:3) {
    temp <- 2^i
    if(floor(bb.rule.index / temp) == 1) {
      bb.value.index <- c(i, bb.value.index)
      bb.rule.index <- bb.rule.index - temp
    }
  }

  bb.rule <- c(bb.rule, c("weightreverse", "branchreverse", "greedy",
               "pseudocost", "depthfirst", "randomize", "gub", "dynamic",
               "restart", "breadthfirst", "autoorder", "rcostfixing",
               "stronginit")[bb.value.index - 2])

  break.at.first <- .Call(RlpSolve_is_break_at_first, lprec)

  break.at.value <- .Call(RlpSolve_get_break_at_value, lprec)

  epsilon <- c(epsb = .Call(RlpSolve_get_epsb, lprec),
               epsd = .Call(RlpSolve_get_epsd, lprec),
               epsel = .Call(RlpSolve_get_epsel, lprec),
               epsint = .Call(RlpSolve_get_epsint, lprec),
               epsperturb = .Call(RlpSolve_get_epsperturb, lprec),
               epspivot = .Call(RlpSolve_get_epspivot, lprec))

  improve <- .Call(RlpSolve_get_improve, lprec)
  improve.index <- integer(0)

  for(i in 3:0) {
    temp <- 2^i
    if(floor(improve / temp) == 1) {
      improve.index <- c(i, improve.index)
      improve <-improve - temp
    }
  }

  if(length(improve.index))
    improve <- c("solution", "dualfeas", "thetagap",
                 "bbsimplex")[1 + improve.index]
  else
    improve <- "none"

  infinite <- .Call(RlpSolve_get_infinite, lprec)

  maxpivot <- .Call(RlpSolve_get_maxpivot, lprec)

  mip.gap <- c(absolute = .Call(RlpSolve_get_mip_gap, lprec, TRUE),
               relative = .Call(RlpSolve_get_mip_gap, lprec, FALSE))

  negrange <- .Call(RlpSolve_get_negrange, lprec)

  obj.in.basis <- .Call(RlpSolve_is_obj_in_basis, lprec)

  pivot.rule <- .Call(RlpSolve_is_piv_rule, lprec, as.integer(0:3))
  pivot.rule <- c("firstindex", "dantzig", "devex", "steepestedge")[pivot.rule]
  pivot.mode <- .Call(RlpSolve_is_piv_mode, lprec,
                      as.integer(c(2^(2:5), 128, 2^(9:12), 16384)))
  pivot.mode <- c("primalfallback", "multiple", "partial", "adaptive",
                  "randomize", "autopartial", "loopleft", "loopalternate",
                  "harristwopass", "truenorminit")[pivot.mode]
  pivoting <- c(pivot.rule, pivot.mode)

  presolve <- .Call(RlpSolve_is_presolve, lprec,
                    as.integer(c(0, 2^(0:2), 2^(5:20))))
  presolve <- c("none", "rows", "cols", "lindep", "sos", "reducemip",
                "knapsack", "elimeq2", "impliedfree", "reducedgcd", "probefix",
                "probereduce", "rowdominate", "coldominate", "mergerows",
                "impliedslk", "colfixdual", "bounds", "duals",
                "sensduals")[presolve]

  scalelimit <- .Call(RlpSolve_get_scalelimit, lprec)

  scale.type <- .Call(RlpSolve_is_scaletype, lprec, as.integer(c(0, 1,2,3,4,7)))
  scale.type <- c("none", "extreme", "range", "mean", "geometric",
                  "curtisreid")[scale.type]
  scale.mode <- .Call(RlpSolve_is_scalemode, lprec,
                      as.integer(c(8, 16, 2^(5:10))))
  scale.mode <- c("quadratic", "logarithmic", "power2", "equilibrate",
                  "integers", "dynupdate", "rowsonly", "colsonly")[scale.mode]
  scaling <- c(scale.type, scale.mode)

  sense <- ifelse(.Call(RlpSolve_is_maxim, lprec), "maximize", "minimize")

  simplextype <- switch(as.character(.Call(RlpSolve_get_simplextype, lprec)),
                         "5" = c("primal", "primal"),
                         "6" = c("dual", "primal"),
                         "9" = c("primal", "dual"),
                        "10" = c("dual", "dual")
                       )

  timeout <- .Call(RlpSolve_get_timeout, lprec)

  ch <- c("neutral", "critical", "severe", "important", "normal", "detailed",
          "full")
  verbose <- ch[1+.Call(RlpSolve_get_verbose, lprec)]

  list(anti.degen = anti.degen, basis.crash = basis.crash,
       bb.depthlimit = bb.depthlimit, bb.floorfirst = bb.floorfirst,
       bb.rule = bb.rule, break.at.first = break.at.first,
       break.at.value = break.at.value, epsilon = epsilon, improve = improve,
       infinite = infinite, maxpivot = maxpivot, mip.gap = mip.gap,
       negrange = negrange, obj.in.basis = obj.in.basis, pivoting = pivoting,
       presolve = presolve, scalelimit = scalelimit, scaling = scaling,
       sense = sense, simplextype = simplextype, timeout = timeout,
       verbose = verbose)
}


