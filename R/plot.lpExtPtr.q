plot.lpExtPtr <- function(x, y, ...)
{
  m <- dim(x)[1]
  n <- dim(x)[2]

  if(n != 2
  || any(get.bounds(x)$lower != 0)
  || any(get.bounds(x)$upper != Inf)
  || any(get.type(x) != "real"))
    stop("cannot plot linear program - see help(plot.lpExtPtr)")

  X <- matrix(0, m, n)

  col <- get.column(x, 1)
  if(col$nzrow[1] == 0) {
    col$nzrow <- col$nzrow[-1]
    col$column <- col$column[-1]
  }

  X[col$nzrow, 1] <- col$column

  col <- get.column(x, 2)
  if(col$nzrow[1] == 0) {
    col$nzrow <- col$nzrow[-1]
    col$column <- col$column[-1]
  }

  X[col$nzrow, 2] <- col$column
  X <- rbind(X, diag(2))

  b <- c(get.rhs(x), rep(0, 2))

  pts <- matrix(0, 2, 0)

  for(i in 1:(m + 1)) {
    for(j in (i + 1):(m + 2)) {
      A <- X[c(i, j), ]
      if(abs(det(A)) > 1e-9) {
        pts <- cbind(pts, solve(A, b[c(i, j)]))
      }
    }
  }

  X <- X[1:m, ]
  b <- b[1:m]

  feasible <- double(0)
  for(j in 1:dim(pts)[2])
    if(all(X %*% pts[, j, drop = FALSE] <= b) && all(pts[, j] >= 0))
      feasible <- c(feasible, j)

  pts <- t(pts[,feasible])
  pts <- pts[chull(pts), ]
  limits <- max(pts) * c(-0.1, 1.4)
  box <- max(pts) * c(-0.05, 1.2)

  old.par <- par(mar = c(3, 2, 4, 2) + 0.1, pty = "s")
  on.exit(par(old.par))
  plot(NA, NA, type = "n", axes = FALSE, xlim = limits, ylim = limits,
       xlab = "", ylab = "", pty = "s", main = "The Feasible Set")

  polygon(pts, col = "gray")
  arrows(0, limits[1], 0, limits[2], lwd = 1.5, length = 0.125)
  arrows(limits[1], 0, limits[2], 0, lwd = 1.5, length = 0.125)
  for(i in 1:floor(limits[2])) {
    segments(i, 0, i, 0.0625)
    text(i, 0, as.character(i), adj = c(0.5, 1.5))
    segments(0, i, 0.0625, i)
    text(0, i, as.character(i), adj = 1.5)
  }

  decision.vars <- dimnames(x)[[2]]
  text(limits[2], limits[1], paste(decision.vars[1], " ", sep = ""), adj = 1)
  text(0, limits[2], paste("   ", decision.vars[2], sep = ""), adj = 0)

  for(i in 1:m) {
    if(abs(X[i, 1]) < 1e-9)
      segments(box[1], b[i] / X[i, 2], box[2], b[i] / X[i, 2])
    else if(abs(X[i, 2]) < 1e-9)
      segments(b[i] / X[i, 1], box[1], b[i] / X[i, 1], box[2])
    else {
      y.intercept <- b[i] / X[i, 2]
      if(y.intercept > box[2]) {
        x0 <- (b[i] - X[i, 2] * box[2]) / X[i, 1]
        y0 <- box[2]
      }
      else if(y.intercept < box[1]) {
        x0 <- (b[i] - X[i, 2] * box[1]) / X[i, 1]
        y0 <- box[1]
      }
      else {
        x0 <- box[1]
        y0 <- (b[i] - X[i, 1] * box[1]) / X[i, 2]
      }

      y.intercept <- (b[i] - X[i, 1] * box[2]) / X[i, 2]
      if(y.intercept > box[2]) {
        x1 <- (b[i] - X[i, 2] * box[2]) / X[i, 1]
        y1 <- box[2]
      }
      else if(y.intercept < box[1]) {
        x1 <- (b[i] - X[i, 2] * box[1]) / X[i, 1]
        y1 <- box[1]
      }
      else {
        x1 <- box[2]
        y1 <- (b[i] - X[i, 1] * box[2]) / X[i, 2]
      }

      segments(x0, y0, x1, y1)
    }
  }

  invisible()
}


