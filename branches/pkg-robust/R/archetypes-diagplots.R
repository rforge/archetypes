

residuals.diagplot <- function(object, ref.order = 1, ...) {
  y <- residuals(object)
  x <- seq(length = nrow(y))

  m <- ncol(y)

  ylim <- range(y)
  ylab <- colnames(y)

  if ( is.null(ref.order) ) {
    ref.order <- 0
    ix <- seq(length = nrow(y))
  }
  else {
    ix <- order(y[, ref.order])
  }

  layout(matrix(seq(length = m), nrow = m, ncol = 1))
  for ( i in seq(length = m) ) {
    plot(x, y[ix, i], ylim = ylim,
         ylab = sprintf('Residuals %s', ylab[i]),
         xlab = sprintf('Index%s', ifelse(i == ref.order, ' (reference order)', '')),
         ...)
    abline(h = 0, lty = 2, col = gray(0.7), ...)
  }

  invisible(NULL)
}



rss.diagplot <- function(object, sort = FALSE, ...) {
  y <- rss(object, type = 'single')
  x <- seq(length = length(y))

  if ( sort )
    y <- sort(y)

  plot(x, y, xlab = 'Index', ylab = 'RSS', ...)
  abline(h = 0, lty = 2, col = gray(0.7), ...)
}


weights.diagplot <- function(object, weights.type, ...) {
  y <- weights(object, weights.type)
  x <- seq(length = length(y))

  plot(x, y, ylim = c(1, 0), xlab = 'Index', ylab = 'Weights', ...)
}
