

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



rss.diagplot <- function(object, ...) {
  UseMethod('rss.diagplot')
}

rss.diagplot.archetypes <- function(object, sort = FALSE, ...) {
  y <- rss(object, type = 'single')
  x <- seq(length = length(y))

  if ( sort )
    y <- sort(y)

  plot(x, y, xlab = 'Index', ylab = 'RSS', ...)
  abline(h = 0, lty = 2, col = gray(0.7), ...)
}

rss.diagplot.repArchetypes <- function(object, ...) {
  y <- lapply(object, rss, type = 'single')
  x <- seq(length = length(y[[1]]))

  ylim <- range(sapply(y, range))

  plot(x, y[[1]], xlab = 'Index', ylab = 'RSS',
       ylim = ylim, type = 'n', ...)
  for ( i in seq(along = y) )
    lines(x, y[[i]], col = i, ...)
}



weights.diagplot <- function(object, weights.type, ...) {
  y <- weights(object, weights.type)
  x <- seq(length = length(y))

  ylab <- sprintf('%s%s', toupper(substring(weights.type, 1, 1)),
                  substring(weights.type, 2))

  plot(x, y, ylim = c(1, 0), xlab = 'Index', ylab = ylab, ...)
}



reweights.diagplot <- function(object, highlight = NULL,
                               highlight.col = (seq(length(highlight)) + 1), ...) {

  y <- rev(lapply(object$history, function(x) x[[1]]$reweights))
  x <- seq(along = y[[1]])
  col <- rep(1, length = length(x))
  col[highlight] <- highlight.col

  n <- sqrt(length(y))

  par(mfrow = c(ceiling(n), ceiling(n)), mar = c(0, 0, 0, 0))
  for ( i in seq(along = y) )
    plot(x, y[[i]], type = 'p', col = col, xlab = 'Index',
         ylab = 'Reweights', ...)
}



i.reweights.diagplot <- function(object, i, lty = 1,
                                 col = (seq(length(i)) + 1), ...) {
  y <- sapply(object$history, function(x) x[[1]]$reweights[i])
  y <- apply(y, 1, rev)

  matplot(y, type = 'l', lty = lty, col = col, ylab = 'Reweights',
          xlab = 'Iterations', ...)
}



