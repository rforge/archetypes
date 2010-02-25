### distanz von jedem archetyp zum nächsten punkt

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


  invisible(list(x = x, y = y))
}



rss.diagplot <- function(object, ...) {
  UseMethod('rss.diagplot')
}

rss.diagplot.archetypes <- function(object, sort = FALSE, ...) {
  y <- rss(object, type = 'single')
  x <- seq(length = length(y))

  if ( sort )
    xy <- sort(y)

  plot(x, y, xlab = 'Index', ylab = 'RSS', ...)
  abline(h = 0, lty = 2, col = gray(0.7), ...)


  invisible(list(x = x, y = y))
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



reweights.diagplot <- function(object, col = 1, pch = 1, highlight = NULL,
                               highlight.col = (seq(length(highlight)) + 1),
                               highlight.pch = 13, ...) {

  y <- rev(lapply(object$history, function(x) x[[1]]$reweights))
  x <- seq(along = y[[1]])

  col <- rep(col, length = length(x))
  col[highlight] <- highlight.col

  pch <- rep(pch, length = length(x))
  pch[highlight] <- highlight.pch

  n <- sqrt(length(y))

  par(mfrow = c(ceiling(n), ceiling(n)), mar = c(0, 0, 0, 0))
  for ( i in seq(along = y) )
    plot(x, y[[i]], type = 'p', col = col, pch = pch, ylim = c(0, 1),
         xlab = 'Index', ylab = 'Reweights',  ...)
}



reweights.curve.diagplot <- function(object, i, lty = 1,
                                     col = (seq(length(i)) + 1), ...) {
  y <- sapply(object$history, function(x) x[[1]]$reweights[i])
  y <- apply(y, 1, rev)

  matplot(y, type = 'l', lty = lty, col = col, ylab = 'Reweights',
          xlab = 'Iterations', ylim = c(0, 1), ...)
}



reweights.liftoff.diagplot <- function(object, ...) {

  y <- sapply(object$history, function(x) x[[1]]$reweights)
  y <- rev(colSums(y != 0))

  barplot(y, xlab = 'Iterations', ylab = 'Reweights > 0', ...)
}



reweights.rss.diagplot <- function(object, ...) {
  y1 <- sapply(object$history, function(x) x[[1]]$reweights)
  y1 <- rev(colSums(y1))

  y2 <- rev(sapply(object$history, function(x) x[[1]]$rss))

  x <- seq(along = y1)

  par(mfrow = c(2, 1))
  plot(x, y1, type = 'l', xlab = 'Iterations', ylab = 'Reweights', ...)
  plot(x, y2, type = 'l', xlab = 'Iterations', ylab = 'RSS', ...)
}


archetypes.view.diagplot <- function(object, data, ref.order = NULL,
                                     distfn = distEuclidean, ...) {

  d <- distfn(data, parameters(object))
  x <- seq(length = nrow(d))

  if ( is.null(ref.order) )
    ix <- x
  else
    ix <- order(d[, 1])

  ylim <- c(0, max(d))

  par(mfrow = c(ncol(d), 1))
  for ( i in seq(length = ncol(d)) )
    plot(x, d[ix, i], xlab = sprintf('Archetype %s', i), ylab = 'Distance',
         ylim = ylim, ...)

  invisible(d)
}
