

#' Barplot of archetypes.
#' @param height An \code{\link{archetypes}} object.
#' @param data The original data matrix.
#' @param which \code{below} creates a barplot for each archetype,
#'    \code{beside} creates one barplot with bars side by side.
#' @param which.beside Barplot according to \code{atypes} or \code{variables}.
#' @param which.below \code{compressed} plots the labels only once.
#' @param percentage Show real values or percentages according to the
#'    original data.
#' @param ... Passed to the underlying \code{\link{barplot}} call.
#' @return Undefined.
#' @method barplot archetypes
#' @importFrom graphics barplot
#' @export
barplot.archetypes <- function(height, data,
                               which = c('below', 'beside'),
                               which.beside = c('atypes', 'variables'),
                               which.below = c('compressed', 'default'),
                               percentage=FALSE, ...) {

  .beside.atypes <- function() {
    barplot(t(atypes), ylab=ylab, beside=TRUE, ylim=ylim, ...)
  }

  .beside.variables <- function() {
    barplot(atypes, ylab=ylab, beside=TRUE, ylim=ylim, ...)
  }

  .below.default <- function() {
    p <- nrow(atypes)

    layout(matrix(1:p, nrow = p, byrow = TRUE))
    for ( i in 1:p )
      barplot(atypes[i,], main=paste('Archetype', i),
              ylab=ylab, ylim=ylim, ...)
  }

  .below.compressed <- function() {
    p <- nrow(atypes) + 1

    layout(matrix(1:p, nrow = p, byrow = TRUE))
    for ( i in 1:(p - 1) ) {
      par(mar = c(0, 4, 1, 0) + 0.1)
      x.at <- barplot(atypes[i,], ylab=ylab, ylim=ylim, names.arg='',
                      las=2, ...)
    }
    text(x.at, par("usr")[3] - 1, srt = 90, adj = 1,
         labels = colnames(atypes), xpd = NA)
  }

  which <- match.arg(which)
  if ( which == 'beside' )
    which.arg <- match.arg(which.beside)
  else
    which.arg <- match.arg(which.below)

  atypes <- atypes(height)
  rownames(atypes) <- sprintf('Archetype %s',
                              seq(length = nrow(atypes)))

  if ( !percentage ) {
    ylab <- 'Value'
    ylim <- NULL
  }
  else {
    m <- sapply(data, max)
    atypes <- t(t(atypes) / m * 100)
    ylab <- 'Percentage'
    ylim <- c(0,100)
  }

  do.call(sprintf('.%s.%s', which, which.arg), list())

  invisible(atypes)
}

