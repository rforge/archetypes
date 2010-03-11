

#' Barplot of archetypes.
#' @param height An \code{\link{archetypes}} object.
#' @param data The original data matrix.
#' @param beside Show one barplot for each archetype, or one barplot
#'    with stacked bars, either per archetype or per variable.
#' @param percentage Show real values or percentages according to the
#'    original data.
#' @param ... Passed to the underlying \code{\link{barplot}} call.
#' @return Undefined.
#' @method barplot archetypes
#' @importFrom graphics barplot
#' @export
barplot.archetypes <- function(height, data,
                               beside=c('FALSE', 'atypes', 'variables'),
                               percentage=FALSE, ...) {
  beside <- match.arg(beside)

  atypes <- atypes(height)
  rownames(atypes) <- sprintf('Archetype %s', seq(length = nrow(atypes)))

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

  if ( beside == 'variables' ) {
    barplot(atypes, ylab=ylab, beside=TRUE, ylim=ylim, ...)
  }
  else if ( beside == 'atypes' ) {
    barplot(t(atypes), ylab=ylab, beside=TRUE, ylim=ylim, ...)
  }
  else {
    p <- nrow(atypes)

    par(mfrow=c(p,1))
    for ( i in 1:p )
      barplot(atypes[i,], main=paste('Archetype', i),
              ylab=ylab, ylim=ylim, ...)
  }
}

