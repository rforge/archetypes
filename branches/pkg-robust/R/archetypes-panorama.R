

panorama <- function(object, ...) {
  UseMethod('panorama')
}



panorama.archetypes <- function(object, data, distfn = distEuclidean,
                                ref.order = NULL, xlab = 'Index',
                                ylab = 'Distance', col = 1, pch = 1,
                                atypes.col = (seq(length = ntypes(object)) + 1),
                                atypes.pch = rep(19, ntypes(object)), ...) {

  n1 <- nrow(data)
  n2 <- ntypes(object)

  data <- rbind(data, parameters(object))
  dist <- distfn(data, parameters(object))

  x <- seq(length = n1 + n2)

  col <- c(rep(col, n1), atypes.col)
  pch <- c(rep(pch, n1), atypes.pch)

  if ( is.null(ref.order) )
    ix <- x
  else
    ix <- order(dist[, ref.order])


  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  ylim <- c(0, max(dist))

  mar <- opar$mar
  mar[2] <- max(mar[2], 5)

  par(mfrow = c(n2, 1), mar = mar)

  for ( i in seq(length = n2) ) {
    plot(x, dist[ix, i], ylim = ylim, xlab = xlab,
         ylab = ylab, col = col[ix], pch = pch[ix], ...)

    mtext(sprintf('Archetype %s', i), side = 2, line = 4,
          cex = par('cex'))
  }


  invisible(dist)
}



