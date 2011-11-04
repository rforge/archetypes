#' @include generics.R
{}



xyplot.archetypes <- function(x, data = NULL, atypes.args = list(colour = "red"),
                              chull = NULL,
                              chull.args = list(colour = "gray"),
                              ahull.show = FALSE, ahull.args = atypes.args,
                              adata.show = FALSE, adata.args = list(colour = "green"),
                              data.args = list(), ...) {

  atypes <- as.data.frame(parameters(x))

  stopifnot(ncol(atypes) == 2)

  xlab <- colnames(atypes)[1]
  ylab <- colnames(atypes)[2]


  ## Archetypes:
  p <- ggplot(atypes, aes_string(x = xlab, y = ylab))


  ## Data, convex hull:
  if ( !is.null(data) ) {
    data <- as.data.frame(data)

    p <- p + do.call(geom_point, c(list(data = data), data.args))

    if ( !is.null(chull) ) {
      chull <- data[c(chull, chull[1]), ]

      p <- p + do.call(geom_point, c(list(data = chull), chull.args))
      p <- p + do.call(geom_path, c(list(data = chull), chull.args))
    }
  }


  ## Approximated data:
  if ( adata.show ) {
    adata <- as.data.frame(fitted(x))

    p <- p + do.call(geom_point, c(list(data = adata), adata.args))

    if ( !is.null(data) ) {
      colnames(adata) <- sprintf("fitted.%s", colnames(adata))

      xlab_adata <- colnames(adata)[1]
      ylab_adata <- colnames(adata)[2]

      adata <- cbind(data, adata)

      p <- p + do.call(geom_segment,
                       c(list(data = adata,
                              mapping = aes_string(xend = xlab_adata,
                                                   yend = ylab_adata)),
                         adata.args))
    }
  }


  ## Approximated convex hull:
  if ( ahull.show ) {
    ahull <- atypes[ahull(atypes), ]

    p <- p + do.call(geom_path, c(list(data = ahull), ahull.args))
  }


  ## The archetypes should be plotted on top:
  p <- p + do.call(geom_point, atypes.args)


  p
}



ahull <- function(a) {
  xc <- a[,1]; xm <- mean(xc)
  yc <- a[,2]; ym <- mean(yc)

  real <- xc - xm
  imag <- yc - ym
  angle <- atan2(imag, real)

  index <- order(angle)

  return(c(index, index[1]))
}


