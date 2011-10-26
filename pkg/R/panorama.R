#' @include generics.R
{}



#' Archetypes panorama
#'
#' @param object An \code{\link{archetypes}} object.
#' @param data The corresponding data set.
#' @param distfn The distance function; note that this function has to
#'   match with the archtypes blocks (see
#'   \code{\link{archetypesFamily}}).
#' @param ordered Order the distances.
#' @param ... Ignored.
#'
#' @return A list (class attribute \code{atypes_panorama}) with the
#'   distance matrix for each archetype and the data points and
#'   additional meta information.
#'
#' @method panorama archetypes
#'
#' @examples
#'   \dontrun{
#'     data(toy)
#'     a <- archetypes(toy, 3)
#'     plot(panorama(a, toy))
#'
#'     ## See demo(robust-ozone).
#'   }
#'
#' @rdname panorama
#'
#' @S3method panorama archetypes
panorama.archetypes <- function(object, data, distfn = distEuclidean,
                                ordered = TRUE, ...) {

  n1 <- nrow(data)
  n2 <- nparameters(object)

  atypes <- parameters(object)

  data <- rbind(data, atypes)
  dist <- distfn(data, atypes)

  type <- c(rep("Data point", n1), rownames(atypes))
  type <- matrix(rep(type, n2), ncol = n2)

  order <- seq(length = n1)
  order <- matrix(rep(order, n2), ncol = n2)

  if ( ordered ) {
    order <- sapply(seq(length = n2), function(i) order(dist[, i]))
    dist <- sapply(seq(ncol(dist)), function(i) dist[order[, i], i])
    type <- sapply(seq(ncol(type)), function(i) type[order[, i], i])
  }

  colnames(dist) <- rownames(atypes)
  colnames(type) <- rownames(atypes)
  colnames(order) <- rownames(atypes)

  panorama <- list()
  panorama$dist <- dist
  panorama$type <- type
  panorama$order <- order
  panorama$ordered <- ordered

  class(panorama) <- c("atypes_panorama", class(panorama))

  panorama
}



#' @rdname panorama
#' @method plot atypes_panorama
#' @S3method plot atypes_panorama
plot.atypes_panorama <- function(x, y = NULL, ...) {
  x0 <- melt(x)

  x1 <- subset(x0, Archetype != "Data point")
  x1$Archetype <- x1$Archetype[, drop = TRUE]

  xlab <- {
    if ( x$ordered )
      "Index by distance"
    else
      "Index by observation"
  }

  p <- ggplot(x0, aes(X1, value))
  p <- p + geom_point() + facet_grid(X2 ~ .)
  p <- p + geom_point(data = x1, aes(colour = Archetype))
  p <- p + xlab(xlab) + ylab("Distance")

  p
}



distEuclidean <- function (x, centers) {
  if (ncol(x) != ncol(centers)) {
    stop(sQuote("x"), " and ", sQuote("centers"),
         " must have the same number of columns")
  }

  z <- matrix(0, nrow = nrow(x), ncol = nrow(centers))
  for (k in 1:nrow(centers)) {
    z[, k] <- sqrt(colSums((t(x) - centers[k, ])^2))
  }
  z
}



melt.atypes_panorama <- function(data, ...) {
  d <- melt(data$dist)
  d$Archetype <- melt(data$type)$value
  d
}

