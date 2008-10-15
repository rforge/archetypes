
rssplot <- function(zs, ...) {
  x <- sapply(zs, ntypes)
  y <- sapply(zs, rss)
  
  plot(x, y, type='b', xlab='Archetypes', ylab='RSS', ...)
  axis(1, at=x, ...)
}
