
ahull <- function(zs) {
  a <- rbind(atypes(zs), atypes(zs)[1,])
  xc <- a[,1]; xm <- mean(xc)
  yc <- a[,2]; ym <- mean(yc)
  
  real <- xc - xm
  imag <- yc - ym
  angle <- atan2(imag, real)
  
  index <- order(angle)
  
  return(a[c(index, index[1]),])
}

  

plot.archetypes <- function(x, y,
                            data.col=gray(0.7), data.pch=19,
                            atypes.col=2, atypes.pch=19,
                            ahull.show=TRUE, ahull.col=atypes.col,
                            chull=NULL, chull.col=1, chull.pch=19,
                            adata.show=FALSE, adata.col=3, adata.pch=13, link.col=data.col, ...) {

  zs <- x; data <- y;

  plot(data, col=data.col, pch=data.pch, ...)
  points(atypes(zs), col=atypes.col, pch=atypes.pch, ...)

  if ( !is.null(chull) ) {
    points(data[chull,], col=chull.col, pch=chull.pch, ...)
    lines(data[c(chull, chull[1]),], col=chull.col, ...)
  }

  if ( ahull.show )
    lines(ahull(zs), col=ahull.col)


  if ( adata.show ) {
    ### Based on an idea of Bernard Pailthorpe.
    adata <- adata(zs)
    
    points(adata, col=adata.col, pch=adata.pch, ...)
    for ( i in seq_len(nrow(data)) )
      lines(rbind(data[i,], adata[i,]), col=link.col, ...)
  }
}


plot.stepArchetypes <- function(x, y,
                                data.col=gray(0.7), data.pch=19,
                                atypes.col=(seq_len(length(x) * length(x[[1]]))+1), atypes.pch=19,
                                ahull.show=TRUE, ahull.col=atypes.col, ...) {
  
  zs <- x; data <- y;
  
  flatzs <- unlist(zs, recursive=FALSE)
  
  plot(data, col=data.col, pch=data.pch, ...)
  for ( i in seq_along(flatzs) ) {
    a <- flatzs[[i]]
    points(atypes(a), col=atypes.col[i], pch=atypes.pch, ...)

    if ( ahull.show )
      lines(ahull(a), col=ahull.col[i])
  }
}


