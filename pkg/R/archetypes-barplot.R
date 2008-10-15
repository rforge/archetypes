
barplot.archetypes <- function(height, data, beside=TRUE, percentage=FALSE, ...) {
  atypes <- atypes(height)

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

  if ( beside ) {
    barplot(atypes, ylab=ylab, beside=TRUE, ylim=ylim, ...)
  }
  else {
    p <- nrow(atypes)
    
    par(mfrow=c(p,1))
    for ( i in 1:p ) 
      barplot(atypes[i,], main=paste('Archetype', i),
              ylab=ylab, ylim=ylim, ...)
  }  
}

