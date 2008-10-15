
movieplot <- function(zs, data, show=c('atypes', 'adata'),
                                 ssleep=0, bsleep=0, ...) {
  
  steps <- length(zs$history)
  atypesmovie <- ifelse(show[1] == 'atypes', TRUE, FALSE)

  Sys.sleep(ssleep)

  # ... and play:
  for ( i in seq_len(steps)-1 ) {
    a <- ahistory(zs, step=i)
    if ( atypesmovie )
      plot(a, data, ...)
    else
      plot(adata(a), ...)
  }
  Sys.sleep(bsleep)
}

#movieparcoord


movieplot.archetypes <- function(zs, data, startscreen=TRUE, ...) {
  steps <- length(zs$history)

  # Start screen ...
  if ( startscreen ) {
    plot(data, ...)
    Sys.sleep(2)
  }
  
  # ... and play:
  for ( i in seq_len(steps)-1 ) {
    plot(ahistory(zs, step=i), data, ...)
    box()
  }
}

movieplot.data <- function(zs, data, ...) {
  steps <- length(zs$history)

  # Start screen ...
  plot(data, type='n', ...)
  Sys.sleep(2)

  # ... and play:
  for ( i in seq_len(steps)-1 )
    plot(adata(ahistory(zs, step=i)), ...)
 
}



# movie.parcoord2.archetypes

# movie.parcoord2.data



movie <- function(x, ...) {
  UseMethod('movie')
}

movie.archetypes <- function(x, data, ...) {
  zs <- x
  
  history <- zs$history
  steps <- length(history)

  # Start screen ...
  parcoord2(data, var.label=TRUE, col=gray(0.7), lwd=1)
  Sys.sleep(2)

  # ... and play:
  for ( i in (seq_len(steps)-1) ) {
    h <- history[[paste('s', i, sep='')]]$archetypes
    parcoord2(h, data)
  }
}


movie.data <- function(x, data, ...) {
  zs <- x
  
  history <- zs$history
  steps <- length(history)

  # Start screen ...
  parcoord2(data, var.label=TRUE, col=0, lwd=0)
  Sys.sleep(2)

  # ... and play:
  for ( i in seq_len(steps-1) ) {
    h <- history[[paste('s', i, sep='')]]$archetypes
    d <- t(t(atypes(h)) %*% t(h$alphas))
    parcoord2(d, var.label=TRUE, col=gray(0.7), lwd=1)
  }
}


