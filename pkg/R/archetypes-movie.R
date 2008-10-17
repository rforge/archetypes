
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
    
    Sys.sleep(bsleep)
  }
}



moviepcplot <- function(zs, data, show=c('atypes', 'adata'),
                        ssleep=0, bsleep=0, ...) {

  steps <- length(zs$history)
  atypesmovie <- ifelse(show[1] == 'atypes', TRUE, FALSE)
  rx <- apply(data, 2, range, na.rm=TRUE)

  Sys.sleep(ssleep)

  # ... and play:
  for ( i in seq_len(steps)-1 ) {
    a <- ahistory(zs, step=i)

    if ( atypesmovie )
      pcplot(a, data, ...)
    else
      pcplot(adata(a), rx=rx, ...)
    
    Sys.sleep(bsleep)
  }
}
