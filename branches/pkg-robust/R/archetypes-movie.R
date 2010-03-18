

#' Archetypes plot movie.
#' @param zs An \code{\link{archetypes}} object.
#' @param data The data matrix.
#' @param show Show archetypes or approximated data.
#' @param ssleep Seconds to sleep before start.
#' @param bsleep Seconds to sleep between each plot.
#' @param postfn Post plot function; is called in each
#'   iteration after the plot call.
#' @param ... Passed to underlying plot functions.
#' @return Undefined.
#' @usage
#' movieplot(zs, data, show=c('atypes', 'adata'),
#'           ssleep=0, bsleep=0, postfn=function(iter){}, ...)
#' @aliases movieplot
#' @export
movieplot <- function(zs, data, show=c('atypes', 'adata', 'rwdata'),
                      ssleep=0, bsleep=0, postfn=function(iter){},
                      rwdata.col1 = gray(0.7), rwdata.col2 = 2, ...) {

  show <- match.arg(show)
  steps <- length(zs$history)

  if ( show == 'rwdata' )
    data <- zs$family$scalefn(t(data))

  # ... and play:
  Sys.sleep(ssleep)

  for ( i in seq_len(steps)-1 ) {
    a <- ahistory(zs, step=i)

    switch(show,

           atypes = {
             plot(a, data, ...)
           },

           adata = {
             plot(adata(a), ...)
           },

           rwdata = {
             d <- zs$family$weightfn(data, a$reweights)

             plot(t(data), col = rwdata.col1, ...)
             points(t(d), col = rwdata.col2, ...)
           })

    postfn(i)

    Sys.sleep(bsleep)
  }
}


#' Archetypes plot movie 2.
#'
#' Shows the intermediate steps of the algorithm;
#'
#' @param zs An \code{\link{archetypes}} object.
#' @param data The data matrix.
#' @param show Shows only archetypes currently.
#' @param ssleep Seconds to sleep before start.
#' @param bsleep Seconds to sleep between each plot.
#' @param zas.col Color of the intermediate archetypes.
#' @param zas.pch Type of the intermediate archetypes points.
#' @param old.col Color of the archetypes on step further.
#' @param ... Passed to underlying plot functions.
#' @return Undefined.
#' @export
movieplot2 <- function(zs, data, show='atypes',
                       ssleep=0, bsleep=0,
                       zas.col=2, zas.pch=13,
                       old.col=rgb(1,0.5,0.5), ...) {

  steps <- length(zs$history)

  Sys.sleep(ssleep)

  # Initial archetypes:
  a <- ahistory(zs, step=0)
  plot(a, data, ...)
  Sys.sleep(bsleep)

  # Alternating loop:
  for ( i in seq_len(steps-1) ) {
    a0 <- ahistory(zs, step=(i-1))
    a <- ahistory(zs, step=i)

    plot(a0, data, atypes.col=old.col, ...)
    points(a$zas, col=zas.col, pch=zas.pch, ...)
    Sys.sleep(bsleep)

    plot(a0, data, atypes.col=old.col, ...)
    points(a$zas, col=zas.col, pch=zas.pch, ...)
    par(new=TRUE)
    plot(a, data, ...)
    Sys.sleep(bsleep)
  }

  plot(a, data, ...)
}


#' Archetypes parallel coordinates plot movie.
#' @param zs An \code{\link{archetypes}} object.
#' @param data The data matrix.
#' @param show Show archetypes or approximated data.
#' @param ssleep Seconds to sleep before start.
#' @param bsleep Seconds to sleep between each plot.
#' @param ... Passed to underlying pcplot functions.
#' @return Undefined.
#' @export
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
