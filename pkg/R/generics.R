
#' Generic functions
#'
#' Generic functions defined by the archetypes package:
#' Return residual sum of squares
#'
#' @param object An object
#' @param ... Futher arguments
#' @rdname archetypes-generics
#'
#' @export
rss <- function(object, ...) {
  UseMethod('rss')
}



#' Return number of parameters
#'
#' @rdname archetypes-generics
#'
#' @export
nparameters <- function(object, ...) {
  UseMethod('nparameters')
}




#' Return best model
#'
#' @rdname archetypes-generics
#'
#' @export
bestModel <- function(object, ...) {
  UseMethod('bestModel')
}



#' Panorama
#'
#' @rdname archetypes-generics
#'
#' @export
panorama <- function(object, ...) {
  UseMethod('panorama')
}



#' Parallel coordinates plot
#'
#' @param x An object.
#' @rdname archetypes-generics
#'
#' @export
pcplot <- function(x, ...) {
  UseMethod('pcplot')
}



#' Scatter plot.
#'
#' @rdname archetypes-generics
#'
#' @export
xyplot <- function(x, ...) {
  UseMethod('xyplot')
}



### Utility functions: ###############################################

subclass <- function(x, subclass) {
  structure(x, class = c(subclass, class(x)))
}



## http://gettinggeneticsdone.blogspot.com/2010/03/arrange-multiple-ggplot2-plots-in-same.html
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
 dots <- list(...)
 n <- length(dots)
 if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
 if(is.null(nrow)) { nrow = ceiling(n/ncol)}
 if(is.null(ncol)) { ncol = ceiling(n/nrow)}
        ## NOTE see n2mfrow in grDevices for possible alternative
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
 ii.p <- 1
 for(ii.row in seq(1, nrow)){
 ii.table.row <- ii.row
 if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
  for(ii.col in seq(1, ncol)){
   ii.table <- ii.p
   if(ii.p > n) break
   print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
   ii.p <- ii.p + 1
  }
 }
}



params_plot <- function(x, transpose = FALSE) {
  nullplot <- function(p) {
    if ( is.null(p) )
      NULL
    else
      plot(p)
  }

  plots <- lapply(unlist(x, recursive = FALSE), nullplot)

  dim <- c(ncol = max(sapply(x, length)),
           nrow = length(x))

  if ( transpose ) {
    dim <- rev(dim)
    order <- as.numeric(t(matrix(seq(along = plots), nrow = dim[2])))
    plots <- plots[order]
  }

  do.call(arrange, c(plots, list(nrow = dim[2], ncol = dim[1])))
}

