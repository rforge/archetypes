

#' Archetypes object constructor.
#' @param archetypes The archetypes; a $p \times m$ matrix, see
#'   \link{atypes}.
#' @param alphas The coefficients; a $n \times p$ matrix.
#' @param rss The residual sum of squares; see \link{rss}.
#' @param iters The number of iterations to the convergence.
#' @param call The call of the \code{\link{archetypes}} function.
#' @param history If \code{saveHistory} set then an environment with the
#'   archetypes object for each execution step;
#' @param kappas The kappas for each system of linear equations.
#' @return A list with an element for each parameter and class attribute
#'   \code{archetypes}.
#' @seealso \code{\link{archetypes}}
#' @export
#' @aliases as.archetypes archetypes-class
as.archetypes <- function(archetypes, alphas, rss, iters=NULL, call=NULL,
                          history=NULL, kappas=NULL) {
  
  return(structure(list(archetypes=archetypes,
                        alphas=alphas,
                        rss=rss,
                        iters=iters,
                        kappas=kappas,
                        call=call,
                        history=history),
                   class='archetypes'))  
}



#' Print method for archetypes object.
#' @param x The archetypes object.
#' @param full Full information or just convergence and rss information.
#' @param ... Ignored.
#' @return Undefined.
#' @method print archetypes
#' @S3method print archetypes
print.archetypes <- function(x, full=TRUE, ...) {
  if ( full ) {
    cat('Archetypes\n\n')
    
    args <- as.list(x$call[-1])
    cat(paste(names(args), args, sep='=', collapse=', '), '\n\n')
  }
  
  cat('Convergence after', x$iters, 'iterations\n')
  cat('with RSS = ', rss(x), '.\n', sep='')
}



#' Generic archetypes getter.
#' @param zs An object with archetypes.
#' @param ... Further arguments.
#' @export
atypes <- function(zs, ...) {
  UseMethod('atypes')
}

#' Archetypes getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Archetypes matrix.
#' @method atypes archetypes
#' @S3method atypes archetypes
atypes.archetypes <- function(zs, ...) {
  return(zs$archetypes)
}



#' Generic number of archetypes getter.
#' @param zs An object with archetypes.
#' @param ... Further arguments.
#' @export
ntypes <- function(zs, ...) {
  UseMethod('ntypes')
}

#' Number of archetypes getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Number of archetypes.
#' @method ntypes archetypes
#' @S3method ntypes archetypes
ntypes.archetypes <- function(zs, ...) {
  return(nrow(atypes(zs)))
}



#' Generic residual sum of squares getter.
#' @param zs An object.
#' @param ... Further arguments.
#' @export
rss <- function(zs, ...) {
  UseMethod('rss')
}

#' Archetypes residual sum of squares getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Residual sum of squares.
#' @method rss archetypes
#' @S3method rss archetypes
rss.archetypes <- function(zs, ...) {
  return(zs$rss)
}



#' Generic data approximation.
#' @param zs An object.
#' @param ... Further arguments.
#' @export
adata <- function(zs, ...) {
  UseMethod('adata')
}

#' Archetypes data approximation.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Approximated data matrix.
#' @method adata archetypes
#' @S3method adata archetypes
adata.archetypes <- function(zs, ...) {
  return(t(t(zs$archetypes) %*% t(zs$alphas)))
}



#' Generic alpha getter.
#' @param zs An object with archetypes.
#' @param ... Further arguments.
#' @export
alphas <- function(zs, ...) {
  UseMethod('alphas')
}

#' Alpha getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Alpha matrix.
#' @method alphas archetypes
#' @S3method alphas archetypes
alphas.archetypes <- function(zs, ...) {
  return(zs$alphas)
}



#' Generic history getter.
#' @param zs An object.
#' @param ... Further arguments.
#' @export
ahistory <- function(zs, ...) {
  UseMethod('ahistory')
}

#' Archetypes history getter.
#' @param zs An \code{archetypes} object.
#' @param step The step number to return.
#' @param ... Ignored.
#' @return The \code{archetypes} object of the requested step.
#' @method ahistory archetypes
#' @S3method ahistory archetypes
ahistory.archetypes <- function(zs, step, ...) {
  if ( is.null(zs$history) )
    stop('No history available')

  if ( step >= 0 )
    s <- paste('s', step, sep='')
  else
    s <- paste('s', nhistory(zs) + step - 1, sep='')
  
  return(zs$history[[s]][[1]])
}



#' Generic number of history steps getter.
#' @param zs An object.
#' @param ... Further arguments.
#' @export
nhistory <- function(zs, ...) {
  UseMethod('nhistory')
}

#' Archetypes number of history steps getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return The number of history steps available.
#' @method nhistory archetypes
#' @S3method nhistory archetypes
nhistory.archetypes <- function(zs, ...) {
  if ( is.null(zs$history) )
    stop('No history available')

  return(length(zs$history))
}
