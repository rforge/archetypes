.v2 <- function(old, new) {
  warning(sprintf('Function %s is deprecated; please use %s instead.',
                  sQuote(old), sQuote(new)),
          call. = FALSE)
}

#' Archetypes getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Archetypes matrix.
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
  .v2('atypes', 'parameters')
  return(zs$archetypes)
}



#' Number of archetypes getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Number of archetypes.
#' @export
ntypes <- function(zs, ...) {
  UseMethod('ntypes')
}

#' @S3method ntypes archetypes
ntypes.archetypes <- function(zs, ...) {
  return(zs$k)
}



#' Residual sum of squares getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Residual sum of squares.
#' @export
rss <- function(zs, ...) {
  UseMethod('rss')
}

#' Residual sum of squares getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Residual sum of squares.
#' @method rss archetypes
#' @S3method rss archetypes
rss.archetypes <- function(zs, ...) {
  return(zs$rss)
}



#' Archetypes data approximation.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Approximated data matrix.
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
  .v2('adata', 'fitted')
  return(t(t(zs$archetypes) %*% t(zs$alphas)))
}



#' Alpha getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Alpha matrix.
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
  .v2('alphas', 'coef')
  return(zs$alphas)
}



#' Beta getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Beta matrix.
#' @export
betas <- function(zs, ...) {
  UseMethod('betas')
}

#' Beta getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Beta matrix.
#' @method betas archetypes
#' @S3method betas archetypes
betas.archetypes <- function(zs, ...) {
  .v2('betas', 'coef')
  return(zs$betas)
}



#' Iteration getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Number of iterations.
#' @export
iters <- function(zs, ...) {
  UseMethod('iters')
}

#' Iteration getter.
#' @param zs An \code{archetypes} object.
#' @param ... Ignored.
#' @return Number of iterations.
#' @method iters archetypes
#' @S3method iters archetypes
iters.archetypes <- function(zs, ...) {
  return(zs$iters)
}



#' Archetypes history getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return The \code{archetypes} object of the requested step.
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



#' Number of history steps getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return The number of history steps available.
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


#' Kappa getter.
#' @param z An \code{archetypes} object.
#' @param ... Ignored.
#' @return A vector of kappas.
#' @method kappa archetypes
#' @S3method kappa archetypes
kappa.archetypes <- function(z, ...) {
  return(z$kappas)
}
