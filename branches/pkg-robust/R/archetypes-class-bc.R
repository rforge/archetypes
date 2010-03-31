

#' Archetypes getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Archetypes matrix.
#' @export
#' @rdname archetypes-deprecated
atypes <- function(zs, ...) {
  .Deprecated('parameters')
  UseMethod('atypes')
}

#' @S3method atypes archetypes
#' @nord
atypes.archetypes <- function(zs, ...) {
  return(zs$archetypes)
}

#' @S3method atypes stepArchetypes
#' @nord
atypes.stepArchetypes <- function(zs, ...) {
  return(lapply(zs, atypes))
}

#' @S3method atypes repArchetypes
#' @nord
atypes.repArchetypes <- function(zs, ...) {
  lapply(zs, atypes)
}


#' Number of archetypes getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Archetypes matrix.
#' @export
#' @rdname archetypes-deprecated
ntypes <- function(zs, ...) {
  .Deprecated('nparameters')
  UseMethod('ntypes')
}

#' @S3method atypes archetypes
#' @nord
ntypes.archetypes <- function(zs, ...) {
  return(zs$k)
}

#' @S3method ntypes stepArchetypes
#' @nord
ntypes.stepArchetypes <- function(zs, ...) {
  return(sapply(zs, ntypes))
}

#' @S3method ntypes repArchetypes
#' @nord
ntypes.repArchetypes <- function(zs, ...) {
  ntypes(object[[1]])
}



#' Archetypes data approximation.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Approximated data matrix.
#' @export
#' @rdname archetypes-deprecated
adata <- function(zs, ...) {
  .Deprecated('fitted')
  UseMethod('adata')
}

#' @S3method adata archetypes
#' @nord
adata.archetypes <- function(zs, ...) {
  return(t(t(zs$archetypes) %*% t(zs$alphas)))
}



#' Alpha getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Alpha matrix.
#' @export
#' @rdname archetypes-deprecated
alphas <- function(zs, ...) {
  .Deprecated('coef')
  UseMethod('alphas')
}

#' @S3method alphas archetypes
#' @nord
alphas.archetypes <- function(zs, ...) {
  return(zs$alphas)
}



#' Beta getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Beta matrix.
#' @export
#' @rdname archetypes-deprecated
betas <- function(zs, ...) {
  .Deprecated('coef')
  UseMethod('betas')
}

#' @S3method betas archetypes
#' @nord
betas.archetypes <- function(zs, ...) {
  return(zs$betas)
}



#' Iteration getter.
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Number of iterations.
#' @export
#' @rdname archetypes-deprecated
iters <- function(zs, ...) {
  .Deprecated()
  UseMethod('iters')
}

#' @S3method iters archetypes
#' @nord
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

