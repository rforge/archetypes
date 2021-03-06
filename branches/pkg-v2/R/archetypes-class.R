#' @include generics.R
{}



#' Archetypes object constructor
#'
#' @param object The archetypes; a \eqn{p \times m} matrix, see
#'   \code{\link{parameters}}.
#' @param k The number of archetypes;
#' @param alphas The coefficients; a \eqn{n \times p} matrix, see
#'   \code{\link{coef}}.
#' @param rss The residual sum of squares; see \code{\link{rss.archetypes}}.
#' @param iters The number of iterations to the convergence.
#' @param call The call of the \code{\link{archetypes}} function.
#' @param history If \code{saveHistory} set then an environment with the
#'   archetypes object for each execution step;
#' @param kappas The kappas for each system of linear equations.
#' @param betas The data coefficients; a \eqn{p \times n} matrix.
#' @param zas The temporary archetypes.
#' @param family The archetypes family.
#' @param familyArgs Additional arguments for family blocks.
#' @param residuals The residuals.
#' @param weights The data weights.
#' @param reweights The data reweights.
#'
#' @return A list with an element for each parameter and class attribute
#'   \code{archetypes}.
#'
#' @family archetypes
#'
#' @export
as.archetypes <- function(object, k, alphas, rss, iters = NULL, call = NULL,
                          history = NULL, kappas = NULL, betas = NULL, zas = NULL,
                          family = NULL, familyArgs = NULL, residuals = NULL,
                          weights = NULL, reweights = NULL) {

  return(structure(list(archetypes = object,
                        k = k,
                        alphas = alphas,
                        rss = rss,
                        iters = iters,
                        kappas = kappas,
                        betas = betas,
                        zas = zas,
                        call = call,
                        history = history,
                        family = family,
                        familyArgs = familyArgs,
                        residuals = residuals,
                        weights = weights,
                        reweights = reweights),
                   class = c(family$class, 'archetypes')))
}



setOldClass(c("archetypes"))


#' @S3method print archetypes
print.archetypes <- function(x, full = TRUE, ...) {
  if ( full ) {
    cat('Archetypes object\n\n')
    cat(paste(deparse(x$call), collapse = '\n'), '\n\n')
  }

  cat('Convergence after', x$iters, 'iterations\n')
  cat('with RSS = ', rss(x), '.\n', sep = '')
}



#' Return fitted data
#'
#' Returns the approximated data.
#'
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Matrix with approximated data.
#' @method fitted archetypes
#' @rdname fitted
#'
#' @importFrom stats fitted
#' @S3method fitted archetypes
fitted.archetypes <- function(object, ...) {
  t(t(object$archetypes) %*% t(object$alphas))
}



#' Return coefficients
#'
#' @param object An \code{archetypes} object.
#' @param type Return alpha or beta coefficients.
#' @param ... Ignored.
#' @return Coefficient matrix.
#' @method coef archetypes
#' @rdname coef
#'
#' @importFrom stats coef
#' @S3method coef archetypes
coef.archetypes <- function(object, type = c("alphas", "betas"), ...) {
  type <- match.arg(type)
  object[[type]]
}



#' Return number of archetypes
#'
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Number of archetypes.
#' @rdname nparameters
#'
#' @method nparameters archetypes
#' @S3method nparameters archetypes
nparameters.archetypes <- function(object, ...) {
  return(object$k)
}



#' Return residuals
#'
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Matrix with residuals.
#' @method residuals archetypes
#' @rdname residuals
#'
#' @importFrom stats residuals
#' @S3method residuals archetypes
residuals.archetypes <- function(object, ...) {
  object$residuals
}



#' Return weights
#'
#' @param object An \code{archetypes} object.
#' @param type Return global weights (weighted archetypes) or
#'   weights calculated during the iterations (robust archetypes).
#' @param ... Ignored.
#' @return Vector of weights.
#' @method weights archetypes
#' @rdname weights
#'
#' @importFrom stats weights
#' @S3method weights archetypes
weights.archetypes <- function(object, type = c('weights', 'reweights'), ...) {
  type <- match.arg(type)
  object[[type]]
}



#' Return kappa
#'
#' @param z An \code{archetypes} object.
#' @param ... Ignored.
#' @return A vector of kappas.
#' @rdname kappa
#'
#' @method kappa archetypes
#' @S3method kappa archetypes
kappa.archetypes <- function(z, ...) {
  return(z$kappas)
}



#' Return residual sum of squares
#'
#' @param object An \code{archetypes} object.
#' @param type Return scaled, single or global RSS.
#' @param ... Ignored.
#' @return Residual sum of squares.
#' @method rss archetypes
#' @rdname rss
#'
#' @S3method rss archetypes
rss.archetypes <- function(object, type = c('scaled', 'single', 'global'), ...) {
  type <- match.arg(type)
  resid <- residuals(object)

  switch(type,
         scaled = object$rss,
         single = apply(resid, 1, object$family$normfn),
         global = object$family$normfn(resid) / nrow(resid))
}



#' Fitted archetypes
#'
#' @param object An \code{archetypes} object.
#' @param ... Ignored.
#' @return Matrix (with class \code{atypes_parameters}) with \eqn{k}
#'   archetypes.
#'
#' @aliases parameters-methods
#' @aliases parameters,archetypes-method
#'
#' @seealso \code{\link{profile,archetypes-method}}
#'
#' @importFrom modeltools parameters
#' @exportMethod parameters
setMethod('parameters', signature = c(object = 'archetypes'),
function(object, ...) {
  parameters <- object$archetypes

  if ( is.null(parameters) )
    return(parameters)


  rownames(parameters) <- sprintf("Archetype %s",
                                  seq(length = object$k))

  subclass(parameters, "atypes_parameters")
})



#' @param height An \code{atypes_parameters} object.
#' @rdname parameters
#' @method barplot atypes_parameters
#' @S3method barplot atypes_parameters
barplot.atypes_parameters <- function(height, ...) {
  p <- ggplot(melt(height), aes(X2, value))
  p <- p + geom_bar(stat = "identity") + facet_grid(X1 ~ .)
  p <- p + xlab("Variable") + ylab("Value")
  p
}



#' @param x An \code{atypes_parameters} object.
#' @param y Ignored.
#' @rdname parameters
#' @method plot atypes_profile
#' @S3method plot atypes_profile
plot.atypes_parameters <- function(x, y = NULL, ...) {
  barplot.atypes_parameters(x, ...)
}



### Not implemented yet: #############################################

predict.archetypes <- function(object, newdata = NULL,
                               typxe = c('alphas', 'data'), ...) {
  type <- match.arg(type)

  if ( is.null(newdata) )
    return(switch(type,
                  alphas = coef(object, type = 'alphas'),
                  data = fitted(object)))

  stop('Not implemented yet.')

  ### Something like the following ...
  #if ( type == 'alphas' )
  #  object$family$alphasfn(NULL, t(object$archetypes), t(newdata))
}
