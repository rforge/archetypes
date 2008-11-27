#' @include archetypes-kit-blocks.R
#' @include archetypes-class.R
roxygen()



#' Perform archetypal analysis on a data matrix.
#' @param data A numeric $n \times m$ data matrix.
#' @param k The number of archetypes.
#' @param maxIterations The maximum number of iterations.
#' @param minImprovement The minimal value of improvement between two
#'   iterations.
#' @param maxKappa The limit of kappa to report an ill-ness warning.
#' @param verbose Print some details during execution.
#' @param saveHistory Save each execution step in an environment for
#'   further analyses.
#' @param normfn 
#' @param scalefn Data scaling block.
#' @param rescalefn Archetypes rescaling block.
#' @param dummyfn Add dummy row block.
#' @param undummyfn Remove dummy row block.
#' @param initfn Alpha and beta initialization block.
#' @param alphasfn Calculate alpha block.
#' @param zalphasfn Calculate archetypes block.
#' @param betasfn Calculate beta block.
#' @return An object of class \code{archetypes}, see
#'   \code{\link{archetypes-class}}.
#' @seealso \link{stepArchetypes}
#' @references Cutler and Breiman. Archetypal Analysis. Technometrics,
#'   36(4), 1994. 338-348.
#' @example
#'   data(toy)
#'   a <- archetypes(toy, 3)
#' @export
archetypes <- function(data, k, maxIterations=100, minImprovement=.Machine$double.eps,
                       maxKappa=1000, verbose=TRUE, saveHistory=TRUE, normfn=norm2.normfn,
                       scalefn=std.scalefn, rescalefn=std.rescalefn, dummyfn=make.dummyfn(200),
                       undummyfn=rm.undummyfn, initfn=make.random.initfn(1),
                       alphasfn=nnls.alphasfn, zalphasfn=ginv.zalphasfn,
                       betasfn=nnls.betasfn) {
  
  ### Helpers:
  mycall <- match.call()
  
  history <- NULL
  snapshot <- function(history, name, ...) {
    history[[paste('s', name, sep='')]] <- list(...)
  }


  ### Data preparation:
  x <- t(data)
  x <- scalefn(x)
  x <- dummyfn(x)

  n <- ncol(x)
  m <- nrow(x)


  ### Initialization:
  init <- initfn(x, k)
  
  betas <- init$betas
  alphas <- init$alphas

  zs <- x %*% betas
  rss <- normfn(zs %*% alphas - x) / n

  kappas <- c(alphas=kappa(alphas), betas=kappa(betas),
              zas=-Inf, zs=kappa(zs))
  ill <- c(kappas) > maxKappa
  
  if ( saveHistory ) {
    history <- new.env(parent=emptyenv())
    snapshot(history, 0,
             archetypes=as.archetypes(t(rescalefn(x, undummyfn(x, zs))),
               alphas=t(alphas), rss=rss, kappas=kappas))
  }

  
  ### Main loop:
  i <- 1
  imp <- +Inf
  
  while ( (i <= maxIterations) & (imp >= minImprovement) ) {
    
    ## Alpha's:
    alphas <- alphasfn(alphas, zs, x)
    zas <- zalphasfn(alphas, x)
    rss1 <- normfn(zas %*% alphas - x) / n

    kappas[c('alphas', 'zas')] <- c(kappa(alphas), kappa(zas))

    
    ## Beta's:
    betas <- betasfn(betas, x, zas)
    zs <- x %*% betas

    kappas[c('betas', 'zs')] <- c(kappa(betas), kappa(zs))

    
    ## RSS and improvement:
    rss2 <- normfn(zs %*% alphas - x) / n
    
    imp <- rss - rss2
    rss <- rss2

    kappas <- c(alphas=kappa(alphas), betas=kappa(betas),
                zas=kappa(zas), zs=kappa(zs))
    ill <- ill & (kappas > maxKappa)
    

    ## Loop Zeugs:
    if ( verbose )
      cat(i, ': rss = ', rss, ', improvement = ', imp, '\n', sep = '')
    
    if ( saveHistory )
      snapshot(history, i,
               archetypes=as.archetypes(t(rescalefn(x, undummyfn(x, zs))),
                 alphas=t(alphas), rss=rss, kappas=kappas))
    
    i <- i + 1
  }


  ### Check illness:
  if ( any(ill) )
    warning('k=', k, ': ', paste(names(ill)[ill], collapse=', '),
            ' > maxKappa', sep='')

  
  ### Rescale archetypes:
  zs <- undummyfn(x, zs)
  zs <- rescalefn(x, zs)
  zs <- t(zs)

  
  return(as.archetypes(zs, t(alphas), rss, iters=(i-1),
                       call=mycall, history=history, kappas=kappas))
}
