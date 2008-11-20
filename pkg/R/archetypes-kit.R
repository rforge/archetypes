

archetypes <- function(data, p, max.iterations=100, min.improvement=.Machine$double.eps,
                       max.kappa=1000, verbose=TRUE, save.history=TRUE,
                       normfn=norm2.normfn,
                       scalefn=std.scalefn,
                       rescalefn=std.rescalefn,
                       dummyfn=make.dummyfn(200),
                       undummyfn=rm.undummyfn,
                       initfn=make.random.initfn(1),
                       alphasfn=nnls.alphasfn,
                       zalphasfn=ginv.zalphasfn,
                       betasfn=nnls.betasfn) {

  ### Helpers:
  mycall <- match.call()
  
  history <- NULL
  snapshot <- function(history, name, ...) {
    history[[name]] <- list(...)
  }

  object <- function(archetypes, alphas, rss, iters=NULL, call=NULL,
                     history=NULL, kappas=NULL) {
    a <- list(archetypes=archetypes,
              alphas=alphas,
              rss=rss,
              iters=iters,
              kappas=kappas,
              call=call,
              history=history)
    class(a) <- 'archetypes'

    return(a)
  }
  

  ### Data preparation:
  x <- t(data)
  x <- scalefn(x)
  x <- dummyfn(x)

  n <- ncol(x)
  m <- nrow(x)


  ### Initialization:
  init <- initfn(x, p)
  
  betas <- init$betas
  alphas <- init$alphas

  zs <- x %*% betas
  rss <- normfn(zs %*% alphas - x) / n

  kappas <- c(alphas=kappa(alphas), betas=kappa(betas),
              zas=-Inf, zs=kappa(zs))
  ill <- c(kappas) > max.kappa
  
  if ( save.history ) {
    history <- new.env(parent=emptyenv())
    snapshot(history, 's0',
             archetypes=object(t(rescalefn(x, undummyfn(x, zs))),
               alphas=t(alphas), rss=rss, kappas=kappas))
  }

  
  ### Main loop:
  i <- 1
  imp <- +Inf
  
  while ( (i <= max.iterations) & (imp >= min.improvement) ) {
    
    ## Alpha's:
    alphas <- alphasfn(alphas, zs, x)
    zas <- zalphasfn(alphas, x)
    rss1 <- normfn(zas %*% alphas - x) / n

    kappas[c('alphas', 'zas')] <- c(kappa(alphas),
                                    kappa(zas))

    
    ## Beta's:
    betas <- betasfn(betas, x, zas)
    zs <- x %*% betas

    kappas[c('betas', 'zs')] <- c(kappa(betas),
                                  kappa(zs))

    
    ## RSS and improvement:
    rss2 <- normfn(zs %*% alphas - x) / n
    
    imp <- rss - rss2
    rss <- rss2

    kappas <- c(alphas=kappa(alphas), betas=kappa(betas),
                zas=kappa(zas), zs=kappa(zs))
    ill <- ill & (kappas > max.kappa)
    

    ## Loop Zeugs:
    if ( verbose )
      cat(i, ': rss = ', rss, ', improvement = ', imp, '\n', sep = '')
    
    if ( save.history )
      snapshot(history, paste('s', i, sep=''),
               archetypes=object(t(rescalefn(x, undummyfn(x, zs))),
                 alphas=t(alphas), rss=rss, kappas=kappas))
    
    i <- i + 1
  }


  ### Check illness:
  if ( any(ill) )
    warning('p=', p, ': ', paste(names(ill)[ill], collapse=', '),
            ' > max.kappa', sep='')

  
  ### Rescale archetypes:
  zs <- undummyfn(x, zs)
  zs <- rescalefn(x, zs)
  zs <- t(zs)

  
  return(object(zs, t(alphas), rss,
                iters=(i-1), call=mycall, history=history))
}



print.archetypes <- function(x, full=TRUE, ...) { 
  if ( full ) {
    cat('Archetypes\n\n')
    
    args <- as.list(x$call[-1])
    cat(paste(names(args), args, sep='=', collapse=', '), '\n\n')
  }
  
  cat('Convergence after', x$iters, 'iterations\n')
  cat('with RSS = ', rss(x), '.\n', sep='')
}



atypes <- function(zs, ...) {
  UseMethod('atypes')
}

atypes.archetypes <- function(zs, ...) {
  return(zs$archetypes)
}

ntypes <- function(zs, ...) {
  UseMethod('ntypes')
}

ntypes.archetypes <- function(zs, ...) {
  return(nrow(atypes(zs)))
}



rss <- function(zs, ...) {
  UseMethod('rss')
}

rss.archetypes <- function(zs, ...) {
  return(zs$rss)
}



adata <- function(zs, ...) {
  UseMethod('adata')
}

adata.archetypes <- function(zs) {
  return(t(t(zs$archetypes) %*% t(zs$alphas)))
}



ahistory <- function(zs, ...) {
  UseMethod('ahistory')
}

ahistory.archetypes <- function(zs, step) {
  if ( is.null(zs$history) )
    stop('No history available')

  if ( step >= 0 )
    s <- paste('s', step, sep='')
  else
    s <- paste('s', nhistory(zs) + step - 1, sep='')
  
  return(zs$history[[s]][[1]])
}


nhistory <- function(zs, ...) {
  UseMethod('nhistory')
}

nhistory.archetypes <- function(zs) {
  if ( is.null(zs$history) )
    stop('No history available')

  return(length(zs$history))
}
