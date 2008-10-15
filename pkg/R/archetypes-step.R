
stepArchetypes <- function(..., p, r=3, verbose=TRUE) {

  mycall <- match.call()
  as <- list()
  
  for ( i in 1:length(p) ) { 
    as[[i]] <- list()
    class(as[[i]]) <- 'repArchetypes'
    
    for ( j in seq_len(r) ) {
      if ( verbose )
        cat('\n*** p=', p[i], ', r=', j, ':\n', sep='')
      
      as[[i]][[j]] <- archetypes(..., p=p[i], verbose=verbose)
    }
  }

  return(structure(as, class='stepArchetypes', call=mycall))
}

print.stepArchetypes <- function(x, ...) {
  cat('StepArchetypes\n\n')
 
  thecall <- attr(x, 'call')
  args <- as.list(thecall)[-1]
  cat(paste(names(args), args, sep='=', collapse=', '), '\n')  
}

summary.stepArchetypes <- function(x, ...) {
  print(x)
  
  ps <- ntypes(x)

  for ( i in seq_along(x) ) {
    cat('\np=', ps[i], ':\n', sep='')
    print(x[[i]], full=FALSE)
  }
}

print.repArchetypes <- function(x, ...) {
  for ( i in seq_along(x) )
    print(x[[i]], ...)
}


atypes.repArchetypes <- function(zs, ...) {
  ret <- lapply(zs, atypes)
  return(ret)
}

rss.repArchetypes <- function(zs, ...) {
  ret <- sapply(zs, rss)
  names(ret) <- paste('r', seq_along(ret), sep='')

  return(ret)
}

ntypes.repArchetypes <- function(zs, ...) {
  return(nrow(atypes(zs[[1]])))
}

bestModel <- function(zs, ...) {
  UseMethod('bestModel')
}

bestModel.repArchetypes <- function(zs) {
  return(zs[[which.min(rss(zs))]])
}



atypes.stepArchetypes <- function(zs, simplify=TRUE, ...) {
  ret <- lapply(zs, atypes)
  
  return(ret)
}

rss.stepArchetypes <- function(zs, ...) {
  ret <- t(sapply(zs, rss))
  rownames(ret) <- paste('p', ntypes(zs), sep='')
  return(ret)
}

ntypes.stepArchetypes <- function(zs, ...) {
  return(sapply(zs, ntypes))
}

bestModel.stepArchetypes <- function(zs) {
  zsmin <- lapply(zs, bestModel)

  if ( length(zsmin) == 1 )
    return(zsmin[[1]])
  else
    return(zsmin)
}

