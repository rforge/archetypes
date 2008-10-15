

### Data scale and unscale functions:

std.scalefn <- function(x) {
  m = rowMeans(x)
  x = x - m

  s = apply(x, 1, sd)
  x = x / s

  attr(x, '.Meta') = list(mean = m, sd = s)

  return(x)
}

std.rescalefn <- function(x, zs) {

  m = attr(x, '.Meta')$mean
  s = attr(x, '.Meta')$sd

  zs = zs * s
  zs = zs + m

  return(zs)
}

no.scalefn <- function(x) {
  return(x)
}

no.rescalefn <- function(x, zs) {
  return(zs)
}



### Dummy and undummy functions:

make.dummyfn <- function(huge=200) {

	bp.dummyfn <- function(x) {
	  y = rbind(x, rep(huge, ncol(x))) 

	  attr(y, '.Meta') = attr(x, '.Meta')
	  attr(y, '.Meta')$dummyrow = nrow(y)
  
	  return(y)
	}


	return(bp.dummyfn)
}

rm.undummyfn <- function(x, zs) {
  dr = attr(x, '.Meta')$dummyrow
  
  return(zs[-dr,])
}

no.dummyfn <- function(x) {
  return(x)
}

no.undummyfn <- function(x, zs) {
  return(zs)
}



### `From X and alpha to archetypes` functions:

solve.zalphasfn <- function(alphas, x) {
  return(t(solve(alphas %*% t(alphas)) %*% alphas %*% t(x)))
}

ginv.zalphasfn <- function(alphas, x) {
  require(MASS)
  
  return(t(ginv(alphas %*% t(alphas)) %*% alphas %*% t(x)))
}

opt.zalphasfn <- function(alphas, x)
{
    z <- rnorm(nrow(x)*nrow(alphas))
               
    fun <- function(z){
        z <- matrix(z, ncol=nrow(alphas))
        sum( (x - z %*% alphas)^2)
    }

    z <- optim(z, fun, method="BFGS")
    z <- matrix(z$par, ncol=nrow(alphas))
    return(z)
}



### Alpha calculation functions:

nnls.alphasfn <- function(coefs, C, d) {
  require(nnls)
  
  n = ncol(d)
  
  for ( j in 1:n )
    coefs[,j] = coef(nnls(C, d[,j]))

  return(coefs)
}

snnls.alphasfn <- function(coefs, C, d) {
  require(nnls)

  n = ncol(d)

  nc = ncol(C)
  nr = nrow(C)
  

  s = svd(C, nv=nc)
  yint = t(s$u) %*% d

  for ( j in 1:n )
    coefs[,j] = coef(nnls(diag(s$d, nrow=nr, ncol=nc) %*% t(s$v), yint[,j]))

  return(coefs)
}



### Beta calculation functions:

nnls.betasfn <- nnls.alphasfn
  
snnls.betasfn <- snnls.alphasfn



### Norm functions:

norm2.normfn <- function(m) {
  ## the standard matrix norm (as defined in matlab)
  return(max(svd(m)$d))
}


euc.normfn <- function(m) {
  return(sum(apply(m, 2, function(x){sqrt(sum(x^2))})))
}


  
### Archetypes initialization functions:

make.random.initfn <- function(k) {

  bp.initfn <- function(x, p) {
  
    n = ncol(x)
    b = matrix(0, nrow = n, ncol = p)

    for ( i in 1:p )
      b[sample(n, k, replace = FALSE),i] = 1 / k
    
    a = matrix(1, nrow = p, ncol = n) / p
    
    return(list(betas = b, alphas = a))
  }

  return(bp.initfn)
}

make.fix.initfn <- function(indizes) {

  fix.initfn <- function(x, p) {
    n = ncol(x)
  
    b = matrix(0, nrow = n, ncol = p)
    b[indizes,] = diag(p)

    a = matrix(1, nrow = p, ncol = n) / p
  
    return(list(betas = b, alphas = a))
  }

  return(fix.initfn)
}


