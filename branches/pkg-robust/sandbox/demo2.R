
#library(archetypes.dev.robust)

library(flexclust)
library(modeltools)
sapply(list.files('../R', full = TRUE), source, echo = TRUE)



### Data:

load('../data/toy.RData')

toy.outlier <- function(n, mean, sigma, ...) {
  require(mvtnorm)
  data(toy, package = 'archetypes')

  for ( i in seq(length = length(n)) )
    toy <- rbind(toy, rmvnorm(n[i], mean[[i]], sigma[[i]]))

  toy
}


set.seed(1234)
toy.o1 <- toy.outlier(5, mean = list(c(0, 30)),
                      sigma = list(diag(0.5, 2)))

plot(toy.o1)



### Robust archetypes on a data set with outliers:

set.seed(1234)
ra1 <- archetypes(toy.o1, 3, family = archetypesFamily('robust'))

plot(ra1, toy.o1, adata.show = TRUE)

weights(ra1, type = 'reweights')


## Stability:

set.seed(1235)
sra1 <- stepArchetypes(toy.o1, family = archetypesFamily('robust', reweightsfn = bisquare0.reweightsfn),
                       k = 3, nrep = 5)

plot(sra1, toy.o1)



### Robust archetypes on a data set without outliers:

set.seed(1235)
a2 <- archetypes(toy, 3, family = archetypesFamily('robust'))

plot(a2, toy, adata.show = TRUE)

weights(a2, type = 'reweights')


## Stability:

set.seed(1235)
sa1 <- stepArchetypes(toy, family = archetypesFamily('robust'),
                      k = 3, nrep = 5)

plot(sa1, toy)



### Bisquare0 reweights function:

## ... data set with outliers:

opar <- par(mfrow = c(5, 3), mar = c(0, 0, 0, 0))
movieplot(ra1, toy.o1, show = 'atypes', adata.show = TRUE,
          link.col.show = FALSE, link.lty = 0, axes = FALSE,
          postfn = function(iter) box())
par(opar)


opar <- par(mfrow = c(5, 3), mar = c(0, 0, 0, 0))
movieplot(ra1, toy.o1, show = 'rwdata', axes = FALSE,
          postfn = function(iter) box())
par(opar)



## ... data set without outliers:

opar <- par(mfrow = c(5, 5), mar = c(0, 0, 0, 0))
movieplot(a2, toy, show = 'atypes', adata.show = TRUE,
          link.col.show = FALSE, link.lty = 0, axes = FALSE,
          postfn = function(iter) box())
par(opar)


par(mfrow = c(5, 5), mar = c(0, 0, 0, 0))
movieplot(a2, toy, show = 'rwdata', axes = FALSE,
          postfn = function(iter) box())
par(opar)



### Binary bisquare0 reweights function:

set.seed(1235)
ra2 <- archetypes(toy.o1, 3, family = archetypesFamily('robust',
                             reweightsfn = binary.bisquare0.reweightsfn))

plot(ra2, toy.o1, adata.show = TRUE)

weights(ra2, type = 'reweights')


par(mfrow = c(2, 5), mar = c(0, 0, 0, 0))
movieplot(ra2, toy.o1, show = 'rwdata', axes = FALSE,
          postfn = function(iter) box())
par(opar)



### Compared against leastwise tricube reweightsfn on a
### data set without outliers:

set.seed(1234)
a3 <- archetypes(toy, 3, family = archetypesFamily('robust',
                         reweightsfn = leastwise.tricube.reweightsfn))
plot(a3, toy)

weights(a3, type = 'reweights')


par(mfrow = c(5, 4), mar = c(0, 0, 0, 0))
movieplot(a3, toy, show = 'atypes', adata.show = TRUE,
          link.col.show = FALSE, link.lty = 0, axes = FALSE,
          postfn = function(iter) box())
par(opar)


par(mfrow = c(5, 4), mar = c(0, 0, 0, 0))
movieplot(a3, toy, show = 'rwdata', axes = FALSE,
          postfn = function(iter) box())
par(opar)



### Archetypes' alpine world:

## Panorma view in the case of the original algorithm:
set.seed(1234)
a4 <- archetypes(toy.o1, 3)

plot(a4, toy.o1)

panorama(a4, toy.o1)
panorama(a4, toy, ref.order = 1)


## Panorma view in the case of the robust algorithm:
panorama(a2, toy)
panorama(a2, toy, ref.order = 1)

par(mfrow = c(3, 1))
plot(alphas(a4)[, 1])
plot(alphas(a4)[, 2])
plot(alphas(a4)[, 3])


## A strange idea:
archetypes.distance.diagplot(a2, toy)



