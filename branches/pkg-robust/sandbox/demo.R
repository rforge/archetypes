
library(archetypes.dev.robust)

library(modeltools)
sapply(list.files('../R', full = TRUE), source, echo = TRUE)

load('../data/toy.RData')




### Archetypes with one outlier; breakdown point simulations  ########

data(toy)


### Move one data point off the data cloud:

plot(toy, pch = 21, col = 1, bg = c(rep(0, 183), 2, rep(0, 66)))

out.start <- toy[184, ]
out.end <- c(0, 30)

p184 <- list()
a184 <- list()

set.seed(1234)
for ( i in 1:10 ) {
  p184[[i]] <- out.end - (i/10) * (out.end - out.start)
  toy[184, ] <- p184[[i]]

  a184[[i]] <- archetypes(toy, 3)
}

save(p184, a184, file = 'a184.RData')
load(file = 'a184.RData')


plot(rbind(toy, do.call(rbind, p184)), pch = 21, col = 1,
     bg = rep(c(0, 2), c(250, 10)))
for ( a in a184 ) {
  points(parameters(a), col = gray(0.7))
  lines(ahull(a), col = gray(0.7))
}

# => For a given k, breakdown point 0. This means we can make the
#    corresponding archetype arbitrarily "large" just by changing
#    any of the data points.



### One region with outliers: ########################################

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



### Original archetypes:

set.seed(1234)
a1 <- archetypes(toy.o1, 3)

plot(a1, toy.o1)
plot(a1, toy.o1, adata.show = TRUE)

residuals.diagplot(a1)
rss.diagplot(a1)



### Weighted archetypes:

w <- rep(c(1, 0), c(250, 5))

set.seed(1234)
wa1 <- archetypes(toy.o1, 3, family = archetypesFamily('weighted'), weights = w)

plot(wa1, toy.o1)
plot(wa1, toy.o1, adata.show = TRUE)



### Robust archetypes:

set.seed(1234)
ra1 <- archetypes(toy.o1, 3, family = archetypesFamily('robust'))

plot(ra1, toy.o1, adata.show = TRUE)

residuals.diagplot(ra1)
rss.diagplot(ra1)
weights.diagplot(ra1, weights.type = 'reweights')

par(mfrow = c(6, 7), mar = c(0, 0, 0, 0))
movieplot(ra1, toy.o1, adata.show = TRUE, link.col.show = FALSE,
          link.lty = 0, axes = FALSE, postfn = function(iter) box())

# => looks like it does the right thing, but ...

set.seed(1235)
ra2 <- archetypes(toy.o1, 3, family = archetypesFamily('robust'))

plot(ra2, toy.o1, adata.show = TRUE)

weights.diagplot(ra2, weights.type = 'reweights')
rss.diagplot(ra2)

par(mfrow = c(1, 6), mar = c(0, 0, 0, 0))
movieplot(ra2, toy.o1, adata.show = TRUE, link.col.show = FALSE,
          link.lty = 0, axes = FALSE, postfn = function(iter) box())

# => hmm ... it really depends on the starting values. What about
#    the weights?

reweights.diagplot(ra1)
reweights.diagplot(ra2)

reweights.diagplot(ra1, highlight = c(1, 2))
reweights.curve.diagplot(ra1, c(1, 2))

reweights.liftoff.diagplot(ra1)
reweights.liftoff.diagplot(ra2)

reweights.rss.diagplot(ra1)
reweights.rss.diagplot(ra2)



### Robust archetypes on a data set without outliers:

set.seed(1234)
a2 <- archetypes(toy, 3, family = archetypesFamily('robust'))

plot(a2, toy, adata.show = TRUE)

par(mfrow = c(6, 6), mar = c(0, 0, 0, 0))
movieplot(a2, toy, adata.show = TRUE, link.col.show = FALSE,
          link.lty = 0, axes = FALSE, postfn = function(iter) box())

reweights.diagplot(a2)
reweights.curve.diagplot(a2, 1:50)

# => Hmm ...

set.seed(1234)
a3 <- stepArchetypes(toy, family = archetypesFamily('robust'), k = 3, nrep = 5)

plot(a3, toy)

# => Not the intended behavior!



### So, as always, it is not the easy solution ... ###################

set.seed(1235)
ra2 <- archetypes(toy.o1, 3, family = archetypesFamily('robust',
                             reweightsfn = bisquare.reweightsfn))

plot(ra2, toy.o1, adata.show = TRUE)

weights.diagplot(ra2, weights.type = 'reweights')
rss.diagplot(ra2)

par(mfrow = c(2, 5), mar = c(0, 0, 0, 0))
movieplot(ra2, toy.o1, adata.show = TRUE, link.col.show = FALSE,
          link.lty = 0, axes = FALSE, postfn = function(iter) box())

reweights.rss.diagplot(ra2)
