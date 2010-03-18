
#library(archetypes.dev.robust)

library(flexclust)
library(modeltools)
sapply(list.files('../R', full = TRUE), source, echo = TRUE)



### Ozone data:

data('Ozone', package = 'mlbench')

oz <- Ozone[, -c(1, 2, 3, 9)]
oz <- na.omit(oz)
colnames(oz) <- c('OZONE', '500MH', 'WDSP', 'HMDTY', 'STMP',
                  'INVHT', 'PRGRT', 'INVTMP', 'VZBLTY')

oz0 <- scale(oz)



### The three original archetypes:

set.seed(1234)
a.oz <- archetypes(oz0, 3)

barplot(a.oz, oz0, percentiles = TRUE)

panorama(a.oz, oz0)
panorama(a.oz, oz0, ref.order = 1)



### The robust archetypes:

set.seed(1236)
ra.oz <- archetypes(oz0, 3, family = archetypesFamily('robust'))

weights(ra.oz, type = 'reweights')

barplot(ra.oz, oz0, percentiles = TRUE)



### Data set with outliers:

outliers <- t(sapply(runif(5, min = 1.5, max = 2),
                     function(x)
                     x * apply(oz, 2, max) + apply(oz, 2, IQR)))

oz1 <- scale(rbind(oz, outliers))



### Original archetypes:

set.seed(1234)
a.oz1 <- archetypes(oz1, 3)

barplot(a.oz, oz1, percentiles = TRUE)

panorama(a.oz1, oz1)



### Robust archetypes:

set.seed(1236)
ra.oz1 <- archetypes(oz1, 3, family = archetypesFamily('robust'))

weights(ra.oz1, type = 'reweights')

barplot(ra.oz1, oz1, percentiles = TRUE)

panorama(ra.oz1, oz1)



## Compared to the original archetypes:

parameters(a.oz)
parameters(ra.oz1)

barplot(ra.oz1, oz1, percentiles = TRUE)
x11(); barplot(a.oz, oz0, percentiles = TRUE)

