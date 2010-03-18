#' @include archetypes-kit-blocks.R
{}



### Class and Family:

.weighted.archetypesFamily <- function() {
  f <- .original.archetypesFamily()
  f$class <- 'weightedArchetypes'
  f$weightfn <- center.weightfn
  f
}

setOldClass('weightedArchetypes')



### Methods:

#' @importFrom modeltools parameters
setMethod('parameters', 'weightedArchetypes', parameters.archetypes)


