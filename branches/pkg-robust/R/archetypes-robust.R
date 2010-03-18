#' @include archetypes-kit-blocks.R
{}



### Class and Family:

.robust.archetypesFamily <- function() {
  f <- .original.archetypesFamily()
  f$class <- 'robustArchetypes'
  f$weightfn <- center.weightfn
  f$reweightsfn <- bisquare0.reweightsfn
  f
}

setOldClass('robustArchetypes')



### Methods:

#' @importFrom modeltools parameters
setMethod('parameters', 'robustArchetypes', parameters.archetypes)

