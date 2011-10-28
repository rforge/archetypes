#' @include archetypes-class.R
#' @include archetypes-rep.R
{}


#' Run archetypes algorithm repeatedly
#'
#' Run archetypes algorithm repeatedly for different numbers of
#' archetypes. One step is defined by the number of archetypes
#' \code{k} and the number of replications \code{nrep}.
#'
#' @param ... Passed to the specific archetype function.
#' @param k A vector of integers passed in turn to the k argument of
#'   \code{\link{archetypes}}.
#' @param nrep For each value of \code{k} run \code{\link{archetypes}}
#'   \code{nrep} times.
#' @param method Archetypes function to use, typically
#'   \code{\link{archetypes}}, \code{\link{weightedArchetypes}} or
#'   \code{\link{robustArchetypes}},
#' @param verbose Show progress during exection.
#'
#' @return A list with \code{length(k)} elements and class attribute
#'   \code{stepArchetypes}.
#'
#' @family archetypes
#'
#' @examples
#'   \dontrun{
#'   data(skel)
#'   skel2 <- subset(skel, select=-Gender)
#'   as <- stepArchetypes(skel2, k=1:5, verbose=FALSE)
#'
#'   ## Residual sum of squares curve:
#'   screeplot(as)
#'
#'   ## Select three archetypes and from that the best
#'   ## recurrence:
#'   a3 <- bestModel(as[[3]])
#'   }
#'
#' @export
stepArchetypes <- function(..., k, nrep = 3, method = archetypes, verbose = TRUE) {
  stopifnot(nrep > 0)

  as <- list()
  as$call <- match.call()
  as$nrep <- nrep
  as$k <- k
  as$models <- list()

  for ( i in seq(along = k) ) {
    as$models[[i]] <- step()
    for ( j in seq(length = nrep) ) {
      as$models[[i]][[j]] <- method(..., k = k[i], verbose = verbose)
    }
  }

  subclass(as, "stepArchetypes")
}



setOldClass("stepArchetypes")



#' @S3method print stepArchetypes
print.stepArchetypes <- function(x, ...) {
  cat('stepArchetypes object\n\n')
  cat(deparse(attr(x, 'call')), '\n')
}



#' Summary method for stepArchetypes object
#'
#' @param object A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return Undefined.
#'
#' @method summary stepArchetypes
#' @rdname summary
#'
#' @S3method summary stepArchetypes
summary.stepArchetypes <- function(object, ...) {
  print(object)

  ps <- nparameters(object)

  for ( i in seq_along(object) ) {
    cat('\nk=', ps[i], ':\n', sep='')
    print(object[[i]], full=FALSE)
  }
}



#' Extract method
#'
#' An extraction on a \code{stepArchetypes} object returns again a
#' \code{stepArchetypes} object.
#'
#' @param x A \code{stepArchetypes} object.
#' @param i The indizes to extract.
#' @return A \code{stepArchetypes} object containing only the parts
#'   defined in \code{i}.
#' @method [ stepArchetypes
#' @rdname extract
#'
#' @S3method "[" stepArchetypes
`[.stepArchetypes` <- function(x, i) {
  y <- unclass(x)[i]
  attributes(y) <- attributes(x)

  return(y)
}



#' @rdname rss
#' @method rss stepArchetypes
#'
#' @S3method rss stepArchetypes
rss.stepArchetypes <- function(object, ...) {
  ret <- lapply(object$models, rss)
  ret <- do.call(rbind, ret)
  ret <- data.frame(Archetypes = nparameters(as), ret)
  subclass(ret, "stepArchetypes_rss")
}



plot.stepArchetypes_rss <- function(x, y = NULL, ...) {
  p <- ggplot(melt(x, "Archetypes"),
              aes(ordered(Archetypes), value, group = variable))
  p <- p + geom_line()
  p <- p + xlab("Number of archetypes") + ylab("RSS")
  p
}



#' Return best model
#'
#' @param object An \code{archetypes} object.
#' @param ... Ignored
#'
#' @rdname bestModel
#' @method bestModel stepArchetypes
#'
#' @S3method bestModel stepArchetypes
bestModel.stepArchetypes <- function(object, ...) {
  object$models <- lapply(object$models, bestModel)
  object$nrep <- 1
  object
}



#' @aliases parameters,stepArchetypes-method
#' @rdname parameters
#' @importFrom modeltools parameters
#' @exportMethod parameters
setMethod('parameters', signature = c(object = 'stepArchetypes'),
function(object, ...) {
  subclass(lapply(object, parameters), "stepArchetypes_parameters")
})



#' @rdname nparameters
#' @method nparameters stepArchetypes
#'
#' @S3method nparameters stepArchetypes
nparameters.stepArchetypes <- function(object, ...) {
  return(sapply(object$model, nparameters))
}




step <- function() {
  structure(list(), class = c("step", "list"))
}

rss.step <- function(object, ...) {
  rss <- sapply(object, rss)
  names(rss) <- sprintf("Replication%s", seq(along = rss))
  rss
}

nparameters.step <- function(object, ...) {
  sapply(object, nparameters)[1]
}

bestModel.step <- function(object, ...) {
  which <- which.min(rss(object))
  subclass(list(object[[which]]), "step")
}

