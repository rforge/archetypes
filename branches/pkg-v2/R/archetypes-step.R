#' @include archetypes-class.R
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
#' @param show.progress Show progress during exection.
#'
#' @return A list with \code{length(k)} elements and class attribute
#'   \code{stepArchetypes}.
#'
#' @family archetypes
#' @seealso \code{\link{bestModel}}
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
stepArchetypes <- function(..., k, nrep = 3, method = archetypes, show.progress = TRUE) {
  stopifnot(nrep > 0)

  print_msg <- function(...) {
    if ( show.progress ) {
      cat(paste(...))
    }
  }

  as <- list()
  as$call <- match.call(expand.dots = TRUE)
  as$nrep <- nrep
  as$k <- k
  as$models <- list()

  for ( i in seq(along = k) ) {
    print_msg("k = ", i, ": ", sep = "")

    as$models[[i]] <- step()
    for ( j in seq(length = nrep) ) {
      print_msg("*")
      as$models[[i]][[j]] <- method(..., k = k[i])
    }

    print_msg("\n")
  }

  subclass(as, "stepArchetypes")
}



setOldClass("stepArchetypes")



#' @S3method print stepArchetypes
print.stepArchetypes <- function(x, ...) {
  cat("stepArchetypes object\n\n")
  cat(deparse(x$call), "\n\n")
  cat("Residual sum of squares:\n")
  print(round(rss(x), 2))
}



#' @rdname rss
#' @method rss stepArchetypes
#'
#' @S3method rss stepArchetypes
rss.stepArchetypes <- function(object, ...) {
  ret <- lapply(object$models, rss)
  ret <- do.call(rbind, ret)
  ret <- data.frame(Archetypes = nparameters(object), ret)
  subclass(ret, "stepArchetypes_rss")
}



#' @param x A \code{stepArchetypes_rss} object
#' @param y Ignored.
#' @rdname rss
#' @method plot stepArchetypes
#'
#' @S3method plot stepArchetypes
plot.stepArchetypes_rss <- function(x, y = NULL, ...) {
  p <- ggplot(melt(x, "Archetypes"),
              aes(ordered(Archetypes), value, group = variable,
                  colour = variable))
  p <- p + geom_line()
  p <- p + geom_point()
  p <- p + xlab("Number of archetypes") + ylab("RSS")
  p
}



#' Return best model per step
#'
#' @param object An \code{archetypes} object.
#' @param reduced Reduce list to valid objects; i.e., remove step if
#'   no replication was successfull.
#' @param ... Ignored
#'
#' @rdname bestModels
#' @method bestModels stepArchetypes
#'
#' @S3method bestModels stepArchetypes
bestModels.stepArchetypes <- function(object, reduced = TRUE, ...) {
  best <- lapply(object$models, bestModel, reduced = reduced)

  if ( reduced )
    best <- best[sapply(best, Negate(is.null))]

  object$models <- best
  object$k <- sapply(best, sapply, "[[", "k")
  object$nrep <- 1
  object
}



#' @aliases getModel,stepArchetypes-method
#' @rdname stepArchetypes
#' @importFrom modeltools getModel
#' @exportMethod getModel
setMethod("getModel", signature = c(object = "stepArchetypes"),
function(object, k, nrep = 1) {
  where <- which(sapply(object$models, nparameters) == k)
  object$models[[where]][[nrep]]
})



#' @aliases parameters,stepArchetypes-method
#' @rdname parameters
#' @importFrom modeltools parameters
#' @exportMethod parameters
setMethod('parameters', signature = c(object = 'stepArchetypes'),
function(object, ...) {
  subclass(lapply(object$models, parameters), "stepArchetypes_parameters")
})



#' @param transpose Transpose plots arrangement.
#' @rdname parameters
#' @method plot stepArchetypes_parameters
#' @S3method plot stepArchetypes_parameters
plot.stepArchetypes_parameters <- function(x, y = NULL, transpose = FALSE, ...) {
  params_plot(x, transpose)
}



#' @aliases profile,stepArchetypes-method
#' @rdname profile
#' @importFrom stats profile
#' @exportMethod profile
setMethod('profile', signature = c(fitted = 'stepArchetypes'),
function(fitted, data, type = percentiles, ...) {
  subclass(lapply(fitted$models, profile, data, type), "stepArchetypes_profile")
})



#' @param transpose Transpose plots arrangement.
#' @rdname profile
#' @method plot stepArchetypes_profile
#' @S3method plot stepArchetypes_profile
plot.stepArchetypes_profile <- function(x, y = NULL, transpose = FALSE, ...) {
  params_plot(x, transpose)
}



#' @rdname nparameters
#' @method nparameters stepArchetypes
#'
#' @S3method nparameters stepArchetypes
nparameters.stepArchetypes <- function(object, ...) {
  return(sapply(object$model, nparameters))
}



### Step utility functions: ##########################################

setOldClass(c("step", "list"))

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

bestModel.step <- function(object, reduced = TRUE, ...) {
  which <- which.min(rss(object))

  if ( length(which) == 0 )
    if ( reduced )
      return(NULL)
    else
      which <- 1

  subclass(list(object[[which]]), "step")
}

setMethod('parameters', signature = c(object = 'step'),
function(object, ...) {
  lapply(object, parameters)
})

setMethod('profile', signature = c(fitted = 'step'),
function(fitted, data, type = percentiles, ...) {
 lapply(fitted, profile, data, type)
})
