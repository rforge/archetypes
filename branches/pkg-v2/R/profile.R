#' @include archetypes-class.R
{}



#' Archetypes profile
#'
#' @param fitted An \code{\link{archetypes}} object.
#' @param data The corresponding data set.
#' @param type The profile function; currently only percentiles are
#'   supported.
#'
#' @return A matrix (with class attribute \code{atypes_profile}) with
#'   the computed profile.
#'
#' @aliases profile-methods
#' @aliases profile,archetypes-method
#'
#' @examples
#'   \dontrun{
#'     data(toy)
#'     a <- archetypes(toy, 3)
#'     plot(profile(a, toy))
#'   }
#'
#' @usage
#'   \S4method{profile}{archetypes}(fitted, data, type = percentiles, ...)
#'
#' @importFrom stats profile
#' @exportMethod profile
setMethod("profile", signature = c(fitted = "archetypes"),
function(fitted, data, type = percentiles, ...) {
  stopifnot(!is.null(data))

  if ( is.na(rss(fitted)) )
    return(NULL)

  profile <- parameters(fitted)
  profile <- sapply(seq(length = ncol(data)),
                    function(i) percentiles(profile[, i], data[, i]))

  if ( !is.matrix(profile) ) {
    profile <- t(as.matrix(profile))
  }

  rownames(profile) <- sprintf("Archetype %s", seq(length = nrow(profile)))
  colnames(profile) <- colnames(data)

  class(profile) <- c("atypes_profile", class(profile))


  profile
})



percentiles <- function(x, data, digits = 0) {
  Fn <- ecdf(data)
  round(Fn(x) * 100, digits = digits)
}



#' @param height An \code{atypes_profile} object.
#' @param ... Ignored.
#' @rdname profile
#' @method barplot atypes_profile
#' @S3method barplot atypes_profile
barplot.atypes_profile <- function(height, ...) {
  p <- ggplot(melt(height), aes(X2, value))
  p <- p + geom_bar(stat = "identity") + facet_grid(X1 ~ .)
  p <- p + ylim(c(0, 100)) + xlab("Variable") + ylab("Percentile")
  p
}



#' @param x An \code{atypes_profile} object.
#' @param y Ignored.
#' @rdname profile
#' @method plot atypes_profile
#' @S3method plot atypes_profile
plot.atypes_profile <- function(x, y = NULL, ...) {
  barplot.atypes_profile(x, ...)
}
