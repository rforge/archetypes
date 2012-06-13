
# @export
archmap <- function(object, cex = 1.5, rotate = 0, col = 1, pch = 1,
                    xlab = "", ylab = "", axes = FALSE, asp = TRUE,
                    space = alpha_space, ...) {
    k <- object$k
    if(k<3) stop("Need at least 3 archetypes.\n")
    cmds <- space(dist(parameters(object)))

    if(rotate!=0){
        a <- pi*rotate/180
        A <- matrix(c(cos(a), -sin(a), sin(a),
                      cos(a)), ncol=2)
        cmds <- cmds %*% A
    }
    hmds <- chull(cmds)
    active <- 1:k %in% hmds

    ## archetypes give border of plotting region
    plot(cmds, type="n", xlab=xlab, ylab=ylab, axes=axes, asp=asp, ...)
    points(coef(object) %*% cmds, col=col, pch=pch)

    rad <- ceiling(log10(k)) + 1.5
    polygon(cmds[hmds,])
    points(cmds[active,], pch=21, cex=rad*cex, bg="grey")
    text(cmds[active,], labels=(1:k)[active], cex=cex)
    if(any(!active)){
        points(cmds[!active,,drop=FALSE], pch=21, cex=rad*cex,
               bg="white", fg="grey")
        text(cmds[!active,,drop=FALSE], labels=(1:k)[!active],
             cex=cex, col="grey20")
    }

    invisible(cmds)
}



# @export
alpha_space <- function(x, r = 10) {
  n <- nrow(as.matrix(x))

  phi <- seq(-pi, pi, length.out = n + 1)

  x <- r * cos(phi)
  y <- r * sin(phi)

  m <- cbind(x, y)

  m[-1, ]
}



# @export
atypes_space <- function(x) {
  cmdscale(x)
}
