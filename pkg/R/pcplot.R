
pcplot <- function(x, ...) {
  UseMethod('pcplot')
}


pcplot.default <- function (x, col=gray(0.7), lty=1, var.label=TRUE,
                            rx=NULL, ...) {

  calcrx <- TRUE
  if ( is.null(rx) ) {
    rx <- apply(x, 2, range, na.rm=TRUE)
  }
  else {
    x <- rbind(rx, x)
    calcrx <- FALSE
  }

  sx <- sapply(1:ncol(x),
               function(i) {
                 (x[,i] - rx[1,i]) / (rx[2,i] - rx[1,i])
               })
  colnames(sx) <- colnames(x)

  x <- sx
  
  matplot(1:ncol(x), t(x), type="l", col=col, lty=lty, 
          xlab="", ylab="", axes=FALSE, ...)
  axis(1, at=1:ncol(x), labels=colnames(x), ...)
   
  for (i in 1:ncol(x)) {
    lines(c(i, i), c(0, 1), col="grey70")
    if (var.label) 
      text(c(i, i), c(0, 1), labels=format(rx[, i], digits=3), 
           xpd=NA, offset=0.3, pos=c(1, 3), cex=0.7)
  }
}


lines.pcplot <- function(x, data, col=1, lty=1, ...) {
  rx <- apply(data, 2, range, na.rm=TRUE)

  x <- sapply(1:ncol(x),
              function(i) {
                (x[,i] - rx[1,i]) / (rx[2,i] - rx[1,i])
              })
  
  matlines(1:ncol(x), t(x), col=col, lty=lty, ...)
}
