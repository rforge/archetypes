
pcplot.archetypes <- function(zs, data, var.label=TRUE,
                              data.col=gray(0.7), data.lwd=1,
                              atypes.col=2, atypes.lwd=2, atypes.lty=1,
                              chull=NULL, chull.col=1, chull.lwd=2, chull.lty=1, ...) {

  pcplot(data, var.label=var.label,
         col=data.col, lwd=data.lwd, ...)

  if ( !is.null(chull) )
    lines.pcplot(data[chull,], data,
                 col=chull.col, lwd=chull.lwd, lty=chull.lty, ...)
  
  lines.pcplot(atypes(zs), data,
               col=atypes.col, lwd=atypes.lwd, lty=atypes.lty, ...)
}

