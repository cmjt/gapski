#' Identifies the "likely" voids via Delauney circumcircles of a  gapski model fit.
#'
#' @param object A fitted model from \link{fit.gap}.
#' @param plot.delauney Logical, if \code{TRUE}, the delauney triangulation will also be plotted.
#' @param ... Other parameters (for S3 generic compatibility).
#'
#' @inheritParams coef.gapski

#' @export
voids.gap <- function(object=NULL,plot.delauney=FALSE , ...){
     parm <- 1:3
     coefs <- object$pars[parm]
     points <- object$args$points
     lims <- object$args$lims
     if (ncol(lims) != 2){
        stop("Plotting circumcircles only implemented for 2D")}
     cent<-circumcircle(points=points,lims=lims)
     x<-cent[,1]
     y<-cent[,2]
     rad<-cent[,3]
     area<-cent[,4]
     symbols(x,y,circles=rad,fg="black",bg="gray",asp=1,inches=FALSE,xlim=lims[1,],ylim=lims[2,],lty=2)
     points(points,pch="+",cex=2)
     if(plot.delauney){
         plot(deldir(points[,1],points[,2]),add=TRUE)
     }
}



