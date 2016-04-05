#' Identifies the "likely" voids via Delauney circumcircles of a  gapski model fit.
#'
#' @param object A fitted model from \link{fit.gap}.
#' @param plot.delauney Logical, if \code{TRUE}, the delauney triangulation will also be plotted.
#' @param ... Other parameters (for S3 generic compatibility).
#'
#' @inheritParams coef.gapski

#' @export
voids.gap <- function(object=NULL,plot.delauney=FALSE ,circumcirc=FALSE,...){
     parm <- 1:3
     coefs <- object$pars[parm]
     points <- object$args$points
     lims <- object$args$lims
     if (ncol(lims) != 2){
        stop("Plotting circumcircles only implemented for 2D")}
     cent<-circumcircle(points=points,lims=lims)
     r <- data.frame(x=cent[,1],y=cent[,2],radius=cent[,3],area=cent[,4])
     r<-r[r[,3]>=coefs[1],]
     dists<-rdist(r[,1:2])
     diag(dists)<-coefs[1]+1
     #r<- r[apply(dists,1,min)>=coefs[1]/2,]
     r<-r[order(-r[,3]),]
     r<-r[1:(2*round(coefs[2])),]
     if(circumcirc){
         colnames(r)<-c("x","y","radius","area")
         return(r)
     }else{ symbols(r[,1],r[,2],circles=r[,3],fg="black",bg="gray",asp=1,inches=FALSE,xlim=lims[1,],ylim=lims[2,],lty=2,xlab="",ylab="",xaxt="n",yaxt="n")
         box()
         points(points,pch="+",cex=2)}
     if(plot.delauney){
         plot(deldir(points[,1],points[,2]),add=TRUE)
     }
}



