#' Plotting the either the Delauney circumcircles for a set of points in 2D, or a gapski model fit.
#'
#' Plots Delauney circumcircles in 2D
#'
#' @param points A matrix of points or a fitted model from \link{fit.gap}.
#' @param lims A matrix with two colums, corresponding to the upper
#' and lower limits of each dimension, respectively.
#' @param plot.delauney Logical, if \code{TRUE}, the delauney triangulation will also be plotted.
#' @param ... Other parameters (for S3 generic compatibility).
#'
#' @inheritParams coef.gapski
#' @method plot gapski
#'
#' @export
plot.gapski <- function(object=NULL,lims=rbind(c(0,1),c(0,1)),plot.delauney=FALSE , ...){
    if(class(object)=="gapski"){
        parm <- 1:3
    coefs <- coef(object)[parm]
    xx <- seq(0,0.5,length.out=100)
        dims<-ncol(object$args$lims)
    plot(xx,palm.intensity(xx,coefs[3],coefs[2],coefs[1],dims),type="l")
    }
    if(class(object)=="matrix"){
        if (ncol(object) != 2){
        stop("Plotting circumcircles only implemented for 2D")
    }
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
    }



