#' Plotting the Delauney circumcircles for a set of points in 2D.
#'
#' Plots Delauney circumcircles in 2D
#'
#' @param points A matrix of points.
#' @param lims A matrix with two colums, corresponding to the upper
#' and lower limits of each dimension, respectively.
#' @param plot.delauney Logical, if \code{TRUE}, the delauney triangulation will also be plotted.
#' @param ... Graphical parameters (e.g., to be passed to
#' \link{par}().
#'
#' @method plot gapski
#'
#' @export
plot.gapski <- function(points=NULL,lims=NULL,plot.delauney=FALSE , ...){
   cent<-circumcircle(points=points,lims=lims)
   x<-cent[,1]
   y<-cent[,2]
   rad<-cent[,3]
   area<-cent[,4]
   symbols(x,y,circles=rad,fg="gray",bg="gray",xlim=lims[1,],ylim=lims[2,])
   points(points,pch="+",cex=2)
   if(plot.delauney){
       plot(deldir(points[,1],points[,2]),add=TRUE)
   }  
}

#' Plotting the empirical void probability function.
#'
#' Plots empirical void probability function
#'
#' @param points A matrix of points.
#' @param lims A matrix with two colums, corresponding to the upper
#' and lower limits of each dimension, respectively.
#' @param plot.poisson Logical, if \code{TRUE}, the void probability for a Poisson process will also be plotted.
#' @param ... Graphical parameters (e.g., to be passed to
#' \link{par}().
#'
