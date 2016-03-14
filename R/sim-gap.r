#' Simulating point patterns with gaps
#'
#' Simulates point locations from a point process with deletion areas.
#'
#' The \code{rchild} function may only take a single distributional
#' parameter. If the distribution for the number of children generated
#' by each parent is Poisson, then the native \code{rpois} is
#' appropriate, as this distribution has a single parameter. For
#' distributions with two or more parameters, those other than
#' \code{child.par} must be hard-coded into \code{rchild}. For
#' example, if a Binomial(n, 2, p) is required, then \code{function(n,
#' p) rbinom(n = n, size = 2, prob = p)} would be an appropriate
#' function for \code{rchild}.
#'
#' @return A matrix containing simulated point locations.
#' @param pars A named vector of parameter values. Required
#' parameters are \code{D}, the density of gaps in the pattern;
#' \code{lambda} the density of all points in the domain, before deletion, the parameter \code{R} the radius of the spherical gaps in the pattern.
#' @param d a numeric value specifying the number of dimensions the point process is to be simulated in, by default this is 2.
#' @param lims An argument specifying the limits of the domain, for 1D this is a vector, for 2D or greater. By deafault this specifies the unit square.
#' @param plot.points Logical, if \code{TRUE}, simulated aunts and niece
#' point locations will be plotted (only for 2D).
#' @param plot.empirical Logical, if \code{TRUE}, the empirical Palm
#' intensity is plotted, taken from the \code{nspp} package 
#' @param return.parents Logical, if \code{TRUE}, a named list of matricies is returned
#' of both the observed points and aunt locations

#' @export
sim.gap <- function(pars = NULL, d = 2 , lims = rbind(c(0, 1), c(0, 1)),plot.points=FALSE,plot.empirical=FALSE,return.parents=FALSE){
    ## Allowing lims to be a vector if only one dimension.
    if (!is.matrix(lims)){
        lims <- matrix(lims, nrow = 1)
    }
    ## Parameter values.
    D <- pars["D"]
    lambda <- pars["lambda"]
    R <- pars["R"]
    ## Number of dimensions.
    dims <- d
    ## Calculating survey area.
    area <- prod(apply(lims, 1, diff))
    #Generating number of original points and their locations
    expected.n.points = area*lambda
    n.points = rpois(n =1 ,lambda=expected.n.points)
    points <- matrix(runif(dims*n.points), ncol = dims)
     if(nrow(points)==0){
        stop("No points generated")
    }
    ## Generating number of parents and their locations
    n.parents <- rpois(1, D)
    parents <- matrix(runif(dims*n.parents), ncol = dims)
    if(nrow(parents)==0){
        stop("No monsters generated")
    }
    ## If any parents exist, delete nearby points (with distances
    ## subject to periodic boundary conditions - this is taken directly from Ben Stevenson's nspp package.).
    if (n.parents > 1){
        pbc_dists <- matrix(0, nrow = n.points, ncol = n.parents)
        for (j in 1:n.points){
            for (k in 1:n.parents){
                pbc_dists[j, k] <- nspp:::pbc_distances(rbind(points[j, , drop = FALSE],
                                                       parents[k, , drop = FALSE]),
                                                 lims = lims)[1]
            }
        }
        ## Indicator for whether or not a monster is near each point.
        is.near.parent <- apply(pbc_dists, 1, function(x, R) any(x < R), R = R)
    } else {
        ## No points are deleted if there are no monsters.
        is.near.parent <- rep(FALSE, n.points)
    }
    final.points <- points[!is.near.parent, ]
     if(nrow(final.points)==0){
        stop("All points deleted")
    }
    if (plot.points){
        if (dims == 2){
            plot.new()
            plot.window(xlim = lims[1, ], ylim = lims[2, ])
            points(parents, pch = 4, lwd = 2, col = "grey")
            points(final.points,pch=20)
            box()
        } else {
            warning("Plotting points only implemented for two dimensions.")
        }
        if (plot.empirical){
            warning("Both 'plot.points' and 'plot.empirical' are TRUE, the latter is being ignored.")
        }
    } else if (plot.empirical){
        nspp:::empirical.palm(final.points, lims)
    }
   if(return.parents){return(list(observed=final.points,aunts=parents))}else{
                                                                            return(final.points)}
}

