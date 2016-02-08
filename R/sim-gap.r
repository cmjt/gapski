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
#' parameters are \code{gap.par}, the parameter relating to the deletion algorithm;
#' \code{lambda} the density of points in the domain.
#' @param d a numeric value specifying the number of dimensions the point process is to be simulated in, by default this is 2.
#' @param lims An argument specifying the limits of the domain, for 1D this is a vector, for 2D or greater; for \code{rectangle} domain a matrix with two colums, corresponding to the upper and lower limits of each dimension, respectively, for \code{hypersphere} domain a vector specifying the radius of the sphere. By deafault this specifies the unit square.
#' @param model A character specifying the type of void point process wanted, \code{void} by deafault. Other options are
#' \code{Mat1}, \code {Mat2}, and \code{dominance}.
#' @param domain.type A character specifying the type of domain requested, either \code{rectangle} or \code{hypersphere} for
#' simulated point proces in 2 or more dimensions, by deafault this is \code{rectangle}.
#' @export
sim.gap <- function(pars = NULL, d = 2 , lims = rbind(c(0, 1), c(0, 1)), model="void", domain.type="rectangle"){
    ## Allowing lims to be a vector if only one dimension.
    if (!is.matrix(lims)){
        lims <- matrix(lims, nrow = 1)
    }
    ## Parameter values.
    gap.par <- pars["gap.par"]
    lambda <- pars["lambda"]
    ## Number of dimensions.
    dims <- d
    ## Calculating survey area.
    area <- prod(apply(lims, 1, diff))
    expected.n.points = area*lambda
    n.points = rpois(n =1 ,lambda=expected.n.points)
    if(domain.type=="rectangle"){
        ## Generating location points.
        locs <- matrix(0, nrow = n.points, ncol = dims)
        for (i in 1:dims){
            locs[, i] <- runif(n.points, lims[i, 1], lims[i, 2])
        }
    }else if (domain.type=="hypersphere"){
        locs<-as.matrix(as.matrix(unifsphere(n=n.points,d=dims,R=lims[,1])))
    }
    if (n.points == 0){
        stop("No points generated.")
    }
    #point ids
    browser()
    id = seq(1:n.points)
    #deletion algorithms - put in helper file when sorted
    if(model=="void"){
        # let user chose number of gaps i.e. 2 in sample
      id<-sample(id,2)
      locs.d<-locs[-id,]
      dists <- rdist(locs.d,locs[id,])
      dists<-which(dists<=gap.par,arr.ind=TRUE)
      locs.out<-locs.d[-dists[,1],]
    }
    locs.out
}

