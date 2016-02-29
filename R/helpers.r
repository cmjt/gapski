## function which finds the circumcircles of the Delauney triangulation of the input point locations
## all circumcircles with radii > 2 "radius" of the area are deleted. Function called by the circumcircle plot
## function. centers, radii and areas of circumcircles found through call to a c++ function.
## uses deldi, and triang-list function from the deldir package

circumcircle<-function(points,lims){
    points<-triang.list(delauney<-deldir(points[,1],points[,2]))
    points<-lapply(points,as.matrix)
    points<-lapply(points,function (x) x[,2:3])
    points<-do.call(rbind, lapply(points, unlist))
    centers<-centers(points)
    colnames(centers)<-c("x","y","r","Ar")
    centers<-centers[inside.owin(centers[,1],centers[,2],w=owin(lims[1,],lims[2,])),]
    return(centers)
}

## function which deletes points at less than a distance gap.par from a number (gaps) of
## randomly selected points.

voids<-function(n.points,locs,gap.par,gaps){
    id = seq(1:n.points)
    id<-sample(id,gaps)
    locs.d<-locs[-id,]
    dists <- rdist(locs.d,locs[id,])
    dists<-which(dists<=gap.par,arr.ind=TRUE)
    locs.out<-locs.d[-dists[,1],]
    return(locs.out)
}


## Volume of d-dimensional hypersphere with radius r.
Vd <- function(r, d){
    pi^(d/2)*r^d/gamma(d/2 + 1)
}
##surface area of d-dimensional hypersphere with radius r
Sd <- function(r, d){
    d*pi^(d/2)*r^(d - 1)/gamma(d/2 + 1)
}


## Volume of intersection of two d-dimensional spheres with common radius R and distance between them equal to r
V.in<-function(r,R,d){
    Vd(r=R,d=d)*pbeta(1-(r/(2*R))^2,(d+1)/2,1/2)
}
##palm intensity for the Matern monster process
palm.intensity<-function(r,lambda, D, R,d){
    lambda*exp(-D*(Vd(R,d) - V.in(r,R,d)))
}

##the intractable part of the intensity containing the CDF of the Beta distribution, eveluated using R's integrate function
## parameters are D-density of parents, R-gap radius in d-dimensions, here we integrate wrt r i.e. distance from an arbitrary point.
integrand<-function(r,D,R,d){
    r^(d-1)*exp(D*Vd(r=R,d=d)*pbeta(1-(r/(2*R))^2,(d+1)/2,1/2))
}
    
##the log Likelihood called by the fitting function in optim

mmpp.ll <- function(pars, n.points, dists, d, par.names,
                   palm.intensity, trace,integrand){
    ## Extracting parameters
    names(pars) <- par.names
    pars <- exp(pars)
    R <- pars["R"]
    D <- pars["D"]
    lambda<-pars["lambda"]
    ## first part of the log-Likelihood i.e sum of the palm.intensities
    ll1 <- sum(log(n.points*palm.intensity(dists,lambda, D, R, d)))
    ## second part of log-Likelihood
    ll2 <-((n.points*lambda*exp(-D*Vd(R,d))*d*pi^(d/2))/gamma(d/2 +1))*integrate(integrand,0,max(dists),D=D,R=R,d=d)$value
    ll <- ll1 - ll2
    ## return negative of log-Likelihood as optim minimizes by deafault
    if (trace){
        cat("lambda = ", lambda, ", D = ", D, ", R = ", R, ", LL = ", ll, "\n", sep = "")
    }
    -ll
}

