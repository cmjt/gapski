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
