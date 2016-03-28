 ## function to extract the vertices of a delauney triangulation of the points
## uses deldir::deldir, and triang.list::deldir. Outputs a list of matricies specifying the vertices of each Delauney
## tile

delauney.list<-function(points){
    triang<-triang.list(deldir(points[,1],points[,2]))
    lapply(triang,as.matrix)
    lapply(triang,function (x) x[,2:3])
}
    

