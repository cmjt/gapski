#include <Rcpp.h>
#include <iostream>
#include <math.h>  
using namespace Rcpp;
using namespace std;
 


//' Calculating centres of radii of Delauney circumcircles
//' @inheritParams D.gaps
// [[Rcpp::export]]
NumericVector D_gaps(const NumericMatrix& points){
  int edges = 3;
  int n_points = 2;
  double x1, x2, x3, y1, y2, y3;
  double mr, mt, x, y, r;
  NumerixMatrix out(points.nrow/edges,n_points);
  NumericMatrix triangle(edges, n_points);
  for(i = 0; i < edges; i +=3){
    triangle(i,) = points(i,);
  }
for (i = 0; i < (n_points - 1); i++){
    for (j = i + 1; j < n_points; j++){
     mr = triangle(y2-y1)/triangle(x2-x1);
     mt = triangle(y3-y2)/triangle(x3-x2);
     x = (mr*mt*triangle(y3-y1)+mr*triangle(x2+x3)-mt*triangle(x1+x2))/(2*(mr-mt));
     y = (-1/mr)*(x-(triangle(x1+x2)/2))+triangle(y1+y2)/2;
     r = sqrt(pow(triangle(x1)-x,2) + pow(triangle(y1)-y,2));
	      }
 }
