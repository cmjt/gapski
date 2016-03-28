#include <Rcpp.h>
#include <iostream>
#include <math.h>  
using namespace Rcpp;
using namespace std;
 


//' Calculating centres of radii of Delauney circumcircles
// [[Rcpp::export]]
NumericVector centers(const NumericMatrix& triangs){
  int nrow = triangs.nrow();
  int ncol = triangs.ncol();
  NumericMatrix out(nrow/3,4);
  double mr, mt, x, y, r, ar;
  int j = 0;
  int m=0;
  for (int i = 0; i < nrow; i +=3){
  mr = (triangs(i+1,j+1)-triangs(i,j+1))/(triangs(i+1,j)-triangs(i,j));
  mt = (triangs(i+2,j+1)-triangs(i+1,j+1))/(triangs(i+2,j)-triangs(i+1,j));
  x = (mr*mt*(triangs(i+2,j+1)-triangs(i,j+1)) + mr*(triangs(i+1,j)+triangs(i+2,j)) - mt*(triangs(i,j)+triangs(i+1,j)))/(2*(mr-mt));
  y = (-1/mr)*(x -((triangs(i,j)+triangs(i+1,j))/2)) + ((triangs(i,j+1)+triangs(i+1,j+1))/2);
  r = sqrt(pow((x-triangs(i,j)),2) + pow((y-triangs(i,j+1)),2));
  ar = M_PI*pow(r,2);
  out(m,0) = x; out(m,  1) = y; out(m, 2) = r; out(m,3) = ar;
  m ++;
  }
  return out;

 }
