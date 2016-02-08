#include <Rcpp.h>
#include <iostream>
#include <math.h>  
using namespace Rcpp;
using namespace std;
 


//' Calculating centres of radii of Delauney circumcircles
//' @inheritParams D.gaps
// [[Rcpp::export]]
NumericVector D_gaps(const NumericMatrix& triangs){
  int nrow = triangs.nrow();
  int ncol = triangs.ncol();
  NumericVector out(ncol + 1);
  double l1 [2] = {5,5} ;
  double l2 [2] = {6,-2};
  double l3 [2]  = {2,-4};
  double mr = (l2[1]-l1[1])/(l2[0]-l1[0]);
  double mt = (l3[1]-l2[1])/(l3[0]-l2[0]);
  double x = (mr*mt*(l3[1]-l1[1]) + mr*(l2[0]+l3[0]) - mt*(l1[0]+l2[0]))/(2*(mr-mt));
  double y = (-1/mr)*(x -((l1[0]+l2[0])/2)) + ((l1[1]+l2[1])/2);
  double r = sqrt(pow((x-l1[0]),2) + pow((y-l1[1]),2));
  out [0] = x; out [1] = y; out [2] = r;
  return out;
 }

//' Calculating centres of radii of Delauney circumcircles
//' @inheritParams D.gaps
// [[Rcpp::export]]
NumericVector gaps(const NumericMatrix& triangs){
  int nrow = triangs.nrow();
  int ncol = triangs.ncol();
  NumericMatrix out(nrow/3,3);
  double mr, mt, x, y, r;
  int m = 0;
  for (int i = 0; i < nrow; i +=2){
    for (int j = 0; j < ncol; j++){
      mr = (triangs(i+1,j+1)-triangs(i,j+1))/(triangs(i+1,j)-triangs(i,j));
      mt = (triangs(i+2,j+1)-triangs(i+1,j+1))/(triangs(i+2,j)-triangs(i+1,j));
      x = (mr*mt*(triangs(i+2,j+1)-triangs(i,j+1)) + mr*(triangs(i+1,j)+triangs(i+2,j)) - mt*(triangs(i,j)+triangs(i+1,j)))/(2*(mr-mt));
      y = (-1/mr)*(x -((triangs(i,j)+triangs(i+1,j))/2)) + ((triangs(i,j+1)+triangs(i+1,j+1))/2);
      r = sqrt(pow((x-triangs(i,j)),2) + pow((y-triangs(i+1,j+1)),2));
      out(m,0) = x; out(m,  1) = y; out(m, 2) = r;
      m ++;
    }
  }

  return out;

 }
