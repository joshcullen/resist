// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <ctime>
#include <fstream>
using namespace Rcpp;

/***************************************************************************************************************************/
/*********************************                      UTILS          *****************************************************/
/***************************************************************************************************************************/

// This function calculates the sum of means for each segment based on the appropriate group, given by z
// [[Rcpp::export]]
NumericVector GetSomaMedia(IntegerVector z, NumericMatrix media, int ngroups, int nysoma,
                           IntegerVector SegID) {
  int n=media.nrow();
  NumericVector soma(nysoma);
  
  for(int i=0;i<n;i++){
    for(int j=0; j<ngroups;j++){
      if(z[SegID[i]]==j) {
        soma[SegID[i]]=soma[SegID[i]]+media(i,j);
      }      
    }      
  }
  return soma;
}

// This function calculates the sum of means for each segment and each group
// [[Rcpp::export]]
NumericMatrix GetSomaMediaAllGroups(NumericMatrix media, int ngroups, int nysoma,IntegerVector SegID) {
  int n=media.nrow();
  NumericMatrix soma(nysoma,ngroups);
  
  for(int i=0;i<n;i++){
    for(int j=0; j<ngroups;j++){
      soma(SegID[i],j)=soma(SegID[i],j)+media(i,j);
    }      
  }
  return soma;
}

// This function helps with multinomial draws
// [[Rcpp::export]]
int whichLessDVPresence(double value, NumericVector prob) {
  int res=prob.length()-1;
  double probcum = 0;
  
  for (int i = 0; i < prob.length(); i++) {
    probcum = probcum + prob(i);
    if (value < probcum) {
      res = i;
      break;
    }
  }
  return res;
}

// This function draws one RV from a multinomial distribution for each line of prob
// [[Rcpp::export]]
IntegerVector rmultinom1(NumericMatrix prob, NumericVector runif1){
  int n=prob.nrow();
  IntegerVector z(n);
  
  for (int i = 0; i < n; i++) {
    z[i]=whichLessDVPresence(runif1[i], prob(i,_));
  }
  return z;
}

// This function sums means for one group (TargetGrp)
// [[Rcpp::export]]
NumericVector GetSomaMediaOneGroup(NumericVector media,IntegerVector z,int TargetGrp,int nysoma,
                                   IntegerVector SegID){
  int n=media.length();
  NumericVector res(nysoma);
  for (int i = 0; i < n; i++){
    if (z[SegID[i]]==TargetGrp){
      res[SegID[i]]=res[SegID[i]]+media[i];
    }
  }
  return(res);
}
