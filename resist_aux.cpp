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

// This function calculates the sum of llk for each group
// [[Rcpp::export]]
NumericVector GetSomaLlkGroups(NumericMatrix llk, IntegerVector z, int ngroups) {
  int n=llk.nrow();
  NumericVector llk1(ngroups);
  
  for(int i=0;i<n;i++){
    for(int j=0; j<ngroups;j++){
      if (z[i]==j){
        llk1[j]=llk1[j]+llk(i,j);
      }
    }      
  }
  return llk1;
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

// This function calculates probabilities to sample z
// [[Rcpp::export]]
NumericMatrix GetProbZ(NumericMatrix llk){
  double max1;
  double soma;
  for (int i = 0; i < llk.nrow(); i++){
    max1=max(llk(i,_));
    llk(i,_)=llk(i,_)-max1;
    llk(i,_)=exp(llk(i,_));
    soma=sum(llk(i,_));
    llk(i,_)=llk(i,_)/soma;
  }
  return llk;
}
