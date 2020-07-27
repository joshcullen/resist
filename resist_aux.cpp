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

// This function sums means for one group (TargetGrp)
// [[Rcpp::export]]
NumericVector GetSomaMediaOneGroup(NumericVector media,int nagg,IntegerVector SegID){
  int n=media.length();
  NumericVector res(nagg);
  for (int i = 0; i < n; i++){
    res[SegID[i]]=res[SegID[i]]+media[i];
  }
  return(res);
}
