#include "fstWrapper.h"

#include <Rcpp.h>
#include <fstcore.h>
#include "utils.h"

using namespace Rcpp;


SEXP fstMeta(SEXP fileName){
  return fstcore::fstmetadata(as<String>(fileName));
}

/*
 * Slightly different than fscore::fstretrieve. It supports 
 * 
 */
SEXP fstRetrieve(SEXP fileName, SEXP colSel, SEXP start, SEXP end){
  return fstcore::fstretrieve(as<String>(fileName), colSel, start, end);
}


bool checkFstMeta(const String file, const int64_t expect_nrows, StringVector cnames){
  List meta = fstcore::fstmetadata( file );
  if( Rf_inherits(meta, "fst_error") || 
      !meta.containsElementNamed("nrOfRows") ||
      !meta.containsElementNamed("colNames")){
      return(false);
  }
  int64_t nrows = as<int64_t>(meta["nrOfRows"]);
  if( expect_nrows >= 0 && nrows != expect_nrows ){
    stop("Unable to read from file: partition size not matches with expected size.");
  }
  StringVector colNames = as<StringVector>(meta["colNames"]);
  
  if(colNames.size() < cnames.size()){
    return(false);
  }
  
  R_xlen_t touched = 0;
  for(R_xlen_t ii = 0; ii < cnames.size(); ii++ ){
    const String cn = cnames[ii];
    for(StringVector::iterator ptr = colNames.begin(); ptr != colNames.end(); ptr++){
      if((*ptr) == cn){
        touched++;
        continue;
      }
    }
  }
  
  if(touched >= cnames.size()){
    return(true);
  }
  
  return(false);
}

