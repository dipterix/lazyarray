#include "fstWrapper.h"

// [[Rcpp::plugins("cpp11")]]

#include <fstcore.h>
#include "common.h"
#include "utils.h"

using namespace Rcpp;


SEXP fstMeta(String fileName){
  return fstcore::fstmetadata(fileName);
}

/*
 * Slightly different than fscore::fstretrieve. It supports 
 * 
 */
SEXP fstRetrieve(String fileName, SEXP colSel, SEXP start, SEXP end){
#ifdef LAZYARRAY_DEBUG
  print(wrap("Reading from fst..."));
#endif
  return fstcore::fstretrieve(fileName, colSel, start, end);
}

SEXP fstStore(String fileName, SEXP table, SEXP compression, SEXP uniformEncoding){
  return fstcore::fststore(fileName, table, compression, uniformEncoding);
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

