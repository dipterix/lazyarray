#ifndef API_LAZYARRAY_FSTMATRIX_H
#define API_LAZYARRAY_FSTMATRIX_H

#include "FstArray.h"

namespace lazyarray {

class FstMatrix : public FstArray {
  
  // Constructors and field getter/setter
public:
  FstMatrix(
    const std::string rootPath, std::vector<int64_t> dimension, bool& transposed,
    SEXPTYPE dataType, int& compression, bool& uniformEncoding
  ): FstArray(rootPath, dimension, dataType, compression, uniformEncoding), _transposed(transposed) {}
  
  virtual ~FstMatrix(){ destroy(); }
  
  bool validate(bool stopIfError = true) {
    //_nparts _totalLen fstFiles dimension
    bool isValid = FstArray::validate(stopIfError);
    isValid = isValid && stopIfNot(_dimension.size() == 2, "FstMatrix must be two dimension", stopIfError);
    return isValid;
  }
  
  NumericVector getDim() override {
    NumericVector dim(2);
    if( _transposed ){
      dim[0] = _dimension[1];
      dim[1] = _dimension[0];
      dim.attr("is_transposed") = true;
    } else {
      dim[0] = _dimension[0];
      dim[1] = _dimension[1];
    }
    return dim;
  }
  
  // SEXP subset(SEXP listOrEnv, SEXP reshape = R_NilValue, bool drop = false) override {
  //   validate();
  //   
  //   List sliceIdx = extractSlices(listOrEnv, 2);
  //   bool resultNeedTranspose = false;
  //   if(_transposed){
  //     resultNeedTranspose = true;
  //     if( sliceIdx.size() == 2 ){
  //       SEXP tmp = sliceIdx[0];
  //       sliceIdx[0] = sliceIdx[1];
  //       sliceIdx[1] = tmp;
  //     } else if( sliceIdx.size() == 1 && sliceIdx[0] != R_MissingArg ){
  //       // x[i] -> calculate row & col for i, switch back and calculate new i
  //       resultNeedTranspose = false;
  //       std::vector<int64_t> idx = sliceIdx[0];
  //       int64_t tmp;
  //       int64_t nrow = _dimension[1]; // column is row now
  //       int64_t ncol = _dimension[0];
  //       for(std::vector<int64_t>::iterator ptr_idx = idx.begin(); ptr_idx != idx.end(); ptr_idx++ ){
  //         if(*ptr_idx != NA_REAL && *ptr_idx != LLONG_MIN){
  //           tmp = ((*ptr_idx) - 1) % nrow;
  //           *ptr_idx = ((*ptr_idx) - 1 - tmp) / nrow + tmp * ncol + 1;
  //         }
  //       }
  //     }
  //   }
  //   NumericVector dim = int64t2NumericVector(_dimension);
  //   const List subparsed = parseAndScheduleBlocks(sliceIdx, dim);
  //   Rcpp::checkUserInterrupt();
  //   
  //   Rcpp::StringVector fstFiles = get_partition_path();
  //   
  //   SEXP res = subsetFSTBare(fstFiles, subparsed, dim, _dataType);
  //   if( resultNeedTranspose ){
  //     Environment base = Environment::base_env();
  //     res = as<Function>(base["t.default"])(res);
  //   }
  //   return res;
  // };
  
  // SEXP subsetAssign(SEXP values, SEXP listOrEnv) override {
  //   validate();
  //   // List sliceIdx = extractSlices(listOrEnv, 2);
  //   // bool resultNeedTranspose = false;
  //   // if(_transposed){
  //   //   resultNeedTranspose = true;
  //   //   if( sliceIdx.size() == 2 ){
  //   //     SEXP tmp = sliceIdx[0];
  //   //     sliceIdx[0] = sliceIdx[1];
  //   //     sliceIdx[1] = tmp;
  //   //   } else if( sliceIdx.size() == 1 && sliceIdx[0] != R_MissingArg ){
  //   //     // x[i] -> calculate row & col for i, switch back and calculate new i
  //   //     resultNeedTranspose = false;
  //   //     std::vector<int64_t> idx = sliceIdx[0];
  //   //     int64_t tmp;
  //   //     int64_t nrow = _dimension[1]; // column is row now
  //   //     int64_t ncol = _dimension[0];
  //   //     for(std::vector<int64_t>::iterator ptr_idx = idx.begin(); ptr_idx != idx.end(); ptr_idx++ ){
  //   //       if(*ptr_idx != NA_REAL && *ptr_idx != NA_INTEGER64){
  //   //         tmp = ((*ptr_idx) - 1) % nrow;
  //   //         *ptr_idx = ((*ptr_idx) - 1 - tmp) / nrow + tmp * ncol + 1;
  //   //       }
  //   //     }
  //   //   }
  //   // }
  //   // NumericVector dim = int64t2NumericVector(_dimension);
  //   // const List subparsed = parseAndScheduleBlocks(sliceIdx, dim);
  //   // Rcpp::checkUserInterrupt();
  //   // 
  //   // SEXP res = subsetFSTBare(fstFiles, subparsed, dim, _dataType);
  //   // if( resultNeedTranspose ){
  //   //   Environment base = Environment::base_env();
  //   //   res = as<Function>(base["t.default"])(res);
  //   // }
  //   // return res;
  //   // subsetAssignFST(values, fstFiles, listOrEnv, dim, _dataType, _compression, _uniformEncoding);
  //   return R_NilValue;
  // }
  
protected:
  bool _transposed;
  

};


}


#endif // API_LAZYARRAY_FSTMATRIX_H
