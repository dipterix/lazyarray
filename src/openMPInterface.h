#ifndef LAZYARRAY_OPENMP_H
#define LAZYARRAY_OPENMP_H

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]

#ifdef _OPENMP
#include <omp.h>
#define LAZYARRAY_HAS_OPENMP true
#else
#define omp_get_thread_num() 0
#define omp_get_max_threads() 1
#define LAZYARRAY_HAS_OPENMP false
#endif

#include <Rcpp.h>

static int lazyThreads = 0;

// stores n threads when fork occurs 
static bool detectFork = false;
static int reset_forked = false;


// [[Rcpp::export]]
int getLazyThread();

// [[Rcpp::export]]
int setLazyThread(int n, SEXP reset_after_fork = R_NilValue);

// [[Rcpp::export]]
bool hasOpenMP();

// [[Rcpp::init]]
int detectForked(DllInfo *dll);

#endif  // OPEN_MP_HELPER_H
