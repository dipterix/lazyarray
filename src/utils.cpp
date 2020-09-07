
#include "utils.h"

using namespace Rcpp; 


template <typename T, typename I>
bool contains(T vec, SEXP el){
  for(I ptr = vec.begin(); ptr != vec.end(); vec++ ){
    if(*ptr == el){
      return true;
    }
  }
  return false;
}

SEXP getListElement(SEXP list, const char *str){
  SEXP elmt = R_NilValue, names = Rf_getAttrib(list, R_NamesSymbol);
  
  const String str_copy(str);
  
  for (R_len_t i = 0; i < Rf_length(list); i++){
    if(str_copy == String(CHAR(STRING_ELT(names, i)))) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  return elmt;
}


/*** R
# devtools::load_all()
require(lazyarray)

dim = c(4, 9, 2)
a <- function(i,...){
  cpp_subset_idx(environment(), dim, TRUE)
}

e <- a(,-(1:3),)


cat("Test loc2idx2 with small data set - validate\n")
x = array(1:8,c(2,2,2))
dim = dim(x)
locs = list(0:1L,1L, c(0:3L,NA))
target_dim = sapply(locs, length)
tmp = loc2idx2(locs, as.numeric(dim)); tmp
# validate in R
mfactor <- c(1, cumprod(dim))[seq_along(dim)]
scaled_loc <- lapply(seq_along(locs), function(ii){
  x <- as.integer(locs[[ii]])
  d <- dim[[ii]]
  x[x < 1 | x > d] <- NA 
  (x - 1) * mfactor[[ii]]
})
t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
sum(is.na(t1)) - sum(is.na(tmp))
sum(xor(is.na(t1), is.na(tmp))) / length(t1)
range(t1 - tmp, na.rm = TRUE)

# Performance check
cat("Test loc2idx with large data set - performance\n")
dim = c(300L, 200L, 600L, 100L)
locs = list(
  1:300L,
  1:200L,
  1:600L,
  1L
)
target_dim = sapply(locs, length)

m0 <- bench::mark({
  tmp = loc2idx2(sapply(locs, as.numeric), as.numeric(dim));# tmp
  dim(tmp) = target_dim
})
m <- bench::mark({
  tmp = loc2idx(locs, dim);# tmp
  dim(tmp) = target_dim
})

m1 <- bench::mark({
  # validate in R
  mfactor <- c(1, cumprod(dim))[seq_along(dim)]
  scaled_loc <- lapply(seq_along(locs), function(ii){
    x <- as.integer(locs[[ii]])
    d <- dim[[ii]]
    x[x < 1 | x > d] <- NA 
    (x - 1) * mfactor[[ii]]
  })
  t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
})
sum(is.na(t1)) - sum(is.na(tmp))
sum(xor(is.na(t1), is.na(tmp))) / length(t1)
range(t1 - tmp)




# dim = as.integer(c(200,100,20)); x = array(seq_len(prod(dim)), dim); idx = sample(x, 1)
# # cpp_index_to_loc(idx+prod(dim), dim)
# 
# locs = lapply(dim, sample)
# system.time({
#   y1 = loc2idx(seq_len(prod(dim)), locs, dim, TRUE)
# })
# # which(x == idx, arr.ind = TRUE)
# system.time({
#   y2 = x[locs[[1]], locs[[2]], locs[[3]]]
# })
# 
# range(y1-y2)
cat("Test loc2idx with missing values - robust\n")
x = (1:5000000); pryr::object_size(x)
# arr2df(x, c(0L, 25L, 50L), 9L)
dim = c(50000L, 10L, 10L)
locs = list(
  as.integer(sample(100000L, 300) - 2000),
  as.integer(sample(12)-1),
  as.integer(sample(12)-1)
)
target_dim = sapply(locs, length)
tmp = loc2idx(locs, dim);# tmp
dim(tmp) = target_dim

# validate in R
mfactor <- c(1, cumprod(dim))[seq_along(dim)]
scaled_loc <- lapply(seq_along(locs), function(ii){
  x <- as.integer(locs[[ii]])
  d <- dim[[ii]]
  x[x < 1 | x > d] <- NA 
  (x - 1) * mfactor[[ii]]
})
t1 <- Reduce(function(a, b){ outer(a, b, '+') }, scaled_loc) + 1
sum(is.na(t1)) - sum(is.na(tmp))
sum(xor(is.na(t1), is.na(tmp))) / length(t1)
range(t1 - tmp, na.rm = TRUE)

dim(x) = c(50000L, 10L, 10L)
a = locs[[1]]; a[(a<1) | (a >50000)] = NA
b = locs[[2]]; b[b<1 | b > 10] = NA
c = locs[[3]]; c[c<1 | c > 10] = NA
y2 <- x[a,b,c]
range(y2 - tmp, na.rm = TRUE)

y3 = tmp
sum(abs(is.na(y2) - is.na(y3)))
which(abs(is.na(y2) - is.na(y3)) > 0, arr.ind = TRUE)
range(y2 - y3, na.rm = T)

# s = sample(500000, size = 5, replace = TRUE)
# 
# v1 = lazyLoadOld_base(f, c(2,3,1), c(-1L, as.integer(s)), 1); v1
# v1 = lazyLoadOld_base(f, c(2,3,2), c(-1L, as.integer(s)), c(0,1)); v1

# cpp_fstmeta(f)

# system.time({
#   cpp_array_loc_to_index_homogeneous(as.integer(c(200, 500, 100)), list(
#     as.integer(sample(300)),
#     as.integer(sample(200)),
#     as.integer(sample(300))
#   ))
# })

# f= normalizePath("~/Desktop/junk/junk.fst", mustWork = FALSE)
# s = sample(500000, size = 5000000, replace = TRUE)
# system.time({
#   v1 = lazyLoadOld_base(f, c(length(s), 10L), as.integer(s), 1:10)
# })
# system.time({
#   v2 = fst::read_fst(f, sprintf('V%d', 1:10), min(s), max(s))
#   v2[s - min(s) + 1,]
# })


# range(as.matrix(v2[s - min(s) + 1,] - v1))

# cpp_create_lazyarray(x, c(length(x) / 10,10), normalizePath(f, mustWork = FALSE), 13L, 100L, TRUE)
# file.info(f)
# # Sys.chmod(f, mode = "0777", use_umask = FALSE)
# # file.copy(f, '~/Desktop/junk/junk2.fst')
# 
# s = as.integer(sample(1e5, 200))
# ss = lazyLoadOld(normalizePath('~/Desktop/junk/junk.fst'), 'V10', min(s), max(s))
# 
# t1 = Sys.time(); lapply(1:10, function(ii){
#   lazyLoadOld(normalizePath('~/Desktop/junk/junk.fst'), sprintf('V%d', ii), min(s), max(s))
# }); Sys.time() - t1
# 
# t1 = Sys.time(); lapply(1:10, function(ii){
#   lapply(s, function(s1){
#     lazyLoadOld(normalizePath('~/Desktop/junk/junk.fst'), sprintf('V%d', ii), s1, s1 + 1000L)
#     NULL
#   })
# }); Sys.time() - t1

*/
