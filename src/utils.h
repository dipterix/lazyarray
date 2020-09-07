
#ifndef DIP_LAZYARRAY_UTILS_H 
#define DIP_LAZYARRAY_UTILS_H

#include "common.h"

template <typename T, typename I>
bool contains(T vec, SEXP el);

SEXP getListElement(SEXP list, const char *str); 


#endif // DIP_LAZYARRAY_UTILS_H
