#ifndef MATHUTIL
#define MATHUTIL

#include "float.h"
#include <cmath>

inline Float DifferenceOfProducts(Float a, Float b, Float c, Float d) {
  Float cd = c * d;
  Float err = std::fma(-c, d, cd);
  Float dop = std::fma(a, b, -cd);
  return(dop + err);
  // return(a*b-c*d);
}

inline bool approximateEqualFloat(Float a, Float b, Float tol) {
  return(std::fabs(a-b) <= tol);
}


#endif
