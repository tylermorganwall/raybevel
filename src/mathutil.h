#ifndef MATHUTIL
#define MATHUTIL

#include "float.h"

inline Float DifferenceOfProducts(Float a, Float b, Float c, Float d) {
  Float cd = c * d;
  Float err = std::fma(-c, d, cd);
  Float dop = std::fma(a, b, -cd);
  return(dop + err);
}


#endif
