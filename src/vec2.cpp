#include "vec2.h"
#include "mathutil.h"

template<typename T>
inline bool vec2<T>::is_equivalent(point2<T> p, Float tolerance) {
  return(approximateEqualFloat(p.e[0],e[0],tolerance) &&
         approximateEqualFloat(p.e[1],e[1],tolerance));
}
