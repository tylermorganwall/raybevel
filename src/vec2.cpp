#include "vec2.h"

template<typename T>
inline bool vec2<T>::is_equivalent(point2<T> p, Float tolerance) {
  return(abs(p.e[0] - e[0]) < tolerance && abs(p.e[1] - e[1]) < tolerance);
}
