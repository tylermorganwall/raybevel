#include "ray.h"

std::string Ray::PrintInfo() {
  return(vformat("Ray2(<%.2f, %.2f> + u<%.2f, %.2f>)",
                  o.x(), o.y(), d.x(), d.y()));
}
