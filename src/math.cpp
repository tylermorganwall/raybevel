#include "math.h"

// Helper function to calculate determinant of two 2D vectors
double determinant2x2(const std::vector<double>& v1, const std::vector<double>& v2) {
  return v1[0] * v2[1] - v1[1] * v2[0];
}

// Helper function to calculate dot product of two 2D vectors
double dot(const std::vector<double>& v1, const std::vector<double>& v2) {
  return v1[0] * v2[0] + v1[1] * v2[1];
}
