#ifndef RAYDEBUGH
#define RAYDEBUGH

#include <stdexcept>

void ThrowAssertionFailure(const char* message, const char* file, int line) {
  throw std::runtime_error(std::string(message) + " (at " + file + ":" + std::to_string(line) + ")");
}

#define RAY_ASSERT(cond) \
if (!(cond)) ThrowAssertionFailure(#cond, __FILE__, __LINE__)

#endif
