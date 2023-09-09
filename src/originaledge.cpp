#include "originaledge.h"

std::string OriginalEdge::PrintInfo() {
  return(vformat("_OriginalEdge(edge=%s, bisector_left=%s, bisector_right=%s)",
                 Edge.PrintInfo().c_str(),
                 PrevBisector.PrintInfo().c_str(),
                 Bisector.PrintInfo().c_str()));
}
