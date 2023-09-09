#include <iostream>
#include <memory>
#include <vector>
#include <queue>

using real_t = double;

class vector_t {
public:
  real_t x, y;

  // Constructor
  vector_t(real_t _x = 0.0, real_t _y = 0.0)
    : x(_x), y(_y) {}

  // Addition
  vector_t operator+(const vector_t& other) const {
    return vector_t(x + other.x, y + other.y);
  }

  // Subtraction
  vector_t operator-(const vector_t& other) const {
    return vector_t(x - other.x, y - other.y);
  }

  // Scalar multiplication
  vector_t operator*(real_t scalar) const {
    return vector_t(x * scalar, y * scalar);
  }

  // Scalar division
  vector_t operator/(real_t scalar) const {
    return vector_t(x / scalar, y / scalar);
  }

  // Dot product
  real_t operator*(const vector_t& other) const {
    return x * other.x + y * other.y;
  }

  // Unary minus (negation)
  vector_t operator-() const {
    return vector_t(-x, -y);
  }

  // Dot product
  real_t dot(const vector_t& other) const {
    return x * other.x + y * other.y;
  }

  // Length squared (useful for avoiding the sqrt operation in some scenarios)
  real_t length_squared() const {
    return x * x + y * y;
  }

  vector_t normalized() const {
    real_t len = length();
    if(len == 0) return *this; // Avoid division by zero. Return the vector as is.
    return *this / len;
  }

  // Length
  real_t length() const {
    return std::sqrt(length_squared());
  }
};

// Scalar multiplication from the left
vector_t operator*(real_t scalar, const vector_t& vec) {
  return vec * scalar;
}

struct sknode_t {
  vector_t pos;
};

class kvertex_t {
public:
  vector_t o, v, ccw_wavefront, cw_wavefront;
  real_t starts_at, stops_at;
  std::shared_ptr<sknode_t> start_node, stop_node;
};


enum EventType {
  EDGE_EVENT,
  SPLIT_EVENT,
  FLIP_EVENT,
  // Add any other events if necessary
};


struct originalEdge {
  vector_t startPoint;
  vector_t endPoint;
  vector_t edgeNormal;

  originalEdge(const vector_t& a, const vector_t& b) : startPoint(a), endPoint(b) {
    computeEdgeNormal();
  }

  // Other members and methods...

  void computeEdgeNormal() {
    // Compute the edge vector from startPoint to endPoint
    vector_t edgeVector = endPoint - startPoint;

    // Compute the normal. For a 2D vector (x, y), the normal would be (-y, x) or (y, -x)
    edgeNormal = vector_t(-edgeVector.y, edgeVector.x);

    // Normalize the normal vector
    edgeNormal = edgeNormal.normalized();
  }
  // Function to return a point on the supporting line.
  // Here, we return the midpoint of the edge.
  vector_t pointOnEdge() const {
    return 0.5 * (startPoint + endPoint);
  }
};

class ktriangle_t;

struct wavefrontEdge {
  ktriangle_t* triangle;  // The triangle associated with the wavefront edge.
  originalEdge* support;  // Reference to the supporting edge.

  // Constructor
  wavefrontEdge(ktriangle_t* tri, originalEdge* sup) : triangle(tri), support(sup) {}

  // Function to get a point M on the supporting line of the wavefront edge.
  vector_t getM() const {
    return support->pointOnEdge();
  }
};

class ktriangle_t {
public:
  kvertex_t *v[3];
  union {
    ktriangle_t *n[3];  // Neighboring triangles.
    wavefrontEdge *w[3];  // Wavefront edges. Used only if the corresponding neighbor pointer is null.
  };
  originalEdge *wavefrontEdgeRef[3];
  int heap_position;
  real_t root1, root2;

  void collapseTimeArea() {
    real_t a = 0.5 * (
        v[0]->v.y * v[1]->v.x + v[0]->v.x * v[1]->v.y +
        v[0]->v.y * v[2]->v.x - v[1]->v.y * v[2]->v.x -
        v[0]->v.x * v[2]->v.y + v[1]->v.x * v[2]->v.y);

    real_t b = (
        v[1]->o.y * v[0]->v.x - v[2]->o.y * v[0]->v.x -
        v[1]->o.x * v[0]->v.y + v[2]->o.x * v[0]->v.y -
        v[0]->o.y * v[1]->v.x + v[2]->o.y * v[1]->v.x +
        v[0]->o.x * v[1]->v.y - v[2]->o.x * v[1]->v.y +
        v[0]->o.y * v[2]->v.x - v[1]->o.y * v[2]->v.x -
        v[0]->o.x * v[2]->v.y + v[1]->o.x * v[2]->v.y);

    real_t c = (
       -v[0]->o.y * v[1]->o.x + v[0]->o.x * v[1]->o.y +
        v[0]->o.y * v[2]->o.x - v[1]->o.y * v[2]->o.x -
        v[0]->o.x * v[2]->o.y + v[1]->o.x * v[2]->o.y);

    real_t discriminant = b * b - 4 * a * c;

    // If the discriminant is negative, there are no real roots.
    if (discriminant < 0) {
      root1 = root2 = std::numeric_limits<real_t>::infinity(); // Not a valid time
    } else {
      root1 = (-b + std::sqrt(discriminant)) / (2 * a);
      root2 = (-b - std::sqrt(discriminant)) / (2 * a);
    }

    // Return some value from the quadratic function to represent collapse time.
    // In practice, you would solve this quadratic equation to get the roots.
    // return t_squared_term + t_term + constant_term;
  }
  bool isWavefrontEdge(int index) const {
    return (n[index] == nullptr);
  }

  real_t collapseTimeEdge() const {
    vector_t e = v[1]->o - v[0]->o + (v[1]->v - v[0]->v);
    real_t te = (v[0]->v - v[1]->v).dot(v[1]->o - v[0]->o) /
      (v[0]->v - v[1]->v).dot(v[0]->v - v[1]->v);
    return te;
  }

  real_t collapseTimeVertexCrash() const {
    vector_t s_prime = v[0]->v * -1;
    vector_t n = (v[1]->o - v[0]->o).normalized();
    real_t speed = s_prime.dot(-n);
    real_t combined_speed = 1.0 - v[0]->v.dot(n);
    vector_t Mv = v[0]->o - v[1]->o; // Assuming v1 is the vertex and v2 and v3 make the edge.
    real_t t = (Mv.dot(n)) / combined_speed;
    return t;
  }

  EventType determineEventType() const;

  void collapseTime() const {
    // Placeholder logic: Returns the minimum collapse time of all three methods.
    // This can be modified based on priority or other conditions.
    // return std::min({collapseTimeArea(), collapseTimeEdge(), collapseTimeVertexCrash()});
  }

  bool isWavefrontEdge(const ktriangle_t* edgeNeighbor) const;
  real_t edgeCollapseTime(const kvertex_t* v1, const kvertex_t* v2) const;
  real_t vertexCrashTime(const kvertex_t* v, size_t edgeIndex) const;

};

bool ktriangle_t::isWavefrontEdge(const ktriangle_t* edgeNeighbor) const {
  return edgeNeighbor == nullptr;
}

real_t ktriangle_t::edgeCollapseTime(const kvertex_t* v1, const kvertex_t* v2) const {
  vector_t s1 = v1->v;
  vector_t s2 = v2->v;
  vector_t o1 = v1->o;
  vector_t o2 = v2->o;

  real_t numerator = (s1 - s2).dot(o2 - o1);
  real_t denominator = (s1 - s2).dot(s1 - s2);

  if (denominator == 0.0) {
    // Parallel vectors. Either they already met or will never meet.
    return std::numeric_limits<real_t>::infinity();
  }

  real_t te = numerator / denominator;

  vector_t e = o2 - o1 + te * (s2 - s1);
  if (e.dot(e) == 0) {
    return te;
  }

  // The edge will not collapse
  return std::numeric_limits<real_t>::infinity();
}

real_t ktriangle_t::vertexCrashTime(const kvertex_t* v, size_t edgeIndex) const {
  if (!isWavefrontEdge(n[edgeIndex])) {
    return std::numeric_limits<real_t>::infinity();
  }

  vector_t M = w[edgeIndex]->getM();  // Get point M from the wavefrontEdge.
  vector_t Mv = M - v->o;  // vector from M to v at time zero.

  vector_t s = v->v;
  real_t numerator = Mv.dot(wavefrontEdgeRef[edgeIndex]->edgeNormal);
  real_t denominator = 1 - s.dot(wavefrontEdgeRef[edgeIndex]->edgeNormal);

  if (denominator == 0.0) {
    // Either the vertex is already on the edge or will never be on it.
    return std::numeric_limits<real_t>::infinity();
  }

  return numerator / denominator;
}


// Custom comparator for priority queue of triangles
struct CompareTriangle {
  bool operator()(const std::shared_ptr<ktriangle_t>& t1, const std::shared_ptr<ktriangle_t>& t2) {
    return t1->collapseTime() > t2->collapseTime(); // Smaller times have higher priority
  }
};

real_t computeLengthAtTime(const kvertex_t* v1, const kvertex_t* v2, real_t t) {
  // Compute the position of the vertices at time t
  vector_t pos_v1_t = v1->o + t * v1->v;
  vector_t pos_v2_t = v2->o + t * v2->v;

  // Compute the length of the edge between the two vertices at time t
  return (pos_v1_t - pos_v2_t).length();
}

EventType ktriangle_t::determineEventType() const {
  int wavefrontEdgesCount = 0;
  for (int i = 0; i < 3; i++) {
    if (isWavefrontEdge(i)) {
      wavefrontEdgesCount++;
    }
  }

  switch (wavefrontEdgesCount) {
  case 3:
    // All three edges are wavefront edges.
    return EDGE_EVENT;

  case 2: {
      // Two wavefront edges.
      // Find the earliest edge collapse time among the wavefront edges.
      real_t earliestCollapseTime = std::numeric_limits<real_t>::infinity();
      for (int i = 0; i < 3; i++) {
        if (isWavefrontEdge(i)) {
          earliestCollapseTime = std::min(earliestCollapseTime, edgeCollapseTime(v[i], v[(i+1)%3]));
        }
      }
      // Assuming root1 and root2 are the collapse times computed elsewhere in the class,
      // decide between EDGE_EVENT and FLIP_EVENT based on the timings.
      return (earliestCollapseTime == root1 || earliestCollapseTime == root2) ? EDGE_EVENT : FLIP_EVENT;
    }

  case 1: {
    // One wavefront edge.
    int wavefrontIdx;
    for (int i = 0; i < 3; i++) {
      if (isWavefrontEdge(i)) {
        wavefrontIdx = i;
        break;
      }
    }

    real_t te = edgeCollapseTime(v[wavefrontIdx], v[(wavefrontIdx + 1) % 3]);
    kvertex_t* opposingVertex = v[(wavefrontIdx + 2) % 3];
    real_t tv = vertexCrashTime(opposingVertex, wavefrontIdx);

    if (te <= tv) {
      return EDGE_EVENT;
    } else {
      // Compute the lengths of all sides at time tv
      real_t length1 = computeLengthAtTime(v[0], v[1], tv);
      real_t length2 = computeLengthAtTime(v[1], v[2], tv);
      real_t length3 = computeLengthAtTime(v[2], v[0], tv);

      // Determine the longest side at time tv
      real_t longestLengthAtTV = std::max({length1, length2, length3});

      return (v[wavefrontIdx]->o - v[(wavefrontIdx + 1) % 3]->o).length() == longestLengthAtTV ? SPLIT_EVENT : FLIP_EVENT;

    }
  }

  default: {
    // Bounded only by spokes.
    real_t spoke1CollapseTime = edgeCollapseTime(v[0], v[1]);
    real_t spoke2CollapseTime = edgeCollapseTime(v[1], v[2]);
    real_t spoke3CollapseTime = edgeCollapseTime(v[2], v[0]);
    if (root1 < spoke1CollapseTime && root1 < spoke2CollapseTime && root1 < spoke3CollapseTime) {
      return FLIP_EVENT;
    }
    // If none of the above conditions are met, default to EDGE_EVENT or other event based on the scenario.
    return EDGE_EVENT;
  }
  }
}

using TriangleQueue = std::priority_queue<std::shared_ptr<ktriangle_t>, std::vector<std::shared_ptr<ktriangle_t>>, CompareTriangle>;

class StraightSkeleton {
  std::vector<std::shared_ptr<kvertex_t>> vertices;
  std::vector<std::shared_ptr<ktriangle_t>> triangles;
  TriangleQueue triangleQueue;

public:
  // Add methods to populate vertices, triangles and queue here.
  // ...

  void processWavefront() {
    while (!triangleQueue.empty()) {
      auto topTriangle = triangleQueue.top();
      triangleQueue.pop();

      // Process the collapse of the triangle
      // Handle flip, split and edge events here.
      // ...
    }
  }
};
