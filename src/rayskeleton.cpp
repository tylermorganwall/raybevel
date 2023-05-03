#include <Rcpp.h>
using namespace Rcpp;

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/Polygon_with_holes_2.h>
#include <CGAL/create_straight_skeleton_from_polygon_with_holes_2.h>
#include <CGAL/create_offset_polygons_2.h>
#include "print.h"
#include <boost/shared_ptr.hpp>
#include <vector>
#include <cassert>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K ;
typedef K::Point_2                   Point ;
typedef CGAL::Polygon_2<K>           Polygon_2 ;
typedef CGAL::Straight_skeleton_2<K> Ss ;
typedef boost::shared_ptr<Polygon_2> PolygonPtr ;
typedef boost::shared_ptr<Ss> SsPtr ;
typedef std::vector<PolygonPtr> PolygonPtrVector ;
typedef CGAL::Polygon_with_holes_2<K> Polygon_with_holes ;

typedef typename Ss::Vertex_const_handle     Vertex_const_handle ;
typedef typename Ss::Halfedge_const_handle   Halfedge_const_handle ;
typedef typename Ss::Halfedge_const_iterator Halfedge_const_iterator ;

// [[Rcpp::export]]
List skeletonize_rcpp(NumericMatrix vertices,
                      List holes,
                      double offset) {
  Polygon_2 base_poly;
  for(size_t i = 0; i < vertices.rows(); i++) {
    base_poly.push_back(Point(vertices(i,0),vertices(i,1)));
  }
  if(!base_poly.is_counterclockwise_oriented()) {
    throw std::runtime_error("rayskeleton: Polygon is not CCW oriented.");
  }
  if(!base_poly.is_simple()) {
    throw std::runtime_error("rayskeleton: Polygon is not simple.");
  }
  Polygon_with_holes poly(base_poly);
  for(size_t i = 0; i < holes.length(); i++) {
    Polygon_2 new_hole;
    NumericMatrix hole = Rcpp::as<NumericMatrix>(holes(i));
    for(size_t i = 0; i < hole.rows(); i++) {
      new_hole.push_back(Point(hole(i,0),hole(i,1)));
    }
    if(!new_hole.is_clockwise_oriented()) {
      throw std::runtime_error("rayskeleton: Hole in polygon is not CW oriented.");
    }
    if(!new_hole.is_simple()) {
      throw std::runtime_error("rayskeleton: Hole in polygon is not simple.");
    }
    poly.add_hole(new_hole);
  }

  SsPtr ss = CGAL::create_interior_straight_skeleton_2(poly);
  double lOffset = offset ;

  int contours = 0;
  int bisectors = 0;


  for (Halfedge_const_iterator i = ss->halfedges_begin(); i != ss->halfedges_end(); ++i ) {
    if(!i->is_border()) {
      bisectors++;
    }
  }
  List bisector_list(bisectors);
  int bis_cntr = 0;

  for (Halfedge_const_iterator i = ss->halfedges_begin(); i != ss->halfedges_end(); ++i ) {
    if(!i->is_border()) {
      NumericVector temp = Rcpp::NumericVector::create(
        i->vertex()->point().x(),
        i->vertex()->point().y(),
        i->opposite()->vertex()->point().x(),
        i->opposite()->vertex()->point().y(),
        i->vertex()->time(),
        i->opposite()->vertex()->time(),
        i->vertex()->id(),
        i->opposite()->vertex()->id()
      );
      bisector_list(bis_cntr) = temp;
      bis_cntr++;
    }
  }

  List all_data = List::create(_["bisectors"] = bisector_list);
  return(all_data);
}
