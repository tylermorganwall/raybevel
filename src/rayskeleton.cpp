#include <Rcpp.h>
using namespace Rcpp;

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Polygon_2.h>
#include <CGAL/Polygon_with_holes_2.h>
#include <CGAL/Polygon_set_2.h>
#include <CGAL/create_straight_skeleton_from_polygon_with_holes_2.h>
#include <CGAL/create_offset_polygons_2.h>
#include <CGAL/Boolean_set_operations_2/Gps_polygon_validation.h>
#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/General_polygon_set_2.h>
#include "print.h"
#include <boost/shared_ptr.hpp>
#include <vector>
#include <cassert>


#define is_inside(os) ((os == CGAL::ON_ORIENTED_BOUNDARY) ? true :  \
(os == CGAL::POSITIVE) ? true : false)

typedef CGAL::Exact_predicates_inexact_constructions_kernel K ;
typedef K::Point_2                   Point ;
typedef CGAL::Polygon_2<K>           Polygon_2 ;
// typedef CGAL::Polygon_set_2<K>       PolygonSet;
typedef CGAL::Straight_skeleton_2<K> Ss ;
typedef boost::shared_ptr<Polygon_2> PolygonPtr ;
typedef boost::shared_ptr<Ss> SsPtr ;
typedef std::vector<PolygonPtr> PolygonPtrVector ;
typedef CGAL::Polygon_with_holes_2<K> Polygon_with_holes ;
typedef CGAL::Gps_segment_traits_2<K> Traits_2;


typedef typename Ss::Vertex_const_handle     Vertex_const_handle ;
typedef typename Ss::Halfedge_const_handle   Halfedge_const_handle ;
typedef typename Ss::Halfedge_const_iterator Halfedge_const_iterator ;

// [[Rcpp::export]]
bool is_ccw_polygon(NumericMatrix vertices) {
  Polygon_2 base_poly;
  for(int i = 0; i < vertices.rows(); i++) {
    base_poly.push_back(Point(vertices(i,0),vertices(i,1)));
  }
  if(base_poly.is_counterclockwise_oriented()) {
    return(true);
  } else {
    return(false);
  }
}

// [[Rcpp::export]]
bool is_simple_polygon(NumericMatrix vertices) {
  Polygon_2 base_poly;
  for(int i = 0; i < vertices.rows(); i++) {
    base_poly.push_back(Point(vertices(i,0),vertices(i,1)));
  }
  if(base_poly.is_simple()) {
    return(true);
  } else {
    return(false);
  }
}

// [[Rcpp::export]]
List skeletonize_rcpp(NumericMatrix vertices,
                      List holes,
                      double offset) {
  Polygon_2 base_poly;
  for(int i = 0; i < vertices.rows(); i++) {
    // Rcpp::Rcout << "Vert: " << vertices(i,0) << " " << vertices(i,1) << "\n";

    base_poly.push_back(Point(vertices(i,0),vertices(i,1)));
  }
  if(!base_poly.is_counterclockwise_oriented()) {
    throw std::runtime_error("raybevel: Polygon is not CCW oriented.");
  }
  if(!base_poly.is_simple()) {
    throw std::runtime_error("raybevel: Polygon is not simple.");
  }
  Polygon_with_holes poly(base_poly);

  for(int i = 0; i < holes.length(); i++) {
    Polygon_2 new_hole;
    bool all_inside = true;
    NumericMatrix hole = Rcpp::as<NumericMatrix>(holes(i));
    for(int i = 0; i < hole.rows(); i++) {
      Point tmp_point(hole(i,0),hole(i,1));
      if(!is_inside(base_poly.oriented_side(tmp_point))) {
        all_inside = false;
        break;
      }
      new_hole.push_back(tmp_point);
    }
    if(all_inside) {
      // if(!new_hole.is_clockwise_oriented()) {
      //   throw std::runtime_error("rayskeleton: Hole in polygon is not CW oriented.");
      // }
      // if(!new_hole.is_simple()) {
      //   throw std::runtime_error("rayskeleton: Hole in polygon is not simple.");
      // }
      poly.add_hole(new_hole);
    }
  }
  // auto trait = PolygonSet::Traits_2();
  // Traits_2 trait_val;
  // if(!is_closed_polygon_with_holes(poly, trait_val)) {
  //   throw std::runtime_error("rayskeleton: Isn't closed polygon with holes.");
  // }
  //
  // if(!has_valid_orientation_polygon_with_holes(poly, trait_val)) {
  //   throw std::runtime_error("rayskeleton: Polygon doesn't have valid orientation.");
  // }
  // if(!are_holes_and_boundary_pairwise_disjoint(poly, trait_val)) {
  //   throw std::runtime_error("rayskeleton: Holes either intersect themselves or intersect with the outer boundary.");
  // }

  SsPtr ss;
  try {
    ss = CGAL::create_interior_straight_skeleton_2(poly);
  } catch (...) {
    throw std::runtime_error("unknown cgal error occurred");
  };
  // double lOffset = offset ;

  // int contours = 0;
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
