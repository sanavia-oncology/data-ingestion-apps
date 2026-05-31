# Author: Kwame Okrah
# Date: 2026-03-07

# =============================================================================
# points_in_polygon()
#
# Test whether a set of 2D points lie inside a polygon.
# Uses the ray-casting algorithm (Jordan curve theorem).
#
# inputs:
#   poly   : Mx2 matrix of polygon vertices (x, y), in order
#   points : Nx2 matrix or data frame of points to test (col 1 = x, col 2 = y)
#
# output:
#   logical vector of length N — TRUE if the point is inside the polygon
# =============================================================================

points_in_polygon <- function(poly, points) {
  px <- points[,1]   # x coords of points
  py <- points[,2]   # y coords of points
  vx <- poly[,1]     # x coords of polygon vertices
  vy <- poly[,2]     # y coords of polygon vertices

  n      <- length(vx)
  inside <- rep(FALSE, length(px))
  j      <- n   # start with last vertex paired against first

  for (i in seq_len(n)) {
    xi <- vx[i];  yi <- vy[i]
    xj <- vx[j];  yj <- vy[j]

    # does ray from point cross the edge i-j?
    crosses <- ((yi > py) != (yj > py)) &
               (px < (xj - xi) * (py - yi) / (yj - yi) + xi)

    inside <- xor(inside, crosses)
    j <- i
  }

  inside
}
