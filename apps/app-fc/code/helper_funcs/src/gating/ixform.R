# Author: Kwame Okrah
# Date: 2025-03-21
# Purpose: inverse bi-exponential transform

# input:  numeric vector, matrix, or data frame
# output: same type as input

ixform <- function(y) {
  I   <- (abs(y) < 1) * 1L
  res <- I * (y * 10) + (1 - I) * sign(y) * (10^abs(y) - 1)
  res
}
