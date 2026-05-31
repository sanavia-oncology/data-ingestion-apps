# Author: Kwame Okrah
# Date: 2025-03-21
# Purpose: implement simple bi-exponential (log10) transform

# input:  numeric vector, matrix, or data frame
# output: same type as input

xform <- function(x) {
  I   <- (abs(x) < 10) * 1L
  res <- I * (x / 10) + (1 - I) * sign(x) * log10(abs(x) + 1)
  res
}
