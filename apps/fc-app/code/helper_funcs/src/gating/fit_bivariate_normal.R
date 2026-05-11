# Author: Kwame Okrah
# Date: 2026-03-07

# =============================================================================
# fit_bivariate_normal()
#
# Fits a bivariate normal density to 2D data using maximum likelihood.
# MLE for the bivariate normal has closed-form solutions.
#
# input:
#   x : numeric matrix or data frame with 2 columns (n x 2)
#
# output: a list with
#   mu    : length-2 vector of means (mu_x, mu_y)
#   sigma : 2x2 covariance matrix
#   rho   : correlation coefficient
#   sd    : length-2 vector of standard deviations
# =============================================================================

fit_bivariate_normal <- function(x) {
    x  <- as.matrix(x)
    stopifnot(ncol(x) == 2)
    
    n  <- nrow(x)
    
    # ── MLE estimates (closed form) ───────────────────────────────────────────
    mu    <- colMeans(x)                          # means
    sigma <- cov(x) * (n - 1) / n                # MLE covariance (divide by n)
    sd    <- sqrt(diag(sigma))                    # std deviations
    rho   <- sigma[1, 2] / prod(sd)              # correlation
    
    list(mu = mu, sigma = sigma, rho = rho, sd = sd)
}
