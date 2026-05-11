# Author: Kwame Okrah
# Date: 2026-03-30

fit_bivariate_normal_robust <- function(x, trim = 0.90) {
    x <- as.matrix(x)
    stopifnot(ncol(x) == 2, trim > 0, trim <= 1)

    # ── Mahalanobis-based outlier trimming ────────────────────────────────────
    # Use median / Spearman-seeded estimates for an initial robust distance
    mu0    <- apply(x, 2, median)
    sds0   <- apply(x, 2, function(v) median(abs(v - median(v))) / 0.6745)
    rho0   <- cor(x, method = "spearman")[1, 2]
    sigma0 <- matrix(c(sds0[1]^2,
                       rho0 * prod(sds0),
                       rho0 * prod(sds0),
                       sds0[2]^2), 2, 2)

    d2  <- mahalanobis(x, mu0, sigma0)
    cut <- quantile(d2, trim)
    x   <- x[d2 <= cut, , drop = FALSE]

    # ── MLE on trimmed data ───────────────────────────────────────────────────
    n     <- nrow(x)
    mu    <- colMeans(x)
    sigma <- cov(x) * (n - 1) / n
    sds   <- sqrt(diag(sigma))
    rho   <- cov2cor(sigma)[1, 2]

    list(mu = mu, sigma = sigma, rho = rho, sd = sds)
}