# Author: Kwame Okrah
# Date: 2026-03-07

generate_intact_verts = function(pts) {
    
    result <- tryCatch({
        fit_bvnorm = fit_bivariate_normal_robust(pts, trim = 0.8)
        
        probs = c(0.05, 0.95)
        qx <- qnorm(probs, mean = fit_bvnorm$mu[1], sd = sqrt(fit_bvnorm$sigma[1,1]))
        qy <- qnorm(probs, mean = fit_bvnorm$mu[2], sd = sqrt(fit_bvnorm$sigma[2,2]))
        
        qx[qx < 0] = 0
        qy[qy < 0] = 0
        
        verts = rbind("A"=c(qx[1], qy[1]),
                      "B"=c(qx[2], qy[1]),
                      "C"=c(qx[2], qy[2]),
                      "D"=c(qx[1], qy[2]))
        colnames(verts) = c("x", "y")
        attr(verts, "method") = "auto"
        
        verts
        
    }, error = function(e) {
        verts = rbind("A"=c(107422.7, 0.0),
                      "B"=c(882256.2, 0.0),
                      "C"=c(882256.2, 627644.9),
                      "D"=c(107422.7, 627644.9))
        colnames(verts) = c("x", "y")
        attr(verts, "method") = "manual"
        
        verts
    })
    
    return(result)
}


generate_singlet_verts = function(pts) {
    result <- tryCatch({
        hold = lowess(pts[,2], pts[,1])
        
        fit_bvnorm = fit_bivariate_normal(cbind(hold$y, hold$x))
        
        EX = fit_bvnorm$mu[1]
        EY = fit_bvnorm$mu[2]
        SR = (sqrt(fit_bvnorm$sigma[2,2])/sqrt(fit_bvnorm$sigma[1,1]))
        Rho = fit_bvnorm$rho
        
        mfit = function(x) {
            EY + Rho * SR * (x - EX) 
        }
        
        a1 = min(pts[,1])
        a2 = mfit(a1)
        
        probs = 0.999
        b1 = qnorm(probs, mean = fit_bvnorm$mu[1], sd = sqrt(fit_bvnorm$sigma[1,1]))
        b2 = mfit(b1)
        
        shift = IQR(pts[,2] - mfit(pts[,1])) * 1.5
        
        A = c(a1, a2 - shift)
        B = c(b1, b2 - shift)
        C = c(b1, b2 + shift)
        D = c(a1, a2 + shift)
        
        verts = rbind(A, B, C, D)
        colnames(verts) = c("x", "y")
        attr(verts, "method") = "auto"
        
        verts
        
    }, error = function(e) {
        verts = rbind("A"=c(108079.0, 62823.83),
                      "B"=c(720052.9, 380813.59),
                      "C"=c(720052.9, 481123.20),
                      "D"=c(108079.0, 163133.43))
        colnames(verts) = c("x", "y")
        attr(verts, "method") = "manual"
        
        verts
    })
    
    return(result)
}


generate_viable_verts = function(pts) {
    result <- tryCatch({
        x = pts[,1]
        
        mx = mean(x)
        sdx = sd(x)
        
        a1 = mx - sdx*2.25
        a2 = 0
        
        b1 = mx + sdx*1.25
        b2 = 0
        
        c1 = b1
        c2 = 8e5
        
        d1 = a1
        d2 = c2
        
        verts = rbind(A=c(a1, a2), B=c(b1, b2), C=c(c1, c2), D=c(d1, d2))
        colnames(verts) = c("x", "y")
        attr(verts, "method") = "auto"
        
        verts
        
    }, error = function(e) {
        verts = rbind("A"=c(1.360837, 0e+00),
                      "B"=c(3.511222, 0e+00),
                      "C"=c(3.511222, 8e+05),
                      "D"=c(1.360837, 8e+05))
        colnames(verts) = c("x", "y")
        attr(verts, "method") = "manual"
        
        verts
    })
    
    return(result)
}
