# Kwame Okrah

events_boxplot = function(x, y=NULL) {
  op = par(mar=c(4, 1, 1.5, 1), mgp=c(1.85, 0.65, 0), lwd=1.5)

  ylim = c(0, max(x, na.rm = T))
  if (ylim[2] < 15000) y[2] = 15000
  
  if (is.null(y)) {
      main = paste0("Summary statistics on all events (N=",
                    length(x), ")")
    
      boxplot(x, horizontal = T, ylim=ylim, xlab="Total Events from all Plates",
             main = main, cex.main=1.2)
    
      abline(v=c(2500, 5000, 10000), lty=c(1, 2, 1), col=c("tomato", "black", "lightblue"))
  }else{
      main = paste0("Summary statistics on all events (N=",
                    length(x), ")")
    
      boxplot(x, horizontal = T, ylim=ylim, xlab="Total Events from all Plates",
             main = main, cex.main=1.2)
    
      boxplot(y, horizontal = T, add=T, col="steelblue", axes=FALSE,
              ylab="", xlab="", border = "blue4")
    
      abline(v=c(2500, 5000, 10000), lty=c(1, 2, 1), col=c("tomato", "black", "lightblue"))
  }

  par(op)
}

