# author: Kwame Okrah
# date: 2026-03-05

plot_plate_events = function(pinfos, is_gated=FALSE, fig_path=NULL) {
    
    pinfos_by_platename = split(pinfos, pinfos$Platename)
    tmax = max(pinfos$Result, na.rm=TRUE)
    
    if (!is.null(fig_path)) {
        pdf(fig_path, height = 2.85, width = 6.85)
    }

    op = par(mar=c(3.25, 3.25, 1.25, 0.75), mgp=c(2, 0.75, 0))
    
    if (is_gated) {
        par(mfrow=c(1, 2))
        for (pl in names(pinfos_by_platename)) {
            plate_df = pinfos_by_platename[[pl]]
            
            y = plate_df$Result
            x = plate_df$"Plate Position"
            z = plate_df$"/intact/singlet/viable"
            
            nr = 8; nc = 12
            lvls = paste0(rep(LETTERS[1:nr], each=nc), 1:nc)
            x = factor(x, levels = lvls)
            
            names(y) = x
            y = y[levels(x)]
            names(y) = levels(x)
            
            names(z) = x
            z = z[levels(x)]
            names(z) = levels(x)
            
            ylim = c(0, tmax)
            if (ylim[2] < 15000) y[2] = 15000

            plot(0, 0, pch="", xlim=c(0, 97), ylim=ylim,
                 xaxs="i", xaxt="n", xlab="Well ID", 
                 ylab="Number of Events",
                 main=pl)
            
            axis(1, at=1:length(y), names(y), cex.axis=0.9)
            abline(v=seq(12, 84, by=12) + 0.5, col="gray80")

            points(y, pch=19, col="black", cex=0.65)
            
            abline(h=c(2500, 5000, 10000), lty=c(1, 2, 1), 
            col=c("tomato", "black", "lightblue"))

            plot(0, 0, pch="", xlim=c(0, 97), ylim=ylim,
                 xaxs="i", xaxt="n", xlab="Well ID", 
                 ylab="Number of Events",
                 main="Viable Events")
            
            axis(1, at=1:length(y), names(y), cex.axis=0.9)
            abline(v=seq(12, 84, by=12) + 0.5, col="gray80")

            points(z, pch=19, col="steelblue", cex=0.65)
            
            abline(h=c(2500, 5000, 10000), lty=c(1, 2, 1), 
            col=c("tomato", "black", "lightblue"))
        }
        
    }else{
        
        for (pl in names(pinfos_by_platename)) {
            plate_df = pinfos_by_platename[[pl]]
            
            y = plate_df$Result
            x = plate_df$"Plate Position"
            
            nr = 8; nc = 12
            lvls = paste0(rep(LETTERS[1:nr], each=nc), 1:nc)
            x = factor(x, levels = lvls)
            
            names(y) = x
            y = y[levels(x)]
            names(y) = levels(x)
            
            ylim = c(0, tmax)
            if (ylim[2] < 15000) y[2] = 15000

            plot(0, 0, pch="", xlim=c(0, 97), ylim=ylim,
                 xaxs="i", xaxt="n", xlab="Well ID", 
                 ylab="Number of Events",
                 main=pl)
            
            axis(1, at=1:length(y), names(y), cex.axis=0.9)
            abline(v=seq(12, 84, by=12) + 0.5, col="gray80")
            
            points(y, pch=19, col="black", cex=0.65)
            
            abline(h=c(2500, 5000, 10000), lty=c(1, 2, 1), col=c("tomato", "black", "lightblue"))
        }
    }

    par(op)

    if (!is.null(fig_path)) {
        dev.off()
    }
}

