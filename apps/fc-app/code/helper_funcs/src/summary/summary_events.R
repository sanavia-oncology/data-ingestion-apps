# author: Kwame Okrah
# date: 2026-03-05

summary_events = function(x, nam="Events") {
    x = x[!is.na(x)]
    
    y = t(round(summary(x)))
    y = y[,c("Min.", "Median", "Max."),drop=F]
    colnames(y) = c("Minimum", "Median", "Max")
    
    a = ifelse(x > 10000, "10000+",
               ifelse(x > 5000, "5000+",
                      ifelse(x > 2500, "2500+", "2500-")))
    
    a = factor(a, levels = rev(c("2500-", "2500+", "5000+", "10000+")))
    
    tab = t(table(a))
    
    res = c()
    for (k in 1:ncol(tab)) {
        res = cbind(res, "|"="|", tab[,k,drop=F])
    }
    for (k in 1:ncol(y)) {
        res = cbind(res, "|"="|", y[,k,drop=F])
    }

    res = cbind(res, "|"="|")
    res = as.data.frame(res)
    rownames(res) = nam
    
    return(res)
}

summary_total_events = function(pinfos) {
    a = summary_events(pinfos$Result, "Total Events")
    z = cbind("Total Events", a)
    rownames(z) = NULL
    colnames(z)[1] = ""
    z
}

summary_viable_events = function(results_table) {
    a = summary_events(results_table$Result, "Total Events")
    b = summary_events(results_table$"/intact/singlet/viable", "Viable Events")
    z = cbind(c("Total Events", "Viable Events"), rbind(a, b))
    rownames(z) = NULL
    colnames(z)[1] = ""
    z
}
