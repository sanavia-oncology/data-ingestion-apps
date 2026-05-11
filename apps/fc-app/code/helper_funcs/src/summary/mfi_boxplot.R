# Kwame Okrah
# 2026-03-15

mfi_boxplot = function(x) {
  op = par(mar=c(4, 1, 1.5, 1), mgp=c(1.85, 0.65, 0), lwd=1.5)

  xj = jitter(rep(0.5, length(x)), amount = 0.5)

  main = paste0("MFI for each sample (N=", length(x), ")")
  plot(x, xj, ylim=c(-0.1, 1.1), xlab = "Log10 MFI",
      col = "steelblue", pch=19, ylab="", cex=0.85,
      main=main, yaxt="n")

  par(op)
}
