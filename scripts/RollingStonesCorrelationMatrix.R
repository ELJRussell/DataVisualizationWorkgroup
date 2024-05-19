library(here)
library(corrplot)
library(RColorBrewer)
library(tidyverse)

RollingStone500 <- read_csv(here("data", "RollingStones.csv"))

Rollingstonescorrelation <- cor(RollingStone500[, 7:15])

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(RollingStone500[, 7:15])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(Rollingstonescorrelation, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05,
         pch.col="gray",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         title="Rolling Stone's 500 Greatest Songs of All Time",
         mar=c(0,0,1,0))
