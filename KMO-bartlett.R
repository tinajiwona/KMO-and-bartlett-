install.packages('psych')
library(psych)
cortest.bartlett(airquality)
library(reshape2)
airquality
new1 <- data.frame(rnorm(100),runif(100))
new1
####KMO检验
install.packages('MASS')
library(MASS)
kmo <- function(data){
  library(MASS)
  X <- cor(as.matrix(data))###
  iX <- ginv(X)###矩阵翻转
  S2 <- diag(diag(iX^-1))###取对角线
  AIS <- S2%*%iX%*%S2##反图像协方差
  IS <- X+AIS-2*S2  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
  # correlation matrix. That is the
  # negative of the partial correlations,
  # partialling out all other variables.
  
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  
  # Reporting the conclusion
  if (kmo >= 0.00 && kmo < 0.50){
    test <- 'The KMO test yields a degree of common variance
unacceptable for FA.'
  } else if (kmo >= 0.50 && kmo < 0.60){
    test <- 'The KMO test yields a degree of common variance miserable.'
  } else if (kmo >= 0.60 && kmo < 0.70){
    test <- 'The KMO test yields a degree of common variance mediocre.'
  } else if (kmo >= 0.70 && kmo < 0.80){
    test <- 'The KMO test yields a degree of common variance middling.'
  } else if (kmo >= 0.80 && kmo < 0.90){
    test <- 'The KMO test yields a degree of common variance meritorious.'
  } else {
    test <- 'The KMO test yields a degree of common variance marvelous.'
  }
  
  ans <- list(  overall = kmo,
                report = test,
                individual = MSA,
                AIS = AIS,
                AIR = AIR )
  return(ans)
  
}    # end of kmo()
kmo(new1)
}