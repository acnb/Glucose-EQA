library(boot)
library(robustbase)

getSfromAlgA <- function(x){
  huberM(x)$s
}

getMufromAlgA <- function(x){
  huberM(x)$mu
}

sFromAWrapper <- function(d, i){
  getSfromAlgA(d[i])
}

getStErrorForS <- function(x){
  b <- boot(x, sFromAWrapper, R= 2000)
  sd(b$t, na.rm=T)
}

getMaxDiff <- function(x, idx){
  ag <- aggregate(value~lot, data = x, subset=idx, FUN=getMufromAlgA)
  (max(ag$value, na.rm = T)  - min(ag$value, na.rm=T))/x$target[1]
}

quantilesFromA <- function(x){
  btrp <- boot(x, statistic = getMaxDiff, strata = x$lot, R=2000)
  ci <- boot.ci(btrp, type = "bca")
  data.frame(p025 = ci$bca[1, 4], all = btrp$t0, p975 = ci$bca[1, 5]) 
}

