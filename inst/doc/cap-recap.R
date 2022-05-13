## ----echo=FALSE---------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
tab <- data.frame(
  id = c("GB","YR","RO","PP","GY","PR"),
  site = c(rep("A",4), "B", "B"),
  cap = c("101","101","111","100","100","010")
)
names(tab) <- c("Animal ID", "Site", "Capture history")

knitr::kable(tab, align="lcc",
  caption="Table 1. Capture-recapture data for 6 individuals sampled on 3 occasions"
)

## ---- echo=FALSE--------------------------------------------------------------
tab2 <- data.frame(
  Site=c("A","B","C","D"),
  eh100=c(1,1,0,0), eh010=c(0,1,0,0), eh001=c(0,0,0,0),
  eh110=c(0,0,0,0), eh011=c(0,0,0,0), eh101=c(2,0,0,0),
  eh111=c(1,0,0,0))
names(tab2) <- c("Site", "100","010","001","110","011","101","111")

knitr::kable(tab2, align="lccccccc",
  caption="Table 2. Capture-recapture data from Table 1 in the format required by unmarked")

## -----------------------------------------------------------------------------
alfl <- read.csv(system.file("csv", "alfl.csv", package="unmarked"))
head(alfl, 5)

## -----------------------------------------------------------------------------

alfl.covs <- read.csv(system.file("csv", "alflCovs.csv",
    package="unmarked"), row.names=1)
head(alfl.covs)

## -----------------------------------------------------------------------------
alfl$captureHistory <- paste(alfl$interval1, alfl$interval2, alfl$interval3, sep="")
alfl$captureHistory <- factor(alfl$captureHistory,
    levels=c("001", "010", "011", "100", "101", "110", "111"))
## Don't do this:
#levels(alfl$id) <- rownames(alfl.covs)
alfl$id <- factor(alfl$id, levels=rownames(alfl.covs))

## -----------------------------------------------------------------------------
alfl.v1 <- alfl[alfl$survey==1,]
alfl.H1 <- table(alfl.v1$id, alfl.v1$captureHistory)
head(alfl.H1, 5)

## -----------------------------------------------------------------------------
crPiFun <- function(p) {
    p1 <- p[,1]
    p2 <- p[,2]
    p3 <- p[,3]
    cbind("001" = (1-p1) * (1-p2) * p3,
          "010" = (1-p1) * p2     * (1-p3),
          "011" = (1-p1) * p2     * p3,
          "100" = p1     * (1-p2) * (1-p3),
          "101" = p1     * (1-p2) * p3,
          "110" = p1     * p2     * (1-p3),
          "111" = p1     * p2     * p3)
}

## -----------------------------------------------------------------------------
p <- matrix(0.4, 2, 3)
crPiFun(p)
rowSums(crPiFun(p))

## -----------------------------------------------------------------------------
o2y <- matrix(1, 3, 7)

## -----------------------------------------------------------------------------
library(unmarked)
intervalMat <- matrix(c('1','2','3'), 50, 3, byrow=TRUE)
class(alfl.H1) <- "matrix"
umf.cr1 <- unmarkedFrameMPois(y=alfl.H1,
    siteCovs=alfl.covs[,c("woody", "struct", "time.1", "date.1")],
    obsCovs=list(interval=intervalMat),
    obsToY=o2y, piFun="crPiFun")

## -----------------------------------------------------------------------------
M0 <- multinomPois(~1 ~1, umf.cr1, engine="R")
Mt <- multinomPois(~interval-1 ~1, umf.cr1, engine="R")
Mx <- multinomPois(~time.1 ~1, umf.cr1, engine="R")

## -----------------------------------------------------------------------------
(M0.woody <- multinomPois(~1 ~woody, umf.cr1, engine="R"))

## ---- fig.width=5, fig.height=5-----------------------------------------------
nd <- data.frame(woody=seq(0, 0.8, length=50))
E.abundance <- predict(M0.woody, type="state", newdata=nd, appendData=TRUE)
plot(Predicted ~ woody, E.abundance, type="l", ylim=c(0, 6),
     ylab="Alder flycatchers / plot", xlab="Woody vegetation cover")
lines(lower ~ woody, E.abundance, col=gray(0.7))
lines(upper ~ woody, E.abundance, col=gray(0.7))

## -----------------------------------------------------------------------------
backTransform(M0.woody, type="det")

## -----------------------------------------------------------------------------
round(getP(M0.woody), 2)[1,]

## -----------------------------------------------------------------------------
crPiFun.Mb <- function(p) { # p should have 3 columns
    pNaive <- p[,1]
    pWise <- p[,3]
    cbind("001" = (1-pNaive) * (1-pNaive) * pNaive,
          "010" = (1-pNaive) * pNaive     * (1-pWise),
          "011" = (1-pNaive) * pNaive     * pWise,
          "100" = pNaive     * (1-pWise)  * (1-pWise),
          "101" = pNaive     * (1-pWise)  * pWise,
          "110" = pNaive     * pWise      * (1-pWise),
          "111" = pNaive     * pWise      * pWise)
}

## -----------------------------------------------------------------------------
behavior <- matrix(c('Naive','Naive','Wise'), 50, 3, byrow=TRUE)
umf.cr1Mb <- unmarkedFrameMPois(y=alfl.H1,
    siteCovs=alfl.covs[,c("woody", "struct", "time.1")],
    obsCovs=list(behavior=behavior),
    obsToY=o2y, piFun="crPiFun.Mb")
M0 <- multinomPois(~1 ~1, umf.cr1Mb, engine="R")

## -----------------------------------------------------------------------------
(Mb <- multinomPois(~behavior-1 ~1, umf.cr1Mb, engine="R"))

## -----------------------------------------------------------------------------
plogis(confint(Mb, type="det", method="profile"))

## -----------------------------------------------------------------------------
MhPiFun <- function(p) {
mu <- qlogis(p[,1]) # logit(p)
sig <- exp(qlogis(p[1,2]))
J <- ncol(p)
M <- nrow(p)
il <- matrix(NA, nrow=M, ncol=7)
dimnames(il) <- list(NULL, c("001","010","011","100","101","110","111"))
for(i in 1:M) {
  il[i,1] <- integrate( function(x) {
    (1-plogis(mu[i]+x))*(1-plogis(mu[i]+x))*plogis(mu[i]+x)*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
  il[i,2] <- integrate( function(x) {
    (1-plogis(mu[i]+x))*plogis(mu[i]+x)*(1-plogis(mu[i]+x))*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
  il[i,3] <- integrate( function(x) {
    (1-plogis(mu[i]+x))*plogis(mu[i]+x)*plogis(mu[i]+x)*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
  il[i,4] <- integrate( function(x) {
    plogis(mu[i]+x)*(1-plogis(mu[i]+x))*(1-plogis(mu[i]+x))*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
  il[i,5] <- integrate( function(x) {
    plogis(mu[i]+x)*(1-plogis(mu[i]+x))*plogis(mu[i]+x)*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
  il[i,6] <- integrate( function(x) {
    plogis(mu[i]+x)*plogis(mu[i]+x)*(1-plogis(mu[i]+x))*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
  il[i,7] <- integrate( function(x) {
    plogis(mu[i]+x)*plogis(mu[i]+x)*plogis(mu[i]+x)*dnorm(x,0,sig)
  }, lower=-Inf, upper=Inf, stop.on.error=FALSE)$value
}
return(il)
}

## ---- eval=FALSE--------------------------------------------------------------
#  parID <- matrix(c('p','sig','sig'), 50, 3, byrow=TRUE)
#  umf.cr2 <- unmarkedFrameMPois(y=alfl.H1,
#    siteCovs=alfl.covs[,c("woody", "struct", "time.1")],
#    obsCovs=list(parID=parID),
#    obsToY=o2y, piFun="MhPiFun")
#  multinomPois(~parID-1 ~woody, umf.cr2)

## -----------------------------------------------------------------------------
alfl.H <- table(alfl$id, alfl$captureHistory, alfl$survey)
alfl.Hmat <- cbind(alfl.H[,,1], alfl.H[,,2], alfl.H[,,3])
nVisits <- 3
o2yGMM <- kronecker(diag(nVisits), o2y)
umf.cr <- unmarkedFrameGMM(y=alfl.Hmat,
    siteCovs=alfl.covs[,c("woody", "struct")],
    yearlySiteCovs=list(date=alfl.covs[,3:5], time=alfl.covs[,6:8]),
    obsCovs=list(interval=cbind(intervalMat,intervalMat,intervalMat)),
    obsToY=o2yGMM, piFun="crPiFun", numPrimary=nVisits)

## -----------------------------------------------------------------------------
(fm1 <- gmultmix(~woody, ~1, ~time+date, umf.cr, engine="R"))

