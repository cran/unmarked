### R code from vignette source 'simulate.Rnw'

###################################################
### code chunk number 1: simulate.Rnw:1-3
###################################################
options(width=70)
options(continue=" ")


###################################################
### code chunk number 2: simulate.Rnw:99-100
###################################################
set.seed(123)


###################################################
### code chunk number 3: simulate.Rnw:103-107
###################################################
library(unmarked)
umf <- unmarkedFrameOccu(y=matrix(c(0,1,0,1,1,0,0,0,1), nrow=3))
mod <- occu(~1~1, umf)
names(mod)


###################################################
### code chunk number 4: simulate.Rnw:116-117
###################################################
forms <- list(state=~elev, det=~1)


###################################################
### code chunk number 5: simulate.Rnw:129-130
###################################################
coefs <- list(state=c(intercept=0, elev=-0.4), det=c(intercept=0))


###################################################
### code chunk number 6: simulate.Rnw:142-143
###################################################
design <- list(M=300, J=8) # 300 sites, 8 occasions per site


###################################################
### code chunk number 7: simulate.Rnw:152-153 (eval = FALSE)
###################################################
## simulate("occu", formulas=forms, design=design)


###################################################
### code chunk number 8: simulate.Rnw:156-157
###################################################
try(simulate("occu", formulas=forms, design=design))


###################################################
### code chunk number 9: simulate.Rnw:163-165
###################################################
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design)
head(occu_umf)


###################################################
### code chunk number 10: simulate.Rnw:171-172
###################################################
(occu(~1 ~elev, occu_umf))


###################################################
### code chunk number 11: simulate.Rnw:182-183
###################################################
guide <- list(elev=list(dist=rnorm, mean=2, sd=0.5))


###################################################
### code chunk number 12: simulate.Rnw:194-196
###################################################
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design, guide=guide)
head(occu_umf)


###################################################
### code chunk number 13: simulate.Rnw:203-206
###################################################
guide <- list(elev=list(dist=runif, min=0, max=1)) 
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design, guide=guide)
head(occu_umf)


###################################################
### code chunk number 14: simulate.Rnw:214-215
###################################################
forms2 <- list(state=~elev+landcover, det=~1)


###################################################
### code chunk number 15: simulate.Rnw:220-221
###################################################
guide <- list(landcover=factor(levels=c("forest","grass","urban")))


###################################################
### code chunk number 16: simulate.Rnw:230-233
###################################################
# forest is the reference level for landcover since it was listed first
coefs2 <- list(state=c(intercept=0, elev=-0.4, landcovergrass=0.2, 
                       landcoverurban=-0.7), det=c(intercept=0))


###################################################
### code chunk number 17: simulate.Rnw:236-237
###################################################
head(simulate("occu", formulas=forms2, coefs=coefs2, design=design, guide=guide))


###################################################
### code chunk number 18: simulate.Rnw:251-253
###################################################
coefs$alpha <- c(alpha=0.5)
head(simulate("pcount", formulas=forms, coefs=coefs, design=design, mixture="NB"))


###################################################
### code chunk number 19: simulate.Rnw:267-268
###################################################
forms <- list(lambda=~elev, dist=~1, rem=~wind)


###################################################
### code chunk number 20: simulate.Rnw:278-280
###################################################
coefs <- list(lambda=c(intercept=log(5), elev=0.7), 
              dist=c(intercept=log(50)), rem=c(intercept=-1, wind=-0.3))


###################################################
### code chunk number 21: simulate.Rnw:286-287
###################################################
design <- list(M = 300, Jdist=4, Jrem=5)


###################################################
### code chunk number 22: simulate.Rnw:294-297
###################################################
umf <- simulate("gdistremoval", formulas=forms, coefs=coefs, design=design,
                dist.breaks=c(0,25,50,75,100), keyfun="halfnorm", unitsIn="m")
head(umf)


###################################################
### code chunk number 23: simulate.Rnw:303-305
###################################################
(fit <- gdistremoval(lambdaformula=~elev, removalformula=~wind, 
                    distanceformula=~1, data=umf))


