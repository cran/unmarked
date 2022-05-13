## -----------------------------------------------------------------------------
set.seed(123)
library(unmarked)
umf <- unmarkedFrameOccu(y=matrix(c(0,1,0,1,1,0,0,0,1), nrow=3))
mod <- occu(~1~1, umf)
names(mod)

## -----------------------------------------------------------------------------
forms <- list(state=~elev, det=~1)

## -----------------------------------------------------------------------------
coefs <- list(state=c(intercept=0, elev=-0.4), det=c(intercept=0))

## -----------------------------------------------------------------------------
design <- list(M=300, J=8) # 300 sites, 8 occasions per site

## ---- eval=FALSE--------------------------------------------------------------
#  simulate("occu", formulas=forms, design=design)

## ---- echo=FALSE--------------------------------------------------------------
try(simulate("occu", formulas=forms, design=design))

## -----------------------------------------------------------------------------
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design)
head(occu_umf)

## -----------------------------------------------------------------------------
(occu(~1 ~elev, occu_umf))

## -----------------------------------------------------------------------------
guide <- list(elev=list(dist=rnorm, mean=2, sd=0.5))

## -----------------------------------------------------------------------------
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design, guide=guide)
head(occu_umf)

## -----------------------------------------------------------------------------
guide <- list(elev=list(dist=runif, min=0, max=1)) 
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design, guide=guide)
head(occu_umf)

## -----------------------------------------------------------------------------
forms2 <- list(state=~elev+landcover, det=~1)

## -----------------------------------------------------------------------------
guide <- list(landcover=factor(levels=c("forest","grass","urban")))

## -----------------------------------------------------------------------------
# forest is the reference level for landcover since it was listed first
coefs2 <- list(state=c(intercept=0, elev=-0.4, landcovergrass=0.2, 
                       landcoverurban=-0.7), det=c(intercept=0))

## -----------------------------------------------------------------------------
head(simulate("occu", formulas=forms2, coefs=coefs2, design=design, guide=guide))

## -----------------------------------------------------------------------------
coefs$alpha <- c(alpha=0.5)
head(simulate("pcount", formulas=forms, coefs=coefs, design=design, mixture="NB"))

## -----------------------------------------------------------------------------
forms <- list(lambda=~elev, dist=~1, rem=~wind)

## -----------------------------------------------------------------------------
coefs <- list(lambda=c(intercept=log(5), elev=0.7), 
              dist=c(intercept=log(50)), rem=c(intercept=-1, wind=-0.3))

## -----------------------------------------------------------------------------
design <- list(M = 300, Jdist=4, Jrem=5)

## -----------------------------------------------------------------------------
umf <- simulate("gdistremoval", formulas=forms, coefs=coefs, design=design,
                dist.breaks=c(0,25,50,75,100), keyfun="halfnorm", unitsIn="m")
head(umf)

## -----------------------------------------------------------------------------
(fit <- gdistremoval(lambdaformula=~elev, removalformula=~wind, 
                    distanceformula=~1, data=umf))

