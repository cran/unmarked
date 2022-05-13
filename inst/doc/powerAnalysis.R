## ---- warning=FALSE-----------------------------------------------------------
set.seed(123)
library(unmarked)
data(crossbill)

umf <- unmarkedFrameOccu(y=crossbill[,11:13], 
                         siteCovs=data.frame(elev=scale(crossbill$ele)))
(mod <- occu(~1~elev, umf))

## -----------------------------------------------------------------------------
z = sqrt_w = coef(mod)[2] / SE(mod)[2]
2*pnorm(abs(z), lower.tail=FALSE)

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
forms2 <- list(state=~elev+landcover, det=~1)
guide <- list(landcover=factor(levels=c("forest","grass")), # landcover is factor 
              elev=list(dist=rnorm, mean=2, sd=0.5)) # custom distribution

## -----------------------------------------------------------------------------
coefs2 <- list(state=c(intercept=0, elev=-0.4, landcovergrass=0.2), det=c(intercept=0))

## -----------------------------------------------------------------------------
head(simulate("occu", formulas=forms2, coefs=coefs2, design=design, guide=guide))

## -----------------------------------------------------------------------------
coefs$alpha <- c(alpha=0.5)
head(simulate("pcount", formulas=forms, coefs=coefs, design=design, mixture="NB"))

## -----------------------------------------------------------------------------
template_model <- occu(~1~elev, occu_umf)

## ---- eval=FALSE--------------------------------------------------------------
#  powerAnalysis(template_model)

## ---- echo=FALSE--------------------------------------------------------------
try(powerAnalysis(template_model))

## -----------------------------------------------------------------------------
effect_sizes <- list(state=c(intercept=0, elev=-0.4), det=c(intercept=0))

## -----------------------------------------------------------------------------
(pa <- powerAnalysis(template_model, coefs=effect_sizes, alpha=0.05, nsim=20))

## ---- eval=FALSE--------------------------------------------------------------
#  pa@estimates

## -----------------------------------------------------------------------------
# 50 sites and 3 obs per site
(pa2 <- powerAnalysis(template_model, effect_sizes, design=list(M=50, J=3), nsim=20))

## -----------------------------------------------------------------------------
(pa3 <- powerAnalysis(template_model, effect_sizes, design=list(M=400, J=4), nsim=20))

## -----------------------------------------------------------------------------
unmarkedPowerList(list(pa, pa2, pa3))

## -----------------------------------------------------------------------------
scenarios <- expand.grid(M=c(50,200,400),
                         J=c(3,5,8))
pl <- unmarkedPowerList(template_model, effect_sizes, design=scenarios, nsim=20)
head(summary(pl))
tail(summary(pl))

## ---- fig.height=5------------------------------------------------------------
plot(pl, power=0.8, param="elev")

