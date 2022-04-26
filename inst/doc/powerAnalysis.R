### R code from vignette source 'powerAnalysis.Rnw'

###################################################
### code chunk number 1: powerAnalysis.Rnw:1-3
###################################################
options(width=70)
options(continue=" ")


###################################################
### code chunk number 2: powerAnalysis.Rnw:66-67
###################################################
set.seed(123)


###################################################
### code chunk number 3: powerAnalysis.Rnw:70-76
###################################################
library(unmarked)
data(crossbill)

umf <- unmarkedFrameOccu(y=crossbill[,11:13], 
                         siteCovs=data.frame(elev=scale(crossbill$ele)))
(mod <- occu(~1~elev, umf))


###################################################
### code chunk number 4: powerAnalysis.Rnw:99-101
###################################################
z = sqrt_w = coef(mod)[2] / SE(mod)[2]
2*pnorm(abs(z), lower.tail=FALSE)


###################################################
### code chunk number 5: powerAnalysis.Rnw:174-175
###################################################
forms <- list(state=~elev, det=~1)


###################################################
### code chunk number 6: powerAnalysis.Rnw:184-185
###################################################
coefs <- list(state=c(intercept=0, elev=-0.4), det=c(intercept=0))


###################################################
### code chunk number 7: powerAnalysis.Rnw:192-193
###################################################
design <- list(M=300, J=8) # 300 sites, 8 occasions per site


###################################################
### code chunk number 8: powerAnalysis.Rnw:200-201 (eval = FALSE)
###################################################
## simulate("occu", formulas=forms, design=design)


###################################################
### code chunk number 9: powerAnalysis.Rnw:204-205
###################################################
try(simulate("occu", formulas=forms, design=design))


###################################################
### code chunk number 10: powerAnalysis.Rnw:211-213
###################################################
occu_umf <- simulate("occu", formulas=forms, coefs=coefs, design=design)
head(occu_umf)


###################################################
### code chunk number 11: powerAnalysis.Rnw:225-228
###################################################
forms2 <- list(state=~elev+landcover, det=~1)
guide <- list(landcover=factor(levels=c("forest","grass")), # landcover is factor 
              elev=list(dist=rnorm, mean=2, sd=0.5)) # custom distribution


###################################################
### code chunk number 12: powerAnalysis.Rnw:233-234
###################################################
coefs2 <- list(state=c(intercept=0, elev=-0.4, landcovergrass=0.2), det=c(intercept=0))


###################################################
### code chunk number 13: powerAnalysis.Rnw:237-238
###################################################
head(simulate("occu", formulas=forms2, coefs=coefs2, design=design, guide=guide))


###################################################
### code chunk number 14: powerAnalysis.Rnw:250-252
###################################################
coefs$alpha <- c(alpha=0.5)
head(simulate("pcount", formulas=forms, coefs=coefs, design=design, mixture="NB"))


###################################################
### code chunk number 15: powerAnalysis.Rnw:266-267
###################################################
template_model <- occu(~1~elev, occu_umf)


###################################################
### code chunk number 16: powerAnalysis.Rnw:272-273 (eval = FALSE)
###################################################
## powerAnalysis(template_model)


###################################################
### code chunk number 17: powerAnalysis.Rnw:276-277
###################################################
try(powerAnalysis(template_model))


###################################################
### code chunk number 18: powerAnalysis.Rnw:282-283
###################################################
effect_sizes <- list(state=c(intercept=0, elev=-0.4), det=c(intercept=0))


###################################################
### code chunk number 19: powerAnalysis.Rnw:291-292
###################################################
(pa <- powerAnalysis(template_model, coefs=effect_sizes, alpha=0.05, nsim=20))


###################################################
### code chunk number 20: powerAnalysis.Rnw:304-305 (eval = FALSE)
###################################################
## pa@estimates


###################################################
### code chunk number 21: powerAnalysis.Rnw:316-318
###################################################
# 50 sites and 3 obs per site
(pa2 <- powerAnalysis(template_model, effect_sizes, design=list(M=50, J=3), nsim=20))


###################################################
### code chunk number 22: powerAnalysis.Rnw:325-326
###################################################
(pa3 <- powerAnalysis(template_model, effect_sizes, design=list(M=400, J=4), nsim=20))


###################################################
### code chunk number 23: powerAnalysis.Rnw:334-335
###################################################
unmarkedPowerList(list(pa, pa2, pa3))


###################################################
### code chunk number 24: powerAnalysis.Rnw:341-346
###################################################
scenarios <- expand.grid(M=c(50,200,400),
                         J=c(3,5,8))
pl <- unmarkedPowerList(template_model, effect_sizes, design=scenarios, nsim=20)
head(summary(pl))
tail(summary(pl))


###################################################
### code chunk number 25: powerAnalysis.Rnw:353-354
###################################################
plot(pl, power=0.8, param="elev")


