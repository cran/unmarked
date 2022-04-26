### R code from vignette source 'occuMulti.Rnw'

###################################################
### code chunk number 1: occuMulti.Rnw:1-3
###################################################
options(width=70)
options(continue=" ")


###################################################
### code chunk number 2: occuMulti.Rnw:98-102
###################################################
library(unmarked)
library(AHMbook)
data(MesoCarnivores)
names(MesoCarnivores)


###################################################
### code chunk number 3: occuMulti.Rnw:110-111 (eval = FALSE)
###################################################
## ?unmarkedFrameOccuMulti


###################################################
### code chunk number 4: occuMulti.Rnw:117-120
###################################################
ylist <- list(bobcat=MesoCarnivores$bobcat, coyote=MesoCarnivores$coyote, 
              redfox=MesoCarnivores$redfox)
lapply(ylist, head)


###################################################
### code chunk number 5: occuMulti.Rnw:125-126
###################################################
head(MesoCarnivores$sitecovs)


###################################################
### code chunk number 6: occuMulti.Rnw:132-133
###################################################
umf <- unmarkedFrameOccuMulti(y=ylist, siteCovs=MesoCarnivores$sitecovs)


###################################################
### code chunk number 7: occuMulti.Rnw:144-145
###################################################
umf@fDesign


###################################################
### code chunk number 8: occuMulti.Rnw:151-152
###################################################
colnames(umf@fDesign)


###################################################
### code chunk number 9: occuMulti.Rnw:158-159
###################################################
stateformulas <- c("~1","~1","~1","~1","~1","~1","0")


###################################################
### code chunk number 10: occuMulti.Rnw:171-172
###################################################
detformulas <- c("~1","~1","~1")


###################################################
### code chunk number 11: occuMulti.Rnw:179-180 (eval = FALSE)
###################################################
## ?occuMulti


###################################################
### code chunk number 12: occuMulti.Rnw:185-187
###################################################
mod_null <- occuMulti(detformulas=detformulas, stateformulas=stateformulas, data=umf)
summary(mod_null)


###################################################
### code chunk number 13: occuMulti.Rnw:206-208
###################################################
occ_prob <- predict(mod_null, type="state")
head(occ_prob$Predicted)


###################################################
### code chunk number 14: occuMulti.Rnw:221-223
###################################################
redfox_marginal <- predict(mod_null, type="state", species="redfox")
head(redfox_marginal)


###################################################
### code chunk number 15: occuMulti.Rnw:231-235
###################################################
coy_marginal <- predict(mod_null, type="state", species="coyote")
bob_marginal <- predict(mod_null, type="state", species="bobcat")
all_marginal <- rbind(redfox_marginal[1,], coy_marginal[1,], bob_marginal[1,])
all_marginal$Species <- c("Red fox", "Coyote", "Bobcat")


###################################################
### code chunk number 16: occuMulti.Rnw:240-252
###################################################
plot(1:3, all_marginal$Predicted, ylim=c(0.1,0.4), 
     xlim=c(0.5,3.5), pch=19, cex=1.5, xaxt='n', 
     xlab="", ylab="Marginal occupancy and 95% CI")
axis(1, at=1:3, labels=all_marginal$Species)

# CIs
top <- 0.1
for (i in 1:3){
  segments(i, all_marginal$lower[i], i, all_marginal$upper[i])
  segments(i-top, all_marginal$lower[i], i+top)
  segments(i-top, all_marginal$upper[i], i+top)
}


###################################################
### code chunk number 17: occuMulti.Rnw:260-262
###################################################
redfox_coy <- predict(mod_null, type="state", species="redfox", cond="coyote")
head(redfox_coy)


###################################################
### code chunk number 18: occuMulti.Rnw:268-270
###################################################
redfox_nocoy <- predict(mod_null, type="state", species="redfox", cond="-coyote")
head(redfox_nocoy)


###################################################
### code chunk number 19: occuMulti.Rnw:277-292
###################################################
cond_data <- rbind(redfox_coy[1,], redfox_nocoy[1,])
cond_data$Coyote_status <- c("Present","Absent")

plot(1:2, cond_data$Predicted, ylim=c(0,0.3), 
     xlim=c(0.5,2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="Coyote status", ylab="Red fox occupancy and 95% CI")
axis(1, at=1:2, labels=cond_data$Coyote_status)

# CIs
top <- 0.1
for (i in 1:2){
  segments(i, cond_data$lower[i], i, cond_data$upper[i])
  segments(i-top, cond_data$lower[i], i+top)
  segments(i-top, cond_data$upper[i], i+top)
}


###################################################
### code chunk number 20: occuMulti.Rnw:302-303
###################################################
colnames(umf@fDesign)


###################################################
### code chunk number 21: occuMulti.Rnw:308-309
###################################################
head(siteCovs(umf))


###################################################
### code chunk number 22: occuMulti.Rnw:318-319
###################################################
sf <- c("~HDens_5km","~HDens_5km","~HDens_5km","~1","~1","~1","0")


###################################################
### code chunk number 23: occuMulti.Rnw:324-325
###################################################
sf <- c("~scale(HDens_5km)","~scale(HDens_5km)","~scale(HDens_5km)","~1","~1","~1","0")


###################################################
### code chunk number 24: occuMulti.Rnw:330-332
###################################################
mod_hdens <- occuMulti(stateformulas=sf, detformulas=detformulas, umf)
summary(mod_hdens)


###################################################
### code chunk number 25: occuMulti.Rnw:349-351
###################################################
hdens_range <- range(siteCovs(umf)$HDens_5km)
hdens_seq <- seq(hdens_range[1], hdens_range[2], length.out=100)


###################################################
### code chunk number 26: occuMulti.Rnw:358-363
###################################################
nd <- data.frame(HDens_5km = hdens_seq)
occ_hdens_coy <- predict(mod_hdens, type="state", species="coyote", newdata=nd)
occ_hdens_coy$Species <- "Coyote"
occ_hdens_coy$Hdens <- hdens_seq
head(occ_hdens_coy)


###################################################
### code chunk number 27: occuMulti.Rnw:368-374
###################################################
occ_hdens_bob <- predict(mod_hdens, type="state", species="bobcat", newdata=nd)
occ_hdens_fox <- predict(mod_hdens, type="state", species="redfox", newdata=nd)
occ_hdens_bob$Species <- "Bobcat"
occ_hdens_fox$Species <- "Red fox"
occ_hdens_bob$Hdens <- hdens_seq
occ_hdens_fox$Hdens <- hdens_seq


###################################################
### code chunk number 28: occuMulti.Rnw:380-386
###################################################
plot(occ_hdens_coy$Hdens, occ_hdens_coy$Predicted, type='l', ylim=c(0,0.6),
     col='red', lwd=2, xlab="Housing density", ylab="Marginal occupancy")
lines(occ_hdens_bob$Hdens, occ_hdens_bob$Predicted, col='blue', lwd=2)
lines(occ_hdens_fox$Hdens, occ_hdens_fox$Predicted, col='green', lwd=2)
legend('topleft', col=c('red', 'blue', 'green'), lty=1,
       legend=c("Coyote", "Bobcat", "Red fox"))


###################################################
### code chunk number 29: occuMulti.Rnw:394-395
###################################################
mods <- fitList(mod_null, mod_hdens)


###################################################
### code chunk number 30: occuMulti.Rnw:400-401
###################################################
modSel(mods)


###################################################
### code chunk number 31: occuMulti.Rnw:420-425
###################################################
state_complex <- c(rep("~scale(Dist_5km)+scale(HDens_5km)", 6), 0)
det_complex <- rep("~Trail",3)

mod_complex <- occuMulti(stateformulas=state_complex, detformulas=det_complex, umf)
summary(mod_complex)


###################################################
### code chunk number 32: occuMulti.Rnw:459-462 (eval = FALSE)
###################################################
## set.seed(123)
## mod_penalty <- optimizePenalty(mod_complex, penalties=c(0.5,1))
## summary(mod_penalty)


###################################################
### code chunk number 33: occuMulti.Rnw:465-510 (eval = FALSE)
###################################################
## ## Optimal penalty is 1
## ## Bootstraping covariance matrix
## 
## ## Call:
## ## occuMulti(detformulas = c("~Trail", "~Trail", "~Trail"), stateformulas = c("~scale(Dist_5km)+scale(HDens_5km)",
## ## "~scale(Dist_5km)+scale(HDens_5km)", "~scale(Dist_5km)+scale(HDens_5km)",
## ## "~scale(Dist_5km)+scale(HDens_5km)", "~scale(Dist_5km)+scale(HDens_5km)",
## ## "~scale(Dist_5km)+scale(HDens_5km)", "0"), data = object@data,
## ##     maxOrder = 3L, penalty = 1, boot = boot)
## 
## ## Occupancy (logit-scale):
## ##                                  Estimate    SE      z  P(>|z|)
## ## [bobcat] (Intercept)              -1.7810 0.221 -8.054 8.00e-16
## ## [bobcat] scale(Dist_5km)          -1.3143 0.337 -3.903 9.48e-05
## ## [bobcat] scale(HDens_5km)         -2.8200 0.529 -5.334 9.61e-08
## ## [coyote] (Intercept)              -0.6049 0.178 -3.407 6.56e-04
## ## [coyote] scale(Dist_5km)           0.0285 0.150  0.190 8.49e-01
## ## [coyote] scale(HDens_5km)         -1.0908 0.397 -2.748 5.99e-03
## ## [redfox] (Intercept)              -1.5659 0.310 -5.059 4.22e-07
## ## [redfox] scale(Dist_5km)          -0.3068 0.138 -2.226 2.60e-02
## ## [redfox] scale(HDens_5km)          0.4730 0.797  0.593 5.53e-01
## ## [bobcat:coyote] (Intercept)        1.1871 0.372  3.195 1.40e-03
## ## [bobcat:coyote] scale(Dist_5km)    0.9347 0.368  2.537 1.12e-02
## ## [bobcat:coyote] scale(HDens_5km)  -0.3218 1.043 -0.309 7.58e-01
## ## [bobcat:redfox] (Intercept)       -0.8831 0.346 -2.553 1.07e-02
## ## [bobcat:redfox] scale(Dist_5km)    0.0364 0.233  0.156 8.76e-01
## ## [bobcat:redfox] scale(HDens_5km)   2.5609 1.074  2.384 1.71e-02
## ## [coyote:redfox] (Intercept)        1.0001 0.249  4.009 6.09e-05
## ## [coyote:redfox] scale(Dist_5km)    0.0236 0.229  0.103 9.18e-01
## ## [coyote:redfox] scale(HDens_5km)   1.3920 0.424  3.281 1.03e-03
## 
## ## Detection (logit-scale):
## ##                      Estimate    SE      z  P(>|z|)
## ## [bobcat] (Intercept)    -2.44 0.150 -16.22 3.72e-59
## ## [bobcat] Trail           1.74 0.164  10.61 2.59e-26
## ## [coyote] (Intercept)    -1.89 0.117 -16.24 2.72e-59
## ## [coyote] Trail           2.10 0.150  14.00 1.52e-44
## ## [redfox] (Intercept)    -1.49 0.206  -7.21 5.66e-13
## ## [redfox] Trail           1.72 0.254   6.79 1.14e-11
## 
## ## AIC: 6135.555
## ## Number of sites: 1437
## ## optim convergence code: 0
## ## optim iterations: 100
## ## Bootstrap iterations: 30


