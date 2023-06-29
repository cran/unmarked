## ----echo=FALSE---------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(unmarked)
data(MesoCarnivores)
names(MesoCarnivores)

## ---- eval=FALSE--------------------------------------------------------------
#  ?unmarkedFrameOccuMulti

## -----------------------------------------------------------------------------
ylist <- list(bobcat=MesoCarnivores$bobcat, coyote=MesoCarnivores$coyote, 
              redfox=MesoCarnivores$redfox)
lapply(ylist, head)

## -----------------------------------------------------------------------------
head(MesoCarnivores$sitecovs)

## -----------------------------------------------------------------------------
umf <- unmarkedFrameOccuMulti(y=ylist, siteCovs=MesoCarnivores$sitecovs)

## -----------------------------------------------------------------------------
umf@fDesign

## -----------------------------------------------------------------------------
colnames(umf@fDesign)

## -----------------------------------------------------------------------------
stateformulas <- c("~1","~1","~1","~1","~1","~1","0")

## -----------------------------------------------------------------------------
detformulas <- c("~1","~1","~1")

## ---- eval=FALSE--------------------------------------------------------------
#  ?occuMulti

## -----------------------------------------------------------------------------
mod_null <- occuMulti(detformulas=detformulas, stateformulas=stateformulas, data=umf)
summary(mod_null)

## -----------------------------------------------------------------------------
occ_prob <- predict(mod_null, type="state")
head(occ_prob$Predicted)

## -----------------------------------------------------------------------------
redfox_marginal <- predict(mod_null, type="state", species="redfox")
head(redfox_marginal)

## -----------------------------------------------------------------------------
coy_marginal <- predict(mod_null, type="state", species="coyote")
bob_marginal <- predict(mod_null, type="state", species="bobcat")
all_marginal <- rbind(redfox_marginal[1,], coy_marginal[1,], bob_marginal[1,])
all_marginal$Species <- c("Red fox", "Coyote", "Bobcat")

## ---- fig.height=5------------------------------------------------------------
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

## -----------------------------------------------------------------------------
redfox_coy <- predict(mod_null, type="state", species="redfox", cond="coyote")
head(redfox_coy)

## -----------------------------------------------------------------------------
redfox_nocoy <- predict(mod_null, type="state", species="redfox", cond="-coyote")
head(redfox_nocoy)

## ---- fig.height=5------------------------------------------------------------
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

## -----------------------------------------------------------------------------
colnames(umf@fDesign)

## -----------------------------------------------------------------------------
head(siteCovs(umf))

## -----------------------------------------------------------------------------
sf <- c("~HDens_5km","~HDens_5km","~HDens_5km","~1","~1","~1","0")

## -----------------------------------------------------------------------------
sf <- c("~scale(HDens_5km)","~scale(HDens_5km)","~scale(HDens_5km)","~1","~1","~1","0")

## -----------------------------------------------------------------------------
mod_hdens <- occuMulti(stateformulas=sf, detformulas=detformulas, umf)
summary(mod_hdens)

## -----------------------------------------------------------------------------
hdens_range <- range(siteCovs(umf)$HDens_5km)
hdens_seq <- seq(hdens_range[1], hdens_range[2], length.out=100)

## -----------------------------------------------------------------------------
nd <- data.frame(HDens_5km = hdens_seq)
occ_hdens_coy <- predict(mod_hdens, type="state", species="coyote", newdata=nd)
occ_hdens_coy$Species <- "Coyote"
occ_hdens_coy$Hdens <- hdens_seq
head(occ_hdens_coy)

## -----------------------------------------------------------------------------
occ_hdens_bob <- predict(mod_hdens, type="state", species="bobcat", newdata=nd)
occ_hdens_fox <- predict(mod_hdens, type="state", species="redfox", newdata=nd)
occ_hdens_bob$Species <- "Bobcat"
occ_hdens_fox$Species <- "Red fox"
occ_hdens_bob$Hdens <- hdens_seq
occ_hdens_fox$Hdens <- hdens_seq

## ---- fig.height=5------------------------------------------------------------
plot(occ_hdens_coy$Hdens, occ_hdens_coy$Predicted, type='l', ylim=c(0,0.6),
     col='red', lwd=2, xlab="Housing density", ylab="Marginal occupancy")
lines(occ_hdens_bob$Hdens, occ_hdens_bob$Predicted, col='blue', lwd=2)
lines(occ_hdens_fox$Hdens, occ_hdens_fox$Predicted, col='green', lwd=2)
legend('topleft', col=c('red', 'blue', 'green'), lty=1,
       legend=c("Coyote", "Bobcat", "Red fox"))

## -----------------------------------------------------------------------------
mods <- fitList(mod_null, mod_hdens)

## -----------------------------------------------------------------------------
modSel(mods)

## ---- eval=FALSE--------------------------------------------------------------
#  state_complex <- c(rep("~scale(Dist_5km)+scale(HDens_5km)", 6), 0)
#  det_complex <- rep("~Trail",3)
#  
#  mod_complex <- occuMulti(stateformulas=state_complex, detformulas=det_complex, umf)
#  summary(mod_complex)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(123)
#  mod_penalty <- optimizePenalty(mod_complex, penalties=c(0.5,1))
#  summary(mod_penalty)

