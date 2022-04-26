### R code from vignette source 'random-effects.Rnw'

###################################################
### code chunk number 1: random-effects.Rnw:1-3
###################################################
options(width=70)
options(continue=" ")


###################################################
### code chunk number 2: random-effects.Rnw:73-93
###################################################
set.seed(35)
nsites <- 100
nyears <- 3
nvisits <- 3

# Abundance parameters
beta0 <- 0 # Intercept
beta1 <- 1 # fixed covariate slope
sd_site <- 0.5 # SD of site-level random effect
re_site <- rnorm(nsites, 0, sd_site) # simulate random effect

# Detection parameters
alpha0 <- 0 # Intercept
sd_obs <- 0.3 # SD of observer-level random effect (20 unique observers)
re_obs <- rnorm(20, 0, sd_obs) # simulate random effect

# Covariates
x <- rnorm(nsites*nyears) # a covariate on abundance
site_id <- rep(1:100, each=3)
obs_id <- sample(1:20, nsites*nyears*nvisits, replace=TRUE)


###################################################
### code chunk number 3: random-effects.Rnw:98-104
###################################################
lambda <- exp(beta0 + beta1*x + # fixed part of linear predictor
              re_site[site_id])  # site random effect

# Generate latent abundance N
N <- rpois(nsites*nyears, lambda) 
hist(N)


###################################################
### code chunk number 4: random-effects.Rnw:109-119
###################################################
p <- plogis(alpha0 + re_obs[obs_id])
p <- matrix(p, nrow=nsites*nyears, ncol=nvisits, byrow=TRUE)

y <- matrix(NA, nsites*nyears, nvisits)

for (i in 1:(nsites*nyears)){
  for (j in 1:nvisits){
    y[i,j] <- rbinom(1, N[i], p[i,j])
  }
}


###################################################
### code chunk number 5: random-effects.Rnw:125-130
###################################################
library(unmarked)
site_covs <- data.frame(x=x,
                        site_id=factor(as.character(site_id)))
obs_covs <- data.frame(obs_id=factor(as.character(obs_id)))
umf <- unmarkedFramePCount(y=y, siteCovs=site_covs, obsCovs=obs_covs)


###################################################
### code chunk number 6: random-effects.Rnw:137-139
###################################################
mod_x <- pcount(~1~x, umf, K=40)
summary(mod_x)


###################################################
### code chunk number 7: random-effects.Rnw:149-151
###################################################
mod_site <- pcount(~1~x+(1|site_id), umf, K=40)
mod_site


###################################################
### code chunk number 8: random-effects.Rnw:159-161
###################################################
mod_obs <- pcount(~1 + (1|obs_id) ~ x + (1|site_id), umf, K=40)
mod_obs


###################################################
### code chunk number 9: random-effects.Rnw:170-171
###################################################
sigma(mod_obs)


###################################################
### code chunk number 10: random-effects.Rnw:177-178
###################################################
head(randomTerms(mod_obs, "state"))


###################################################
### code chunk number 11: random-effects.Rnw:185-191
###################################################
ran <- randomTerms(mod_obs, "state")
ran <- ran[order(as.numeric(ran$Level)),] # sort them correctly
ints <- coef(mod_obs)[1] + ran$Estimate # Calculate the total intercept for each level

plot(re_site, ints, xlab="Truth", ylab="Estimate", pch=19)
abline(a=0, b=1)


###################################################
### code chunk number 12: random-effects.Rnw:199-204
###################################################
# with random effect
head(predict(mod_obs, "det"))

# without random effect
head(predict(mod_obs, "det", re.form=NA))


###################################################
### code chunk number 13: random-effects.Rnw:213-214 (eval = FALSE)
###################################################
## ~1 + (1|obs_id) + (1|site_id)


###################################################
### code chunk number 14: random-effects.Rnw:220-221 (eval = FALSE)
###################################################
## ~x + (1 + x || site_id)


