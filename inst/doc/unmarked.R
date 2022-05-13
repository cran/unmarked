## ----echo=FALSE---------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
tab1 <- data.frame(
  Model=c("Occupancy", "Royle-Nichols", "Point Count", "Distance-sampling",
          "Generalized distance-sampling", "Arbitrary multinomial-Poisson",
          "Colonization-extinction", "Generalized multinomial-mixture"),
  `Fitting Function`=c("occu","occuRN","pcount","distsamp","gdistsamp",
                       "multinomPois","colext","gmultmix"),
  Data=c("unmarkedFrameOccu","unmarkedFrameOccu","unmarkedFramePCount",
         "unmarkedFrameDS","unmarkedFrameGDS","unmarkedFrameMPois",
         "unmarkedMultFrame","unmarkedFrameGMM"),
  Citation=c("@mackenzie_estimating_2002","@royle_estimating_2003",
             "@royle_n-mixture_2004","@royle_modeling_2004",
             "@chandlerEA_2011","@royle_generalized_2004",
             "@mackenzie_estimating_2003","@royle_generalized_2004"),
        check.names=FALSE)

knitr::kable(tab1, format='markdown', align="lccc",
             caption="Table 1. Models handled by unmarked.")

## -----------------------------------------------------------------------------
library(unmarked)
wt <- read.csv(system.file("csv","widewt.csv", package="unmarked"))
y <- wt[,2:4]
siteCovs <-  wt[,c("elev", "forest", "length")]
obsCovs <- list(date=wt[,c("date.1", "date.2", "date.3")],
    ivel=wt[,c("ivel.1",  "ivel.2", "ivel.3")])
wt <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obsCovs)
summary(wt)

## -----------------------------------------------------------------------------
wt <- csvToUMF(system.file("csv","widewt.csv", package="unmarked"),
               long = FALSE, type = "unmarkedFrameOccu")

## -----------------------------------------------------------------------------
pcru <- csvToUMF(system.file("csv","frog2001pcru.csv", package="unmarked"),
                 long = TRUE, type = "unmarkedFrameOccu")

## -----------------------------------------------------------------------------
obsCovs(pcru) <- scale(obsCovs(pcru))

## -----------------------------------------------------------------------------
fm1 <- occu(~1 ~1, pcru)
fm2 <- occu(~ MinAfterSunset + Temperature ~ 1, pcru)
fm2

## -----------------------------------------------------------------------------
backTransform(fm2, 'state')

## -----------------------------------------------------------------------------
backTransform(linearComb(fm2, coefficients = c(1,0,0), type = 'det'))

## -----------------------------------------------------------------------------
newData <- data.frame(MinAfterSunset = 0, Temperature = -2:2)
round(predict(fm2, type = 'det', newdata = newData, appendData=TRUE), 2)

## ---- eval=FALSE--------------------------------------------------------------
#  confint(fm2, type='det')
#  confint(fm2, type='det', method = "profile")

## ---- echo=FALSE--------------------------------------------------------------
confint(fm2, type='det')
nul <- capture.output(ci <- confint(fm2, type='det', method = "profile"))
ci

## -----------------------------------------------------------------------------
fms <- fitList('psi(.)p(.)' = fm1, 'psi(.)p(Time+Temp)' = fm2)
modSel(fms)
predict(fms, type='det', newdata = newData)

## ---- warning=FALSE-----------------------------------------------------------
chisq <- function(fm) {
    umf <- fm@data
    y <- umf@y
    y[y>1] <- 1
    sr <- fm@sitesRemoved
    if(length(sr)>0)
        y <- y[-sr,,drop=FALSE]
    fv <- fitted(fm, na.rm=TRUE)
    y[is.na(fv)] <- NA
    sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
    }

(pb <- parboot(fm2, statistic=chisq, nsim=100, parallel=FALSE))

## -----------------------------------------------------------------------------
re <- ranef(fm2)
EBUP <- bup(re, stat="mode")
sum(EBUP) / numSites(pcru)

