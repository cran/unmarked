## ----echo=FALSE---------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(unmarked)
data(crossbill)
umf <- unmarkedFrameOccu(
    y=as.matrix(crossbill[,c("det991", "det992", "det993")]),
    siteCovs=crossbill[,c("ele", "forest")],
    obsCovs=list(date=crossbill[,c("date991", "date992", "date993")]))
sc <- scale(siteCovs(umf))
siteCovs(umf) <- sc
head(umf)

## -----------------------------------------------------------------------------
(fm.occu <- occu(~date ~ele + I(ele^2) + forest, umf))

## ---- eval=FALSE--------------------------------------------------------------
#  library(lattice)
#  data(Switzerland)
#  print(levelplot(elevation ~ x + y, Switzerland, aspect="iso",
#                  xlab="Easting (m)", ylab="Northing (m)",
#                  col.regions=terrain.colors(100)))

## ---- echo=FALSE, fig.height=4, fig.width=5, fig.cap="Figure 1. Elevation in Switzerland"----
if(requireNamespace("lattice", quietly = TRUE)){
  library(lattice)
  data(Switzerland)
  print(levelplot(elevation ~ x + y, Switzerland, aspect="iso",
                xlab="Easting (m)", ylab="Northing (m)",
                col.regions=terrain.colors(100)))
} else {
  message("lattice package is required for this vignette but is not available\n")
  knitr::knit_exit()
}

## ----eval=FALSE---------------------------------------------------------------
#  library(raster)

## ----echo=FALSE---------------------------------------------------------------
if(requireNamespace("raster", quietly = TRUE)){
  suppressMessages(library(raster))
} else {
  message("raster package is required for this vignette but is not available\n")
  knitr::knit_exit()
}

## -----------------------------------------------------------------------------
elevation <- rasterFromXYZ(Switzerland[,c("x","y","elevation")],
    crs="+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")

forest <- rasterFromXYZ(Switzerland[,c("x","y","forest")],
    crs="+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")

## ---- fig.height=3, fig.width=6, fig.cap="Figure 2. Elevation and forest cover, standardized"----
attr(sc, "scaled:center")
attr(sc, "scaled:scale")
ele.s <- (elevation-1189)/640
forest.s <- (forest-34.7)/27.7
ef <- stack(ele.s, forest.s)
names(ef) <- c("ele", "forest")
plot(ef, col=terrain.colors(100))

## ---- fig.height=4, fig.width=4, fig.cap="Figure 3. A species distribution map for the European crossbill in Switzerland. The colors represent occurrence probability."----
(beta <- coef(fm.occu, type="state"))
logit.psi <- beta[1] + beta[2]*ele.s + beta[3]*ele.s^2 + beta[4]*forest.s
psi <- exp(logit.psi) / (1 + exp(logit.psi))
print(spplot(psi, col.regions=terrain.colors(100)))

## ---- fig.height=5, fig.width=5, fig.cap="Figure 4. Expected occurrence probability along with standard errors and the limits of the asymptotic 95% confidence interval."----
E.psi <- predict(fm.occu, type="state", newdata=ef)
plot(E.psi, axes=FALSE, col=terrain.colors(100))

## -----------------------------------------------------------------------------
data(issj)
covs <- scale(issj[,c("elevation", "forest", "chaparral")])
area <- pi*300^2 / 10000
jayumf <- unmarkedFrameDS(y=as.matrix(issj[,1:3]),
                          siteCovs=data.frame(covs, area),
                          dist.breaks=c(0,100,200,300),
                          unitsIn="m", survey="point")
head(jayumf)
fm1 <- distsamp(~chaparral ~chaparral + elevation + offset(log(area)),
                jayumf, keyfun="halfnorm", output="abund",
                starts=c(-2.8,1,0,4.5,0))
fm1

## -----------------------------------------------------------------------------
data(cruz)
elev <- rasterFromXYZ(cruz[,c("x","y","elevation")],
     crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
forest <- rasterFromXYZ(cruz[,c("x","y","forest")],
     crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
chap <- rasterFromXYZ(cruz[,c("x","y","chaparral")],
     crs="+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
area.raster <- chap
values(area.raster) <- 300*300/10000 # area of a grid pixel
attr(covs, "scaled:center")
attr(covs, "scaled:scale")
elev.s <- (elev-202)/125
forest.s <- (forest-0.0673)/0.137
chap.s <- (chap-0.270)/0.234
habitat <- stack(elev.s, forest.s, chap.s, area.raster)
names(habitat) <- c("elevation", "forest", "chaparral", "area")

## ---- fig.height=5, fig.width=6, fig.cap="Figure 5. Expeted Island Scrub-Jay abundance, SEs, and 95% CIs."----
E <- predict(fm1, type="state", newdata=habitat)
plot(E, axes=FALSE, col=terrain.colors(100))

