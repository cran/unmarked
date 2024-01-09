## ----echo=FALSE---------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(unmarked)
dists <- read.csv(system.file("csv", "distdata.csv", package="unmarked"),
                  stringsAsFactors=TRUE)
head(dists, 10)
levels(dists$transect) <- c(levels(dists$transect), "g")
levels(dists$transect)
yDat <- formatDistData(dists, distCol="distance",
	transectNameCol="transect", dist.breaks=c(0, 5, 10, 15, 20))
yDat

## -----------------------------------------------------------------------------
(covs <- data.frame(canopyHt = c(5, 8, 3, 2, 4, 7, 5),
	habitat = c('A','A','A','A','B','B','B'), row.names=letters[1:7]))

## -----------------------------------------------------------------------------
umf <- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=covs, survey="line",
	dist.breaks=c(0, 5, 10, 15, 20), tlength=rep(100, 7),
	unitsIn="m")

## ----fig.height=4, fig.width=4, fig.cap="Figure 1. Histogram of detection distances"----
summary(umf)
hist(umf, xlab="distance (m)", main="", cex.lab=0.8, cex.axis=0.8)

## -----------------------------------------------------------------------------
hn_Null <- distsamp(~1~1, umf)
hn_Null <- distsamp(~1~1, umf, keyfun="halfnorm", output="density",
	unitsOut="ha")
haz_Null <- distsamp(~1~1, umf, keyfun="hazard")
hn_Hab.Ht <- distsamp(~canopyHt ~habitat, umf)

## -----------------------------------------------------------------------------
haz_Null

## -----------------------------------------------------------------------------
names(haz_Null)
backTransform(haz_Null, type="state")
backTransform(haz_Null, type="det")
backTransform(haz_Null, type="scale")
backTransform(linearComb(hn_Hab.Ht['det'], c(1, 5)))

## -----------------------------------------------------------------------------
site.level.density <- predict(hn_Hab.Ht, type="state")$Predicted
plotArea.inHectares <- 100 * 40 / 10000
site.level.abundance <- site.level.density * plotArea.inHectares
(N.hat <- sum(site.level.abundance))

## -----------------------------------------------------------------------------
getN.hat <- function(fit) {
    d <- predict(fit, type="state")$Predicted
    a <- d * (100 * 40 / 10000)
    N.hat <- c(N.hat = sum(a))
    return(N.hat)
    }
pb <- parboot(hn_Hab.Ht, statistic=getN.hat, nsim=25)
pb

## -----------------------------------------------------------------------------
head(habConstant <- data.frame(canopyHt = seq(2, 8, length=20),
	habitat=factor("A", levels=c("A", "B"))))
(htConstant <- data.frame(canopyHt = 5,
	habitat=factor(c("A", "B"))))

## -----------------------------------------------------------------------------
(Elambda <- predict(hn_Hab.Ht, type="state", newdata=htConstant,
	appendData=TRUE))
head(Esigma <- predict(hn_Hab.Ht, type="det", newdata=habConstant,
	appendData=TRUE))

## ----fig.height=3, fig.width=6, fig.cap="Figure 2. Predicted covariate relationships"----
par(mfrow=c(1, 2))
with(Elambda, {
	x <- barplot(Predicted, names=habitat, xlab="Habitat",
		ylab="Density (animals / ha)", ylim=c(0, 25), cex.names=0.7,
        cex.lab=0.7, cex.axis=0.7)
	arrows(x, Predicted, x, Predicted+SE, code=3, angle=90, length=0.05)
	box()
	})
with(Esigma, {
	plot(canopyHt, Predicted, type="l", xlab="Canopy height",
		ylab=expression(paste("Half-normal scale parameter (", sigma, ")",
        sep="")), ylim=c(0, 20), cex=0.7, cex.lab=0.7,
    cex.axis=0.7)
	lines(canopyHt, Predicted+SE, lty=2)
	lines(canopyHt, Predicted-SE, lty=2)
	})

## ----fig.width=6, fig.height=3, fig.cap="Figure 3. Detection and probability density functions"----
par(mfrow=c(1, 2))
plot(function(x) gxhn(x, sigma=10.8), 0, 20, xlab="Distance (m)",
	ylab="Detection prob. at 2m canopy ht.", cex.lab=0.7,
    cex.axis=0.7, las=1)
hist(hn_Null, xlab="Distance (m)", ylab="Probability density", main="",
    ylim=c(0, 0.1), cex.lab=0.7, cex.axis=0.7, las=1)
plot(function(x) dxhn(x, sigma=10.8), 0, 20, add=TRUE, col="blue")
plot(function(x) dxhn(x, sigma=9.9), 0, 20, add=TRUE, col="green")
legend('topright', c("Canopy ht. = 2m", "Null", "Canopy ht. = 8m"),
	col=c("blue", "black", "green"), lty=1, cex=0.4)

