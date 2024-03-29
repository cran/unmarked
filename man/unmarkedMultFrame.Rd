\name{unmarkedMultFrame}

\alias{unmarkedMultFrame}
\alias{yearlySiteCovs}
\alias{yearlySiteCovs,unmarkedMultFrame-method}
\alias{yearlySiteCovs<-}
\alias{yearlySiteCovs<-,unmarkedMultFrame-method}
\alias{unmarkedFrameGMM}
\alias{unmarkedFrameGDS}
\alias{unmarkedFrameGPC}
\alias{unmarkedFrameGOccu}

\title{Create an unmarkedMultFrame, unmarkedFrameGMM, unmarkedFrameGDS,
  or unmarkedFrameGPC object}

\usage{
  unmarkedMultFrame(y, siteCovs, obsCovs, numPrimary, yearlySiteCovs)
  unmarkedFrameGMM(y, siteCovs, obsCovs, numPrimary, yearlySiteCovs, type,
    obsToY, piFun)
  unmarkedFrameGDS(y, siteCovs, numPrimary, yearlySiteCovs, dist.breaks,
    survey, unitsIn, tlength)
  unmarkedFrameGPC(y, siteCovs, obsCovs, numPrimary, yearlySiteCovs)
}



\description{These functions construct unmarkedFrames for data
    collected during primary and secondary sampling periods.}

\details{
unmarkedMultFrame objects are used by \code{\link{colext}}.

unmarkedFrameGMM objects are used by \code{\link{gmultmix}}.

unmarkedFrameGDS objects are used by \code{\link{gdistsamp}}.

unmarkedFrameGPC objects are used by \code{\link{gpcount}}.

For a study with \emph{M} sites, \emph{T} years, and a maximum of
\emph{J} observations per site-year, the data can be supplied in a
variety of ways but are stored as follows.
\code{y} is an \eqn{M \times TJ}{M by TJ} matrix, with each row
corresponding to a site.  \code{siteCovs} is a data frame with \eqn{M}
rows.  \code{yearlySiteCovs} is a data frame with \eqn{MT} rows which
are in site-major, year-minor order.  \code{obsCovs} is a data frame
with \eqn{MTJ} rows, which are ordered by site-year-observation, so that
a column of \code{obsCovs} corresponds to \command{as.vector(t(y))},
element-by-element.  The number of years must be specified in
\code{numPrimary}.

If the data are in long format, the convenience function
\code{\link{formatMult}} is useful for creating the unmarkedMultFrame.

unmarkedFrameGMM and unmarkedFrameGDS are superclasses of
unmarkedMultFrame containing information on
the survey design used that resulted in multinomial outcomes. For
unmarkedFrameGMM and constant-interval removal sampling, you can set
type="removal" and ignore
the arguments obsToY and piFun. Similarly, for double-observer sampling,
setting type="double" or type="depDouble" will automatically create an appropiate 
obsToY matrix and \code{\link{piFuns}}. For all other situations, the type 
argument of unmarkedFrameGMM should be
ignored and the obsToY and piFun arguments must be specified. piFun must be a
function that converts an MxJ matrix of detection probabilities into an MxJ
matrix of multinomial cell probabilities. obsToY is a matrix describing how
the obsCovs relate to the observed counts y. For further discussion and examples
see the help page for \code{\link{multinomPois}} and \code{\link{piFuns}}.

unmarkedFrameGMM and unmarkedFrameGDS objects can be created from an
unmarkedMultFrame using the
"as" conversion method. See examples.

}

\note{Data used with \link{colext}, \link{gmultmix}, and
  \link{gdistsamp} may be collected during a single year, so
  yearlySiteCovs may be a misnomer is some cases. }

\value{an unmarkedMultFrame or unmarkedFrameGMM object}
\arguments{
    \item{y}{A matrix of the observed data.}
    \item{siteCovs}{Data frame of covariates that vary at the site level.}
    \item{obsCovs}{Data frame of covariates that vary within
        site-year-observation level.}
    \item{numPrimary}{Number of primary time periods (seasons in the
        multiseason model).}
    \item{yearlySiteCovs}{Data frame containing covariates at the
        site-year level.}
    \item{type}{Set to "removal" for constant-interval removal
        sampling, "double" for standard double observer sampling, or 
        "depDouble" for dependent double observer sampling. 
        This should be not be specified for other types of survey designs.}
    \item{obsToY}{A matrix specifying relationship between observation-level
        covariates and response matrix}
    \item{piFun}{A function converting an MxJ matrix of detection probabilities
      into an MxJ matrix of multinomial cell probabilities.}
    \item{dist.breaks}{see \code{\link{unmarkedFrameDS}}}
    \item{survey}{see \code{\link{unmarkedFrameDS}}}
    \item{unitsIn}{see \code{\link{unmarkedFrameDS}}}
    \item{tlength}{see \code{\link{unmarkedFrameDS}}}
  }

  \seealso{
    \code{\link{formatMult}}, \code{\link{colext}},
    \code{\link{gmultmix}}, \code{\link{gpcount}}
  }

\examples{

n <- 50   # number of sites
T <- 4    # number of primary periods
J <- 3    # number of secondary periods

site <- 1:50
years <- data.frame(matrix(rep(2010:2013, each=n), n, T))
years <- data.frame(lapply(years, as.factor))
occasions <- data.frame(matrix(rep(1:(J*T), each=n), n, J*T))

y <- matrix(0:1, n, J*T)

umf <- unmarkedMultFrame(y=y,
    siteCovs = data.frame(site=site),
    obsCovs=list(occasion=occasions),
    yearlySiteCovs=list(year=years),
    numPrimary=T)

umfGMM1 <- unmarkedFrameGMM(y=y,
    siteCovs = data.frame(site=site),
    obsCovs=list(occasion=occasions),
    yearlySiteCovs=data.frame(year=c(t(years))),
    # or: yearlySiteCovs=list(year=years),
    numPrimary=T, type="removal")


# A user-defined piFun calculating removal probs when time intervals differ.
instRemPiFun <- function(p) {
	M <- nrow(p)
	J <- ncol(p)
	pi <- matrix(NA, M, J)
	p[,1] <- pi[,1] <- 1 - (1 - p[,1])^2
	p[,2] <- 1 - (1 - p[,2])^3
	p[,3] <- 1 - (1 - p[,3])^5
	for(i in 2:J) {
		pi[,i] <- pi[, i - 1]/p[, i - 1] * (1 - p[, i - 1]) * p[, i]
		}
	return(pi)
	}

# Associated obsToY matrix required by unmarkedFrameMPois
o2y <- diag(ncol(y))
o2y[upper.tri(o2y)] <- 1
o2y


umfGMM2 <- unmarkedFrameGMM(y=y,
    siteCovs = data.frame(site=site),
    obsCovs=list(occasion=occasions),
    yearlySiteCovs=data.frame(year=c(t(years))),
    numPrimary=T, obsToY=o2y, piFun="instRemPiFun")

str(umfGMM2)



}
