\name{randomTerms}
\alias{randomTerms}
\alias{randomTerms-methods}
\alias{randomTerms,unmarkedFit-method}
\alias{randomTerms,unmarkedEstimate-method}

\title{Extract estimates of random effect terms}

\description{
  Extract estimates and summary statistics of random effect terms from an
  \code{unmarkedFit} model or an \code{unmarkedEstimate}.
}

\usage{
\S4method{randomTerms}{unmarkedEstimate}(object, level=0.95, addMean = FALSE, ...)
\S4method{randomTerms}{unmarkedFit}(object, type, level=0.95, addMean = FALSE, ...)
}

\arguments{
  \item{object}{An object inheriting class \code{unmarkedEstimate} or 
    \code{unmarkedFit}}
  \item{level}{Significance level to use for confidence interval}
  \item{type}{If provided, return only random effect terms from the chosen
    submodel type (as a character string)}
  \item{addMean}{Logical. Should the corresponding mean intercept or slope (if any)
    be added to the value of the random effect (yielding the complete random intercept/slope)?}
  \item{...}{Other arguments}
} 

\value{\code{data.frame} containing estimates, SEs, and confidence intervals
 for random effect terms in the model. 
}

\author{Ken Kellner \email{contact@kenkellner.com}}
