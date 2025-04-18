% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertGlobalSolarAtlas.R
\name{convertGlobalSolarAtlas}
\alias{convertGlobalSolarAtlas}
\title{convertGlobalSolarAtlas}
\usage{
convertGlobalSolarAtlas(x)
}
\arguments{
\item{x}{MAgPIE object.}
}
\value{
The "GlobalSolarAtlas" data with spatial entries for each country.
}
\description{
The ISO codes of "GlobalSolarAtlas" data are compared with the official ISO code country list.
}
\examples{
\dontrun{
a <- readSource("GlobalSolarAtlas", convert = TRUE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas
}
