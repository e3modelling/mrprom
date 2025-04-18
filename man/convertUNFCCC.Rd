% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertUNFCCC.R
\name{convertUNFCCC}
\alias{convertUNFCCC}
\title{convertUNFCCC}
\usage{
convertUNFCCC(x)
}
\arguments{
\item{x}{MAgPIE object.}
}
\value{
The "UNFCCC" data with spatial entries for each country.
}
\description{
The ISO codes of "UNFCCC" data are compared with the official ISO code country list.
}
\examples{
\dontrun{
a <- readSource("UNFCCC", subtype = "2.  Industrial Processes and Product Use", convert = TRUE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
}
