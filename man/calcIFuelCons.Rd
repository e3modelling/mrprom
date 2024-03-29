% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcIFuelCons.R
\name{calcIFuelCons}
\alias{calcIFuelCons}
\title{calcIFuelCons}
\usage{
calcIFuelCons(subtype = "DOMSE")
}
\arguments{
\item{subtype}{string, OPENPROM sector (DOMSE, INDSE, NENSE, TRANSE)}
}
\value{
OPENPROM input data iFuelConsXXX
}
\description{
Use ENERDATA and IEA fuel consumption data to derive OPENPROM input parameter iFuelConsXXX
(XXX: NENSE, INDSE, DOMSE, TRANSE). If both sources has data about the same fuel 
the data from ENERDATA is taken.
}
\examples{
\dontrun{
a <- calcOutput(type = "IFuelCons", subtype = "DOMSE", aggregate = FALSE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas
}
