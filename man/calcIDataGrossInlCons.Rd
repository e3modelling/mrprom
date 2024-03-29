% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcIDataGrossInlCons.R
\name{calcIDataGrossInlCons}
\alias{calcIDataGrossInlCons}
\title{calcIDataGrossInlCons}
\usage{
calcIDataGrossInlCons()
}
\value{
magpie object with OPENPROM input data iDataGrossInlCons
}
\description{
Use data from ENERDATA to derive OPENPROM input parameter iDataGrossInlCons
This dataset includes data for gross inland consumption in Mtoe.
}
\examples{
\dontrun{
a <- calcOutput(type = "IDataGrossInlCons", aggregate = FALSE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
}
