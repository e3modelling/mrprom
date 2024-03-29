% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcIDataTransTech.R
\name{calcIDataTransTech}
\alias{calcIDataTransTech}
\title{calcIDataTransTech}
\usage{
calcIDataTransTech()
}
\value{
OPENPROM input data iDataTransTech
The output data for Capital Costs (IC) per vehicle calculated from
technology cost and other data from the "EU Reference Scenario".
The output data for Fixed Costs (FC) per vehicle is from MENA_EDS model.
The output data for Technical Lifetime (LFT) is from US Department of
Transportation, International Union of Railways, Statista, EU CORDIS.
}
\description{
Use technology cost data from the "EU Reference Scenario", MENA_EDS model data,
and Technical Lifetime data from various sources to derive OPENPROM input parameter
iDataTransTech.
}
\examples{
\dontrun{
a <- calcOutput(type = "IDataTransTech", aggregate = FALSE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
}
