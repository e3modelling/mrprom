% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcIInvPlants.R
\name{calcIInvPlants}
\alias{calcIInvPlants}
\title{calcIInvPlants}
\usage{
calcIInvPlants()
}
\value{
OPENPROM input data iInvPlants.
The output data calculated from the "Global Energy Monitor".
}
\description{
Use installed capacities of power plants data from the Global Energy Monitor,
to derive OPENPROM input parameter iInvPlants.
}
\examples{
\dontrun{
a <- calcOutput(type = "IInvPlants", aggregate = FALSE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
}
