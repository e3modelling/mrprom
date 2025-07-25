% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcTDemand.R
\name{calcTDemand}
\alias{calcTDemand}
\title{calcTDemand}
\usage{
calcTDemand()
}
\value{
Secondary Energy Electricity per year
}
\description{
Use Primes, IEA data for Secondary Energy Electricity per year.
Info:
Primes: Trends for Secondary Energy Electricity data, EU countries until 2070
IEA: Trends for Secondary Energy Electricity data, 225 countries until 2050.
The trends are the same for each country depending to the region. For example
HKG and CHN have the same trends for capacity, 225 countries until 2050.
IEA mapping: "Africa" = "SSA", "Middle East" = "MEA", "Eurasia" = "REF",
"Southeast Asia" = "OAS", "Central and South America" = "LAM",
"Asia Pacific" = "CAZ", "Europe" = "NEU", "European Union" = "ELL"
calculate CAZ, NEU and ELL 
"CAZ" <- "CAZ" -  "OAS"
ELL and NEU have the same trends
IEA_non_EU <- "NEU" - "ELL"
"NEU" <- IEA_non_EU
"ELL" <- IEA_non_EU
The trends are multiplied with the
calcOutput(type = "IDataElecProd", mode = "Total", aggregate = FALSE) 
data to find the Secondary Energy Electricity data.
}
\examples{
\dontrun{
a <- calcOutput(type = "TDemand", aggregate = FALSE)
}

}
\author{
Fotis Sioutas
}
