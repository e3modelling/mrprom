% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPOP.R
\name{calcPOP}
\alias{calcPOP}
\title{calcPOP}
\usage{
calcPOP(scenario = "SSP2")
}
\arguments{
\item{scenario}{SSP scenario choice
Available scenario are:
\itemize{
\item `SSP1`:
\item `SSP2`:
\item `SSP3`:
\item `SSP4`:
\item `SSP5`:
}}
}
\value{
The SSP data filtered by pop
}
\description{
Derive population data based on the SSP scenarios
}
\examples{
\dontrun{
POP <- calcOutput("POP", aggregate = FALSE)
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas
}
