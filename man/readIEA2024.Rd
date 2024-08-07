% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readIEA2024.R
\name{readIEA2024}
\alias{readIEA2024}
\title{readIEA2024}
\usage{
readIEA2024(subtype = "MAINELEC")
}
\arguments{
\item{subtype}{Type of data that should be read.}
}
\value{
The read-in data into a magpie object.
}
\description{
Read in energy balances from International Energy Agency until period 2022.
}
\examples{
\dontrun{
a <- readSource("IEA2024", subtype = "MAINELEC")
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas
}
