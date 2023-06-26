% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readENERDATA.R
\name{readENERDATA}
\alias{readENERDATA}
\title{readENERDATA}
\usage{
readENERDATA(subtype = "capacity")
}
\arguments{
\item{subtype}{By choosing a subtype you can filter the main ENERDATA dataset
(1800+ variables) by type, to allow further processing of specific variables}
}
\value{
The read-in data into a magpie object
}
\description{
Read in an csv file and convert it to a magpie object
The dataset contains several energy-related data types from ENERDATA for
various countries and years.
}
\author{
Anastasis Giannousakis, Fotis Sioutas

#' @examples
\dontrun{
a <- readSource("ENERDATA")
}
}