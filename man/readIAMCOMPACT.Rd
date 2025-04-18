% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readIAMCOMPACT.R
\name{readIAMCOMPACT}
\alias{readIAMCOMPACT}
\title{readIAMCOMPACT}
\usage{
readIAMCOMPACT(subtype = "study1")
}
\arguments{
\item{subtype}{The type is referring to the study

 Available types are:
\itemize{
\item `study0`:
\item `study1`:
\item `study2`:
\item `study3`:
\item `study4`:
\item `study6`:
\item `study7`:
\item `all`:
}}
}
\value{
magpie object with the requested output data
}
\description{
Read in a XLSX file from IAMCOMPACT and convert it to a magpie object.
}
\examples{
\dontrun{
a <- readSource("IAMCOMPACT", subtype = "study1")
}

}
\author{
Anastasis Giannousakis, Fotis Sioutas
}
