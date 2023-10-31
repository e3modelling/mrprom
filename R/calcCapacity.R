#' calcCapacity
#' 
#' Derive data on capacity from ENERDATA.
#'
#' @return The ENERDATA data filtered by Capacity.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "Capacity", file = "iCapacity.csvr")
#' }
#'

calcCapacity <- function() {

x <- readSource("ENERDATA", "capacity", convert = TRUE)
x[is.na(x)] <- 0
#items <- c()
x <- x[, c(2010:2017), ]
list(x = x,
     weight = NULL,
     unit = getItems(x, "unit"),
     description = "Installed electricity capacity in MW; Source: ENERDATA")
}
