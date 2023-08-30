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
