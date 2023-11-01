#' fullHISTORICAL
#'
#' Read data, convert it to a mrprom mif file so to compare output mif file
#' with OPEN-PROM output.
#'
#' @return The mif file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("HISTORICAL", regionmapping = "regionmappingOP.csv")
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select
#' @export

fullHISTORICAL <- function() {
  
  y <- NULL
  variable <- NULL
  new <- NULL
  data <- NULL
  for (i in c("TRANSE")) {
    x <- calcOutput(type = "IFuelCons", subtype = i, aggregate = TRUE)
    x[is.na(x)] <- 0
    xq <- as.quitte(x)
    xq$variable <- paste(xq$variable, xq$new)
    xq <- select((xq), -c(new))
    xq["model"] <- "ENERDATA"
    y <- rbind(y, xq)
  }
  
  gdp <- calcOutput("iGDP", aggregate = TRUE)
  q1 <- as.quitte(gdp)
  q1["variable"] <- "GDP|PPP"
  q1["unit"] <- "billion US$2015/yr"
  q1["model"] <- "SSP"
  
  POP <- calcOutput("POP", aggregate = TRUE)
  q2 <- as.quitte(POP)
  q2["variable"] <- "Population"
  q2["unit"] <- "billion"
  q2["model"] <- "SSP"
  q <- rbind(q1, q2)
  q <- select((q), -c(data))
  
  z <- rbind(y, q)
  write.mif(z, 'mrprom.mif', append = FALSE)
}