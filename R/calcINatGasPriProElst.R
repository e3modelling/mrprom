#' calcINatGasPriProElst
#'
#' Rate of coverage of natural gas consumption by domestic production, to derive
#' OPENPROM input parameter iNatGasPriProElst.
#' The data is from the "Enerdata".
#'
#' @return  OPENPROM input data iNatGasPriProElst.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "INatGasPriProElst", aggregate = FALSE)
#' }
#'

calcINatGasPriProElst <- function() {

  x <- readSource("ENERDATA", "rate", convert = TRUE)
  x <- x[, , "Rate of coverage of natural gas consumption by domestic production"] / 100

  qx <- as.quitte(x)
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("model", "scenario", "region",
                                                              "variable", "unit"))
  qx["period"] <- "mean"
  qx["unit"] <- "rate"
  qx <- distinct(qx)

  x <- as.quitte(qx) %>% as.magpie()
  x[is.na(x)] <- 0

  return(list(x = x,
              weight = NULL,
              unit = "Rate",
              description = "Enerdata; Rate of coverage of natural gas consumption by domestic production"))

}
