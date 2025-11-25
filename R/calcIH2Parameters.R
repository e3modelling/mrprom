#' calcIH2Parameters
#'
#' Use technology data from excel Common_DATA from MENA_EDS model
#' to derive OPENPROM input parameter IH2Parameters.
#'
#' @return  OPENPROM input data IH2Parameters.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IH2Parameters", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr intersect %>% filter select
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIH2Parameters <- function() {
  
  Parameters <- c("mpips","lpipu","mpipu","AREA","MAXAREA","B","mid")
  
  x <- as.data.frame(expand.grid(Parameters, "EGY"))
  
  names(x) <- c("parameters", "region")
  x["value"] <-  c(0.54,0.05,1.00,83871,0.50,15,0.5)
  
  x[["value"]] <- as.numeric(x[["value"]])
  
  xq <- as.quitte(x)
  x <- as.magpie(xq)
  
  #the data is for EGY, put value of EGY to the countries
  suppressWarnings({
    x <- toolCountryFill(x, fill = xq["value"])
  })
  
  # set NA to 10^-6
  x[is.na(x)] <- 10^-6
  
  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "Common_DATA;MENA_EDS"))
  
}
