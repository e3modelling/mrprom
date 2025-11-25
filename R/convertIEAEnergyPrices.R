#' convertIEAEnergyPrices
#'
#' The ISO codes of "IEAEnergyPrices" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEAEnergyPrices" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEAEnergyPrices", convert = TRUE)
#' }
#'

convertIEAEnergyPrices <- function(x) {
  
  x <- as.quitte(x)
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("WORLD" = "GLO",
                                                   "MAFGHANIST" = "AFG",
                                                   "MANDORRA" = "AND",
                                                   "MBARBADOS" = "BRB",
                                                   "MBELIZE" = "BLZ",
                                                   "MBURKINAFA" = "BFA",
                                                   "MCABOVERDE" = "CPV",
                                                   "MCHAD" = "TCD",
                                                   "MGREENLAND" = "GRL",
                                                   "MGRENADA" = "GRD",
                                                   "MLESOTHO" = "LSO",
                                                   "MMALAWI" = "MWI",
                                                   "MMALI" = "MLI",
                                                   "MMAURITANI" = "MRT",
                                                   "MPAPUANG" = "PNG",
                                                   "MSEYCHELLE" = "SYC"))
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.magpie(x)
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })
  
  return(x[as.character(getISOlist()), , ])
  
}
