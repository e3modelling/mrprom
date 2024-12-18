#' calcUNFCCC_IP_CO2
#'
#' CO2 emissions from industrial processes "Mineral industry,
#' Chemical industry, Metal industry" from UNFCCC emissions data.
#' To derive OPENPROM input parameter UNFCCC_IP_CO2.
#' The data is from the "UNFCCC".
#'
#' @return  OPENPROM input data UNFCCC_IP_CO2
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios, Maro Baka
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "UNFCCC_IP_CO2", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte

calcUNFCCC_IP_CO2 <- function() {
  
  BM_CO2_IP <- readSource("UNFCCC", subtype = "2.A  Mineral Industry")
  CH_CO2_IP <- readSource("UNFCCC", subtype = "2.B  Chemical Industry")
  IS_CO2_IP <- readSource("UNFCCC", subtype = "2.C  Metal Industry")
  
  BM_CO2_IP <- BM_CO2_IP[,,"Net emissions/removals.kt.2_A  Mineral Industry.Total for category.CO2.NA"]
  getItems(BM_CO2_IP,3.3) <- "BM"
  BM_CO2_IP <- collapseDim(BM_CO2_IP, dim = c(3.1,3.4,3.5,3.6))
  
  CH_CO2_IP <- CH_CO2_IP[,,"Net emissions/removals.kt.2_B  Chemical Industry.Total for category.CO2.NA"]
  getItems(CH_CO2_IP,3.3) <- "CH"
  CH_CO2_IP <- collapseDim(CH_CO2_IP, dim = c(3.1,3.4,3.5,3.6))
  
  IS_CO2_IP <- IS_CO2_IP[,,"Net emissions/removals.kt.2_C  Metal Industry.Total for category.CO2.NA"]
  getItems(IS_CO2_IP,3.3) <- "IS"
  IS_CO2_IP <- collapseDim(IS_CO2_IP, dim = c(3.1,3.4,3.5,3.6))
  
  x <- mbind(BM_CO2_IP, CH_CO2_IP, IS_CO2_IP)
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- sub("y", "", tail(sort(getYears(x)), 1))
  x <- x[, c(fStartHorizon:lastYear), ]
  
  qx <- as.quitte(x)
  qx[["variable"]] <- qx[["category"]]
  qx <- select(qx, -"category")
  qx[["unit"]] <- "Mt CO2/yr"
  qx[["value"]] <- qx[["value"]] / 1000

  x <- as.quitte(qx) %>% as.magpie()
  x[is.na(x)] <- 0
  
  return(list(x = x,
              weight = NULL,
              unit = "Mt CO2/yr",
              description = "CO2 emissions from industrial processes"))
  
}
