#' calcSharesFuelPrices
#'
#' @return SharesFuelPrices
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' SharesFuelPrices <- calcOutput("SharesFuelPrices", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% filter group_by mutate if_else select ungroup
#' @importFrom quitte as.quitte interpolate_missing_periods

calcSharesFuelPrices <- function() {
  
  fEndY <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndY"]
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  a <- readSource("PrimesFuelPrices")
  
  TRANSE <- toolGetMapping("TRANSE.csv",
                           type = "blabla_export",
                           where = "mrprom"
  ) 
  
  SECtoEF <- toolGetMapping("SECtoEF.csv",
                            type = "blabla_export",
                            where = "mrprom"
  )  %>% filter(
    SEC %in% TRANSE[,1],
    EF %in% getItems(a,3.5)
  )
  
  GDOShare <- a[,,"BGDO"] / a[,,"GDO"]
  GDOShare <- collapseDim(GDOShare, c(3.1,3.2,3.3,3.5,3.6))
  GDOShare <- GDOShare[,,c("Transport private", "Transport public", "Rail", "Inland navigation")]
  getItems(GDOShare, 3) <- c("PC", "PB", "PT", "PN") # "GT" = "PT", "GN" = "PN", "GU" = "PB"
  
  GDOShareGT <- GDOShare[,,"PT"]
  getItems(GDOShareGT, 3) <- c("GT")
  
  GDOShareGN <- GDOShare[,,"PN"]
  getItems(GDOShareGN, 3) <- c("GN")
  
  GDOShareGU <- GDOShare[,,"PB"]
  getItems(GDOShareGU, 3) <- c("GU")
  
  GDOShareTotal <- mbind(GDOShare, GDOShareGT, GDOShareGN, GDOShareGU)
  GDOShareTotal <- add_dimension(GDOShareTotal, dim = 3.2, add = "fuel", nm = "shareBGDO")
  
  GSLShare <- a[,,"BGSL"] / a[,,"GSL"]
  GSLShare <- collapseDim(GSLShare, c(3.1,3.2,3.3,3.5,3.6))
  GSLShare <- GSLShare[,,c("Transport private", "Transport public")]
  getItems(GSLShare, 3) <- c("PC", "PB") # "GU" = "PB"
  
  GSLShareGU <- GSLShare[,,"PB"]
  getItems(GSLShareGU, 3) <- c("GU")
  
  GSLShareTotal <- mbind(GSLShare, GSLShareGU)
  GSLShareTotal <- add_dimension(GSLShareTotal, dim = 3.2, add = "fuel", nm = "shareBGSL")
  
  KRSShare <- a[,,"BKRS"] / a[,,"KRS"]
  KRSShare <- collapseDim(KRSShare, c(3.1,3.2,3.3,3.5,3.6))
  KRSShare <- KRSShare[,,c("Transport public")]
  getItems(KRSShare, 3) <- "PA"
  KRSShareTotal <- add_dimension(KRSShare, dim = 3.2, add = "fuel", nm = "shareBKRS")
  
  ######biogas
  NGSShare <- a[,,"BGAS"] / a[,,"NGS"]
  NGSShare <- collapseDim(NGSShare, c(3.1,3.2,3.3,3.5,3.6))
  NGSShare <- NGSShare[,,c("Transport private", "Transport public")]
  getItems(NGSShare, 3) <- c("PC", "PB") #  "GU" = "PB"
  
  NGSShareGU <- NGSShare[,,"PB"]
  getItems(NGSShareGU, 3) <- c("GU")
  
  NGSShareTotal <- mbind(NGSShare, NGSShareGU)
  NGSShareTotal <- add_dimension(NGSShareTotal, dim = 3.2, add = "fuel", nm = "shareBGAS")
  ##########
  
  x <- mbind(GDOShareTotal, GSLShareTotal, KRSShareTotal, NGSShareTotal)
  
  x <- toolCountryFill(x, fill = NA)
  
  q <- as.quitte(x)
  
  q <- q %>%
    group_by(period, subsector, fuel) %>%
    mutate(
      mean_value = mean(value[value != 0], na.rm = TRUE),
      mean_value = if_else(is.nan(mean_value), NA_real_, mean_value),
      value = if_else(is.na(value), mean_value, value)
    ) %>%
    select(-mean_value) %>% 
    ungroup()
  
  q <- as.quitte(q) %>%
    interpolate_missing_periods(period = fStartHorizon : 2025, expand.values = TRUE)
  
  x <- as.quitte(q) %>% as.magpie()
  
  x <- x[,fStartHorizon : 2025,]
  
  list(x = x,
       weight = NULL,
       unit = "1",
       description = "SharesFuelPrices")
}
