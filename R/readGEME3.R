#' readGEME3
#'
#' Read Production Level and Unit Cost data as delivered in GDX files from GEME3.
#'
#' @param subtype Type of carbon prices
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GEME3", subtype = "Npi")
#' }
#'
#' @importFrom gdxrrw rgdx.set
#' @importFrom gdx readGDX
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @import mrdrivers

readGEME3 <- function(subtype = "Npi") {

  .cleanDataAllSets <- function(x) {
    
    x <- as.quitte(x)
    names(x) <- sub("pr", "sector", names(x))

    pr <- rgdx.set("ELV_SSP2_CP_D0_R3_Scenario.gdx", "pr", te = TRUE)
    vctr <- pr
    vctr <- as.data.frame(vctr)
    names(vctr) <- sub("pr", "sector", names(vctr))
    
    ga <- left_join(x, vctr, by = c("sector")) %>%
      select(-c("sector"))
    names(ga) <- sub(".te", "sector", names(ga))
    
    return(ga)
    
  }
  
  x <- readGDX(gdx = "ELV_SSP2_CP_D0_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                  field = "l", restore_zeros = FALSE)

  
  ga <- lapply(x, .cleanDataAllSets)
  
  levels(ga[["A_XD"]][["variable"]]) <- "Production Level"
  levels(ga[["P_PD"]][["variable"]]) <- "Unit Cost"
  levels(ga[["A_HC"]][["variable"]]) <- "Household Consumption"
  levels(ga[["P_HC"]][["variable"]]) <- "End-Use Price (Consumption Products)"
  ga <- rbind(ga[["A_XD"]], ga[["P_PD"]], ga[["A_HC"]], ga[["P_HC"]])
  
  levels(ga$region) <- sub("CRO", "HRV", levels(ga$region))
  levels(ga$region) <- sub("SAR", "SAU", levels(ga$region))
  levels(ga$region) <- sub("SAF", "ZAF", levels(ga$region))
  
  x <- as.magpie(ga)["EU28", , , invert = TRUE] # nolint
  #X <- x[, c(2014, seq(2015, 2100, 5)), ]
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Production Level and Unit Cost data",
                       filename = "Baseline.gdx",
                       `Indicative size (MB)` = 491,
                       dimensions = "3D",
                       unit = "varius",
                       Confidential = "E3M"))

}
