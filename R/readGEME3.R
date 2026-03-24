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
    pr <- rgdx.set("Baseline2026.03.17.gdx", "pr", te = TRUE)
    vctr <- pr
    vctr <- as.data.frame(vctr)
    names(vctr) <- sub("pr", "sector", names(vctr))
    ga <- left_join(x, vctr, by = c("sector")) %>%
      select(-c("sector"))
    names(ga) <- sub(".te", "sector", names(ga))
    return(ga)
  }
  
  x <- readGDX(gdx = "Baseline2026.03.17.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC", "A_EXPOT", "P_PWE", "A_YVTWR"),
                  field = "l", restore_zeros = FALSE)
  
  ga <- lapply(x, .cleanDataAllSets)
  
  levels(ga[["A_XD"]][["variable"]]) <- "Production Level"
  levels(ga[["P_PD"]][["variable"]]) <- "Unit Cost"
  levels(ga[["A_HC"]][["variable"]]) <- "Household Consumption"
  levels(ga[["P_HC"]][["variable"]]) <- "End-Use Prices"
  levels(ga[["A_EXPOT"]][["variable"]]) <- "Total Exports"
  levels(ga[["P_PWE"]][["variable"]]) <- "Unit Cost Exports"
  levels(ga[["A_YVTWR"]][["variable"]]) <- "Activity Exports"

  ga <- rbind(ga[["A_XD"]], ga[["P_PD"]], ga[["A_HC"]], ga[["P_HC"]], ga[["A_EXPOT"]], ga[["P_PWE"]], ga[["A_YVTWR"]])
  
  levels(ga$region) <- sub("CRO", "HRV", levels(ga$region))
  levels(ga$region) <- sub("SAR", "SAU", levels(ga$region))
  levels(ga$region) <- sub("SAF", "ZAF", levels(ga$region))
  
  x <- as.magpie(ga)["EU28", , , invert = TRUE]
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Production Level, Unit Cost data, Household Consumption and Exports",
                       filename = "Baseline2026.03.17.gdx",
                       `Indicative size (MB)` = 491,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
