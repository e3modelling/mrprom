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

  if (subtype == "Npi") {
    x <- readGDX(gdx = "ELV_SSP2_CP_D0_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                  field = "l", restore_zeros = FALSE)
  } else if (subtype == "1p5C") {
    x <- readGDX(gdx = "ELV_SSP2_650P_400F_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                 field = "l", restore_zeros = FALSE)
  } else if (subtype == "2C") {
    x <- readGDX(gdx = "ELV_SSP2_650P_400F_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                 field = "l", restore_zeros = FALSE)
  }
  
  ga <- NULL
  name = c("A_XD", "P_PD", "A_HC", "P_HC")
  for (i in 1:length(x)) {
    z <- x[[i]][,getYears(x[[i]],as.integer = TRUE)[getYears(x[[i]],as.integer = TRUE)>2016],]
    z <- add_dimension(z, dim = 3.2, add = "variable", nm = name[i])
    ga <- mbind(ga, z)
  }

  ga <- as.quitte(ga)
  names(ga) <- sub("allcott", "region", names(ga))
  names(ga) <- sub("pr", "sector", names(ga))
  
  ga[["sector"]] <- as.numeric(ga[["sector"]])
  ga[["sector"]] <- factor(ga[["sector"]])
  pr <- rgdx.set("ELV_SSP2_CP_D0_R3_Scenario.gdx", "pr", te = TRUE)
  vctr <- pr$pr
  names(vctr) <- pr$.te
  
  levels(ga[["sector"]]) <- names(sort(vctr))
  
  ga <- ga %>%
    mutate(variable = case_when(
      variable == "A_XD" ~ "Production Level",
      variable == "P_PD" ~ "Unit Cost",  # won't throw error if "B" not in data
      variable == "A_HC" ~ "Household Consumption",
      variable == "P_HC" ~ "End-Use Price (Consumption Products)",
      TRUE ~ variable  # keep unchanged if no match
    ))
  

  ga <- ga %>%
    mutate(region = case_when(
      region == "CRO" ~ "HRV",
      region == "SAR" ~ "SAU",  # won't throw error if "B" not in data
      region == "SAF" ~ "ZAF",
      TRUE ~ region  # keep unchanged if no match
    ))
  

  ga <- as.quitte(ga)
  x <- as.magpie(ga)["EU28", , , invert = TRUE] # nolint
  X <- x[, c(2014, seq(2015, 2100, 5)), ]
  
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
