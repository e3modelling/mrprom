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

readGEME3 <- function(subtype = "SSP2") {

  # The activities from GEM-E3 file for SSP2 are the same as the older file
  # Baseline2026.03.17.gdx. It was renamed for consistency with the other SSP scenarios. The older file is still available in the package for reference.
  gdxfile <- paste0("Baseline_", subtype, ".gdx")

  # Read sector mapping once
  pr <- rgdx.set(gdxfile, "pr", names = "sector", te = TRUE)
  vctr <- as.data.frame(pr)

  # Explicit naming keeps the sector column independent of GDX domain metadata:
  # sector = sector code
  # .te = sector description
  vctr <- vctr %>%
    rename(
      sector_name = .te
    ) %>%
    mutate(sector = as.character(sector))

  # Function used to clean each GEM-E3 variable
  .cleanDataAllSets <- function(x) {

    x <- as.quitte(x)

    # Rename original GEM-E3 sector dimension
    names(x) <- sub("^pr$", "sector", names(x))

    x <- x %>%
      mutate(sector = as.character(sector))

    # Join sector descriptions and replace sector code with description
    ga <- x %>%
      left_join(vctr, by = "sector") %>%
      mutate(sector = sector_name) %>%
      select(-sector_name)

    return(ga)
  }
  # Read GEM-E3 variables from selected SSP baseline
  x <- readGDX(gdx = gdxfile, name = c("A_XD", "P_PD", "A_HC", "P_HC", "A_EXPOT", "P_PWE", "A_YVTWR"),
                  field = "l", restore_zeros = FALSE)
  
  # Clean all variables
  ga <- lapply(x, .cleanDataAllSets)
  
  levels(ga[["A_XD"]][["variable"]]) <- "Production Level"
  levels(ga[["P_PD"]][["variable"]]) <- "Unit Cost"
  levels(ga[["A_HC"]][["variable"]]) <- "Household Consumption"
  levels(ga[["P_HC"]][["variable"]]) <- "End-Use Prices"
  levels(ga[["A_EXPOT"]][["variable"]]) <- "Total Exports"
  levels(ga[["P_PWE"]][["variable"]]) <- "Unit Cost Exports"
  levels(ga[["A_YVTWR"]][["variable"]]) <- "Activity Exports"

  ga <- rbind(ga[["A_XD"]], ga[["P_PD"]], ga[["A_HC"]], ga[["P_HC"]], ga[["A_EXPOT"]], ga[["P_PWE"]], ga[["A_YVTWR"]])
  
  x <- as.magpie(ga)["EU28", , , invert = TRUE]
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Production Level, Unit Cost data, Household Consumption and Exports",
                       filename = gdxfile,
                       `Indicative size (MB)` = 491,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
