#' readGEME3
#'
#' Read Production Level and Unit Cost data as delivered in GDX files from GEME3.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GEME3")
#' }
#'
#' @importFrom gdxrrw rgdx.set
#' @importFrom gdx readGDX
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr select




readGEME3 <- function() {


  .cleanDataAllSets <- function(x) {

    x <- as.quitte(x)
    x <- select(x, !c("region"))
    names(x) <- sub("allcott", "region", names(x))
    names(x) <- sub("pr", "sector", names(x))

    x[["sector"]] <- as.numeric(x[["sector"]])
    x[["sector"]] <- factor(x[["sector"]])
    pr <- rgdx.set("Baseline.gdx", "pr", te = TRUE)
    vctr <- pr$pr
    names(vctr) <- pr$.te

    levels(x[["sector"]]) <- names(sort(vctr))

    return(x)

  }

  ga <- readGDX(gdx = "Baseline.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"), field = "l")
  ga <- lapply(ga, .cleanDataAllSets)
  levels(ga[["A_XD"]][["variable"]]) <- "Production Level"
  levels(ga[["P_PD"]][["variable"]]) <- "Unit Cost"
  levels(ga[["A_HC"]][["variable"]]) <- "Household Consumption"
  levels(ga[["P_HC"]][["variable"]]) <- "End-Use Price (Consumption Products)"
  ga <- rbind(ga[["A_XD"]], ga[["P_PD"]], ga[["A_HC"]], ga[["P_HC"]])

  levels(ga$region) <- sub("CRO", "HRV", levels(ga$region))
  levels(ga$region) <- sub("SAR", "SAU", levels(ga$region))
  levels(ga$region) <- sub("SAF", "ZAF", levels(ga$region))

  x <- as.magpie(ga)["EU28", , , invert = TRUE] # nolint
  return(x[, c(2014, seq(2015, 2100, 5)), ])


}
