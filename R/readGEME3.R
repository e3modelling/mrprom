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
#' @import mrdrivers

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

  x1 <- readGDX(gdx = "ELV_SSP2_650P_400F_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                field = "l", restore_zeros = FALSE)
  y1 <- NULL
  name = c("A_XD", "P_PD", "A_HC", "P_HC")
  for (i in 1:length(x1)) {
    z <- x1[[i]][,getYears(x1[[i]],as.integer = TRUE)[getYears(x1[[i]],as.integer = TRUE)>2016],]
    z <- add_dimension(z, dim = 3.2, add = "variable", nm = name[i])
    y1 <- mbind(y1, z)
  }
  
  x2 <- readGDX(gdx = "ELV_SSP2_1150F_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                field = "l", restore_zeros = FALSE)
  
  y2 <- NULL
  name = c("A_XD", "P_PD", "A_HC", "P_HC")
  for (i in 1:length(x2)) {
    z <- x2[[i]][,getYears(x2[[i]],as.integer = TRUE)[getYears(x2[[i]],as.integer = TRUE)>2016],]
    z <- add_dimension(z, dim = 3.2, add = "variable", nm = name[i])
    y2 <- mbind(y2, z)
  }
  
  x3 <- readGDX(gdx = "ELV_SSP2_CP_D2_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                field = "l", restore_zeros = FALSE)
  
  y3 <- NULL
  name = c("A_XD", "P_PD", "A_HC", "P_HC")
  for (i in 1:length(x3)) {
    z <- x3[[i]][,getYears(x3[[i]],as.integer = TRUE)[getYears(x3[[i]],as.integer = TRUE)>2016],]
    z <- add_dimension(z, dim = 3.2, add = "variable", nm = name[i])
    y3 <- mbind(y3, z)
  }
  
  x4 <- readGDX(gdx = "ELV_SSP2_CP_D0_R3_Scenario.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC"),
                field = "l", restore_zeros = FALSE)
  
  y4 <- NULL
  name = c("A_XD", "P_PD", "A_HC", "P_HC")
  for (i in 1:length(x4)) {
    z <- x4[[i]][,getYears(x4[[i]],as.integer = TRUE)[getYears(x4[[i]],as.integer = TRUE)>2016],]
    z <- add_dimension(z, dim = 3.2, add = "variable", nm = name[i])
    y4 <- mbind(y4, z)
  }

  
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
