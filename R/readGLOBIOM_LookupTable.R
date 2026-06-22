#' readGLOBIOM_LookupTable
#'
#' Read GLOBIOM's biomass supply lookup table (sheet GLOBIOM_BioenSupCurve) and
#' return it melted to a magclass object. Spatial dim = GLOBIOM region (NOT
#' ISO; aggregated to OPEN-PROM regions later in calc via the bundled mapping),
#' temporal dim = year, third dim = three native subdimensions
#' variable / bioscen / ghgscen.
#'
#' Files expected in the madrat source folder \code{GLOBIOM_LookupTable/}:
#'   \code{GLOBIOM_LookupTable.xlsx} (subtype "lookup"),
#'   \code{eu_agriculture_ch4_n2o.csv} (subtype "euAgriculture").
#'
#' @param subtype one of "lookup" (default, the xlsx supply/emission lookup ->
#'   [region, year, variable.bioscen.ghgscen]); "euAgriculture"
#'   (the supplementary annual emission CSV -> [region, year, emtype]).
#' @return magclass object (dims depend on \code{subtype}, see above)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- readSource("GLOBIOM_LookupTable", convert = FALSE)
#' b <- readSource("GLOBIOM_LookupTable", subtype = "euAgriculture", convert = FALSE)
#' }
#' @seealso \code{\link{calcBmswasSupplyCoefGLOBIOM}}, \code{\link{calcBmswasLandEmisCoefGLOBIOM}}
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of
#' @importFrom magclass as.magpie getSets getSets<-
#' @importFrom utils read.csv
#' @export
readGLOBIOM_LookupTable <- function(subtype = "lookup") {
  if (subtype != "lookup") {
    # supplementary annual emission CSV shipped in the same source folder, read
    # directly (madrat runs readX with the source folder as the working dir):
    #   euAgriculture -> EU agriculture CH4/N2O (MAgPIE single run)
    file <- switch(subtype,
                   euAgriculture = "eu_agriculture_ch4_n2o.csv",
                   stop("readGLOBIOM_LookupTable: unknown subtype '", subtype, "'"))
    d <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
    ycols <- grep("^[0-9]{4}$", names(d), value = TRUE)
    long <- tidyr::pivot_longer(d[c("region", "emtype", ycols)],
              cols = dplyr::all_of(ycols), names_to = "period", values_to = "value")
    long$period <- as.integer(long$period)
    long$value  <- suppressWarnings(as.numeric(long$value))
    x <- as.magpie(as.data.frame(long[c("region", "period", "emtype", "value")]),
                   spatial = 1, temporal = 2, datacol = 4)
    getSets(x) <- c("region", "year", "emtype")
    return(x)
  }
  df <- read_excel("GLOBIOM_LookupTable.xlsx", sheet = "GLOBIOM_BioenSupCurve")
  ycols <- grep("^[0-9]{4}$", names(df), value = TRUE)
  long <- tidyr::pivot_longer(
    df[c("Region", "Variable", "BioScen", "GHGScen", ycols)],
    cols = dplyr::all_of(ycols), names_to = "period", values_to = "value")
  long$period <- as.integer(long$period)
  long$value  <- suppressWarnings(as.numeric(long$value))   # "Eps" etc -> NA

  # 3 native data subdims (variable/bioscen/ghgscen) — let magclass manage them;
  # do NOT pre-join into a string (magclass treats "." as the subdim separator).
  x <- as.magpie(
    as.data.frame(long[c("Region", "period", "Variable", "BioScen", "GHGScen", "value")]),
    spatial = 1, temporal = 2, datacol = 6)
  getSets(x) <- c("region", "year", "variable", "bioscen", "ghgscen")

  list(x = x,
       weight = NULL,
       unit = "mixed (EJ/yr; US$2000/GJ; Mt or kt /yr)",
       description = c(category = "Biomass",
                       type = "GLOBIOM biomass supply & land-use-emission lookup",
                       filename = "GLOBIOM_LookupTable.xlsx",
                       `Indicative size (MB)` = "1",
                       dimensions = "GLOBIOM region x year x variable.bioscen.ghgscen",
                       unit = "see calc",
                       Confidentiality = "open",
                       comment = "Melted; aggregated to OPEN-PROM regions in calcBmswas*Coef"))
}
