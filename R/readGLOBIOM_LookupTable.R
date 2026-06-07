#' readGLOBIOM_LookupTable
#'
#' Read GLOBIOM's biomass supply lookup table (sheet GLOBIOM_BioenSupCurve) and
#' return it melted to a magclass object. Spatial dim = GLOBIOM region (NOT
#' ISO; aggregated to OPEN-PROM regions later in calc via the bundled mapping),
#' temporal dim = year, third dim = three native subdimensions
#' variable / bioscen / ghgscen.
#'
#' Source file expected in the madrat source folder:
#'   GLOBIOM_LookupTable/GLOBIOM_LookupTable.xlsx
#'
#' @return magclass object [GLOBIOM region, year, variable.bioscen.ghgscen]
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- readSource("GLOBIOM_LookupTable", convert = FALSE)
#' }
#' @seealso \code{\link{calcBmswasSupplyCoefGLOBIOM}}, \code{\link{calcBmswasEmisCoefGLOBIOM}}
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of
#' @importFrom magclass as.magpie getSets getSets<-
#' @export
readGLOBIOM_LookupTable <- function() {
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
