#' calcIMatFacPlaAvailCap
#'
#' Use data from MENA_EDS model to derive OPENPROM input parameter iMatFacPlaAvailCap.
#' This dataset includes Maturty factors on Capacity.
#'
#' @return magpie object with OPENPROM input data iMatFacPlaAvailCap.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IMatFacPlaAvailCap", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIMatFacPlaAvailCap <- function() {

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0("PGALL.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])

  sets_remove <- c("CTHBMSWAS", "PGLHYD", "PGSHYD", "PGWND", "PGSOL",
                   "PGANUC", "PGAPSS", "PGAPSSL", "PGACGSL", "PGACGS", "PGAGGS", "ATHBMSCCS", "PGAWND")

  sets <- sets[!(sets %in% sets_remove)]
  
  #This dataset includes Maturty factors on Capacity for Morocco
  # Temporarily adding data from MENA_EDS model/MOR_Calib.xlsx
  df1 <- data.frame(
    variable = rep("ATHLGN", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.0001, 41))

  df2 <- data.frame(
    variable = rep("ATHCOAL", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.03, 41))

  df3 <-  data.frame(
    variable = rep("ATHGAS", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(0.768, 7), rep(0.5, 34)))
  
  df4 <-data.frame(
    variable = rep("ATHBMSWAS", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.05, 41))

  df5 <- data.frame(
    variable = rep("ATHOIL", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))

  df6 <- data.frame(
    variable = rep("PGCSP", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(0, 0.06, 0.12, 0.18, 0.24, 0.3, 25, rep(5, 4), 4.5, 4.05,
              3.645, 3.2805, 2.9525, 2.6572, 2.3915, 2.1523, 1.9371,
              1.7434, 1.5691, 1.4121, 1.2709, rep(1.1438, 17)))


  df7 <- data.frame(
    variable = rep("PGOTHREN", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.0001, 41))

  df8 <- data.frame(
    variable = rep("PGAWNO", 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(1, 41))

  xq <- rbind(df1, df2, df3, df4, df5, df6, df7, df8)
  
  #sectors that are missing equal to 0.5, except PGAWND and PGSOL which are 0.5
  
  PGLHYD <- data.frame(
    variable = rep(sets_remove[3], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGSHYD <- data.frame(
    variable = rep(sets_remove[4], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGSOL <- data.frame(
    variable = rep(sets_remove[6], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.01, 41))
  
  PGANUC <- data.frame(
    variable = rep(sets_remove[7], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGAPSS <- data.frame(
    variable = rep(sets_remove[8], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGAPSSL <- data.frame(
    variable = rep(sets_remove[9], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGACGSL <- data.frame(
    variable = rep(sets_remove[10], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGACGS <- data.frame(
    variable = rep(sets_remove[11], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGAGGS <- data.frame(
    variable = rep(sets_remove[12], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  ATHBMSCCS <- data.frame(
    variable = rep(sets_remove[13], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))
  
  PGAWND <- data.frame(
    variable = rep(sets_remove[14], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(20, 41))

  #rbind with the sectors that are missing
  xq <- rbind(xq, PGLHYD, PGSHYD, PGSOL, PGANUC, PGAPSS,
              PGAPSSL, PGACGSL, PGACGS, PGAGGS, ATHBMSCCS, PGAWND)
  
  # Interpolating the missing values for the specified time period
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)

  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  
  #the data is for Morocco, put value of Morocco to the countries
  x <- toolCountryFill(x, fill = xq["value"])

  list(x = x,
       weight = NULL,
       unit = "factors",
       description = "Maturty factors on Capacity, MENA_EDS model")
}
