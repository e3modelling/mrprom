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
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGALL")
  sets <- unlist(strsplit(sets[, 1], ","))

  sets_remove <- c("CTHBMSWAS", "CCCGT", "PGLHYD", "PGSHYD", "PGWND", "PGSOL",
                   "PGANUC", "PGAPSS", "PGAPSSL", "PGACGSL", "PGACGS", "PGAGGS")

  sets <- sets[!(sets %in% sets_remove)]
  
  #This dataset includes Maturty factors on Capacity for Morocco
  # Temporarily adding data from MENA_EDS model/MOR_Calib.xlsx
  df1 <- data.frame(
    variable = rep(sets[1], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0, 41))

  df2 <- data.frame(
    variable = rep(sets[2], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.03, 41))

  df3 <- data.frame(
    variable = rep(sets[3], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.5, 41))

  df4 <- data.frame(
    variable = rep(sets[4], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(0.768, 7), rep(0.5, 34)))

  df5 <- data.frame(
    variable = rep(sets[5], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.05, 41))

  df6 <- data.frame(
    variable = rep(sets[6], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0, 41))

  df7 <- data.frame(
    variable = rep(sets[7], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.03, 41))

  df8 <- data.frame(
    variable = rep(sets[8], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0, 41))

  df9 <- data.frame(
    variable = rep(sets[9], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.03, 41))

  df10 <- data.frame(
    variable = rep(sets[10], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0, 41))

  df11 <- data.frame(
    variable = rep(sets[11], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.03, 41))

  df12 <- data.frame(
    variable = rep(sets[12], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0, 41))

  df13 <- data.frame(
    variable = rep(sets[13], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(1, 7), rep(0.5, 3), rep(0.1, 1), rep(0.5, 30)))

  df14 <- data.frame(
    variable = rep(sets[14], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(2, 7), rep(0.4, 3), rep(0.1, 1), rep(0.4, 30)))

  df15 <- data.frame(
    variable = rep(sets[15], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(0.768, 7), rep(0.4, 3), rep(0.1, 1), rep(0.4, 30)))

  df16 <- data.frame(
    variable = rep(sets[16], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0.02, 41))

  df17 <- data.frame(
    variable = rep(sets[17], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(0.025, 7), rep(0.23, 3), 0.4, rep(0.23, 11), 0.207, 0.1863,
              0.1677, 0.1509, 0.1358, 0.1222, rep(0.11, 13)))

  df18 <- data.frame(
    variable = rep(sets[18], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(0, 0.06, 0.12, 0.18, 0.24, 0.3, 25, rep(5, 4), 4.5, 4.05,
              3.645, 3.2805, 2.9525, 2.6572, 2.3915, 2.1523, 1.9371,
              1.7434, 1.5691, 1.4121, 1.2709, rep(1.1438, 17)))

  df19 <- data.frame(
    variable = rep(sets[19], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(1, rep(2, 6), rep(1, 3), 5, rep(0.6, 5), rep(0.35, 5),
              0.3255, 0.3027, 0.2815, 0.2618, 0.2435, 0.2264, 0.2106,
              0.1959, 0.1821, 0.1694, 0.1575, 0.1465, 0.1363, 0.1267,
              0.1178, 0.1096, 0.1019, 0.0948, 0.0882, 0.082))

  df20 <- data.frame(
    variable = rep(sets[20], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = rep(0, 41))

  df21 <- data.frame(
    variable = rep(sets[21], 41),
    model = rep("MENA_EDS", 41),
    scenario = rep("(Missing)", 41),
    region = rep("MAR", 41),
    unit = rep("factors", 41),
    period = (2010:2050),
    value = c(rep(0.007, 7), rep(0.1, 34)))

  xq <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12,
              df13, df14, df15, df16, df17, df18, df19, df20, df21)

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
