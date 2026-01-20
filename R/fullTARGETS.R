#' fullTARGETS
#'
#' @return The read-in target data into a magpie object.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>% group_by arrange
#' @importFrom tidyr pivot_wider
#' @importFrom zoo rollmean
#' @examples
#' \dontrun{
#' a <- retrieveData("TARGETS", regionmapping = "regionmappingOP.csv")
#' }
fullTARGETS <- function() {
  # --------- StockPC ----------------------------------------------
  x <- calcOutput(type = "TStockPC", aggregate = TRUE) %>%
    as.quitte() %>%
    mutate(value = ifelse(is.na(value), -1, value)) %>%
    select(region, period, tech, value) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )

  write.table(x,
    file = paste("tStockPC.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
  # ------------ Capacity ------------------------------------------
  df <- calcOutput("TInstCap", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2010) %>%
    group_by(region, variable) %>%
    arrange(period) %>%
    mutate(
      roll = rollmean(value, k = 10, fill = NA, align = "center"),
      roll_filled = ifelse(is.na(roll), value, roll)
    ) %>%
    ungroup() %>%
    select(region, variable, period, roll_filled) %>%
    rename(value = roll_filled)

  x <- df %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )

  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tCapacity.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  x <- getTShares(df)
  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tShares.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # ------------ ProdElec -----------------------------------------
  ProdElec <- calcOutput("TProdElec", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2010)

  x <- ProdElec %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
  names(x)[1:2] <- c("dummy", "dummy")

  write.table(x,
    file = paste("tProdElec.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  ProdElec[is.na(ProdElec)] <- 0

  x <- getTShares(ProdElec)
  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tShares_ProdElec.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # -------------- Elec Demand -------------------------------------
  x <- calcOutput(type = "TDemand", aggregate = TRUE) %>%
    as.quitte() %>%
    filter(period >= 2010) %>%
    select(c("region", "period", "value")) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
  names(x)[1] <- c("dummy")
  write.table(x,
    file = paste("tDemand.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  return(list(
    x = as.magpie(as.quitte(x)),
    weight = NULL,
    unit = "various",
    description = "OPENPROM targets"
  ))
}

# Helpers ------------------------------------------------
getTShares <- function(capacity) {
  shares <- toolTShares(capacity) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
}
