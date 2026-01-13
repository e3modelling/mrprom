#' calcTStockPC
#'
#' Derive car stock per fuel technology for all countries.
#' @param subtype Period that starts the projection.
#'
#' @return magpie object
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' TStockPC <- calcOutput("TStockPC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter group_modify %>% mutate select rename group_by summarise ungroup inner_join full_join right_join recode first
#' @importFrom tidyr complete replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom zoo na.locf
#'
calcTStockPC <- function() {
  technologyMapping <- list(
    "CNG" = "TNGS",
    "Diesel Conventional" = "TGDO",
    "Diesel Hybrid" = "TCHEVGDO",
    "Diesel plug-in hybrid" = "TPHEVGDO",
    "E85" = "TETH",
    "Electric" = "TELC",
    "Gasoline Conventional" = "TGSL",
    "Gasoline Hybrid" = "TCHEVGSL",
    "Gasoline plug-in hybrid" = "TPHEVGSL",
    "Hydrogen" = "TH2F",
    "LPG" = "TLPG"
  )

  stockEU <- readSource("PrimesNewTransport", subtype = "Stock")
  stockEU <- stockEU[, , setdiff(getItems(stockEU, 3.2), "AVIATION")]
  stockEU <- stockEU %>%
    as.quitte() %>%
    filter(period >= 2010, sector == "PC") %>%
    mutate(
      variable = recode(variable, !!!technologyMapping),
      value = value / 1e3,
      unit = "million vehicles"
    ) %>%
    as.quitte() %>%
    interpolate_missing_periods(period = 2010:2070, expand.values = TRUE) %>%
    filter(variable != "AVIATION") %>%
    select(variable, period, region, value)

  names(stockEU) <- sub("variable", "tech", names(stockEU))

  stockPC <- calcOutput("StockPC", aggregate = FALSE)
  qstockPC <- as.quitte(stockPC)
  qstockPC <- select(qstockPC, c("region", "period", "tech", "value"))
  stockPC_until_2100 <- qstockPC %>%
    full_join(stockEU, by = c("region", "period", "tech"))

  # computes the period-to-period growth rate of value.y
  stockPC_until_2100 <- stockPC_until_2100 %>%
    arrange(region, tech, period) %>%
    group_by(region, tech) %>%
    mutate(
      # compute share (example: percent change as fraction)
      share = (value.y - lag(value.y)) / lag(value.y)
    ) %>%
    ungroup()

  # fills in missing combinations
  stockPC_until_2100 <- stockPC_until_2100 %>%
    complete(region, tech, period = 2010:2100)

  # Growth rates missing for some future periods
  # This assumes growth stays constant after the last observed value
  stockPC_until_2100 <- stockPC_until_2100 %>%
    group_by(region, tech) %>%
    mutate(
      share = if_else(is.na(share), NA_real_, share), # ensure NA
      # Forward-fills missing values
      share = zoo::na.locf(share, na.rm = FALSE)
    ) %>%
    ungroup()

  # creates a new column eu28_share that stores the EU28 value of share and copies it to all regions
  stockPC_until_2100 <- stockPC_until_2100 %>%
    group_by(tech, period) %>%
    mutate(eu28_share = first(share[region == "EU28"], default = NA_real_)) %>%
    ungroup()

  stockPC_until_2100 <- stockPC_until_2100 %>%
    select(region, tech, period, value.x, share, eu28_share)

  # fills missing values in share using the EU28 reference value
  stockPC_until_2100 <- stockPC_until_2100 %>%
    mutate(share = coalesce(share, eu28_share)) %>%
    select(-eu28_share)
  names(stockPC_until_2100) <- sub("value.x", "value", names(stockPC_until_2100))

  # difft =difft−1 × (1+sharet)
  stockPC_until_2100 <- stockPC_until_2100 %>%
    arrange(region, tech, period) %>%
    group_by(region, tech) %>%
    group_modify(~ {
      df <- .x
      # initialize diff with original value
      df$diff <- df$value
      # find indices after 2020
      idx <- which(df$period > 2020)
      if (length(idx) > 0) {
        for (i in idx) {
          df$diff[i] <- df$diff[i - 1] * (1 + df$share[i])
        }
      }
      df
    }) %>%
    ungroup()

  stockPC_until_2100 <- stockPC_until_2100 %>% select(-value, -share)
  names(stockPC_until_2100) <- sub("diff", "value", names(stockPC_until_2100))

  stockPC_until_2100 <- stockPC_until_2100 %>%
    as.quitte() %>%
    as.magpie()

  stock <- stockPC_until_2100[, 2021:2100][getISOlist(), , ]
  
  stock[is.na(stock)] <- 0

  list(
    x = stock,
    weight = NULL,
    unit = "million vehicles",
    description = "Activity data for OPENPROM sectors"
  )
}
