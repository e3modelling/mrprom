#' calcFIT
#'
#' Calculate the Feed-In Tariff (FIT) for electricity per year
#'
#' @return  Magpie object with the Feed-In Tariff (FIT) for electricity per year
#'
#' @author Michalis Madianos, Dionysis Pramangioulis
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "FIT", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate pull
#' @importFrom tidyr replace_na
#' @importFrom quitte as.quitte


calcFIT <- function() {
    regions <- unname(getISOlist())
    years <- seq(2010, 2100)
    tech <- toolGetMapping(
        name = "PGALL.csv",
        type = "blabla_export",
        where = "mrprom"
    ) %>% pull("PGALL")

    data <- expand.grid(region = regions, period = years, variable = tech) %>%
        # Fill in the values for the Feed-In Tariff (FIT) for electricity per year
        mutate(
            value = NA,
            value = ifelse(region == "JPN" & period <= 2035 & period >= 2025 & variable == "PGAWND", 0.09, value),
            value = ifelse(region == "JPN" & period <= 2035 & period >= 2025 & variable == "PGAWNO", 0.25, value),
            value = ifelse(region == "JPN" & period <= 2035 & period >= 2025 & variable == "PGLHYD", 0.13, value),
            value = ifelse(region == "JPN" & period <= 2035 & period >= 2025 & variable == "PGSHYD", 0.20, value),
            value = ifelse(region == "JPN" & period <= 2035 & period >= 2025 & variable == "ATHBMSWAS", 0.15, value),
            value = ifelse(region == "JPN" & period <= 2035 & period >= 2025 & variable == "PGSOL", 0.06, value),
            value = ifelse(region == "JPN", value / 1.39, value),
            value = replace_na(value, 0)  # ADD THIS LINE
        ) %>%
        as.quitte() %>%
        as.magpie()

    weights <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) %>%
        add_columns(addnm = "PGH2F", dim = 3, fill = 0) %>%
        add_columns(addnm = paste0("y", seq(2024, 2100)), dim = 2)

    weights[, paste0("y", seq(2024, 2100)), ] <- weights[, "y2023", ]
    list(
        x = data,
        weight = weights,
        unit = "k$2015/MWh",
        description = "Null; Feed-In Tariff for electricity"
    )
}
