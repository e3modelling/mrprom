#' readEU_COM_RES
#'
#' Read in a XLSX file and convert it to a magpie object
#' The data has information about res max potential of EU countries from the
#' EUROPEAN COMMISSION.
#'
#' @return magpie object with the requested output data about
#' res potential of EU countries.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EU_COM_RES")
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate filter select
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#' @importFrom tidyselect starts_with
#'

readEU_COM_RES <- function() {
  #WIND_ONSHORE
  Wind_on <- read_excel("ENSPRESO_WIND_ONSHORE_OFFSHORE.xlsx",
                      sheet = "ONSHORE SUMMARY + graph", range = "T4:AC32")
  names(Wind_on)[1] <- "region"
  Wind_on <- Wind_on[, c(1, 4:6)]
  `15%< CF<20%...4` <- NULL
  `CF > 25%...5` <- NULL
  `20%  < CF < 25%...6` <- NULL
  Wind_on <- mutate(Wind_on, value = sum(c(`15%< CF<20%...4`, `CF > 25%...5`, `20%  < CF < 25%...6`), na.rm = TRUE), .by = c("region"))
  Wind_on <- Wind_on[, c(1, 5)]
  Wind_on["unit"] <- "GW"
  Wind_on["period"] <- "2010"
  Wind_on <- as.quitte(Wind_on) %>%
    interpolate_missing_periods(period = seq(from = 2010, to = 2050, by = 10), expand.values = TRUE)
  Wind_on["variable"] <- "wind onshore"
  Wind_on <- as.data.frame(Wind_on)
  Wind_on[, "region"] <- toolCountry2isocode((Wind_on[, "region"]), mapping = c("Hungaria" = "HUN",
                                                                    "Luxemburg" = "LUX",
                                                                    "EL" = "GRC"))
  #WIND_OFFSHORE
  Wind_off <- read_excel("ENSPRESO_WIND_ONSHORE_OFFSHORE.xlsx",
                        sheet = "OFFSHORE SUMMARY + graph", range = "T4:AA26")
  names(Wind_off)[1] <- "region"
  Wind_off <- Wind_off[, c(1, 6:8)]
  `Water depth 60-100m Floating...6` <- NULL
  `Water depth 30-60m...7` <- NULL
  `Water depth 0-30m...8` <- NULL
  Wind_off <- mutate(Wind_off, value = sum(c(`Water depth 60-100m Floating...6`, `Water depth 30-60m...7`, `Water depth 0-30m...8`), na.rm = TRUE), .by = c("region"))
  Wind_off <- Wind_off[, c(1, 5)]
  Wind_off["unit"] <- "GW"
  Wind_off["period"] <- "2010"
  Wind_off <- as.quitte(Wind_off) %>%
    interpolate_missing_periods(period = seq(from = 2010, to = 2050, by = 10), expand.values = TRUE)
  Wind_off["variable"] <- "wind offsore"
  Wind_off <- as.data.frame(Wind_off)
  Wind_off[, "region"] <- toolCountry2isocode((Wind_off[, "region"]), mapping = c("Hungaria" = "HUN",
                                                                                "Luxemburg" = "LUX",
                                                                                "EL" = "GRC"))
  #SOLAR
  Solar_pot <- read_excel("ENSPRESO_SOLAR_PV_CSP.xlsx",
                          sheet = "MS 170 W per m2 and 3%", range = "E4:AF6")

  Solar_pot <- as.data.frame(t(Solar_pot))
  rownames(Solar_pot) <- 1:28
  names(Solar_pot)[1] <- "region"
  names(Solar_pot)[2] <- "value"
  Solar_pot["unit"] <- "GW"
  Solar_pot["period"] <- "2010"
  Solar_pot["value"] <- as.data.frame(sapply(Solar_pot["value"], as.numeric))
  Solar_pot <- as.quitte(Solar_pot) %>%
    interpolate_missing_periods(period = seq(from = 2010, to = 2050, by = 10), expand.values = TRUE)
  Solar_pot["variable"] <- "solar"
  Solar_pot <- as.data.frame(Solar_pot)
  Solar_pot[, "region"] <- toolCountry2isocode((Solar_pot[, "region"]), mapping = c("Hungaria" = "HUN",
                                                                                  "Luxemburg" = "LUX",
                                                                                  "EL" = "GRC"))

  #BIOMASS
  Biomass_pot <- read.csv("ENSPRESO_BIOMASS.csv")
  Biomass_pot <- Biomass_pot[15:134, 1:37]
  X <- NULL
  Biomass_pot <- filter(Biomass_pot, X == Biomass_pot [4, 1] |
                          X == Biomass_pot [1, 1] | X == Biomass_pot [5, 1] |
                          X == Biomass_pot [6, 1] |
                          X == Biomass_pot [7, 1] | X == Biomass_pot [8, 1])

  colnames(Biomass_pot) <- Biomass_pot[1, ]
  Biomass_pot <- Biomass_pot[-1, ]
  rownames(Biomass_pot) <- 1:85
  Biomass_pot <- as.data.frame(sapply(Biomass_pot, as.numeric))
  counter <- 0
  `Row Labels` <- NULL
  for (i in unique(Biomass_pot[, 1])) {
    x <- filter(Biomass_pot, `Row Labels` == i)
    x <- colSums(x, na.rm = TRUE)
    x <- as.data.frame(x)
    Biomass_pot[counter + 1, ] <- x[1:37, 1]
    counter <- counter + 1
  }
  Biomass_pot <- Biomass_pot[1:5, 2:length(Biomass_pot)]
  Biomass_pot <- as.data.frame(t(Biomass_pot))
  Biomass_pot <- cbind(region = rownames(Biomass_pot), Biomass_pot)
  rownames(Biomass_pot) <- 1:nrow(Biomass_pot)
  names(Biomass_pot)[2:6] <- seq(from = 2010, to = 2050, by = 10)
  Biomass_pot["unit"] <- "GW"
  Biomass_pot <- Biomass_pot %>% pivot_longer(cols = starts_with("20"))
  names(Biomass_pot)[3] <- "period"
  Biomass_pot <- as.quitte(Biomass_pot)
  Biomass_pot["variable"] <- "biomass"
  Biomass_pot["value"] <- Biomass_pot["value"] * 0.0317057705 #from PJ/year to GW
  Biomass_pot <- as.data.frame(Biomass_pot)
  Biomass_pot[, "region"] <- toolCountry2isocode((Biomass_pot[, "region"]), mapping = c("Hungaria" = "HUN",
                                                                                    "Luxemburg" = "LUX",
                                                                                    "EL" = "GRC"))


  qx <- rbind(Wind_on, Wind_off, Solar_pot, Biomass_pot)
  x <- as.quitte(qx) %>% as.magpie()

  return(x)

}
