#' readGEM
#'
#' Read in a XLSX file and convert it to a quitte object
#' The data has information about capacities of power plants from the
#' Global Energy Monitor.
#'
#' @return quitte object with the requested output data about
#' capacities of power plants from the Global Energy Monitor.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GEM")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr drop_na
#' @importFrom dplyr %>% mutate filter select
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#'

readGEM <- function() {

  #coal
  coal <- read_excel("Global-Coal-Plant-Tracker-July-2023.xlsx",
                     sheet = "Units")
  coal <- coal[, c("Country", "Start year", "Retired year", "Capacity (MW)",
                   "Plant name", "Unit name")]
  names(coal)[1] <- "region"
  names(coal)[4] <- "value"
  coal["variable"] <- "coal"
  coal["value"] <- coal["value"] / 1000
  coal["unit"] <- "GW"

  #biomass
  biomass <- read_excel("Global-Bioenergy-Power-Tracker-GBPT-V1.xlsx",
                        sheet = "Data")
  biomass <- biomass[, c("Country", "Unit start year", "Retired year",
                         "Capacity (MW)", "Project name", "Unit name")]
  names(biomass)[1] <- "region"
  names(biomass)[2] <- "Start year"
  names(biomass)[4] <- "value"
  names(biomass)[5] <- "Plant name"
  biomass["variable"] <- "biomass"
  biomass["value"] <- as.numeric(unlist(biomass["value"]))
  biomass["value"] <- biomass["value"] / 1000
  biomass["unit"] <- "GW"

  #geothermal
  geothermal <- read_excel("Global-Geothermal-Power-Tracker-July-2023.xlsx",
                  sheet = "Data")
  geothermal <- geothermal[, c("Country", "Start year", "Retired year",
                               "Unit Capacity (MW)", "Project Capacity (MW)",
                               "Project Name", "Unit Name")]
  names(geothermal)[1] <- "region"
  names(geothermal)[6] <- "Plant name"
  names(geothermal)[7] <- "Unit name"
  `Project Capacity (MW)` <- NULL
  `Unit Capacity (MW)` <- NULL
  #take the Project Capacity, if NA, the Unit Capacity
  geothermal <- geothermal %>% mutate(value = ifelse(is.na(`Project Capacity (MW)`), `Unit Capacity (MW)`, `Project Capacity (MW)`)) %>%
    select(-c("Unit Capacity (MW)", "Project Capacity (MW)"))
  geothermal["variable"] <- "geothermal"
  geothermal["value"] <- as.numeric(unlist(geothermal["value"]))
  geothermal["value"] <- geothermal["value"] / 1000
  geothermal["unit"] <- "GW"

  #hydropower
  Hydropower <- read_excel("Global-Hydropower-Tracker-May-2023.xlsx",
                           sheet = "Data")
  Hydropower_1 <- Hydropower[, c("Country 1", "Start Year", "Retired Year",
                                 "Country 1 Capacity (MW)", "Project Name",
                                 "Status")]
  Hydropower_2 <- Hydropower[, c("Country 2", "Start Year", "Retired Year",
                                 "Country 2 Capacity (MW)", "Project Name",
                                 "Status")]
  names(Hydropower_1)[1] <- "region"
  names(Hydropower_2)[1] <- "region"
  names(Hydropower_1)[2] <- "Start year"
  names(Hydropower_2)[2] <- "Start year"
  names(Hydropower_1)[3] <- "Retired year"
  names(Hydropower_2)[3] <- "Retired year"
  names(Hydropower_1)[4] <- "value"
  names(Hydropower_2)[4] <- "value"
  hydropower <- rbind(Hydropower_1, Hydropower_2)
  names(hydropower)[5] <- "Plant name"
  names(hydropower)[6] <- "Unit name"
  hydropower["variable"] <- "hydropower"
  hydropower["value"] <- as.numeric(unlist(hydropower["value"]))
  hydropower["value"] <- hydropower["value"] / 1000
  hydropower["unit"] <- "GW"

  #small hydropower
  Small_Hydropower <- read_excel("Global-Hydropower-Tracker-May-2023.xlsx",
                                 sheet = "Below Threshold")
  Small_Hydropower_1 <- Small_Hydropower[, c("Country 1", "Start Year",
                                             "Retired Year", "Country 1 Capacity (MW)",
                                             "Project Name", "Status")]
  Small_Hydropower_2 <- Small_Hydropower[, c("Country 2", "Start Year",
                                             "Retired Year", "Country 2 Capacity (MW)",
                                             "Project Name", "Status")]
  names(Small_Hydropower_1)[1] <- "region"
  names(Small_Hydropower_2)[1] <- "region"
  names(Small_Hydropower_1)[2] <- "Start year"
  names(Small_Hydropower_2)[2] <- "Start year"
  names(Small_Hydropower_1)[3] <- "Retired year"
  names(Small_Hydropower_2)[3] <- "Retired year"
  names(Small_Hydropower_1)[4] <- "value"
  names(Small_Hydropower_2)[4] <- "value"
  small_hydropower <- rbind(Small_Hydropower_1, Small_Hydropower_2)
  names(small_hydropower)[5] <- "Plant name"
  names(small_hydropower)[6] <- "Unit name"
  small_hydropower["variable"] <- "small hydropower"
  small_hydropower["value"] <- as.numeric(unlist(small_hydropower["value"]))
  small_hydropower["value"] <- small_hydropower["value"] / 1000
  small_hydropower["unit"] <- "GW"

  #nuclear
  nuclear <- suppressWarnings(read_excel("Global-Nuclear-Power-Tracker-October-2023.xlsx",
                                   sheet = "Data"))
  nuclear <- nuclear[, c("Country", "Start Year", "Retirement Year", "Capacity (MW)",
                         "Project Name", "Unit Name")]
  names(nuclear)[1] <- "region"
  names(nuclear)[2] <- "Start year"
  names(nuclear)[3] <- "Retired year"
  names(nuclear)[4] <- "value"
  names(nuclear)[5] <- "Plant name"
  names(nuclear)[6] <- "Unit name"
  nuclear["variable"] <- "nuclear"
  nuclear["value"] <- nuclear["value"] / 1000
  nuclear["unit"] <- "GW"

  #wind onshore
  `Installation Type` <- NULL
  Wind <- suppressWarnings(read_excel("Global-Wind-Power-Tracker-December-2023.xlsx",
                        sheet = "Data"))
  Wind <- Wind[, c("Country", "Start year", "Retired year", "Capacity (MW)",
                   "Installation Type", "Project Name", "Phase Name")]
  Wind_onshore <- filter(Wind, `Installation Type` == "onshore")
  names(Wind_onshore)[1] <- "region"
  names(Wind_onshore)[4] <- "value"
  names(Wind_onshore)[6] <- "Plant name"
  names(Wind_onshore)[7] <- "Unit name"
  Wind_onshore["variable"] <- "wind onshore"
  Wind_onshore["value"] <- Wind_onshore["value"] / 1000
  Wind_onshore["unit"] <- "GW"
  Wind_onshore <- select(Wind_onshore, - ("Installation Type"))

  #Wind_offshore
  Wind_offshore <- filter(Wind, `Installation Type` %in% c("offshore mount unknown",
                         "offshore hard mount", "offshore floating"))
  names(Wind_offshore)[1] <- "region"
  names(Wind_offshore)[4] <- "value"
  names(Wind_offshore)[6] <- "Plant name"
  names(Wind_offshore)[7] <- "Unit name"
  Wind_offshore["variable"] <- "wind offshore"
  Wind_offshore["value"] <- Wind_offshore["value"] / 1000
  Wind_offshore["unit"] <- "GW"
  Wind_offshore <- select(Wind_offshore, - ("Installation Type"))

  #solar
  Solar_large <- read_excel("Global-Solar-Power-Tracker-December-2023.xlsx",
                        sheet = "Large Utility-Scale",
                        col_types = c(rep("guess", 11), "numeric", rep("guess", 18)))
  Solar_medium <- read_excel("Global-Solar-Power-Tracker-December-2023.xlsx",
                            sheet = "Medium Utility-Scale",
                            col_types = c(rep("guess", 11), "numeric", rep("guess", 18)))
  solar <- rbind(Solar_large, Solar_medium)

  solar <- solar[, c("Country", "Start year", "Retired year", "Capacity (MW)",
                     "Project Name", "Phase Name")]
  names(solar)[1] <- "region"
  names(solar)[4] <- "value"
  names(solar)[5] <- "Plant name"
  names(solar)[6] <- "Unit name"
  solar["variable"] <- "solar"
  solar["value"] <- as.numeric(unlist(solar["value"]))
  solar["value"] <- solar["value"] / 1000
  solar["unit"] <- "GW"
  # add 2024 SOL 217 GW to CHINA
  CHINA_2024_217 <- data.frame(
    "region" = "China",
    "Start year" = 2024,
    "Retired year" = NA,
    "value" = 217,
    "Plant name" = NA,
    "Unit name" = NA,
    "variable" = "solar",
    "unit" = "GW")
  names(CHINA_2024_217) <- names(solar)
  solar <- rbind(solar, CHINA_2024_217)

  #fuel oil
  oil_gas <- suppressWarnings(read_excel("Global-Oil-and-Gas-Plant-Tracker-GOGPT-August-2023.xlsx",
                     sheet = "Oil & Gas Units"))
  oil_gas <- oil_gas[, c("Country", "Start year", "Retired year", "Planned retire",
                         "Capacity (MW)", "Fuel", "Technology", "Plant name",
                         "Unit name")]
  `Planned retire` <- NULL
  `Retired year` <- NULL
  oil_gas <- oil_gas %>% mutate(`Retired year` = ifelse(is.na(`Planned retire`), `Retired year`, `Planned retire`)) %>%
    select(-c("Planned retire"))
  names(oil_gas)[1] <- "region"
  names(oil_gas)[4] <- "value"
  oil_gas["value"] <- as.numeric(unlist(oil_gas["value"]))
  oil_gas["value"] <- oil_gas["value"] / 1000
  oil_gas["unit"] <- "GW"
  oil_gas["Start year"] <- as.numeric(unlist(oil_gas["Start year"]))
  `Fuel` <- NULL
  fuel_oil <- filter(oil_gas, `Fuel` == c("FO"))
  fuel_oil["variable"] <- "fuel oil"
  fuel_oil <- select(fuel_oil, - c("Fuel", "Technology"))

  #natural gas
  natural_gas <- filter(oil_gas, `Fuel` == c("NG"))
  natural_gas["variable"] <- "natural gas"
  natural_gas <- select(natural_gas, - c("Fuel", "Technology"))

  #diesel oil
  diesel_oil <- filter(oil_gas, `Fuel` == c("D"))
  diesel_oil["variable"] <- "diesel oil"
  diesel_oil <- select(diesel_oil, - c("Fuel", "Technology"))

  x <- rbind(coal, biomass, geothermal, hydropower, small_hydropower, nuclear,
             Wind_onshore, Wind_offshore, solar, fuel_oil, natural_gas, diesel_oil)

  x <- as.data.frame(x)
  x[, "region"] <- toolCountry2isocode((x[, "region"]))
  names(x)[2] <- "period"
  x <- x %>% drop_na("value")
  x <- x %>% drop_na("region")

  x <- as.quitte(x)

  list(x = x,
       class = "quitte",
       description = c(category = "Power Generation",
                       type = "Capacities of power plants",
                       filename = "file3ef435ce6b42.xlsx",
                       `Indicative size (MB)` = 0.019,
                       dimensions = "4D",
                       unit = "GW",
                       Confidential = "E3M"))

}
