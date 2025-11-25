#' readWEPP2022
#'
#' Read in an excel file and convert it to a magpie object
#' The data has information about electric power plant
#'
#'@param subtype string. By choosing a subtype you filter the WEPP2022 dataset
#' (UTYPE column), to allow further processing of specific variables.
#' Available types are:
#' \itemize{
#' \item `PV`: Photovoltaic cells
#' \item `IC`: Internal combustion (reciprocating diesel) engine
#' \item `ST/S`: Steam turbine with steam sendout (cogen)
#' \item `IC/H`: Internal combustion engine with heat recovery (cogen - CHP)
#' \item `GT`: Gas/combustion turbine in simple (open) cycle
#' \item `HY`: Hydroelectric turbine generator
#' \item `WTG`: Wind turbine generator
#' \item `GT/H`: Microturbine with exhaust heat recovery (cogen - CHP)
#' \item `GT/S`: Gas turbine with steam sendout or other heat recovery (cogen - CHP)
#' \item `ST`: Steam turbine
#' \item `GT/C`: Gas turbine in combined-cycle
#' \item `ST/C`: Steam turbine in combined-cycle
#' \item `CC`: Combined-cycle, unspecified configuration
#' \item `ORC`: Organic Rankine-cycle (vapor) turbine or ORC energy converter, also includes some turbo-expanders
#' \item `PV/F`: Floating photovoltaic array
#' \item `WTG/O`: Wind turbine generator located offshore
#' \item `TTG`: Tidal or wave turbine generator or riverine kinetic energy system
#' \item `CC/S`: Combined-cycle in CHP (cogen) application with steam sendout or heat recovery
#' \item `CCSS`: Combined-cycle single shaft configuration
#' \item `GT/CP`: Gas turbine in combined-cycle CHP (cogen) power plant
#' \item `ST/CP`: Steam turbine in combined-cycle CHP (cogen) power plant
#' \item `FC`: Fuel cell
#' \item `TEX`: Turboexpander, gas expander or pressure recovery turbine
#' \item `IC/CD`: Internal combustion (reciprocating diesel) engine in combined-cycle
#' \item `ST/CD`: Steam turbine in combined-cycle with gas or diesel engines
#' \item `CCSS/P`: Combined-cycle single shaft configuration in CHP (cogen) application
#' \item `RSE`: Reciprocating steam engine
#' \item `FC/H`: Fuel cell with heat recovery
#' \item `GT/T`: Gas turbine in topping configuration with existing conventional boiler and T/G set
#' \item `TRT`: Top gas pressure recovery turbine, also known as a gas expansion turbine (used in steel works)
#' \item `IC/CP`: Internal combustion (reciprocating or diesel) engine in combined-cycle CHP (cogen) power plant
#' \item `WTG/OF`: Offshore wind turbine on floating platform
#' \item `ST/T`: Steam turbine with heat input from topping gas turbine
#' \item `PVH`: PV with integrated heat recovery, PV cogen
#' \item `IC/D`: Internal combustion engine with heat recovery for desalination
#' \item `ST/s`: Steam turbine with steam sendout (cogen)
#' \item `OTEC`: Ocean thermal energy conversion device
#' \item `ST/D`: Steam turbine with heat recovery for desalination
#' \item `ECE`: External combustion engine, Stirling or other designs, multifuel
#' \item `IFGT`: Indirectly-fired gas turbine, uses heat exchanger and operates as a hot air engine
#' \item `GT/R`: Gas turbine employed for steam-turbine repowering
#' \item `CC/CP`: Combined-cycle in CHP (cogen) application
#' \item `RSEX`: Rotary steam expander
#' \item `ORC/H`: ORC with heat recovery
#' \item `ACT`: Allam Cycle, oxy-combustion technology
#' \item `ST/CS`: Steam turbine in combined-cycle CHP (cogen) power plant for desalination
#' \item `GT/CS`: Gas turbine in combined-cycle CHP (cogen) power plant for desalination
#' \item `GT/D`: Gas turbine with heat recovery for desalination
#' \item `CC/D`: Combined-cycle with heat recovery for desalination
#' \item `SUN`: Solar power
#' }
#'
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WEPP2022", subtype = "PV")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#'

readWEPP2022 <- function(subtype = "PV") {

  Asia <- read_excel("SPG_WorldElectricPowerPlant_Asia_Sep2022_v1.xlsm",
                     sheet = "ASIA", range = "B5:AT61912")

  Europe <- read_excel("SPG_WorldElectricPowerPlants_Europe_Sep2022_v1.xlsm",
                       sheet = "EUROPE", range = "B5:AT73158")

  NAmerica <- read_excel("SPG_WorldElectricPowerPlants_NorthAmerica_Sep2022_v1.xlsm",
                         sheet = "NAMERICA", range = "B5:AT45109")

  Other <- read_excel("SPG_WorldElectricPowerPlants_Other_Sep2022_v1.xlsm",
                      sheet = "Other", range = "B5:AT47080")

  names(Other) <- sub("Country", "Country/Region", names(Other))

  x <- rbind(Asia, Europe, NAmerica, Other)

  names(x) <- sub("Country/Region", "region", names(x))
  names(x) <- sub("MW", "value", names(x))

  x$UTYPE <- factor(x$UTYPE)
  x <- filter(x, x[["UTYPE"]] %in% grep(subtype, (x[["UTYPE"]]), value = TRUE, ignore.case = TRUE))
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode(x[["region"]],
                                         mapping = c("CONGO DEM REP" = "COD",
                                                     "FALKLAND ISLANDS" = "FLK",
                                                     "SAINT-MARTIN" = "MAF",
                                                     "SOUTH GEORGIA" = "SGS",
                                                     "TURKS & CAICOS" = "TCA",
                                                     "ST VINCENT & GRENADINES" = "VCT",
                                                     "ST KITTS & NEVIS" = "KNA",
                                                     "Wallis and Futuna" = "WLF",
                                                     "TRISTAN DA CUNHA" = "SHN",
                                                     "ASCENSION ISLAND" = "SHN",
                                                     "ST HELENA" = "SHN",
                                                     "ENGLAND & WALES" = "GBR",
                                                     "SCOTLAND" = "GBR",
                                                     "NORTHERN IRELAND" = "GBR",
                                                     "WALLIS & FUTUNA" = "WLF",
                                                     "NORTHERN MARIANAS" = "MNP"))
  })

  x <- x[!is.na(x$region), ]
  x <- as.quitte(x)
  x <- as.magpie(x)

  list(x = x,
       weight = NULL,
       description = c(category = "Power Generation",
                       type = "Electric Power Plant",
                       filename = "SPG_WorldElectricPowerPlant_Asia_Sep2022_v1.xlsm",
                       `Indicative size (MB)` = 50,
                       dimensions = "4D",
                       unit = "varius",
                       Confidential = "E3M"))
}
