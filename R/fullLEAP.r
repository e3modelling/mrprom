#' fullLEAP
#' 
#' Read in several files with data from UN world prospects, IMF, GEM-E3, Enerdata and convert it
#' to an xlsx file.
#' 
#' @return The read-in target data into a magpie object.
#'
#' @author Alexandros Tsimpoukis, Anastasis Giannousakis
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>%
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' a <- retrieveData("LEAP" )
#' }
fullLEAP <- function() {

  country = "Bangladesh"
  countryCode = "BGD"

  # total population and growth from UN
  x <- readSource("UNPopDiv")
  pop <- x[country,2010:2050,]
  getItems(pop,1) <- countryCode

  # GDP and growth from World-Bank, IMF for short projections and SSP2 for long term projections
  x1 <- readSource("WorldBankWDI")
  a1 <- x1[country,2010:2024,"GDP (constant 2015 US$)"]
  getItems(a1,1) <- countryCode

  x2 <- readSource("IMF", convert = TRUE)
  a2 <- x2[countryCode,2025:2030,"Gross domestic product (GDP), Constant prices, Domestic currency.Domestic currency"]
  getItems(a2,3) <- "GDP (constant 2015 US$)"
  a2 <- a2*10^7 # Convert from tenth millions dollars to dollars

  x3 <- calcOutput("iGDP", aggregate = FALSE)
  a3 <- x3[countryCode,2030:2050,]
  # Convert from PPP to MER with conversion factor from World Bank. Choose monetary year
  PPPtoMER <- x1[country,2015,"Price level ratio of PPP conversion factor (GDP) to market exchange rate"][1]
  a3 <- a3 * PPPtoMER
  a3 <- a3 * 10^9 #  Convert from Billions dollars to dollars
  # Harmonization via Scaling
  # Apply a scaling factor to all SSP2 values from 2030 onward
  scalingFactor <- a2[countryCode,2030,][1]/a3[countryCode,2030,][1]
  a4 <- a3 * scalingFactor
  getItems(a4,3) <- "GDP (constant 2015 US$)"

  GDP <- Reduce(mbind, list(a1, a2, a4[,2031:2050,]))

  # energy demand data - ENERDATA until 2021
  # shares
  x <- readSource("ENERDATA", subtype =  "Share", convert = TRUE)
  sharesResidential <- getData("Shares","households", countryCode, x)
  sharesAgriculture <- getData("Shares","agriculture", countryCode, x)
  sharesServices <- getData("Shares","services", countryCode, x)
  sharesIndustry <- getData("Shares","industry", countryCode, x)
  sharesTransport <- getData("Shares","transports", countryCode, x)  
  sharesNonEnergyUses <- getData("Shares","non energy industries", countryCode, x)

  shares <- Reduce(mbind, list(sharesResidential,sharesAgriculture, sharesServices,
              sharesIndustry,sharesTransport,sharesNonEnergyUses))

  # total values
  x <- readSource("ENERDATA", subtype =  "energy final consumption", convert = TRUE)
  consumptionAgriculture <- getTotal("agriculture", countryCode, x)
  consumptionIndustry <- getTotal("industry", countryCode, x)
  consumptionTransport <- getTotal("transport", countryCode, x)
  consumptionNonEnergyUses <- getTotal("non energy uses", countryCode, x) 
  consumptionServices <- x[countryCode,2010:2021,"Total energy final consumption of tertiary sector.Mtoe"]

  # specific industry
  consumptionNMMI <- getTotal("non-metallic minerals", countryCode, x)
  consumptionMI <- x[countryCode, 2010:2021, "Total energy final consumption miscellaneous industry"]

  # extract residential demand
  x <- readSource("ENERDATA", subtype =  "residential", convert = TRUE)
  consumptionResidential  <- x[countryCode,2010:2021,"Total final energy consumption of residential sector.Mtoe"]

  consumption <- Reduce(mbind, list(consumptionAgriculture, consumptionServices, consumptionIndustry, consumptionTransport, 
                        consumptionNonEnergyUses,consumptionResidential,consumptionNMMI,consumptionMI))

  # find fuels per category
  x <- readSource("ENERDATA", subtype =  "non-metallic minerals", convert = TRUE)
  fuelConsumptionNMMI <- getData("dummy","non-metallic minerals", countryCode, x)
  x <- readSource("ENERDATA", subtype =  "miscellaneous industry", convert = TRUE)
  fuelConsumptionMI <- getData("dummy","miscellaneous industry", countryCode, x)

  consumption <- Reduce(mbind, list(consumption,fuelConsumptionNMMI, fuelConsumptionMI))

  # activity - used code lines from calcACTV 
  x <- readSource("GEME3", convert = TRUE) #nolint
  map <- toolGetMapping("prom-gem-mappingNEW.csv", type = "sectoral", where = "mrprom") # nolint
  map <- filter(map, map[["PROM.Code"]] != "")
  tmp <- as.quitte(x[, , "Unit Cost"][, , map[["GEME3.Name"]]] * x[, , "Production Level"][, , map[["GEME3.Name"]]]) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>% # nolint
    collapseNames() # nolint

  tmp2 <- as.quitte(dimSums(x[, 2014, "Household Consumption"] * x[, 2014, "End-Use Price (Consumption Products)"], na.rm = TRUE)) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>% # nolint
    collapseNames() %>% # nolint
    magclass::setNames(nm = "HOU") # nolint

  # aggregate to OPEN-PROM sectors (from GEM sectors)
  rel <- select(map, c("GEME3.Name", "PROM.Code")) # gem-prom sectoral mapping
  tmp <- toolAggregate(tmp, rel = rel, weight = NULL, from = "GEME3.Name", to = "PROM.Code", dim = 3) # nolint
  x <- mbind(tmp, tmp2)

  a <- x[countryCode,2010:2050,]
  # convert from PPP to MER 
  a <- a * PPPtoMER
  
  # Separate activies from BM and all other to match energy consumptions from ENERDATA
  allIndustry <- a[, , c("OE", "IS", "NF", "CH", "PP", "BM", "EN", "OI", "TX", "FD")]
  sumIndustry <- dimSums(allIndustry, 3)
  getNames(sumIndustry) <- "TotalINDSE"
  otherIndustry <- sumIndustry - a[, , "BM"] 
  getNames(otherIndustry) <- "OtherINDSE"

  allIndustry <- Reduce(mbind, list(sumIndustry, otherIndustry))

  activity <- mbind(a, allIndustry)





}

# Helpers ------------------------------------------------
getData <- function(type=NULL, sector, countryCode, x,
                          years = 2010:2021,
                          fuels = c("Coal and lignite","coal and lignite", "oil", "Oil","Natural gas","gas",
                                    "electricity", "Electricity","heat","Heat","biomass","Biomass"),
                          warnMissing = TRUE) {
  # build expected item names
  if (type=="Shares") {
    expectedItems <- paste0("Share of ", fuels, " in ", sector, " consumption.%")
    expectedItemsNoOf <- paste0(fuels," final consumption ", sector, ".Mtoe")
  } else {
    expectedItems <- paste0(fuels," final consumption of ", sector, ".Mtoe")
    expectedItemsNoOf <- paste0(fuels," final consumption ", sector, ".Mtoe")
  }
  regions <- as.character(countryCode)

  # what items actually exist in x?
  actualItems <- getItems(x,3)
  presentItems <- intersect(expectedItems, actualItems)
  presentItemsNoOf <- intersect(expectedItemsNoOf, actualItems)
  missingItems <- setdiff(expectedItems, actualItems)

  if (length(missingItems) && warnMissing) {
    warning("Missing columns: ", paste(missingItems, collapse = ", "))
  }

  # subset what exists (could be empty)
  if (length(presentItems)) {
    presentSliceWithOf <- x[regions, years, presentItems, drop = FALSE]
    presentSliceNoOf <- x[regions, years, presentItemsNoOf, drop = FALSE]
    presentSlice <- mbind(presentSliceWithOf,presentSliceNoOf)

  } else {
    # create an empty magpie slice with zero items
    emptyArray <- array(NA_real_,
                        dim = c(length(regions), length(years), 0),
                        dimnames = list(regions, years, character(0)))
    presentSlice <- as.magpie(emptyArray)
  }
}

getTotal <- function(sector, countryCode, x) {
  total <- paste0("Total energy final consumption of ", sector, ".Mtoe")
  x[countryCode, 2010:2021, total]
}