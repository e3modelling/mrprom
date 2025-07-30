#' readWorldBankWDI
#'
#' Use library "WDI" to extract data from the World Bank and convert it to a magpie object
#' The data has information about GDP per country and per year. The country and indicator list is pre-defined and hardcoded. 
#'           indicators= "NY.GDP.MKTP.KD,NY.GDP.PCAP.KD,NY.GDP.DEFL.ZS,NY.GDP.MKTP.KD.ZG"  # GDP (constant 2015 US$), GDP per capita (constant 2015 US$), GDP deflator, annual growth
#' @return magpie object with the requested output data about GDP per selected country and per year
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- readSource("WorldBankWDI", convert = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @import WDI
#'
#' @export

readWorldBankWDI <- function() {

  # define the countries and indicator's list
  indicators= "NY.GDP.MKTP.KD,NY.GDP.PCAP.KD,NY.GDP.DEFL.ZS,NY.GDP.MKTP.KD.ZG"

  # parse parameters
  if (is.null(indicators)) {
    stop("You must provide indicators.")
  }
  indicatorCodes <- trimws(strsplit(as.character(indicators), ",", fixed = TRUE)[[1]])
  
  dat = WDI(indicator=indicatorCodes)

  # drop country codes
  drop <- c("iso2c","iso3c")
  dat = dat[,!(names(dat) %in% drop)]

  # rename columns to facilitate conversion to magpie object
  names(dat)[names(dat) == "NY.GDP.MKTP.KD"] <- "GDP (constant 2015 US$)"
  names(dat)[names(dat) == "NY.GDP.PCAP.KD"] <- "GDP per capita (constant 2015 US$)"
  names(dat)[names(dat) == "NY.GDP.DEFL.ZS"] <- "GDP deflator (base year varies by country)"
  names(dat)[names(dat) == "NY.GDP.MKTP.KD.ZG"] <- "GDP (annual % growth)"
  
  x <- as.magpie(
  x        = dat,
  spatial  = 1,    # Location
  temporal = 2,    # Year
  data     = 3     # data columns
  )
  
  list(x = x,
      weight = NULL,
      description = c(type = "GDP (NY.GDP.MKTP.KD), GDP per capita (NY.GDP.PCAP.KD), 
                    GDP deflator (base year varies by country) (NY.GDP.DEFL.ZS), 	GDP (NY.GDP.MKTP.KD.ZG)",
                      unit = "(constant 2015 US$), constant 2015 US$,-,  (annual % growth)"))
}
