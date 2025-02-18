convertIEA_WEO_2023_ExtendedData <- function(x) {
  
  # Manually map regions: convert ISO-compatible regions and assign custom codes to aggregates
  x[["region"]] <- toolCountry2isocode(x[["region"]], mapping = c(
    # ISO country codes
    "United States" = "USA",
    "China" = "CHN",
    "India" = "IND",
    "Brazil" = "BRA",
    "Russia" = "RUS",
    "Japan" = "JPN",
    
    # Custom aggregate codes
    "North America" = "NAM",
    "Central and South America" = "CSA",
    "Europe" = "EUR",
    "European Union" = "EU27",
    "Africa" = "AFR",
    "Middle East" = "MEA",
    "Eurasia" = "EAS",
    "Asia Pacific" = "ASP",
    "Southeast Asia" = "SEA",
    "OECD" = "OECD",
    "Non-OECD" = "NONOECD",
    "Advanced economies" = "ADVECON",
    "Emerging market and developing economies" = "EMDEV"
  ))
  
  # Remove regions that are not in the mapping (if needed)
  x <- filter(x, !is.na(x[["region"]]))
  
  return(x)
}