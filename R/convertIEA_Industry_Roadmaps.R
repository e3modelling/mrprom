#add description
convertIEA_Industry_Roadmaps <- function(x) {
  x <- as.quitte(x)
  # Manually map of the regions, converting only China, India, and USA to ISO codes
  # Assign custom codes for aggregated regions
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode(x[["region"]], mapping = c(
      "China" = "CHN",
      "India" = "IND",
      "United States" = "USA",
      "European Union" = "EUR",          
      "Middle East" = "MEA",             
      "Central and South America" = "CSA",
      "Africa" = "AFR"
    ))
  })

  
  # Remove regions that are not in the mapping
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- as.magpie(x)
  return(x)
}