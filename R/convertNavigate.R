#' convertNavigate
#'
#' The ISO codes of "Navigate" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "Navigate" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte

convertNavigate <- function(x)
  {
  x <- as.quitte(x)
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("WITCH 5_0|Brazil" = "BRA",
                                           "WITCH 5_0|Canada" = "CAN",
                                           "WITCH 5_0|China" = "CHN",
                                           "WITCH 5_0|India" = "IND",
                                           "WITCH 5_0|Indonesia" = "IDN",
                                           "WITCH 5_0|Mexico" = "MEX",
                                           "WITCH 5_0|South Africa" = "ZAF",
                                           "WITCH 5_0|United States of America" = "USA",
                                           "REMIND 3_2|India" = "IND",
                                           "REMIND 3_2|Japan" = "JPN",
                                           "REMIND 3_2|United States of America" = "USA",
                                           "MESSAGEix-Materials|China" = "CHN",
                                           "IMAGE 3_3|Brazil" = "BRA",
                                           "IMAGE 3_3|Canada" = "CAN",
                                           "IMAGE 3_3|China" = "CHN",
                                           "IMAGE 3_3|India" = "IND",
                                           "IMAGE 3_3|Indonesia" = "IDN",
                                           "IMAGE 3_3|Japan" = "JPN",
                                           "IMAGE 3_3|Kazakhstan region" = "KAZ",
                                           "IMAGE 3_3|Mexico" = "MEX",
                                           "IMAGE 3_3|Russia" = "RUS",
                                           "IMAGE 3_3|South Africa" = "ZAF",
                                           "IMAGE 3_3|Turkey" = "TUR",
                                           "IMAGE 3_3|USA" = "USA",
                                           "IMACLIM 2_0|Brazil" = "BRA",
                                           "IMACLIM 2_0|Canada" = "CAN",
                                           "IMACLIM 2_0|China" = "CHN",
                                           "IMACLIM 2_0|India" = "IND",
                                           "IMACLIM 2_0|USA" = "USA",
                                           "COFFEE 1_5|Brazil" = "BRA",
                                           "COFFEE 1_5|Canada" = "CAN",
                                           "COFFEE 1_5|China" = "CHN",
                                           "COFFEE 1_5|India" = "IND",
                                           "COFFEE 1_5|Japan" = "JPN",
                                           "COFFEE 1_5|South Africa" = "ZAF",
                                           "COFFEE 1_5|Russia" = "RUS",
                                           "COFFEE 1_5|South Korea" = "KOR",
                                           "COFFEE 1_5|United States" = "USA",
                                           "R9CHINA" = "CHN",
                                           "R9INDIA" = "IND",
                                           "R9USA" = "USA"))
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, !is.na(x[["value"]]))
  x <- distinct(x)
  x <- as.quitte(x) %>% as.magpie()
  x <- toolCountryFill(x, fill = NA) 
  return(x[as.character(getISOlist()), , ])
  
}
