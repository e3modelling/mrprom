#' readWSA_2025
#'
#' Read World Steel Association Crude Steel production by process 2024
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WSA_2025", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readWSA_2025 <- function() {
  
  x <- read_excel("Steel_Production_USGS_myb1-2023-feste-ert.xlsx",
                  sheet = "T10")
  
  names(x) <- x[5,]
  x <- x[6:96,c(1,3,5,7,9,11)]
  
  x <- x %>%
    mutate(across(-`Country or locality3`, as.character))
  
  x <- x %>% pivot_longer(!("Country or locality3"), names_to = "period", values_to = "value")
  
  names(x)[1] <- "region"
  x["value"] <- as.numeric(x[["value"]])
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["value"]]))
  x <- filter(x, !is.na(x[["region"]]))
  x["unit"] <- "Mt"
  x["variable"] <- "IS"
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("Bangladeshe" = "BGD",
                                           "Burmae" = "MMR",
                                           "Jordane" = "JOR",
                                           "Kenyae" = "KEN",
                                           "Korea, North" = "PRK",
                                           "Montenegroe" = "MNE",
                                           "Nigeriae" = "NGA",
                                           "Omane" = "OMN",
                                           "Rwandae" = "RWA",
                                           "Tanzaniae" = "TZA",
                                           "Zambiae" = "ZMB",
                                           "Mauritaniae" = "MRT"))
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "World Steel Association Crude Steel production by process 2024",
                       type = "World Steel Association Crude Steel production by process 2024",
                       filename = "ISProd_by process_2024.xlsx",
                       `Indicative size (MB)` = 0.37,
                       dimensions = "2D",
                       unit = "Mt",
                       Confidential = "E3M"))
}
