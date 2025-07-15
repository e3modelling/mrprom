#' readWSA_2025
#'
#' Read World Steel Association Crude Steel production by process 2024
#'
#' @return The read-in data into a quitte object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WSA_2025")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readWSA_2025 <- function() {
  
  x <- read_excel("ISProd_by_process_2024.xlsx")
  
  names(x) <- x[1,]
  
  x <- x[3:51,]
  
  names(x) <- sub("Country or region","region",names(x))
  names(x) <- sub("Year","period",names(x))
  names(x) <- sub("IS_Prod_WSA","value",names(x))
  
  x["value"] <- as.numeric(x[["value"]])
  x["BOFshare"] <- as.numeric(x[["BOFshare"]])
  x["EAFshare"] <- as.numeric(x[["EAFshare"]])
  x["OtherISROUTEshare"] <- as.numeric(x[["OtherISROUTEshare"]])
  x <- as.quitte(x)
  x["unit"] <- "Mt"
  x["variable"] <- "Crude Steel production"
  x <- filter(x, !is.na(x[["value"]]))
  x <- filter(x, !is.na(x[["region"]]))
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("Other CIS" = "Other CIS",
                                           "Other Middle East" = "Other Middle East",
                                           "Other North America" = "Other North America",
                                           "Other South America" = "Other South America",
                                           "Others" = "Others",
                                           "Taiwan, China" = "TWN"))
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  
  list(x = x,
       weight = NULL,
       class = "quitte",
       description = c(category = "World Steel Association Crude Steel production by process 2024",
                       type = "World Steel Association Crude Steel production by process 2024",
                       filename = "ISProd_by_process_2024.xlsx",
                       `Indicative size (MB)` = 0.37,
                       dimensions = "2D",
                       unit = "Mt",
                       Confidential = "E3M"))
}
