#' readIEAVAT
#'
#' Read IEA VAT rates(Value added tax).
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEAVAT", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readIEAVAT <- function() {
  
  x <- read_excel("IEA_Energy_Prices_Taxation_Information.xlsX", sheet = "VAT rates")
  
  names(x) <- x[1,]
  
  x <- x[-1,]
  
  x <- x %>% pivot_longer(!c("Country","Product","Sector","Tax type"), names_to = "period", values_to = "value")
  
  x <- x %>%
    mutate(Product = case_when(
      Product == "Coal" ~ "HCL",
      Product == "Natural gas" ~ "NGS",
      Product == "Liquefied petroleum gas" ~ "LPG",
      Product == "Gasoline" ~ "GSL",
      Product == "Kerosene" ~ "KRS",
      Product == "Gas/diesel oil" ~ "GDO",
      Product == "Fuel oil" ~ "RFO",
      Product == "Electricity" ~ "ELC",
      TRUE ~ Product  # keep original if no match
    ))
  
  x <- x %>%
    mutate(Sector = case_when(
      Sector == "Residential" ~ "SE",
      Sector == "Commercial" ~ "HOU",
      Sector == "Industry" ~ "INDSE",
      Sector == "Electricity generation" ~ "PG",
      Sector == "Transport" ~ "TRANSE",
      TRUE ~ Sector  # keep original if no match
    ))
  
  x <- x[,-4]
  
  names(x) <- c("region","fuel","variable","period","value")
  
  x["value"] <- as.numeric(x[["value"]])
  x <- filter(x, !is.na(x[["value"]]))
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x["unit"] <- "%"
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("Republic of Turkiye" = "TUR"))
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "IEA VAT rates(Value added tax)",
                       type = "IEA VAT rates(Value added tax)",
                       filename = "IEA_Energy_Prices_Taxation_Information.xlsx",
                       `Indicative size (MB)` = 3.4,
                       dimensions = "3D",
                       unit = "%",
                       Confidential = "E3M"))
}
