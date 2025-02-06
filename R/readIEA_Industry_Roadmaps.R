#' readIEA_Industry_Roadmaps
#'
#' Read in Regional energy demand for steelmaking and EAD shares and tech shares
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Sonja Sechi
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Industry_Roadmaps")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr %>%
#' @importFrom tidyr drop_na
#' @importFrom quitte as.quitte


readIEA_Industry_Roadmaps <- function() {
  
  a <- read_excel("IEA_Industries_Roadmaps_Reports.xlsx",
                  sheet = "IEA_IIS_Roadmap")
  
  Regional_energy_demand <- a[-1,1:7]
  Tech_shares <- a[-1,9:15]
  
  names(Regional_energy_demand) <- Regional_energy_demand[1,]
  names(Tech_shares) <- Tech_shares[1,]
  
  Regional_energy_demand <- Regional_energy_demand[-1,]
  Tech_shares <- Tech_shares[-1,]
  
  names(Regional_energy_demand) <- sub("Fuel Share", "variable", names(Regional_energy_demand))
  names(Regional_energy_demand) <- sub("Variable Value", "value", names(Regional_energy_demand))
  names(Regional_energy_demand) <- sub("Year", "period", names(Regional_energy_demand))
  names(Regional_energy_demand) <- sub("Unit", "unit", names(Regional_energy_demand))
  
  names(Tech_shares) <- sub("Process Route Share", "variable", names(Tech_shares))
  names(Tech_shares) <- sub("Variable Value", "value", names(Tech_shares))
  names(Tech_shares) <- sub("Year", "period", names(Tech_shares))
  names(Tech_shares) <- sub("Variable Unit", "unit", names(Tech_shares))
  
  x <- rbind(Regional_energy_demand, Tech_shares)
  
  x[["value"]] <- as.numeric(x[["value"]])
  x[["period"]] <- as.numeric(x[["period"]])
  
  x <- x %>% drop_na()
  x <- as.quitte(x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       description = c(category = "Industry",
                       type = "Regional energy demand for steelmaking and EAD
                       shares and tech shares",
                       filename = "IEA_Industries_Roadmaps_Reports.xlsx",
                       `Indicative size (MB)` = 0.031,
                       dimensions = "3D",
                       unit = "%",
                       Confidential = "E3M"))
}
