#' readIEA_Industry_Roadmaps
#'
#' Read in Regional energy demand for steelmaking and EAD shares and tech shares
#' 
#' @param subtype Type of data that should be read. The type is referring to the
#' excel sheet, from the excel file "IEA_Industries_Roadmaps_Reports.xlsx"
#' and convert it to a magpie object.
#' Available types are:
#' \itemize{
#' \item `IEA_IIS_Roadmap`:
#' \item `IEA_Tech_Assumptions`:
#' }
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


readIEA_Industry_Roadmaps <- function(subtype = "IEA_IIS_Roadmap") {
  
  if (subtype == "IEA_IIS_Roadmap") {
    
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
    
  } else if (subtype == "IEA_Tech_Assumptions") {
    
    a <- read_excel("IEA_Industries_Roadmaps_Reports.xlsx",
                    sheet = subtype)
    
    Techn_Assumptions <- a
    names(Techn_Assumptions) <- a[1,]
    Techn_Assumptions <- Techn_Assumptions[-1,]
    
    names(Techn_Assumptions) <- sub("Variable name", "variable", names(Techn_Assumptions))
    names(Techn_Assumptions) <- sub("Variable Value", "value", names(Techn_Assumptions))
    names(Techn_Assumptions) <- sub("Year", "period", names(Techn_Assumptions))
    names(Techn_Assumptions) <- sub("Unit", "unit", names(Techn_Assumptions))
    
    Techn_Assumptions[["value"]] <- as.numeric(Techn_Assumptions[["value"]])
    Techn_Assumptions[["period"]] <- as.numeric(Techn_Assumptions[["period"]])
    x <- Techn_Assumptions
  }
  
  x <- as.quitte(x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       description = c(category = "Industry",
                       type = "Regional energy demand for steelmaking and EAD
                       shares and tech shares",
                       filename = "IEA_Industries_Roadmaps_Reports.xlsx",
                       `Indicative size (MB)` = 0.093,
                       dimensions = "3D",
                       unit = "varius",
                       Confidential = "E3M"))
}
