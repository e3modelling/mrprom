#' readNavigate
#'
#' Read in NAVIGATE project model scenarios from IIASA.
#' The models are : 
#' COFFEE 1.5, REMIND-MAgPIE 3.2-4.6, IMAGE 3.3,
#' WITCH 5.0, IMACLIM 2.0, MESSAGEix-Materials.
#' 
#' The scenarios are : 
#' SUP_NPi_Default,SUP_2C_Default,SUP_1p5C_Default",
#' SUP_2C_Regional,SUP_1p5C_Regional,SUP_1p5C_Elec,SUP_1p5C_Elec_LimNuc,
#' SUP_1p5C_Elec_LimCCS,SUP_1p5C_Elec_HighVRE,SUP_1p5C_Elec_OptMAC,
#' SUP_1p5C_Comb,SUP_1p5C_Comb_LimNuc,SUP_1p5C_Comb_LimCCS",
#' SUP_1p5C_Comb_HighVRE,SUP_1p5C_Comb_OptMAC,SUP_2C_Elec,
#' SUP_2C_Elec_LimNuc,SUP_2C_Elec_LimCCS,SUP_2C_Elec_HighVRE,
#' SUP_2C_Elec_OptMAC,SUP_2C_Comb,SUP_2C_Comb_LimNuc,
#' SUP_2C_Comb_LimCCS,SUP_2C_Comb_HighVRE,SUP_2C_Comb_OptMAC,
#' NAV_Dem-NPi-ref,NAV_Ind_NPi
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Navigate", subtype = "SUP_NPi_Default")
#' }
#'
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_longer 
#'
readNavigate <- function(subtype = "SUP_NPi_Default") {

  
  if (file.exists("navigate_without_NA_with_NAV_Dem-NPi-ref_NAV_Ind_NPi.rds")) {
    x <- readRDS("navigate_without_NA_with_NAV_Dem-NPi-ref_NAV_Ind_NPi.rds")
  } else {
    x <- list.files(path = ".",
                    pattern = ".xlsx",
                    full.names = TRUE) %>%
      lapply(read_excel) %>%
      bind_rows
    x <- x %>% pivot_longer(cols = names(x)[6 : length(x)],
                             names_to = "period",
                             values_to = "value") 
  }
  names(x) <- sub("Variable", "variable", names(x))
  names(x) <- sub("name", "period", names(x))
  names(x) <- sub("Unit", "unit", names(x))
  names(x) <- sub("Model", "model", names(x))
  names(x) <- sub("Scenario", "scenario", names(x))
  x <- filter(x, x[["scenario"]] == subtype)
  names(x) <- sub("Region", "region", names(x))
  x <- filter(x, !is.na(x[["region"]]))
  x$variable <- factor(x$variable)
  x <- as.quitte(x) 
  x <- as.magpie(x)

  return(x)
}
