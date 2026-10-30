#' calcIDataAgricultureEff
#'
#' @return  Magpie object with the FAOProductionCrops
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgricultureEff", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataAgricultureEff <- function() {
  extdata <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )
  AGRMODEStoTECH <- toolGetMapping("AGRMODEStoTECH.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(AGRITECH, sep = ",")

  AGRITECHTOEF <- toolGetMapping("AGRITECHTOEF.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(EF, sep = ",")
  
  EnergyIntensity <- as.quitte(AgricultureEnergyIntensity()) %>%
    mutate(
      AGRI_MODES = case_when(
        variable %in% c("CLIMATE_HEATING", "CLIMATE_REST") ~ "CLIMATE",
        TRUE ~ variable
      ),
      type = case_when(
        variable == "CLIMATE_HEATING" ~ "HEATING",
        variable == "CLIMATE_REST" ~ "REST",
        TRUE ~ "NA"
      ),
      AGRITECH = case_when(
        AGRI_MODES == "CROPS" ~ "TGDO,TELC",
        AGRI_MODES == "POSTHARVESTING" ~ "TGDO,TELC",
        AGRI_MODES == "IRRIGATION" ~ "TGDO,TELC,TSOL",
        AGRI_MODES == "LIVESTOCK" ~ "TELC",
        AGRI_MODES == "CLIMATE" & type == "HEATING" ~
          "TNGS,THCL,TBMSWAS,TLPG,TLGN,TRFO,TSTE,TKRS",
        AGRI_MODES == "CLIMATE" & type == "REST" ~ "TELC",
        TRUE ~ NA_character_
      )
    ) %>%
    separate_rows(AGRITECH, sep = ",") %>%
    select(region, AGRI_MODES, type, AGRITECH, value)
  
  
  # Calculate efficiency for each agricultural sector and technology
  techEff <- expand.grid(
    region = unname(getISOlist()),
    period = seq(extdata["fStartHorizon"], extdata["fEndY"]),
    AGRI_MODES = unique(AGRMODEStoTECH$AGRI_MODES)
  ) %>%
    left_join(
      AGRMODEStoTECH,
      by = "AGRI_MODES",
      relationship = "many-to-many"
    ) %>%
    left_join(
      AGRITECHTOEF,
      by = "AGRITECH",
      relationship = "many-to-many"
    ) %>%
    left_join(
      EnergyIntensity,
      by = c("AGRI_MODES", "region", "AGRITECH"),
      relationship = "many-to-many"
    ) %>%
    select(-type) %>%
    mutate(
      value = coalesce(
        value,
        case_when(
          AGRI_MODES == "FORESTRY" & AGRITECH == "TGDO" ~ 0.0019021,
          AGRI_MODES == "FORESTRY" & AGRITECH == "TELC" ~ 0.0019021 * 0.7,
          TRUE ~ 1
        )
      ),
      value = if_else(
        AGRI_MODES == "IRRIGATION" & AGRITECH %in% c("TELC", "TSOL"),
        value / 3,
        value
      )
    ) %>%
    rename(variable = AGRI_MODES) %>%
    as.quitte() %>%
    as.magpie()
  
  techEff <- techEff[,seq(extdata["fStartHorizon"], extdata["fEndY"]),]
  techEff <- techEff[,seq(extdata["fStartHorizon"], extdata["fEndY"]),]
  
  # --------------- Weights ---------------------------
  weights <- techEff
  weights[, , ] <- 1

  list(
    x = techEff,
    weight = weights,
    unit = "Mtoe / [Activity]",
    description = "efficiency [Mtoe / Activity]"
  )
}

# ----------- Helper --------------------------------------
AgricultureEnergyIntensity <- function() {
  fEndY <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndY"]
  AGENRES <- readSource("AGENRES")
  ######   Livestock
  animal_energy_summary <- AGENRES[,,c("Dairy cows LU.LU.Scale of animal production","Runinants.LU.Scale of animal production")]
  animal_energy_summary <- dimSums(animal_energy_summary, 3) / 1e9 #1e9 ha
  
  ENERGY_USE <- AGENRES[,,c("Dairy.TJ.TOTAL ENERGY USE","Ruminants.TJ.TOTAL ENERGY USE")]
  ENERGY_USE <- dimSums(ENERGY_USE, 3) / 41868 #TJ to Mtoe
  
  Livestock <- ENERGY_USE / animal_energy_summary

  getItems(Livestock, 3) <- "LIVESTOCK"
  
  ######   Crops
  Crops <- AGENRES[,,c("Crops.TJ.Tillage","Crops.TJ.Crop maintenance","Crops.TJ.Harvesting","Crops.TJ.On farm transportation")]
  Crops <- dimSums(Crops, 3) / 41868 #TJ to Mtoe
  
  Crops_Service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  Crops_Service <- Crops_Service[getRegions(Crops), paste0("y", fEndY),"CROPS.1e9 ha"]
  Crops_Service <- collapseDim(Crops_Service,3)
  Crops_Service <- collapseDim(Crops_Service,2)
  
  Crops_energy <- Crops / Crops_Service
  
  Crops <- Crops_energy
  getItems(Crops, 3) <- "CROPS"
  
  ######   POSTHARVESTING
  POSTHARVESTING <- AGENRES[,,c("Crops.TJ.Drying","Crops.TJ.Feed preparation")]
  POSTHARVESTING <- dimSums(POSTHARVESTING, 3) / 41868 #TJ to Mtoe
  
  POSTHARVESTING_Service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  POSTHARVESTING_Service <- POSTHARVESTING_Service[getRegions(POSTHARVESTING), paste0("y", fEndY),"POSTHARVESTING.1e9 ha"]
  POSTHARVESTING_Service <- collapseDim(POSTHARVESTING_Service,3)
  POSTHARVESTING_Service <- collapseDim(POSTHARVESTING_Service,2)
  
  POSTHARVESTING_energy <- POSTHARVESTING / POSTHARVESTING_Service
  
  POSTHARVESTING <- POSTHARVESTING_energy
  getItems(POSTHARVESTING, 3) <- "POSTHARVESTING"
  
  ######   Climate
  Climate_Rest <- AGENRES[,,c("Greenhouses.TJ.Rest")]
  Climate_Heating <- AGENRES[,,c("Greenhouses.TJ.Heating")]
  Climate <- mbind(Climate_Rest, Climate_Heating) / 41868 #TJ to Mtoe
  
  Climate_Service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  Climate_Service <- Climate_Service[getRegions(Climate), paste0("y", fEndY),"CLIMATE.1e9 ha"]
  Climate_Service <- collapseDim(Climate_Service,3)
  Climate_Service <- collapseDim(Climate_Service,2)
  
  Climate_energy <- Climate / Climate_Service
  
  Climate_energy[Climate_energy==0] <- NA

  Climate_Rest <- Climate_energy[,,"Greenhouses.TJ.Rest"]
  Climate_Heating <- Climate_energy[,,"Greenhouses.TJ.Heating"]
  Climate_Rest <- collapseDim(Climate_Rest, 3)
  Climate_Heating <- collapseDim(Climate_Heating, 3)
  getItems(Climate_Heating, 3) <- "CLIMATE_HEATING"
  getItems(Climate_Rest, 3) <- "CLIMATE_REST"
  
  ######   IRRIGATION
  IRRIGATION <- AGENRES[,,c("Crops.TJ.Irrigation")]
  IRRIGATION <- dimSums(IRRIGATION, 3) / 41868 #TJ to Mtoe
  
  IRRIGATION_Service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  IRRIGATION_Service <- IRRIGATION_Service[getRegions(IRRIGATION), paste0("y", fEndY),"IRRIGATION.1e9 ha"]
  IRRIGATION_Service <- collapseDim(IRRIGATION_Service,3)
  IRRIGATION_Service <- collapseDim(IRRIGATION_Service,2)
  
  IRRIGATION_Service_energy <- IRRIGATION / IRRIGATION_Service
  
  IRRIGATION_Service_energy["IRL"] <- NA
  
  IRRIGATION <- IRRIGATION_Service_energy
  getItems(IRRIGATION, 3) <- "IRRIGATION"
  
  ####### region mean
  
  x <- mbind(Livestock, Crops, Climate_Heating, Climate_Rest, POSTHARVESTING, IRRIGATION)
  
  x <- toolCountryFill(x, fill = NA)
  
  # Assign to countries with NA the mean by period and variable
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  
  qx <- as.quitte(x)
  qx_bu <- as.quitte(x)
  
  names(qx) <- sub("region", "CountryCode", names(qx))
  
  # Add H12 mapping
  qx <- left_join(qx, h12, by = "CountryCode")
  
  # Calculate mean separately for each period and variable
  qx <- mutate(
    qx,
    value = mean(value, na.rm = TRUE),
    .by = c("period", "variable")
  )
  
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  
  names(qx) <- sub("CountryCode", "region", names(qx))
  
  # Fill missing values, matching by variable as well
  qx <- left_join(
    qx_bu,
    qx,
    by = c("region", "period", "variable", "unit")
  ) %>%
    mutate(
      value = coalesce(value.x, value.y)
    ) %>%
    select(-c("value.x", "value.y"))
  
  x <- as.quitte(qx) %>% as.magpie()
  
  final <- x[as.character(getISOlist()), , ]
  
  return(final)
}
