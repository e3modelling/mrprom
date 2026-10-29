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

  # calculate eff_s,k where s is the sector and k the technology
  ## PLACEHOLDER. For now, put the same efficiency for all k.
  techEff <- expand.grid(
    region = unname(getISOlist()),
    period = seq(extdata["fStartHorizon"], extdata["fEndY"]),
    AGRI_MODES = unique(AGRMODEStoTECH$AGRI_MODES)
  ) %>%
    left_join(AGRMODEStoTECH, by = c("AGRI_MODES"), relationship = "many-to-many") %>%
    left_join(AGRITECHTOEF, by = c("AGRITECH"), relationship = "many-to-many") %>%
    mutate(
      value = case_when(
        AGRI_MODES == "IRRIGATION" & AGRITECH == "TELC" ~ 260, # References: 10.1029/2025EF007970, 10.1039/d2va00126h
        AGRI_MODES == "IRRIGATION" & AGRITECH == "TGDO" ~ 260 * 3,
        AGRI_MODES == "IRRIGATION" & AGRITECH == "TSOL" ~ 260,
        AGRI_MODES == "FORESTRY" & AGRITECH == "TGDO" ~ 0.0019021, # Reference https://doi.org/10.1007/s40725-024-00219-3
        AGRI_MODES == "FORESTRY" & AGRITECH == "TGDO" ~ 0.0019021 * 0.7,
        TRUE ~ 1
      )
    ) %>%
    rename(variable = AGRI_MODES) %>%
    as.quitte() %>%
    as.magpie()
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
calcAgricultureEnergyIntensity <- function() {
  fEndY <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndY"]
  AGENRES <- readSource("AGENRES")
  ######   Livestock
  animal_energy_summary <- AGENRES[,,c("Dairy cows LU.LU.Scale of animal production","Runinants.LU.Scale of animal production")]
  animal_energy_summary <- dimSums(animal_energy_summary, 3) / 1e9 #1e9 ha
  
  ENERGY_USE <- AGENRES[,,c("Dairy.TJ.TOTAL ENERGY USE","Ruminants.TJ.TOTAL ENERGY USE")]
  ENERGY_USE <- dimSums(ENERGY_USE, 3) / 41868 #TJ to Mtoe
  
  Livestock <- ENERGY_USE / animal_energy_summary

  getItems(Livestock, 3) <- "Livestock"
  
  ######   Crops
  Crops <- AGENRES[,,c("Crops.TJ.Tillage","Crops.TJ.Crop maintenance","Crops.TJ.Harvesting","Crops.TJ.On farm transportation")]
  Crops <- dimSums(Crops, 3) / 41868 #TJ to Mtoe
  
  Crops_Service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  Crops_Service <- Crops_Service[getRegions(Crops), paste0("y", fEndY),"CROPS.1e9 ha"]
  Crops_Service <- collapseDim(Crops_Service,3)
  Crops_Service <- collapseDim(Crops_Service,2)
  
  Crops_energy <- Crops / Crops_Service
  
  Crops <- Crops_energy
  getItems(Crops, 3) <- "Crops"
  
  ######   Climate
  Climate <- AGENRES[,,c("Greenhouses.TJ.Rest")] + AGENRES[,,c("Greenhouses.TJ.Heating")]
  Climate <- dimSums(Climate, 3) / 41868 #TJ to Mtoe
  
  Climate_Service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  Climate_Service <- Climate_Service[getRegions(Climate), paste0("y", fEndY),"CLIMATE.1e9 ha"]
  Climate_Service <- collapseDim(Climate_Service,3)
  Climate_Service <- collapseDim(Climate_Service,2)
  
  Climate_energy <- Climate / Climate_Service
  
  Climate_energy[Climate_energy==0] <- NA

  
  Climate <- Climate_energy
  getItems(Climate, 3) <- "Climate"
  
  ####### region mean
  
  x <- mbind(Livestock, Crops, Climate)
  
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
