#' calcNDC_LTT_NECP
#' 
#' Reads the raw input table for the dataset type \code{"NDC_LTT_NECP"} and
#' derives **absolute unconditional and conditional emissions targets** for each
#' country and target year.
#' 
#' The raw file can contain targets expressed as:
#' \itemize{
#'   \item absolute target emissions (preferred, used directly),
#'   \item percent reduction relative to a base-year emissions level, or
#'   \item percent reduction relative to a BAU emissions level.
#' }
#' 
#' Targets are parsed from three blocks in the source file:
#' \itemize{
#'   \item \strong{Interim/First NDC},
#'   \item \strong{Revised NDC},
#'   \item \strong{Long-Term target (LTT)}.
#' }
#' \strong{Calculation rules}
#' \itemize{
#'   \item If an absolute target is available (\code{absEmissionsUncond*} /
#'     \code{absEmissionsCond*}), it is used as-is.
#'   \item Else, if base-year emissions and percent reduction are available,
#'     the target is computed as \eqn{baseEmis * (1 - pct/100)}.
#'   \item Else, if BAU emissions and percent reduction are available,
#'     the target is computed as \eqn{bauEmis * (1 - pct/100)}.
#'   \item Otherwise the target is \code{NA}.
#' }
#'
#' For Long-Term targets, the file often stores a single target value; it is
#' interpreted as an unconditional absolute target when numeric. Textual targets
#' (e.g., "Net Zero") will become \code{NA} after numeric conversion.
#' 
#' @return A list with the elements: magpie object with absolute emissions targets in Mt CO2e/yr and some metadata.
#' 
#' @author Anastasis Giannousakis, Alexandros Tsimpoukis
#' 
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "NDC_LTT_NECP", aggregate = FALSE)
#' }
#' 
calcNDC_LTT_NECP <- function() {

  x <- readSource("NDC_LTT_NECP", convert = FALSE)
  result <- processNdcTargets(x[[1]])
  magpieObject <- result %>%
	# Select only the necessary columns
	select(country, targetYear, targetUnconditionalMtCO2e, targetConditionalMtCO2e) %>%

	# Pivot to long format: We need a single "value" column and a "variable" column
	pivot_longer(
			cols = c(targetUnconditionalMtCO2e, targetConditionalMtCO2e),
			names_to = "variable",
			values_to = "value"
	) %>%
	# Ensure years are standard (e.g., integers)
	mutate(year = as.integer(targetYear)) %>%

	# Select final columns for magclass conversion
	select(region = country, year, variable, value)
	magpieObjectNDC <- as.magpie(magpieObject, spatial = "region", temporal = "year")

	finalMagpie <- toolCountryFill(magpieObjectNDC, fill = NA, verbosity = 2)
  
	list(x = finalMagpie,
       weight = NULL,
       unit = "Mt CO2e/yr",
       description = "Condition and unconditional emission targets from NDC and LTT")
}
# Helpers ------------------------------------------------------------------------------------
processNdcTargets <- function(rawData) {
  
  # 1. Define Column Mapping (Same as before)
  # ------------------------------------------------------------------
  columnMapping <- c(
    "lastUpdated", "idCode", "name", "country", "geographicRegion","actorType", "statusDate", "interimTarget",
    # Block 1: Interim/First
    "pctReductionUncond1", "pctReductionCond1", "baseYear1", "baseYearEmissions1", 
    "absEmissionsUncond1", "absEmissionsCond1", "bauEmissions1", "targetYear1", "targetType1",
    # Block 2: Revised
    "revisedTargetYear2", "pctReductionUncond2", "pctReductionCond2", "baseYear2", 
    "emissionsIncluded2", "baseYearEmissions2", "absEmissionsUncond2", "absEmissionsCond2", 
    "bauEmissions2", "targetType2",
    # LTT and Metadata
    "reductionValuesPct", "longTermTargetYear", "targetValue", "emissionsIncluded", 
    "comparisonYear", "legalStatus", "extraCol34", "comments", "extraCol36"
  )
  
  if(ncol(rawData) < length(columnMapping)) {
    warning("File has fewer columns than the mapping. Check structure.")
  }
  rawData <- rawData[, 1:length(columnMapping)]
  colnames(rawData) <- columnMapping
  
  toNum <- function(x) suppressWarnings(as.numeric(as.character(x)))
  
  # Initial NDC
  # ------------------------------------------------------------------
  df1 <- rawData %>%
    transmute(
      country = country,
      idCode = idCode,
      sourceType = "Interim/First",
      targetYear = toNum(targetYear1),
      
      # Calculation Inputs
      absUncond = toNum(absEmissionsUncond1),
      absCond   = toNum(absEmissionsCond1),
      baseEmis  = toNum(baseYearEmissions1),
      bauEmis   = toNum(bauEmissions1),
      pctUncond = toNum(pctReductionUncond1),
      pctCond   = toNum(pctReductionCond1),
      comments  = comments
    )
  
  # Revised NDC
  # ------------------------------------------------------------------
  df2 <- rawData %>%
    transmute(
      country = country,
      idCode = idCode,
      sourceType = "Revised",
      targetYear = toNum(revisedTargetYear2),
      
      # Calculation Inputs
      absUncond = toNum(absEmissionsUncond2),
      absCond   = toNum(absEmissionsCond2),
      baseEmis  = toNum(baseYearEmissions2),
      bauEmis   = toNum(bauEmissions2),
      pctUncond = toNum(pctReductionUncond2),
      pctCond   = toNum(pctReductionCond2),
      comments  = comments
    )
  # LTT
  # ------------------------------------------------------------------
	df3 <- rawData %>%
			transmute(
				country = country,
				idCode = idCode,
				sourceType = "Long-Term",
				targetYear = toNum(longTermTargetYear),
				
				# For LTT, "Target value" (col 30) is usually the absolute emissions.
				# If it's text (e.g. "Net Zero"), toNum() converts it to NA.
				# We map it to Unconditional as the "Main" target.
				absUncond  = toNum(targetValue),
				
				# LTTs usually don't have separate conditional/base/bau columns in this file structure
				# We leave them NA so the calculation logic below skips them.
				absCond    = NA_real_,
				baseEmis   = NA_real_,
				bauEmis    = NA_real_,
				pctUncond  = toNum(reductionValuesPct),
				pctCond    = NA_real_,
				comments   = comments
			)
  # Bind, Calculate, and Deduplicate intelligently
  # ------------------------------------------------------------------
  finalOutput <- bind_rows(df1, df2, df3) %>%
    # Remove rows where targetYear is missing (e.g. no Revised target existed)
    filter(!is.na(targetYear)) %>%
    
    rowwise() %>%
    mutate(
      # --- CALCULATION LOGIC ---
      # Unconditional
      finalUncond = case_when(
        !is.na(absUncond) ~ absUncond,
        !is.na(baseEmis) & !is.na(pctUncond) ~ baseEmis * (1 - (pctUncond / 100)),
        !is.na(bauEmis) & !is.na(pctUncond) ~ bauEmis * (1 - (pctUncond / 100)),
        TRUE ~ NA_real_
      ),
      # Conditional
      finalCond = case_when(
        !is.na(absCond) ~ absCond,
        !is.na(baseEmis) & !is.na(pctCond) ~ baseEmis * (1 - (pctCond / 100)),
        !is.na(bauEmis) & !is.na(pctCond) ~ bauEmis * (1 - (pctCond / 100)),
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    
	# --- DEDUPLICATION ---
	# Priority: Long-Term > Revised > Interim
	# (Though typically Years will differ, e.g. 2030 vs 2050, so they won't clash)
	mutate(sourceType = factor(sourceType, levels = c("Long-Term", "Revised", "Interim/First"))) %>%
	arrange(country, targetYear, sourceType) %>%
	
	distinct(country, targetYear, .keep_all = TRUE) %>%
	
	select(country, idCode, targetYear, sourceType, 
					targetUnconditionalMtCO2e = finalUncond, 
					targetConditionalMtCO2e = finalCond,
					comments)
  
  return(finalOutput)
}
