#' Calculate policy validation checks
#'
#' Reads the Climate Policy Modelling Protocol source through madrat and
#' returns the corresponding Postprom policy validation checks in memory.
#' Always use with aggregate=FALSE, since it is a dataframe.
#' 
#' @return A madrat result containing the checks as a data frame.
#' 
#' @author Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput("PolicyValidationChecks",aggregate = FALSE)
#' write.csv(a,"policy-validation-checks.csv",row.names = FALSE,na = "",quote = TRUE)
#' }
#' 
#' @export
#' 
calcPolicyValidationChecks <- function() {
  policies <- readSource(
    "ClimatePolicyModelling",
    subtype = "Translation to model target",
    convert = FALSE
  )

  list(
    x = buildPolicyValidationChecks(policies),
    weight = NULL,
    unit = "various",
    class = "data.frame",
    description = paste(
      "Postprom policy validation checks derived from the Climate Policy",
      "Modelling Protocol"
    )
  )
}

# Helper function to build the policy validation checks data frame from the policies data.
#' Convert climate policies to policy validation checks
#'
#' Converts the in-memory output of the Climate Policy Modelling Protocol
#' reader into the policy-check schema used by Postprom. This function does
#' not read or write CSV files.
#'
#' @param policies A data frame or quitte object returned by
#'   `readSource("ClimatePolicyModelling", subtype =
#'   "Translation to model target", convert = FALSE)`. A supplementary
#'   madrat result containing the data in an `x` element is also accepted.
#'
#' @return A data frame containing one policy validation check per source
#'   target boundary.
buildPolicyValidationChecks <- function(policies) {
  sourceReference <- paste(
    "Dafnomilis, I., Hooijschuur, E., Schmidt Tagomori, I., van Soest, H.,",
    "Roelfsema, M. & den Elzen, M. (2026). Climate Policy Modelling Protocol",
    "[Dataset]. Zenodo. https://doi.org/10.5281/zenodo.20848687"
  )

  cleanText <- function(x) {
    x <- trimws(gsub("[\r\n]+", " ", as.character(x)))
    x[x %in% c("", "NA", "N/A", "(Missing)")] <- NA_character_
    x
  }

  number <- function(x) suppressWarnings(as.numeric(cleanText(x)))

  slug <- function(x) {
    x <- tolower(cleanText(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    gsub("(^_+|_+$)", "", x)
  }

  appendNote <- function(parts, label, value) {
    if (is.na(value) || !nzchar(value)) {
      return(parts)
    }
    c(parts, paste0(label, ": ", value))
  }

  if (is.list(policies) &&
      !inherits(policies, "data.frame") &&
      "x" %in% names(policies)) {
    policies <- policies$x
  }
  if (!inherits(policies, "data.frame")) {
    stop("`policies` must be a data frame, quitte object, or madrat result.")
  }

  raw <- as.data.frame(policies, stringsAsFactors = FALSE)
  names(raw) <- tolower(trimws(names(raw)))
  raw[] <- lapply(raw, cleanText)

  required <- c(
    "region", "unit", "period", "value_type", "policy id", "country",
    "policy type", "sector", "policy status", "original target indicator",
    "original target value min", "original target value max",
    "original target unit", "model target indicator", "target type",
    "base year", "quantification based on", "comments", "value"
  )
  missingColumns <- setdiff(required, names(raw))
  if (length(missingColumns)) {
    stop(
      "Missing Climate Policy Modelling columns: ",
      paste(missingColumns, collapse = ", ")
    )
  }

  # The protocol uses CHN for China while OPEN-PROM uses CHA. Jurisdictions
  # without an exact OPEN-PROM policy region are intentionally excluded.
  raw$region[raw$region == "CHN"] <- "CHA"
  openPromPolicyRegions <- c("CHA", "EU", "IND", "JPN", "USA")
  raw <- raw[raw$region %in% openPromPolicyRegions, , drop = FALSE]

  policyVariableMap <- c(
    "CO2 intensity" = "Carbon Intensity|GDP",
    "Emissions|CO2|Energy|Supply|Solids|Coal" =
      "Emissions|CO2|Energy|Supply|Solids",
    "Emissions|Kyoto Gases|AFOLU|Land" =
      "Emissions|Kyoto Gases|AFOLU"
  )

  electricityShareMap <- c(
    "Secondary Energy|Electricity|Renewables" =
      "Secondary Energy|Electricity|Renewables Share",
    "Secondary Energy|Electricity|Solar" =
      "Secondary Energy|Electricity|Solar Share",
    "Secondary Energy|Electricity|Wind" =
      "Secondary Energy|Electricity|Wind Share",
    "Secondary Energy|Electricity|Hydro" =
      "Secondary Energy|Electricity|Hydro Share",
    "Secondary Energy|Electricity|Geothermal" = paste0(
      "Secondary Energy|Electricity|Geothermal and other renewable ",
      "sources Share"
    ),
    "Secondary Energy|Electricity|Biomass" =
      "Secondary Energy|Electricity|Biofuels Share",
    "Secondary Energy|Electricity|Nuclear" =
      "Secondary Energy|Electricity|Nuclear Share"
  )

  raw$.value <- number(raw$value)
  raw <- raw[is.finite(raw$.value), , drop = FALSE]
  if (!nrow(raw)) {
    stop("No finite policy target values are available for OPEN-PROM regions.")
  }

  convertRow <- function(row) {
    boundary <- if (identical(row$value_type, "Model Target Value Max")) {
      "maximum"
    } else {
      "minimum"
    }
    variable <- row[["model target indicator"]]
    if (variable %in% names(policyVariableMap)) {
      variable <- unname(policyVariableMap[[variable]])
    }
    unit <- row$unit
    targetValue <- row$.value
    targetYear <- number(row$period)
    baselineYear <- number(row[["base year"]])
    yearMapping <- NA_character_
    if (!is.finite(targetYear) && identical(variable, "Price|Carbon")) {
      targetYear <- if (is.finite(baselineYear)) baselineYear else 2025
      yearMapping <- paste0("missing carbon-price target year set to ", targetYear)
    }
    sourceType <- tolower(row[["target type"]])
    targetType <- boundary
    enabled <- is.finite(targetYear)
    disabledReason <- if (enabled) NA_character_ else "target year is unavailable"
    conversion <- "source value retained"

    if (identical(sourceType, "relative")) {
      context <- tolower(paste(
        variable, row[["original target indicator"]], row$comments
      ))
      reduction <- targetValue < 0 || grepl(
        "emission|intensity|final energy|deforest|reduction|reduce|phase.down",
        context
      )
      targetValue <- abs(targetValue) / 100
      if (reduction) {
        targetType <- if (boundary == "maximum") {
          "maximum_reduction_from_baseline"
        } else {
          "reduction_from_baseline"
        }
      } else {
        targetType <- if (boundary == "maximum") {
          "maximum_increase_from_baseline"
        } else {
          "increase_from_baseline"
        }
      }
      unit <- "1"
      conversion <- paste0(
        "relative percentage converted to a fractional ",
        if (reduction) "reduction" else "increase"
      )
      if (!identical(row$unit, "%")) {
        enabled <- FALSE
        disabledReason <- "relative target is not expressed as a percentage"
      } else if (!is.finite(baselineYear)) {
        enabled <- FALSE
        disabledReason <- "relative target has no exact numeric baseline year"
      } else if (!is.finite(targetYear)) {
        enabled <- FALSE
        disabledReason <- "target year is unavailable"
      }
    } else if (identical(sourceType, "cumulative")) {
      targetType <- paste0("cumulative_", boundary)
      enabled <- is.finite(targetYear) && is.finite(baselineYear)
      disabledReason <- if (enabled) {
        NA_character_
      } else {
        "cumulative target requires numeric start and target years"
      }
      if (grepl("^Emissions\\|CO2", variable) && unit == "MtCO2") {
        unit <- "Mt CO2/yr"
      }
      conversion <- paste0(
        "cumulative avoided-emissions target evaluated by trapezoidal ",
        "integration of reductions from the baseline-year annual value"
      )
    } else if (!identical(sourceType, "absolute")) {
      enabled <- FALSE
      disabledReason <- paste0("unsupported source target type '", sourceType, "'")
    } else {
      baselineYear <- NA_real_

      if (identical(variable, "Price|Carbon") && identical(unit, "USD/tCO2")) {
        unit <- "US$2015/tn CO2"
        conversion <- paste0(
          "carbon-price unit label normalized to the native Postprom unit"
        )
      } else if (variable %in% names(electricityShareMap) &&
                 identical(unit, "%")) {
        variable <- unname(electricityShareMap[[variable]])
        unit <- "1"
        targetValue <- targetValue / 100
        conversion <- paste0(
          "percentage converted to the reported electricity-source share"
        )
      } else if (unit == "EJ" && variable %in% c("Primary Energy", "Final Energy")) {
        unit <- "Mtoe"
        targetValue <- targetValue / 0.041868
        conversion <- "EJ converted to Mtoe using 1 Mtoe = 0.041868 EJ"
      } else if (unit == "PJ" && grepl("^Final Energy", variable)) {
        unit <- "Mtoe"
        targetValue <- targetValue / 41.868
        conversion <- "PJ converted to Mtoe using 1 Mtoe = 41.868 PJ"
      } else if (unit == "MW" && grepl("^Capacity", variable)) {
        unit <- "GW"
        targetValue <- targetValue / 1000
        conversion <- "MW converted to GW"
      } else if (unit == "ha" && grepl("^Land", variable)) {
        unit <- "Mha"
        targetValue <- targetValue / 1e6
        conversion <- "ha converted to Mha"
      } else if (grepl("^Emissions\\|CO2", variable) &&
                 unit %in% c("MtCO2", "MtCO2/y")) {
        unit <- "Mt CO2/yr"
        conversion <- "CO2 unit normalized to the native Postprom unit"
      } else if (grepl("^Carbon (Capture|Removal)", variable) &&
                 unit %in% c("MtCO2", "MtCO2/y")) {
        unit <- "Mt CO2/yr"
        conversion <- "CO2 unit normalized to the native Postprom unit"
      } else if (grepl("^Emissions\\|Kyoto Gases", variable) &&
                 unit %in% c("MtCO2e", "MtCO2e/y")) {
        unit <- "Mt CO2-equiv/yr"
        conversion <- "CO2-equivalent unit normalized to the native Postprom unit"
      } else if (grepl("^Emissions\\|Kyoto Gases", variable) &&
                 unit == "GtCO2e") {
        unit <- "Mt CO2-equiv/yr"
        targetValue <- targetValue * 1000
        conversion <- paste0(
          "Gt CO2-equivalent converted to Mt CO2-equivalent per year"
        )
      }
    }

    notes <- character()
    notes <- appendNote(notes, "Policy ID", row[["policy id"]])
    notes <- appendNote(notes, "Country", row$country)
    notes <- appendNote(notes, "Policy type", row[["policy type"]])
    notes <- appendNote(notes, "Sector", row$sector)
    notes <- appendNote(notes, "Policy status", row[["policy status"]])
    notes <- appendNote(notes, "Original target", row[["original target indicator"]])
    originalBounds <- paste(
      stats::na.omit(c(
        row[["original target value min"]],
        row[["original target value max"]]
      )),
      collapse = "--"
    )
    if (nzchar(originalBounds) && !is.na(row[["original target unit"]])) {
      originalBounds <- paste(originalBounds, row[["original target unit"]])
    }
    notes <- appendNote(notes, "Original bounds", originalBounds)
    notes <- appendNote(
      notes,
      "Quantification based on",
      row[["quantification based on"]]
    )
    notes <- appendNote(notes, "Conversion", conversion)
    notes <- appendNote(notes, "Year mapping", yearMapping)
    notes <- appendNote(notes, "Comments", row$comments)
    if (!enabled) {
      notes <- appendNote(notes, "Disabled", disabledReason)
    }

    data.frame(
      check_id = paste0(
        "policy_", slug(row[["policy id"]]), "_", substr(boundary, 1, 3)
      ),
      country = row$region,
      variable = variable,
      unit = unit,
      baseline_year = baselineYear,
      target_year = targetYear,
      target_type = targetType,
      target_value = targetValue,
      warn_tolerance = 0.1 * abs(targetValue),
      source = sourceReference,
      notes = paste(notes, collapse = "; "),
      enabled = enabled,
      stringsAsFactors = FALSE
    )
  }

  checks <- do.call(rbind, lapply(seq_len(nrow(raw)), function(i) {
    convertRow(raw[i, , drop = FALSE])
  }))
  rownames(checks) <- NULL

  if (anyDuplicated(checks$check_id)) {
    stop("Converted policy check IDs are not unique.")
  }

  checks
}
