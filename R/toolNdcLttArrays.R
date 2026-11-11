toolNdcLttArrays <- function() {
  source <- readSource("NDC_LTT_NECP", convert = FALSE)
  if (length(source) != 2L) {
    stop("NDC/LTT source needs target and historical emissions sheets. Refresh the source cache.")
  }
  rows <- toolNdcLttRecords(source[[1]])
  countries <- unname(getISOlist())
  variants <- c("Unconditional", "Conditional")
  prefixes <- c("target", "targetNDC", "targetLongTerm")
  variables <- unlist(lapply(prefixes, function(prefix) paste0(prefix, variants, "MtCO2e")))
  years <- sort(unique(rows$year))
  values <- new.magpie(countries, years, variables, fill = 0)
  names(dimnames(values))[3] <- "variable"
  present <- values

  capColumns <- c("unconditional", "conditional")
  firstFinite <- function(x) x[is.finite(x)][1L]
  for (country in intersect(countries, rows$country)) {
    policy <- rows[rows$country == country, ]
    ndc <- policy[policy$source != "ltt", ]
    ltt <- policy[policy$source == "ltt" & is.finite(policy$unconditional), ]
    for (year in years) {
      current <- ndc[ndc$year == year, ]
      eligible <- ltt[ltt$year <= year, ]
      latest <- eligible[which.max(eligible$year), ]
      # The combined series uses the first dated record. Dedicated NDC rows
      # can use the first available value from either NDC block.
      combined <- if (nrow(current)) current[1, ] else latest
      caps <- c(unlist(combined[1L, capColumns]), vapply(current[capColumns], firstFinite, numeric(1)),
                unlist(latest[1L, capColumns]))
      available <- is.finite(caps)
      if (any(available)) {
        values[country, paste0("y", year), variables[available]] <- caps[available]
        present[country, paste0("y", year), variables[available]] <- 1
      }
    }
  }

  hist <- source[[2]]
  column <- which(names(hist) == "2019")
  if (!identical(names(hist)[1], "EDGAR Country Code") || length(column) != 1L) {
    stop("Unexpected GHG_2019_histo layout. Expected EDGAR Country Code and one 2019 column.")
  }
  code <- as.character(hist[[1]])
  # Map the workbook's historical EDGAR codes to the current ISO codes.
  code[which(code == "SCG")] <- "SRB"
  code[which(code == "ANT")] <- "CUW"
  keep <- code %in% countries
  w <- suppressWarnings(as.numeric(hist[[column]][keep]))
  if (any(!is.finite(w) | w < 0) || !any(w > 0)) stop("Invalid 2019 historical GHG weights")
  totals <- tapply(w, code[keep], sum)

  weights <- values
  weights[] <- 0
  for (country in names(totals)) weights[country, , ] <- totals[[country]]
  list(values = values, present = present, weights = weights)
}

toolNdcLttRecords <- function(raw) {
  # The two NDC blocks repeat column names. Check the used positions before
  # assigning unique names so a shifted workbook cannot change target variants.
  columns <- c(
    updated = 1, country = 4,
    firstUnconditionalPct = 9, firstConditionalPct = 10, firstBase = 12,
    firstUnconditional = 13, firstConditional = 14, firstBau = 15, firstYear = 16, firstType = 17,
    revisedYear = 18, revisedUnconditionalPct = 19, revisedConditionalPct = 20, revisedBase = 23,
    revisedUnconditional = 24, revisedConditional = 25, revisedBau = 26, revisedType = 27,
    lttYear = 29, lttValue = 30, lttScope = 31, comments = 35
  )
  headings <- c(
    "last_updated", "country",
    "NDC__target_percentage_reduction unconditional(%)",
    "NDC_target_percentage_reduction conditional(%)+ unconditional",
    "NDC_REF_base_year_emissions values", "NDC_target_emissions_unconditional(MtCo2/yr)",
    "NDC_target_emissions_conditional(Mtco2/yr)", "REFERENCE_target_BAU_emissions(MtCO2/yr)",
    "target_year", "target type",
    "Revised NDC _ Medium target _year_R2", "NDC__target_percentage_reduction unconditional(%)",
    "NDC_target_percentage_reduction conditional(%)+unconditional",
    "NDC_REF_base_year_emissions values", "NDC_target_emissions_unconditional(MtCo2/yr)",
    "NDC_target_emissions_conditional(Mtco2/yr)", "REFERENCE_target_bau_emissions(MtCO2/yr)",
    "Target type", "long  term  target_year", "Target value", "EMissions included", "comments"
  )
  normalize <- function(x) tolower(gsub("[[:space:]]+", "", sub("\\.\\.\\.[0-9]+$", "", x)))
  mismatched <- which(is.na(names(raw)[columns]) | normalize(names(raw)[columns]) != normalize(headings))
  if (length(mismatched)) {
    stop("Unexpected global_targets columns: ", paste(columns[mismatched], collapse = ", "))
  }
  # The workbook can put an exclusion note in an unnamed column after comments.
  notes <- apply(raw[seq.int(columns[["comments"]], ncol(raw))], 1, paste, collapse = " ")
  x <- setNames(as.data.frame(raw[columns]), names(columns))
  x$comments <- notes
  x <- x[order(as.character(x$updated), decreasing = TRUE, na.last = TRUE), ]
  x <- x[which(grepl("^[A-Z]{3}$", x$country) & !duplicated(x$country)), ]
  number <- function(value) suppressWarnings(as.numeric(as.character(value)))

  ndc <- function(block) {
    country <- x$country
    year <- number(x[[paste0(block, "Year")]])
    base <- number(x[[paste0(block, "Base")]])
    bau <- number(x[[paste0(block, "Bau")]])
    cap <- function(variant) {
      value <- number(x[[paste0(block, variant)]])
      percent <- number(x[[paste0(block, variant, "Pct")]])
      fraction <- which(percent <= 1)
      percent[fraction] <- 100 * percent[fraction]
      # NAM's cached zero formula refers to a missing BAU.
      invalidZero <- which(country == "NAM" & value == 0 & !is.finite(base) & !is.finite(bau) & percent < 100)
      value[invalidZero] <- NA_real_
      reference <- ifelse(is.finite(base), base, bau)
      missing <- !is.finite(value)
      value[missing] <- reference[missing] * (1 - percent[missing] / 100)
      value
    }
    unconditional <- cap("Unconditional")
    conditional <- cap("Conditional")
    if (block == "revised") {
      # These revised pledges have numeric caps and omit the deadline cell.
      year[which(is.na(year) & country == "GBR" & is.finite(unconditional))] <- 2035
      year[which(is.na(year) & country == "TUR" & is.finite(unconditional))] <- 2030
    } else {
      conditional[which(country == "NAM" & conditional == 0)] <- NA_real_
    }
    conditional[!is.finite(conditional)] <- unconditional[!is.finite(conditional)]
    keep <- which(is.na(x[[paste0(block, "Type")]]) | tolower(trimws(x[[paste0(block, "Type")]])) != "no target")
    data.frame(country = country[keep], year = year[keep], source = rep(block, length(keep)),
               unconditional = unconditional[keep], conditional = conditional[keep])
  }

  # The research convention treats eligible absolute caps as Kyoto-GHG caps.
  # Exclude energy-sector LTTs and explicit CO2-based scopes without a GHG designation.
  scope <- tolower(trimws(x$lttScope))
  excluded <- grepl("not to be included", x$comments, ignore.case = TRUE) |
    grepl("energy sector", scope) | (grepl("co2", scope) & !grepl("ghg|kyoto|greenhouse", scope))
  ltt <- data.frame(
    country = x$country[!excluded], year = number(x$lttYear[!excluded]), source = rep("ltt", sum(!excluded)),
    unconditional = number(x$lttValue[!excluded]), conditional = number(x$lttValue[!excluded])
  )
  rows <- rbind(ndc("revised"), ndc("first"), ltt)
  rows <- rows[which(rows$year >= 2024 & rows$year <= 2100 & rows$year == trunc(rows$year)), ]
  if (!any(is.finite(rows$unconditional) | is.finite(rows$conditional))) {
    stop("No numeric annual NDC/LTT targets found")
  }
  rows
}
