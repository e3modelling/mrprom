#' fullTARGETS
#'
#' @return The read-in target data into a magpie object.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>% group_by arrange
#' @importFrom tidyr pivot_wider
#' @importFrom zoo rollmean
#' @examples
#' \dontrun{
#' a <- retrieveData("TARGETS", regionmapping = "regionmappingOP.csv")
#' }
fullTARGETS <- function() {
  # =================== Transport ====================================
  # ------------------- NewShareStock ----------------------------
  x <- calcOutput(type = "TNewShareStockPC", aggregate = TRUE) %>%
    as.quitte() %>%
    mutate(value = ifelse(is.na(value), -1, value)) %>%
    select(region, period, variable, tech, value) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      # values_fill = list(value = 0)
    )

  names(x)[1:3] <- c("dummy", "dummy", "dummy")
  write.table(x,
    file = paste("tNewShareStockPC.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # =================== Power Generation ===========================
  # ------------ Capacity ------------------------------------------
  df <- calcOutput("TInstCap", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2010) %>%
    group_by(region, variable) %>%
    arrange(period) %>%
    mutate(
      roll = rollmean(value, k = 10, fill = NA, align = "center"),
      roll_filled = ifelse(is.na(roll), value, roll)
    ) %>%
    ungroup() %>%
    select(region, variable, period, roll_filled) %>%
    rename(value = roll_filled)

  x <- df %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )

  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tCapacity.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  x <- getTShares(df)
  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tShares.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # ------------ ProdElec -----------------------------------------
  ProdElec <- calcOutput("TProdElec", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2010)

  x <- ProdElec %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
  names(x)[1:2] <- c("dummy", "dummy")

  write.table(x,
    file = paste("tProdElec.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  x <- getTShares(ProdElec)
  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tShares_ProdElec.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # -------------- Elec Demand -------------------------------------
  x <- calcOutput(type = "TDemand", aggregate = TRUE) %>%
    as.quitte() %>%
    filter(period >= 2010) %>%
    select(c("region", "period", "value")) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
  names(x)[1] <- c("dummy")
  write.table(x,
    file = paste("tDemand.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # Shares and Projections DOMSE
  x <- calcOutput(type = "TFuelConsShares", aggregate = TRUE)
  DOMSE <- toolGetMapping("DOMSE.csv", type = "blabla_export", where = "mrprom")[[1]]
  x <- x[, , DOMSE]
  x[is.na(x)] <- 0
  x <- as.quitte(x) %>%
    select(c("region", "variable", "fuel", "period", "value"))
  xq <- x %>% pivot_wider(names_from = "period", values_from = "value")
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "tSharesFuelBuildings.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "tSharesFuelBuildings.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "TFuelCons", aggregate = TRUE)
  x <- x[, , DOMSE]
  x <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value"))
  xq <- x %>% pivot_wider(names_from = "period", values_from = "value")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "tProjectionsFuelBuildings.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "tProjectionsFuelBuildings.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "TFuelCons", aggregate = TRUE)
  x <- x[, , setdiff(getItems(x, 3), DOMSE)]
  x <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value"))
  xq <- x %>% pivot_wider(names_from = "period", values_from = "value")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "tProjectionsINDSE.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "tProjectionsINDSE.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "TFuelConsShares", aggregate = TRUE)
  x <- x[, , setdiff(getItems(x, 3.1), DOMSE)]
  x[is.na(x)] <- 0
  x <- as.quitte(x) %>%
    select(c("region", "variable", "fuel", "period", "value"))
  xq <- x %>% pivot_wider(names_from = "period", values_from = "value")
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "tSharesINDSE.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "tSharesINDSE.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  # =================== NDC-LTT Targets ====================================
  # Write the cap and coverage tables as a pair.
  results <- list(
    calcOutput("NDC_LTT_NECP", aggregate = TRUE),
    calcOutput("NDC_LTT_NECP_Coverage", aggregate = TRUE)
  )
  tables <- lapply(results, function(x) {
    keys <- c("region", "variable", "period")
    long <- as.quitte(x) %>%
      select(region, variable, period, value) %>%
      mutate(region = as.character(region), variable = as.character(variable))
    if (!nrow(long) || anyNA(long[keys]) || anyDuplicated(long[keys]) ||
        any(!grepl("^[0-9]{4}$", long$period)) || any(!is.finite(long$value))) {
      stop("Invalid NDC/LTT export keys or values")
    }
    wide <- long %>%
      arrange(region, variable, period) %>%
      pivot_wider(names_from = "period", values_from = "value", names_sort = TRUE)
    if (anyNA(wide)) stop("Incomplete NDC/LTT export table")
    names(wide)[1:2] <- c("dummy", "dummy")
    as.data.frame(wide)
  })
  shares <- as.matrix(tables[[2]][-(1:2)])
  if (!identical(tables[[1]][1:2], tables[[2]][1:2]) ||
      !identical(names(tables[[1]]), names(tables[[2]])) ||
      any(shares < 0 | shares > 1)) {
    stop("Misaligned NDC/LTT cap and coverage tables or invalid coverage")
  }

  files <- c("tNDCLTT.csv", "tNDCLTT_coverage.csv")
  if (any(file.exists(paste0(files, ".previous")))) {
    stop("Recover the retained NDC/LTT .previous files before exporting again")
  }
  staging <- ".ndcltt-publish"
  if (!dir.create(staging, showWarnings = FALSE)) {
    stop("NDC/LTT export is active or needs recovery. Check ", staging)
  }
  staged <- file.path(staging, files)
  backups <- paste0(staged, ".previous")
  existed <- file.exists(files)
  replaced <- rep(FALSE, length(files))
  complete <- FALSE
  on.exit({
    restored <- TRUE
    if (!complete) {
      for (i in which(replaced)) {
        ok <- if (existed[i]) file.rename(backups[i], files[i]) else unlink(files[i]) == 0L
        restored <- restored && ok
      }
    }
    if (restored) {
      unlink(staging, recursive = TRUE)
    } else {
      warning("NDC/LTT export rollback failed. Recovery files remain in ", staging)
    }
  }, add = TRUE)

  for (i in seq_along(files)) {
    write.table(tables[[i]], staged[i], sep = ",", quote = FALSE, row.names = FALSE)
    check <- read.csv(staged[i], check.names = FALSE, stringsAsFactors = FALSE)
    if (!identical(names(check), names(tables[[i]])) ||
        !isTRUE(all.equal(check, tables[[i]], check.attributes = FALSE, tolerance = 1e-12))) {
      stop("NDC/LTT CSV verification failed for ", files[i])
    }
  }
  # Stage both CSVs and preserve the old pair before replacing either file.
  for (i in which(existed)) {
    if (!file.copy(files[i], backups[i])) stop("Cannot preserve ", files[i])
  }
  for (i in seq_along(files)) {
    if (!file.rename(staged[i], files[i])) stop("Cannot publish ", files[i])
    replaced[i] <- TRUE
  }
  complete <- TRUE

  return(list(
    x = as.magpie(as.quitte(x)),
    weight = NULL,
    unit = "various",
    description = "OPENPROM targets"
  ))
}

# Helpers ------------------------------------------------
getTShares <- function(capacity) {
  shares <- capacity %>%
    group_by(region, period) %>%
    mutate(
      value = value / sum(value, na.rm = TRUE)
    )  %>%
    filter(period >= 2021) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
}

getTShares2 <- function(capacity) {
  shares <- capacity %>%
    group_by(region, period) %>%
    mutate(
      value = value / sum(value, na.rm = TRUE)
    )  %>%
    filter(period >= 2021) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
}
