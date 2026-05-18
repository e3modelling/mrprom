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
      #values_fill = list(value = 0)
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

  ProdElec[is.na(ProdElec)] <- 0

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

  # NDC - LTT emissions targets
  x <- calcOutput(type = "NDC_LTT_NECP", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>% 
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "tNDCLTT.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "tNDCLTT.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  # Shares and Projections DOMSE
  x <- readSource("TDOMSEshareproj", subtype = "Shares")
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
  
  x <- readSource("TDOMSEshareproj", subtype = "Projections")
  ICT <- calcOutput("IFuelConsICT", aggregate = TRUE)
  ICT <- ICT[,getYears(x),"SSP2.Central.Mtoe"]
  getItems(ICT, 3) <- "ICT"
  x <- mbind(x, ICT)
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
  
  a <- readSource("TSharesINDSE", subtype = "PrimesProjections")
  b <- readSource("TSharesINDSE", subtype = "IEAProjections")
  x <- mbind(a, b)
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
  
  
  a <- readSource("TSharesINDSE", subtype = "PrimesShares")
  b <- readSource("TSharesINDSE", subtype = "IEAShares")
  x <- mbind(a, b)

  # z <- dimSums(a, 3.2, na.rm = TRUE)
  # z <- filter(as.quitte(z), value == 0, period == 2024)
  # zx <- a[unique(z[["region"]]),,unique(z[["variable"]])]
  # 
  # a[getItems(zx,1),,getItems(zx,3)] <- 1/25 # Assuming 25 fuels, we assign an equal share

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
  
  
  return(list(
    x = as.magpie(as.quitte(x)),
    weight = NULL,
    unit = "various",
    description = "OPENPROM targets"
  ))
}

# Helpers ------------------------------------------------
getTShares <- function(capacity) {
  shares <- toolTShares(capacity) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
}
