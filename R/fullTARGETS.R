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
  
  cap <- getTCap()
  
  data <- cap
  k <- 10
  
  df <- data %>%
    group_by(region, variable) %>%
    arrange(period) %>%
    mutate(
      roll = rollmean(value, k = k, fill = NA, align = "center"),
      roll_filled = ifelse(is.na(roll), value, roll)
    ) %>%
    ungroup()
  
  df <- df[,c("region","variable","period","roll_filled")]
  
  names(df) <- sub("roll_filled","value",names(df))
  
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
  
  ####### ProdElec
  
  ProdElec <- getTProdElec()
  
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
  
  ############
  
  x <- getTDem()
  names(x)[1] <- c("dummy")
  write.table(x,
    file = paste("tDemand.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  # NDC - LTT emissions targets
  map <- toolGetMapping(getConfig("regionmapping"), "regional", where = "mrprom")
  x <- calcOutput(type = "NDC_LTT_NECP", aggregate = FALSE)
  # Replace any missing country data with 0. This is a problem when zero is a target instead of NA.
  x[is.na(x)] <- 0
  xtemp <- toolAggregate(x, rel = map, from = "ISO3.Code", to = "Region.Code", weight = NULL)
  xq <- as.quitte(xtemp) %>%
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

  return(list(
    x = as.magpie(as.quitte(x)),
    weight = NULL,
    unit = "various",
    description = "OPENPROM targets"
  ))
}

# Helpers ------------------------------------------------
getTCap <- function() {
  capacity <- calcOutput("TInstCap", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2010)
  return(capacity)
}

getTProdElec <- function() {
  capacity <- calcOutput("TProdElec", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2010)
  return(capacity)
}

getTShares <- function(capacity) {

  shares <- toolTShares(capacity) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
}

getTDem <- function() {
  demand <- calcOutput(
    type = "TDemand", aggregate = TRUE
  ) %>%
    as.quitte() %>%
    filter(period >= 2010) %>%
    select(c("region", "period", "value")) %>%
    pivot_wider(
      names_from = "period",
      values_from = "value",
      values_fill = list(value = 0)
    )
}

getTNDC <- function(){
  
}
