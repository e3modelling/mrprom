#' fullTARGETS
#'
#' @return The read-in target data into a magpie object.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>%
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' a <- retrieveData("TARGETS", regionmapping = "regionmappingOP.csv")
#' }
fullTARGETS <- function() {
  
  cap <- getTCap()
  
  x <- cap %>%
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
  
  x <- getTShares(cap)
  names(x)[1:2] <- c("dummy", "dummy")
  write.table(x,
    file = paste("tShares.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )
  
  x <- getTDem()
  names(x)[1] <- c("dummy")
  write.table(x,
    file = paste("tDemand.csv"),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
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
