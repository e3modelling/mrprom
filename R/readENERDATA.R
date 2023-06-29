#' readENERDATA
#'
#' Read in several excel files with data from ENERDATA.
#' The dataset contains several energy-related data types from ENERDATA for
#' various countries and years.
#'
#' @param subtype string. By choosing a subtype you filter the ENERDATA dataset
#' (1800+ variables) by type, to allow further processing of specific variables
#'
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ENERDATA")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#'
#'


readENERDATA <- function(subtype) {


  x <- list.files(path = ".",
                  pattern = "^export.*.xlsx",
                  full.names = TRUE) %>%
    lapply(read_excel) %>%
    bind_rows

  names(x) <- as.character(x[2, ])

  mapCode2Title <- distinct(x[c(3:334505), c(1, 39)])
  tmp <- as.list(mapCode2Title[["Title"]])
  names(tmp) <- mapCode2Title[["Item code"]]

  x <- pivot_longer(x[, c(2:35, 39)], cols = c(3:34))
  names(x) <- c("region", "unit", "variable", "period", "value")
  x[, "value"] <- as.numeric(unlist(x[, "value"]))
  # remove NAs and duplicates
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, x[["region"]] != "ISO code")
  x <- filter(x, !is.na(x[["value"]]))
  x <- x %>% distinct()

  x[["period"]] <- as.numeric(x[["period"]])

  x$variable <- factor(x$variable)
  levels(x$variable) <- sub("\\.", "", levels(x$variable))
  x <- filter(x, x[["variable"]] %in% grep(subtype, levels(x[["variable"]]), value = TRUE, ignore.case = TRUE))
  x <- as.quitte(x)

  return(as.magpie(x))
}
