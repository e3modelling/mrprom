#' readDGAGRIOUTLOOK
#'
#' Read in a XLSX file and convert it to a magpie.
#' The data has information about DGAGRIOUTLOOK factors.
#'
#' @return magpie DGAGRIOUTLOOK factors.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("DGAGRIOUTLOOK")
#' }
#' 
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate na_if if_else
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#' @importFrom quitte as.quitte
#'

readDGAGRIOUTLOOK <- function() {
  
  x <- read_excel("a09209c0-01e3-4d08-a38c-ec289fd4b195.xlsx")
  
  x <- x %>%
    select(-Year) %>%
    pivot_longer(
      cols = -c(`Balance sheet`, Metric),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(period),
      value = na_if(trimws(value), "-"),
      value = if_else(
        grepl("%", value),
        parse_number(value) / 100,
        parse_number(value)
      )
    )
  
  x <- as.quitte(x) %>% as.magpie()
  
  getItems(x,1) <- "EU27"
  
  list(x = x,
       weight = NULL,
       description = c(category = "DGAGRIOUTLOOK factors",
                       type = "DGAGRIOUTLOOK factors",
                       filename = "a09209c0-01e3-4d08-a38c-ec289fd4b195.xlsx.xlsx",
                       `Indicative size (MB)` = 0.124,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))
  
}

