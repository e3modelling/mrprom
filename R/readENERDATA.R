#' ENERDATA
#'
#' Read in an csv file and convert it to a magpie object
#' The data has information about electricity capacity per country and per year
#'
#' @param subtype The type is referring to the capacity of electricity and the
#' data that should be read is from the csv file "open_prom_database_20_4_2023_1.csv"
#' and convert it to a magpie object.
#'
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' #' @examples
#' \dontrun{
#' a <- readSource("ENERDATA")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#'
#'


readENERDATA <- function(subtype = "capacity") {


  x <- list.files(path = ".",
                  pattern = "export.*.xlsx",
                  full.names = TRUE) %>%
    lapply(read_excel) %>%
    bind_rows

  names(x) <- as.character(x[2, ])

  mapCode2Title <- distinct(x[c(3:334505), c(1, 39)])
  tmp <- as.list(mapCode2Title$Title)
  names(tmp) <- mapCode2Title$`Item code`

  #x[["Title"]] <- paste0(x[["Title"]], " (", x[["Item code"]], ")") #nolint



  x <- pivot_longer(x[, c(2:35, 39)], cols = c(3:34)) #nolint
  names(x) <- c("region", "unit", "variable", "period", "value")
  x[, "value"] <- as.numeric(unlist(x[, "value"]))
  # remove NAs and duplicates
  x <- filter(x, !is.na(region)) #nolint
  x <- filter(x, region != "ISO code") #nolint
  x <- filter(x, !is.na(value)) #nolint
  x <- x %>% distinct()
  x[["region"]] <- toolCountry2isocode(x$region, #nolint
                                       mapping = c("NA" = "NAM",
                                                   "XZ" = "KOS",
                                                   "AN" = "ANT"))
  x <- filter(x, !is.na(region)) #nolint
  x$period <- as.numeric(x$period)


  enernew <- read.csv("open_prom_database_20_4_2023_1.csv")
  enernew[["Countries"]] <- factor(enernew[["Countries"]])
  levels(enernew[["Countries"]]) <- toolCountry2isocode(levels(enernew[["Countries"]])) #nolint
  enernew <- filter(enernew, enernew[["Sheet_Name"]] != "CO2_FF")
  enernew <- filter(enernew, !is.na("Countries"))
  enernew <- filter(enernew, !is.na("Unit"))
  enernew[["Enerdata_Title"]] <- factor(enernew[["Enerdata_Title"]])
  levels(enernew[["Enerdata_Title"]]) <- sub("^ ", "", levels(enernew[["Enerdata_Title"]]))
  levels(enernew$Enerdata_Title)<-sub("Installed electricity capacity of co-generation gas Installed electricity capacity of co-generation gas","Installed electricity capacity of co-generation gas",levels(enernew$Enerdata_Title)) #nolint
  enernew <- pivot_longer(enernew[, c(1, 2, 23:54, 60)], cols = c(3:34)) #nolint
  names(enernew) <- c("region", "unit", "variable", "period", "value")
  enernew[, "value"] <- as.numeric(unlist(enernew[, "value"]))
  enernew <- enernew %>% distinct()
  enernew <- filter(enernew, !is.na(value)) #nolint
  enernew <- filter(enernew, !variable%in%c(" ", "")) #nolint
  enernew$period <- sub("X", "", enernew$period)
  enernew$period <- as.numeric(enernew$period)
  enernew[["variable"]] <- factor(enernew[["variable"]])
  #for (i in levels(enernew[["variable"]])) {
  #    if (any(grepl(paste0("^", i, "$"), mapCode2Title[["Title"]]))) {
  #        levels(enernew[["variable"]])[levels(enernew[["variable"]]) == i] <- paste0(i,
  #                                                                                    " (",
  #                                                                                    mapCode2Title[grep(paste0("^", i, "$"), mapCode2Title[["Title"]]), "Item code"], #nolint
  #                                                                                    ")")
  #    }
  #}

  enernew <- filter(enernew, !is.na(region)) #nolint

  # mapCode2Title[grepl(paste0("^", i, "$"), mapCode2Title[["Title"]]), ]

  x <- rbind(enernew, x)

  x$variable <- factor(x$variable)
  levels(x$variable) <- sub("\\.", "", levels(x$variable))
  x <- filter(x, x[["variable"]] %in% grep(subtype, levels(x[["variable"]]), value = TRUE, ignore.case = TRUE))
  x <- as.quitte(x)

  return(as.magpie(x))
}
