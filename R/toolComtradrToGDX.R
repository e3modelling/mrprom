#' toolComtradrToGDX
#'
#' Read in data from the Comtradr and convert it to gdx file.
#'
#' @param subtypes Type of data.
#' @param start Starting date of data.
#' @param end Ending date of data.
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolComtradrToGDX(subtypes = c("854143","540110"), start = "2011", end = "2022")
#' }
#'
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#'
#' @export


toolComtradrToGDX <- function(subtypes = "854143", start = "2011", end = "2022") {

  Sys.setenv('COMTRADE_PRIMARY' = 'd24dfce1f03744358db382e75f7ea35a')
  refYear <- NULL
  reporterISO <- NULL
  flowCode <- NULL
  partnerISO <- NULL
  cmdCode <- NULL
  cifvalue <- NULL
  fobvalue <- NULL
  primaryValue <- NULL

  if (length(subtypes) > 1) {
    tmp2 <- NULL
    for (i in subtypes) {
      x <- NULL
      x <- comtradr::ct_get_data(reporter = c("all_countries"), partner = c("all_countries"),
                                 flow_direction = c("import", "export"), commodity_code = i,
                                 start_date = start, end_date = end)

      x <- select((x), c(refYear, reporterISO, flowCode, partnerISO,
                         cmdCode, cifvalue, fobvalue, primaryValue))

      x[["fobvalue"]] <- ifelse((x[["fobvalue"]] > 0 & !(x[["cifvalue"]] > 0)) | is.na(x[["cifvalue"]]), "Yes", "No")
      x[["cifvalue"]] <- ifelse(x[["cifvalue"]] > 0 | is.na(x[["fobvalue"]]), "Yes", "No")
      x[["cifvalue"]] <- ifelse(is.na(x[["cifvalue"]]), "No", x[["cifvalue"]])
      x[["fobvalue"]] <- ifelse(is.na(x[["fobvalue"]]), "No", x[["fobvalue"]])

      i <- paste0("number", i)
      type <- "comtradr"
      names(x) <- sub("primaryValue", "values", names(x))
      tmp2 <- c(tmp2, toolSubtype(x, i, type))
    }

    n <- names(tmp2[[2]])

    for (l in seq(4, length(tmp2), 2)){
      ng <- names(tmp2[[l]])
      for (i in 1:length(n)) {
        for (j in 1:length(ng)) {
          if (n[i] == ng[j]) {
            tmp2[[2]][[n[i]]]$uels[[1]] <- union(tmp2[[2]][[n[i]]]$uels[[1]],
                                                 tmp2[[l]][[ng[j]]]$uels[[1]])
            tmp2[[2]][[n[i]]]$val <- matrix(1:length(tmp2[[2]][[n[i]]]$uels[[1]]),
                                            nrow = length(tmp2[[2]][[n[i]]]$uels[[1]]))
          } else if (!(ng[j] %in% n))  {
            tmp2[[2]][[ng[j]]] <- tmp2[[l]][[ng[j]]]
            n <- c(n, ng[j])
          }
        }
      }
    }

    tmp <- list()
    tmp <- tmp2[seq(1, length(tmp2), 2)]

    names(tmp2[[2]]) <- NULL

    wgdx(paste0("COMTRADE_ALL", start, end, ".gdx"), tmp, tmp2[[2]])
  }

  if (length(subtypes) == 1) {
    tmp <- NULL
    x <- NULL
    x <- comtradr::ct_get_data(reporter = c("all_countries"), partner = c("all_countries"),
                               flow_direction = c("import", "export"), commodity_code = subtypes,
                               start_date = start, end_date = end)

    x <- select((x), c(refYear, reporterISO, flowCode, partnerISO,
                       cmdCode, cifvalue, fobvalue, primaryValue))

    x[["fobvalue"]] <- ifelse((x[["fobvalue"]] > 0 & (!(x[["cifvalue"]] > 0)) | is.na(x[["cifvalue"]])), "Yes", "No")
    x[["cifvalue"]] <- ifelse(x[["cifvalue"]] > 0 | is.na(x[["fobvalue"]]), "Yes", "No")
    x[["cifvalue"]] <- ifelse(is.na(x[["cifvalue"]]), "No", x[["cifvalue"]])
    x[["fobvalue"]] <- ifelse(is.na(x[["fobvalue"]]), "No", x[["fobvalue"]])

    type <- "comtradr"
    names(x) <- sub("primaryValue", "values", names(x))
    subtypes <- paste0("number", subtypes)
    tmp <- c(toolSubtype(x, subtypes, type))
    names(tmp[[2]]) <- NULL
    wgdx(paste0(subtypes, start, end, ".gdx"), tmp[[1]], tmp[[2]])
  }

  return(tmp)
}
