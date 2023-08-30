#' toolIEAToGDX
#'
#' @param subtype The year.
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolIEAToGDX(subtype = c("1960","1970"))
#' }
#'
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#'
#' @export


toolIEAToGDX <- function(subtype = "1960") {

  if (!file.exists("iea.rds")) {
    x <- read.csv2("ieaWB.csv")
    names(x) <- c("region", "period", "product", "flow", "value")
    x <- x[-1, ]
    x[["region"]] <- factor(x[["region"]])
    x[["product"]] <- factor(x[["product"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    saveRDS(object = x, file = "iea.rds")
  }

  x <- readRDS("iea.rds")
  x <- filter(x, !is.na(x[["region"]]))


  if (length(subtype) == 1) {
    if (subtype != "all") {
      y <- filter(x, x[["period"]] == subtype)
      y <- droplevels(y)
      tmp <- NULL
      tmp <- c(toolSubtype(y, paste0("year_", subtype)))
      names(tmp[[2]]) <- NULL
      wgdx(paste0(subtype, ".gdx"), tmp[[1]], tmp[[2]])
    }
    }

  if (length(subtype) > 1) {

    tmp2 <- NULL
    for (i in subtype) {
      y <- NULL
      y <- filter(x, x[["period"]] == i)
      y <- droplevels(y)
      tmp2 <- c(tmp2, toolSubtype(y, paste0("year_", i)))
    }
    tmp <- list()
    tmp <- tmp2[seq(1, length(tmp2), 2)]


    n <- names(tmp2[[2]])

    for (j in seq(4, length(tmp2), 2)){
      for (i in 1:length(n)) {
            tmp2[[2]][[n[i]]]$uels[[1]] <- union(tmp2[[2]][[n[i]]]$uels[[1]],
                                                 tmp2[[j]][[n[i]]]$uels[[1]])
            tmp2[[2]][[n[i]]]$val <- matrix(1:length(tmp2[[2]][[n[i]]]$uels[[1]]),
                                            nrow = length(tmp2[[2]][[n[i]]]$uels[[1]]))
      }
    }

    names(tmp2[[2]]) <- NULL
    names(tmp) <- NULL
    wgdx(paste0(subtype[1], " - ", subtype[length(subtype)], ".gdx"), tmp,
         tmp2[[2]])

  }

  if (length(subtype) == 1) {
    if (subtype == "all") {
      tmp <- NULL
      tmp <- c(toolSubtype(x, paste0("year_", subtype)))
      names(tmp[[2]]) <- NULL
      wgdx(paste0(subtype, ".gdx"), tmp[[1]], tmp[[2]])
    }
    }

  return(tmp)

}
