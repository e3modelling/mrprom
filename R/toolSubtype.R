#' toolSubtype
#'
#' For a given dataframe (x), this function returns
#' a list with the values_parameters of (x) and the unique values
#' of each column of (x) with the standard format
#' in order to create a gdx file.
#'
#' @param x dataframe
#'
#' @param subt string. By choosing a subtype you filter the dataset.
#'
#' @param type type of data.
#'
#' @return A list in order to create a gdx file.
#'
#' @author Anastasis Giannousakis Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolSubtype(x, "nama_10_pe", "Eurostat")
#' }
#'
#' @importFrom dplyr select filter
#' @importFrom eurostat get_eurostat_toc
#'
#' @export
#'

toolSubtype <- function(x, subt, type) {

  if (type == "Eurostat") {
    gdx <- NULL
    gdx$dim <- ncol(x) - 1
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$domains <- names(x)
    toc <- get_eurostat_toc()
    k <- NULL
    time <- NULL
    values <- NULL
    title <- filter(toc, toc[["code"]] == subt)
    gdx$name <- title[[2]]
    gdx$ts <- title[[1]]
    gdxset <- list()
    k <- select((x), -c(time, values))
    gdx$val <- matrix(c(rep(1:nrow(x), ncol(x) - 1), x[["values"]]),
                      nrow = nrow(x))

    for (i in names(x)[-ncol(x)]) {
      gdx$uels[[i]] <- as.character(x[[i]])
    }
    names(gdx$uels) <- NULL

    for (i in names(k)) {
      gdxset[[i]] <- toolSet(x, i, type)
    }

  } else if (type == "IEA") {
    gdx <- NULL
    gdx$dim <- ncol(x) - 1
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$domains <- names(x)
    k <- NULL
    period <- NULL
    value <- NULL
    gdx$name <- subt
    gdx$ts <- subt
    gdxset <- list()
    k <- select((x), -c(period, value))
    gdx$val <- matrix(c(rep(1:nrow(x), ncol(x) - 1), x[["value"]]),
                      nrow = nrow(x))

    for (i in names(x)[-ncol(x)]) {
      gdx$uels[[i]] <- as.character(x[[i]])
    }
    names(gdx$uels) <- NULL

    for (i in names(k)) {
      gdxset[[i]] <- toolSet(x, i, type)
    }

  } else if (type == "ILO") {

    gdx <- NULL
    gdx$dim <- ncol(x) - 1
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$domains <- names(x)

    gdx$name <- subt
    gdx$ts <- subt
    gdxset <- list()

    k <- NULL
    value <- NULL
    k <- select((x), -c(values))
    gdx$val <- matrix(c(rep(1:nrow(x), ncol(x) - 1), x[["values"]]),
                      nrow = nrow(x))

    for (i in names(k)) {
      gdx$uels[[i]] <- as.character(x[[i]])
    }
    names(gdx$uels) <- NULL

    for (i in names(k)) {
      gdxset[[i]] <- toolSet(x, i, type)
    }
  } else if (type %in% c("comtradr")) {

    gdx <- NULL
    gdx$dim <- ncol(x) - 1
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$domains <- names(x)

    gdx$name <- subt
    gdx$ts <- subt
    gdxset <- list()

    k <- NULL
    values <- NULL
    k <- select((x), -c(values))
    gdx$val <- matrix(c(rep(1:nrow(x), ncol(x) - 1), as.numeric(x[["values"]])),
                      nrow = nrow(x))

    for (i in names(k)) {
      gdx$uels[[i]] <- as.character(x[[i]])
    }
    names(gdx$uels) <- NULL

    for (i in names(k)) {
      gdxset[[i]] <- toolSet(x, i, type)
    }
  } else if (type %in% c("OECD", "UN")) {

    gdx <- NULL
    gdx$dim <- ncol(x) - 1
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$domains <- names(x)

    gdx$name <- subt
    gdx$ts <- subt
    gdxset <- list()

    k <- NULL
    values <- NULL
    Value <- NULL
    k <- select((x), -c(Value))
    gdx$val <- matrix(c(rep(1:nrow(x), ncol(x) - 1), as.numeric(x[["Value"]])),
                      nrow = nrow(x))

    for (i in names(k)) {
      gdx$uels[[i]] <- as.character(x[[i]])
    }
    names(gdx$uels) <- NULL

    for (i in names(k)) {
      gdxset[[i]] <- toolSet(x, i, type)
    }
  }

  return(list(gdx, gdxset))
}
