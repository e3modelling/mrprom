#' toolSet
#'
#' For a given dataframe (x), this function returns
#' a list with the unique values of the column set
#' and with the standard format in order to create a gdx file.
#'
#' @param x dataframe
#'
#' @param set column of (x)
#'
#' @return A list in order to create a gdx file.
#'
#' @author Anastasis Giannousakis Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolSet(x, "region")
#' }
#'
#'
#' @export

toolSet <- function(x, set) {
  gdxset <- NULL
  gdxset$name <- set
  gdxset$ts <- set
  gdxset$type <- "set"
  gdxset$form <- "sparse"
  gdxset$dim <- 1
  gdxset$domains <- set
  gdxset$val <- matrix(0, length(levels(x[[set]])), gdxset$dim)
  gdxset$val[1:length(levels(x[[set]])), 1] <- 1:length(levels(x[[set]]))
  gdxset$uels[[1]] <- levels(x[[set]])
  return(gdxset)
}
