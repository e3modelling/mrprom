#' toolReadSetFromGDX
#'
#' For the given fulldata gdx file, this function returns a dataframe with the
#' values_parameters of some variables from fulldata.
#'
#' @return Dataframe with the values_parameters of some variables
#' from fulldata.
#'
#' @author Anastasis Giannousakis Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolReadSetFromGDX()
#' }
#'
#' @importFrom gdx readGDX
#'
#' @export

toolReadSetFromGDX <- function() {

  a <- readGDX(gdx = "fulldata.gdx")
  SECTTECH <- as.data.frame(a["SECTTECH"])
  INDSE <- as.data.frame(a["INDSE"])
  DOMSE <- as.data.frame(a["DOMSE"])
  NENSE <- as.data.frame(a["NENSE"])

  df = data.frame(matrix(nrow = nrow(SECTTECH), ncol = 5))
  names(df) <- c(names(SECTTECH),names(INDSE),names(DOMSE),names(NENSE))
  df[, 1:length(SECTTECH)] <- SECTTECH
  df[1:nrow(INDSE), 3] <- INDSE
  df[1:nrow(DOMSE), 4] <- DOMSE
  df[1:nrow(NENSE), 5] <- NENSE

  return(df)
}


