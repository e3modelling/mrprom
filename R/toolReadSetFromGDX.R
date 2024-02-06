#' toolReadSetFromGDX
#'
#' For the given gdx file, this function returns a dataframe with the
#' values_parameters of some variables from the gdx file.
#'
#' @param gdx_file Name of the gdx file
#'
#' @param set sets to read from gdx file
#'
#' @return Dataframe with the values_parameters of some variables
#' from the gdx file.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolReadSetFromGDX(gdx_file = "fulldata.gdx", set = c("SECTTECH", "INDSE"))
#' }
#'
#' @importFrom gdx readGDX
#'
#' @export

toolReadSetFromGDX <- function(gdx_file = "fulldata.gdx", set = "SECTTECH") {

  a <- readGDX(gdx_file, set)

  if (length(set) == 1) {
    df <- as.data.frame(a)
  }

  if (length(set) > 1) {
    df <- NULL
    nc <- 0
    nr <- 0

    for (i in set) {
      i <- as.data.frame(a[i])
      nc <- nc + length(i)
      if (nrow(i) > nr) {
        nr <- nrow(i)
      }
    }

    df <- data.frame(matrix(nrow = nr, ncol = nc))

    nam <- NULL
    counter <- 1

    for (j in set) {
      j <- as.data.frame(a[j])
      nam <- c(nam, names(j))
      df[1:nrow(j), counter:(length(j) + counter - 1)] <- j
      counter <- counter + length(j)
    }
    names(df) <- nam
  }

  return(df)
}
