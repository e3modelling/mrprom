#' toolGetMissingCountries
#'
#' For a given vector of ISO3 country codes, this function returns
#' the full names of countries that are in the official countries list
#' and are missing from the given vector
#'
#' @param x Vector of ISO3 country codes
#'
#' @return A vector of missing country names
#'
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- toolGetMissingCountries(c("DEU", "USA"))
#' }
#'
#'
#' @export

toolGetMissingCountries <- function(x) {
    return(names(getISOlist())[which(getISOlist() %in% setdiff(getISOlist(), x))])
}
