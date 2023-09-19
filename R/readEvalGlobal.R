#' readEvalGlobal
#'
#' Reads all evalglobals given in a GAMS code and returns them.
#'
#' @param file A gams file containing GAMS code.
#' 
#' @return A vector of values the evalglobal variables are set to as names.
#'
#' @author Anastasis Giannousakis Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' readEvalGlobal <- function(main.gms)
#' }
#'
#' @import gms
#'
#' @export
#'

readEvalGlobal <- function(file) {
  f <- gms:::readFileOrVector(file)
  f <- suppressWarnings(grep("^\\$[eE][vV][aA][lL][gG][lL][oO][bB][aA][lL]", 
                             f, value = TRUE))
  pattern <- "^\\$[eE][vV][aA][lL][gG][lL][oO][bB][aA][lL]\\s*([^\\s]*)\\s*(.*?)\\s*(!!.*)?$"
  out <- gsub(pattern, "\\2", f, perl = TRUE)
  names(out) <- gsub(pattern, "\\1", f, perl = TRUE)
  return(out)
}