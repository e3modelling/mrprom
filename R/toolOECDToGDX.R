#' toolOECDToGDX
#'
#' Read in data from the OECD and convert it to gdx file.
#'
#' @param subtypes Type of data.
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolOECDToGDX()
#' }
#'
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#'
#' @export


toolOECDToGDX <- function(subtypes = "all") {
  
  tmp <- NULL
  x <- NULL
  type <- "OECD"
  x <- OECD::get_dataset(dataset = "EO109_LTB")
  if (length(subtypes) == 1) {
    if (subtypes != "all") {
      x <- x %>% filter(VARIABLE == c(subtypes))
    }
  }
  if (length(subtypes) > 1) {
    x <- x %>% filter(VARIABLE %in% c(subtypes))
  }
  names(x) <- sub("ObsValue", "Value", names(x))
  x <- x %>% select(-Value, Value)
  
  x <- x[,colSums(is.na(x))<nrow(x)]
  
  tmp <- c(toolSubtype(x, "EO109_LTB", type))
  
  names(tmp[[2]]) <- NULL
  if (length(subtypes) > 1) {
    wgdx(paste0("OECD_FILTER_BY_VARIABLES", ".gdx"), tmp[[1]], tmp[[2]])
  } 
  if (length(subtypes) == 1) {
    if (subtypes == "all") {
      wgdx(paste0("OECD_ALL_VARIABLES", ".gdx"), tmp[[1]], tmp[[2]])
    }
  }
  if (length(subtypes) == 1) {
    if (subtypes != "all") {
      wgdx(paste0("OECD_", subtypes, ".gdx"), tmp[[1]], tmp[[2]])
    }
  }
  
  return(tmp)
}
