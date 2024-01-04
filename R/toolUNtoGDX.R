#' toolUNtoGDX
#'
#' Read in data from the UN and convert it to gdx file.
#'
#' @param subtypes Type of aggregation.
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolUNtoGDX()
#' }
#'
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#' @importFrom utils data
#'
#' @export


toolUNtoGDX <- function(subtypes = "total") {
  
  tmp <- NULL
  x <- NULL
  name <- NULL
  country_code <- NULL
  year <- NULL
  age <- NULL
  pop <- NULL
  type <- "UN"
  popprojAge1dt <- NULL
  data("popprojAge1dt", package = "wpp2022", envir = environment())
  
  if (subtypes == "by age") {
    x <- popprojAge1dt %>% select(name, country_code, year, age, pop)
  }
  if (subtypes == "total") {
    x <- popprojAge1dt %>% select(name, country_code, year, age, pop)
    x <- mutate(x, pop = sum(pop, na.rm = TRUE), .by = c("name", "country_code", "year")) 
    x <- x %>% select(-age)
    x <- distinct(x)
  }
  
  names(x) <- sub("pop", "Value", names(x))
  
  tmp <- c(toolSubtype(x, "popprojAge1dt", type))
  
  names(tmp[[2]]) <- NULL
  
  wgdx(paste0("UN_popprojAge1dt_", subtypes, ".gdx"), tmp[[1]], tmp[[2]])
  
  return(tmp)
}
