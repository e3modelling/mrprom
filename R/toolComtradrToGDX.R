#' toolComtradrToGDX
#'
#' Read in data from the Comtradr and convert it to gdx file.
#'
#' @param subtypes Type of data.
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolComtradrToGDX(subtypes = c("854143","540110"))
#' }
#'
#' @importFrom comtradr ct_get_data
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#'
#' @export


toolComtradrToGDX <- function(subtypes = "854143") {
  
  Sys.setenv('COMTRADE_PRIMARY' = 'd24dfce1f03744358db382e75f7ea35a')
  ref_year <- NULL
  reporter_iso <- NULL
  flow_code <- NULL
  partner_iso <- NULL
  cmd_code <- NULL
  cifvalue <- NULL
  fobvalue <- NULL
  primary_value <- NULL
  
  if (length(subtypes) > 1) {
    tmp2 <- NULL
    for (i in subtypes) {
      x <- NULL
      x <- ct_get_data(reporter = c('all'), partner = c('all'),
                       flow_direction = c('import','export'), commodity_code = i, 
                       start_date = 2011, end_date = 2022)
      x <- select((x), c(ref_year, reporter_iso, flow_code, partner_iso,
                          cmd_code, cifvalue, fobvalue,primary_value))
      i <- paste0("number", i)
      type <- "comtradr"
      names(x) <- sub("primary_value", "values", names(x))
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
    
    wgdx(paste0("COMTRADE_ALL.gdx"), tmp, tmp2[[2]])
  }
  
  if (length(subtypes) == 1) {
    tmp <- NULL
    x <- NULL
    x <- ct_get_data(reporter = c('all'), partner = c('all'),
                     flow_direction = c('all'), commodity_code = subtypes, 
                     start_date = 2011, end_date = 2022)
    x <- select((x), c(ref_year, reporter_iso, flow_code, partner_iso,
                       cmd_code, cifvalue, fobvalue,primary_value))
    type <- "comtradr"
    names(x) <- sub("primary_value", "values", names(x))
    subtypes <- paste0("number", subtypes)
    tmp <- c(toolSubtype(x, subtypes, type))
    names(tmp[[2]]) <- NULL
    wgdx(paste0(subtypes, ".gdx"), tmp[[1]], tmp[[2]])
  }
  
  return(tmp)
}
