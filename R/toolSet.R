#' toolSet
#'
#' For a given dataframe (x) containing a column called set, 
#' this function returns a list with the unique values of the column set
#' and with the standard format in order to create a gdx file.
#'
#' @param x dataframe
#'
#' @param set column of (x)
#' 
#' @param type type of data.
#'
#' @return A list in order to create a gdx file.
#'
#' @author Anastasis Giannousakis Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolSet(x, "region","IEA")
#' }
#'
#'
#' @export

toolSet <- function(x, set, type) {

  if(type %in% c("IEA")){
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

  } else if(type %in% c("ILO", "Eurostat")){
      gdxset <- NULL
      gdxset$name <- set
      gdxset$ts <- set
      gdxset$type <- "set"
      gdxset$form <- "sparse"
      gdxset$dim <- 1
      gdxset$domains <- set
      gdxset$val <- matrix(0, length(levels(as.factor(x[[set]]))), gdxset$dim)
      gdxset$val[1:length(levels(as.factor(x[[set]]))), 1] <- 1:length(levels(as.factor(x[[set]])))
      gdxset$uels[[1]] <- levels(as.factor(x[[set]]))
      return(gdxset)
      
  } else if(type %in% c("comtradr", "OECD", "UN")){
    gdxset <- NULL
    gdxset$name <- set
    gdxset$ts <- set
    gdxset$type <- "set"
    gdxset$form <- "sparse"
    gdxset$dim <- 1
    gdxset$domains <- set
    gdxset$val <- matrix(0, length(levels(as.factor(x[[set]]))), gdxset$dim)
    gdxset$val[1:length(levels(as.factor(x[[set]]))), 1] <- 1:length(levels(as.factor(x[[set]])))
    gdxset$uels[[1]] <- levels(as.factor(x[[set]]))
    return(gdxset)
  }

}
