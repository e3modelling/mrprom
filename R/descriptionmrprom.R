#' descriptionmrprom
#'
#' Description and in which functions are used the functions of mrprom.
#' 
#' @param file The input.gms file path.
#'
#' @return The description of mrprom functions
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- descriptionmrprom(file)
#' }
#' 
#' @import tools
#' @import gms
#' 
#' @export
#' 

descriptionmrprom <- function(file) {
  
  a <- tools::Rd_db("mrprom")
  x <- NULL
  z <- NULL
  y <- getMadratGraph("mrprom")
  rownames(y) <- 1: nrow(y)
  
  setNames <- gms::readDeclarations(file)
  setNames <- as.data.frame(setNames)
  
  for (i in a) {
    f <- as.character(i[[1]][[1]])
    if (f == "fullOPEN-PROM") {
      f = "fullOPEN_PROM"
    }
    k <- tools:::.Rd_get_text(a[[paste0(f, ".Rd")]])
    index_of_descr <- which(k == "Description:") : which(k == "Usage:")
    description_mrprom <- k[(min(index_of_descr) + 4) : max(index_of_descr) - 2]
    description_mrprom <- paste(unlist(description_mrprom), collapse = " ")
    name <- as.character(k[3])
    used_from <- which(y[, 1] == f)
    used_from <- paste(unlist(y[used_from, 2]), collapse = ",")
    uses_the <- which(y[, 2] == f)
    uses_the <- paste(unlist(y[uses_the, 1]), collapse = ",")
    description <- setNames[which(grepl(substring(f, first = 5), setNames[, 1], ignore.case = TRUE)), ]
    description_open_prom <- description[, 3]
    
    if (length(description_open_prom) == 0) {
      description_open_prom = "No description"
    }
    if (length(description_open_prom) == 2) {
      description_open_prom <- description_open_prom[1]
    }
    
    z <- data.frame(name, description_mrprom, used_from, uses_the, description_open_prom)
    x <- rbind(x, z)
  }
  
  calc <- x[startsWith(x$name, "     calc"), ]
  calc[["name"]] <- trimws(calc[["name"]])
  calc <- filter(calc, !(used_from %in% ""))
  calc <- select(calc, -description_open_prom)
  
  
  return(calc)
  
}
