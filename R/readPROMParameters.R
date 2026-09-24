#' readPROMParameters
#'
#' Read hand-maintained OPEN-PROM / PROMETHEUS model parameters (behavioural
#' response coefficients: elasticities, response shares) from the PROMParameters
#' madrat source folder. One subtype per parameter table.
#'
#' @param subtype One of:
#'   "PriceTransElast" - fuel price pass-through elasticity, source fuel -> target fuel
#'
#' @return magpie object with the requested parameter table
#'
#' @author Songmin Yu
#'
#' @examples
#' \dontrun{
#' a <- readSource("PROMParameters", subtype = "PriceTransElast")
#' }
#'
#' @importFrom magclass new.magpie getItems
#'
readPROMParameters <- function(subtype = "PriceTransElast") {

  if (subtype == "PriceTransElast") {
    # long-format source: source,target,value  (source & target are OPEN-PROM EF codes)
    d <- read.csv("iPriceTransElast.csv", stringsAsFactors = FALSE)
    nm <- paste(d$source, d$target, sep = ".")
    x <- new.magpie(cells_and_regions = "GLO", years = NULL, names = nm, fill = 0)
    x["GLO", , nm] <- d$value
    getSets(x)["d3.1"] <- "source"
    getSets(x)["d3.2"] <- "target"
    return(x)
  }

  stop("readPROMParameters: unknown subtype '", subtype, "'")
}
