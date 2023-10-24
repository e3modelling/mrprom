#' readMENA_EDS
#'
#' Read MENA_EDS gdx files, convert it to a MENA_EDS mif file so to compare output mif file
#' with OPEN-PROM output.
#'
#' @return The read-in data into a mif object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("MENA_EDS")
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom dplyr select
#' @importFrom quitte as.quitte
#' 

readMENA_EDS <- function() { 
  
  TRANSE <- NULL
  EF <- NULL
  data <- NULL
  #TRANSE
  DEMTR <- readGDX(gdx = "fulldata.gdx", name = c("DEMTR"), field = "l")
  DEMTR <- as.quitte(DEMTR)
  DEMTR["model"] <- "MENA_EDS"
  DEMTR["variable"] <- paste(DEMTR$TRANSE, DEMTR$EF)
  DEMTR <- select((DEMTR), -c(TRANSE, EF))
  DEMTR["unit"] <- "Mtoe"
  
  iGDP <- readGDX(gdx = "fulldata.gdx", name = c("GDP"), field = "l")
  q1 <- as.quitte(iGDP)
 
  q1["variable"] <- "GDP|PPP"
  q1["unit"] <- "billion US$2015/yr"
  q1["model"] <- "MENA_EDS"
 
  POP <- readGDX(gdx = "fulldata.gdx", name = c("POP"), field = "l")
  q2 <- as.quitte(POP)
  q2["variable"] <- "Population"
  q2["unit"] <- "billion"
  q2["model"] <- "MENA_EDS"
 
  q <- rbind(q1, q2)
  q <- select((q), -c(data))

  z <- rbind(DEMTR, q)
  write.mif(z, 'MENA_EDS.mif', append = FALSE)

  
  return(suppressWarnings(as.magpie(z)))
}