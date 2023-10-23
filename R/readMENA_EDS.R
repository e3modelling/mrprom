#' readMENA_EDS
#'
#' Read gdx files, convert it to a mif file so to compare MENA_EDS output mif file
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
  
  x <- readGDX(gdx = "fulldata.gdx", name = c("Nen_FCon", "Dom_FCon", "Indu_FCon",
                                              "Trans_FCon", "iGDP", "POP"))
  Nen_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Nen_FCon"), field = "l")
  Nen_FCon <- as.quitte(Nen_FCon)
  Nen_FCon["model"] <- "MENA_EDS"
  Nen_FCon["variable"] <- paste("fuel consumption", Nen_FCon$NENSE, Nen_FCon$EF, "NENSE")
  Nen_FCon <- select((Nen_FCon), -c(NENSE, EF))
  
  Dom_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Dom_FCon"), field = "l")
  Dom_FCon <- as.quitte(Dom_FCon)
  Dom_FCon["model"] <- "MENA_EDS"
  Dom_FCon["variable"] <- paste("fuel consumption", Dom_FCon$DOMSE, Dom_FCon$EF, "DOMSE")
  Dom_FCon <- select((Dom_FCon), -c(DOMSE, EF))
  
  Indu_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Indu_FCon"), field = "l")
  Indu_FCon <- as.quitte(Indu_FCon)
  Indu_FCon["model"] <- "MENA_EDS"
  Indu_FCon["variable"] <- paste("fuel consumption", Indu_FCon$INDSE, Indu_FCon$EF, "INDSE")
  Indu_FCon <- select((Indu_FCon), -c(INDSE, EF))
  
  Trans_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Trans_FCon"), field = "l")
  Trans_FCon <- as.quitte(Trans_FCon)
  Trans_FCon["model"] <- "MENA_EDS"
  Trans_FCon["variable"] <- paste("fuel consumption", Trans_FCon$TRANSE, Trans_FCon$EF, "TRANSE")
  Trans_FCon <- select((Trans_FCon), -c(TRANSE, EF))
  

  
  y <- rbind(Nen_FCon, Dom_FCon, Indu_FCon, Trans_FCon)

  y <- as.quitte(y)
  y["unit"] <- "Mtoe"
  
  iGDP <- readGDX(gdx = "fulldata.gdx", name = c("GDP"), field = "l")
  q1 <- as.quitte(iGDP)
 
  q1["variable"] <- "GDP|PPP"
  q1["unit"] <- "billion US$2015/yr"
  q1["model"] <- "MENA_EDS"
 
  POP <- readGDX(gdx = "fulldata.gdx", name = c("POP"), field = "l")
  q2 <- as.quitte(POP)
  q2["variable"] <- "Population"
  q2["unit"] <- "billion"
  q1["model"] <- "MENA_EDS"
 
  q <- rbind(q1, q2)
  q <- select((q), -c(data))

  z <- rbind(y, q)
  write.mif(z, 'GDP_POP_CONSUMPTION_MENA_EDS.mif', append = FALSE)

  
  return(suppressWarnings(as.magpie(z)))
}