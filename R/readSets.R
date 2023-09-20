#' readSets
#'
#' Reads all set values given in a GAMS code and returns them.
#'
#' @param file A gams file containing GAMS code.
#' 
#' @param sector A set from gams file.
#' 
#' @return The values of set.
#'
#' @author Anastasis Giannousakis Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSets(file,"SBS")
#' }
#'
#' @import gms
#' @importFrom stringr word str_detect fixed str_trim str_sub str_split
#' @importFrom dplyr filter %>%
#'
#' @export
#'

readSets <- function(file, sector) {

  d <- gms:::GAMScodeFilter(gms:::readFileOrVector(file))
  setNames <- gms:::readDeclarations(file, types =  c("set"))[, "names"]
  setNames2 <- gms:::readDeclarations(file, types =  c("set"))
  setNames2 <- as.data.frame(setNames2)
  setNames <- as.data.frame(setNames)
  d <- as.data.frame(d)
  inds <- which(setNames[,1] == sector)
  s <- d %>% filter(grepl(setNames2[inds,3], d))
  if (setNames2[inds,3] =="") {
    s <- d %>% filter(grepl(setNames2[inds,1], d))
  }
  if (setNames2[inds,3] == setNames2[(inds+1),3]) {
    s <- d %>% filter(grepl(setNames2[inds,1], d))
  }
  if (is.na(s[1,1])) {
    s <- d %>% filter(grepl(setNames2[inds,1], d))
  }
  ind1 <- which(d[,1] == s[1,1])
  if (sector == "PGASOL") {ind1 <- which(d[,1] == s[2,1])}
  if (sector == "REFORM1") {ind1 <- which(d[,1] == s[2,1])}
  if (sector == "pg") {ind1 <- which(d[,1] == s[2,1])}
  if (sector == "biomass") {ind1 <- which(d[,1] == s[7,1])}
  counter <- 0
  for (i in ind1:(ind1 + 300)) {
       k <- lengths(regmatches(d[i,1], gregexpr("/", d[i,1])))
       counter <- counter +k
       if (counter == 2){
         ind2 <- i
         break
       }
     }
  if (sector == "TRANSUSE") {ind2 <- ind1 + 4}
  if (sector == "SUPOTH") {ind2 <- ind1 + 12}
  set <- d[ind1:ind2,]
  set <- as.data.frame(set)
  
  m <- as.data.frame(matrix(ncol = 1, nrow = nrow(set)))
  for (i in 1:nrow(set)) {
    m[i,1] = stringr::word(set[i,1], sep = fixed("/"),2)
    if (is.na(m[i,1])) {
      m[i,1] <- set[i,1]
    }
    if (m[i,1] =="") {
      m[i,1] <- set[i,1]
      m[i,1] <- str_sub(m[i,1], end = -2)
    }
  }
  if (sector == "TRANSUSE" | sector == "SUPOTH") {
    m <- set
  }
  z <- NULL
  z <- m[!apply(m == "", 1, all),]
  z <- as.data.frame(z)
  z <- z[-1,]
  z <- as.data.frame(z)
  
  z <- str_trim(z[,1]) 
  z <- as.data.frame(z)
  if (!grepl(",", z[1,1]) | sector == "SCT_GHG" | sector == "EF") {
    z <- word(z[,1], 1)
  }
  z <- as.data.frame(z)
  names(z) <- setNames[inds,1]
  if (ind1 == ind2) { 
    set <- d[ind1,]
    z <- str_split(set, "/")
    z <- as.data.frame(z)
    n <- word(z[1,1], 1)
    z <- z[-1,]
    z <- as.data.frame(z)
    z <- z[!apply(z == "", 1, all),]
    z <- as.data.frame(z)
    names(z) <- n
  }
  if (sector == "TRANSUSE" | sector == "SUPOTH") {
    z <- z[-1, ]
    z <- as.data.frame(z)
    z <- z[-nrow(z), ]
    z <- as.data.frame(z)
  }
  return(z)
}