#' calcIH2InfrCapCosts
#'
#' Use technology cost data from excel Common_DATA from MENA_EDS model
#' to derive OPENPROM input parameter IH2InfrCapCosts.
#'
#' @return  OPENPROM input data H2production.
#' The output data for Investment cost(IC), Fixed Costs (FC), (EFF),
#' Variable Costs (VC), (AVAIL), (SELF) and (H2KMTOE) is from excel Common_DATA
#' from MENA_EDS model.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IH2InfrCapCosts", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr intersect %>% filter select
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIH2InfrCapCosts <- function() {
  
  Infrastructure <- c("TPIPA","HPIPU","MPIPU","LPIPU","MPIPS","HPIPI","SSGG")
  tech <- c("IC", "FC", "EFF", "VC", "AVAIL", "SELF", "LFT", "H2KMTOE")
  
  x <- as.data.frame(expand.grid(Infrastructure, c(2000,2025,2050), tech))
  
  names(x) <- c("infrastructure", "period", "variable")
  x["value"] <- NA
  
  #IC
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "IC"), 4] <- c(1356195,1294245,1139825)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "IC"), 4] <- c(463701,442520,389722)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "IC"), 4] <- c(300028,286323,252161)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "IC"), 4] <- c(151985,134920,106323)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "IC"), 4] <- c(259432,247581,218042)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "IC"), 4] <- c(164178,156679,137985)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "IC"), 4] <- c(978,868,650)
  
  #FC %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "FC"), 4] <- c(2.5,2.5,2.5)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "FC"), 4] <- c(2.5,2.5,2.5)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "FC"), 4] <- c(2.5,2.5,2.5)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "FC"), 4] <- c(2.5,2.5,2.5)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "FC"), 4] <- c(2.5,2.5,2.5)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "FC"), 4] <- c(2.5,2.5,2.5)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "FC"), 4] <- c(3,3,3)
  
  #VC %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "VC"), 4] <- c(1,1,1)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "VC"), 4] <- c(1,1,1)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "VC"), 4] <- c(1,1,1)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "VC"), 4] <- c(1,1,1)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "VC"), 4] <- c(1,1,1)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "VC"), 4] <- c(1,1,1)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "VC"), 4] <- c(1,1,1)
  
  #EFF %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "EFF"), 4] <- c(99,99,99)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "EFF"), 4] <- c(99,99,99)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "EFF"), 4] <- c(99,99,99)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "EFF"), 4] <- c(99,99,99)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "EFF"), 4] <- c(99,99,99)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "EFF"), 4] <- c(99,99,99)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "EFF"), 4] <- c(100,100,100)
  
  #AVAIL %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "AVAIL"), 4] <- c(98,98,98)
  
  #SELF %
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "SELF"), 4] <- c(0.99,0.99,0.99)
  
  #LFT
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "LFT"), 4] <- c(40,40,40)
  
  #H2KMTOE
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "H2KMTOE"), 4] <- c(0.009714286,0.009714286,0.009714286)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "H2KMTOE"), 4] <- c(0.000125,0.000125,0.000125)
  
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
  
  x <- as.magpie(x)
  
  #select years
  x <- x[,2010:2100,]
  
  # set NA to 10^-6
  x[is.na(x)] <- 10^-6
  
  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "Common_DATA;MENA_EDS"))
  
}
