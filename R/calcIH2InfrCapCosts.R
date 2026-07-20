#' calcIH2InfrCapCosts
#'
#' Creates the OPEN-PROM input parameter {IH2InfrCapCosts} using hydrogen
#' infrastructure cost and performance assumptions derived from the
#' {Common_DATA} database of the MENA_EDS model. The dataset provides
#' techno-economic characteristics for a range of hydrogen transport, storage, and
#' distribution infrastructure technologies, including transmission pipelines,
#' high-, medium-, and low-pressure pipelines, pipeline storage systems, hydrogen
#' import infrastructure, and salt cavern gas storage.
#' The dataset contains values for investment costs (IC), fixed operation and
#' maintenance costs (FC), variable costs (VC), efficiency (EFF), availability
#' factors (AVAIL), self-consumption rates (SELF), technical lifetime (LFT), and
#' hydrogen transport conversion parameters (H2KMTOE). Technology-specific values
#' are defined for the years 2000, 2025, and 2050 and are subsequently interpolated
#' to generate annual time series covering the entire model horizon from 2010 to
#' 2100.
#' Investment costs are converted from EUR2000 to USD2015 using a fixed conversion
#' factor. Fixed and variable costs are represented as fractions of investment
#' costs, while efficiency, availability, and self-consumption parameters are
#' expressed as dimensionless shares. Technical lifetime values are assumed to
#' remain constant over time for all infrastructure technologies. Hydrogen
#' transport conversion parameters are specified only for technologies where such
#' information is required.
#' After interpolation, missing values are replaced with a small positive number
#' {10^{-6}} to avoid numerical issues during model execution. The resulting
#' dataset provides a complete set of techno-economic assumptions for hydrogen
#' infrastructure technologies and serves as an input dataset for the OPEN-PROM
#' modeling framework.
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
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "IC"), 4] <- c(1356195,1294245,1139825) * 1.23 #EUR2000 to USD2015
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "IC"), 4] <- c(463701,442520,389722) * 1.23 #EUR2000 to USD2015
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "IC"), 4] <- c(300028,286323,252161) * 1.23 #EUR2000 to USD2015
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "IC"), 4] <- c(151985,134920,106323) * 1.23 #EUR2000 to USD2015
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "IC"), 4] <- c(259432,247581,218042) * 1.23 #EUR2000 to USD2015
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "IC"), 4] <- c(164178,156679,137985) * 1.23 #EUR2000 to USD2015
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "IC"), 4] <- c(978,868,650) * 1.23 #EUR2000 to USD2015
  
  #FC %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "FC"), 4] <- c(2.5/100,2.5/100,2.5/100)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "FC"), 4] <- c(2.5/100,2.5/100,2.5/100)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "FC"), 4] <- c(2.5/100,2.5/100,2.5/100)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "FC"), 4] <- c(2.5/100,2.5/100,2.5/100)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "FC"), 4] <- c(2.5/100,2.5/100,2.5/100)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "FC"), 4] <- c(2.5/100,2.5/100,2.5/100)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "FC"), 4] <- c(3/100,3/100,3/100)
  
  #VC %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "VC"), 4] <- c(1/100,1/100,1/100)
  
  #EFF %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "EFF"), 4] <- c(0.99,0.99,0.99)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "EFF"), 4] <- c(0.99,0.99,0.99)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "EFF"), 4] <- c(0.99,0.99,0.99)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "EFF"), 4] <- c(0.99,0.99,0.99)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "EFF"), 4] <- c(0.99,0.99,0.99)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "EFF"), 4] <- c(0.99,0.99,0.99)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "EFF"), 4] <- c(1,1,1)
  
  #AVAIL %
  x[which(x["infrastructure"] == "TPIPA" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  x[which(x["infrastructure"] == "MPIPU" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  x[which(x["infrastructure"] == "LPIPU" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  x[which(x["infrastructure"] == "MPIPS" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  x[which(x["infrastructure"] == "HPIPI" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  x[which(x["infrastructure"] == "SSGG" & x["variable"] == "AVAIL"), 4] <- c(0.98,0.98,0.98)
  
  #SELF %
  x[which(x["infrastructure"] == "HPIPU" & x["variable"] == "SELF"), 4] <- c(0.01,0.01,0.01)
  
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
