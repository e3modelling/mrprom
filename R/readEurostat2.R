#' readEurostat2
#'
#' Read in data from the Eurostat.
#'
#' @param subtype Type of data that should be read.
#' Available types are:
#' \itemize{
#' \item `SIOT`:Symmetric input-output table at basic prices (product by product)
#' \item `SUPL`:Supply table at basic prices incl. transformation into purchasers' prices
#' \item `USEp`:Use table at purchasers' prices
#' \item `TRM`:Table of trade and transport margins
#' \item `TAX`:Table of taxes less subsidies on product
#' \item `BTRADE`:International trade in services
#' \item `POP_NAT`:Population and employment
#' \item `EMP_SEC`:National accounts aggregates by industry
#' \item `DEMO`:Population on 1 January by age and sex
#' \item `LFS_POP`:Population by sex, age, citizenship and labour status
#' \item `LFS_EMP`:Employment by sex, age, occupation and economic activity
#' \item `LFS_UNEMP`:Previous occupations of the unemployed, by sex
#' \item `NFTR`:Non-financial transactions - annual data
#' \item `FTR`:Financial transactions - annual data
#' \item `ENV`:Air emissions accounts by NACE Rev. 2 activity
#' \item `COICOP`:Final consumption expenditure of households by consumption purpose
#' }
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Eurostat2")
#' }
#'
#' @importFrom eurostat get_eurostat
#' @importFrom quitte as.quitte
#'

readEurostat2 <- function(subtype = "SIOT") {


  if (subtype == "SIOT") {

    SIOT_prd <- get_eurostat("naio_10_cp1700", time_format = "raw", stringsAsFactors = TRUE)
    names(SIOT_prd) <- sub("values","value", names(SIOT_prd))
    names(SIOT_prd) <- sub("time","period", names(SIOT_prd))
    x <- as.quitte(SIOT_prd)

  } else if (subtype == "SUPL") {

    SUPL <- get_eurostat("naio_10_cp15", time_format = "raw", stringsAsFactors = TRUE)
    names(SUPL) <- sub("values","value", names(SUPL))
    names(SUPL) <- sub("time","period", names(SUPL))
    x <- as.quitte(SUPL)

  } else if (subtype == "USEp") {

    USE_p <- get_eurostat("naio_10_cp16", time_format = "raw", stringsAsFactors = TRUE)
    names(USE_p) <- sub("values","value", names(USE_p))
    names(USE_p) <- sub("time","period", names(USE_p))
    x <- as.quitte(USE_p)

  } else if (subtype == "TRM") {

    TRM <- get_eurostat("naio_10_cp1620", time_format = "raw", stringsAsFactors = TRUE)
    names(TRM) <- sub("values","value", names(TRM))
    names(TRM) <- sub("time","period", names(TRM))
    x <- as.quitte(TRM)

  } else if (subtype == "TAX") {

    TAXSUB <- get_eurostat("naio_10_cp1630", time_format = "raw", stringsAsFactors = TRUE)
    names(TAXSUB) <- sub("values","value", names(TAXSUB))
    names(TAXSUB) <- sub("time","period", names(TAXSUB))
    x <- as.quitte(TAXSUB)

  }  else if (subtype == "BTRADE") {

    BTRADE_SRV <- get_eurostat("bop_its6_det", time_format = "raw", stringsAsFactors = TRUE)
    names(BTRADE_SRV) <- sub("values","value", names(BTRADE_SRV))
    names(BTRADE_SRV) <- sub("time","period", names(BTRADE_SRV))
    x <- as.quitte(BTRADE_SRV)

  } else if (subtype == "POP_NAT") {

    POP_NAT <- get_eurostat("nama_10_pe", time_format = "raw", stringsAsFactors = TRUE)
    names(POP_NAT) <- sub("values","value", names(POP_NAT))
    names(POP_NAT) <- sub("time","period", names(POP_NAT))
    x <- as.quitte(POP_NAT)

  } else if (subtype == "EMP_SEC") {

    EMP_SEC <- get_eurostat("nama_10_a64_e", time_format = "raw", stringsAsFactors = TRUE)
    names(EMP_SEC) <- sub("values","value", names(EMP_SEC))
    names(EMP_SEC) <- sub("time","period", names(EMP_SEC))
    x <- as.quitte(EMP_SEC)

  } else if (subtype == "DEMO") {

    DEMO <- get_eurostat("demo_pjan", time_format = "raw", stringsAsFactors = TRUE)
    names(DEMO) <- sub("values","value", names(DEMO))
    names(DEMO) <- sub("time","period", names(DEMO))
    x <- as.quitte(DEMO)

  } else if (subtype == "LFS_POP") {

    LFS_POP <- get_eurostat("lfsa_pganws", time_format = "raw", stringsAsFactors = TRUE)
    names(LFS_POP) <- sub("values","value", names(LFS_POP))
    names(LFS_POP) <- sub("time","period", names(LFS_POP))
    x <- as.quitte(LFS_POP)

  } else if (subtype == "LFS_EMP") {

    LFS_EMP <- get_eurostat("lfsa_eisn2", time_format = "raw", stringsAsFactors = TRUE)
    names(LFS_EMP) <- sub("values","value", names(LFS_EMP))
    names(LFS_EMP) <- sub("time","period", names(LFS_EMP))
    x <- as.quitte(LFS_EMP)

  } else if (subtype == "LFS_UNEMP") {

    LFS_UNEMP <- get_eurostat("lfsa_ugpis", time_format = "raw", stringsAsFactors = TRUE)
    names(LFS_UNEMP) <- sub("values","value", names(LFS_UNEMP))
    names(LFS_UNEMP) <- sub("time","period", names(LFS_UNEMP))
    x <- as.quitte(LFS_UNEMP)

  } else if (subtype == "NFTR") {

    NFTR <- get_eurostat("nasa_10_nf_tr", time_format = "raw", stringsAsFactors = TRUE)
    names(NFTR) <- sub("values","value", names(NFTR))
    names(NFTR) <- sub("time","period", names(NFTR))
    x <- as.quitte(NFTR)

  } else if (subtype == "FTR") {

    FTR <- get_eurostat("nasa_10_f_tr", time_format = "raw", stringsAsFactors = TRUE)
    names(FTR) <- sub("values","value", names(FTR))
    names(FTR) <- sub("time","period", names(FTR))
    x <- as.quitte(FTR)

  } else if (subtype == "ENV") {

    ENV <- get_eurostat("env_ac_ainah_r2", time_format = "raw", stringsAsFactors = TRUE)
    names(ENV) <- sub("values","value", names(ENV))
    names(ENV) <- sub("time","period", names(ENV))
    x <- as.quitte(ENV)

  } else if (subtype == "COICOP") {

    COICOP <- get_eurostat("nama_10_co3_p3", time_format = "raw", stringsAsFactors = TRUE)
    names(COICOP) <- sub("values","value", names(COICOP))
    names(COICOP) <- sub("time","period", names(COICOP))
    x <- as.quitte(COICOP)

  }

  return(as.magpie(x))
}
