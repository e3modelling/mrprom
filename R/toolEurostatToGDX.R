#' toolEurostatToGDX
#'
#' Read in data from the Eurostat and convert it to gdx file.
#'
#' @param subtypes Type of data.
#' Available types are:
#' \itemize{
#' \item `naio_10_cp1700`:Symmetric input-output table at basic prices (product by product)
#' \item `naio_10_cp15`:Supply table at basic prices incl. transformation into purchasers' prices
#' \item `naio_10_cp16`:Use table at purchasers' prices
#' \item `naio_10_cp1620`:Table of trade and transport margins
#' \item `naio_10_cp1630`:Table of taxes less subsidies on product
#' \item `bop_its6_det`:International trade in services
#' \item `nama_10_pe`:Population and employment
#' \item `nama_10_a64_e`:National accounts aggregates by industry
#' \item `demo_pjan`:Population on 1 January by age and sex
#' \item `lfsa_pganws`:Population by sex, age, citizenship and labour status
#' \item `lfsa_eisn2`:Employment by sex, age, occupation and economic activity
#' \item `lfsa_ugpis`:Previous occupations of the unemployed, by sex
#' \item `nasa_10_nf_tr`:Non-financial transactions - annual data
#' \item `nasa_10_f_tr`:Financial transactions - annual data
#' \item `env_ac_ainah_r2`:Air emissions accounts by NACE Rev. 2 activity
#' \item `nama_10_co3_p3`:Final consumption expenditure of households by consumption purpose
#' \item `all`:All the available subtypes
#' }
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolEurostatToGDX(subtypes = c("nama_10_pe","naio_10_cp1630"))
#' }
#'
#' @importFrom eurostat get_eurostat get_eurostat_toc
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#'
#' @export


toolEurostatToGDX <- function(subtypes = "nama_10_pe") {


  if (length(subtypes) == 1) {
    if (subtypes == "all") {subtypes <- c("naio_10_cp1700", "naio_10_cp15",
                                          "naio_10_cp16", "naio_10_cp1620",
                                          "naio_10_cp1630", "bop_its6_det",
                                          "nama_10_pe", "nama_10_a64_e",
                                          "demo_pjan", "lfsa_pganws",
                                          "lfsa_eisn2", "lfsa_ugpis",
                                       "nasa_10_nf_tr", "nasa_10_f_tr",
                                       "env_ac_ainah_r2", "nama_10_co3_p3")
    }
    }


  if (length(subtypes) > 1) {
  tmp2 <- NULL
  for (i in subtypes) {
    x <- NULL
    x <- get_eurostat(i, time_format = "raw", stringsAsFactors = TRUE)
    tmp2 <- c(tmp2, toolSubtype(x, i))
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

  wgdx(paste0("ALL.gdx"), tmp, tmp2[[2]])
  }

  if (length(subtypes) == 1) {
  tmp <- NULL
  x <- NULL
  x <- get_eurostat(subtypes, time_format = "raw", stringsAsFactors = TRUE)
  tmp <- c(toolSubtype(x, subtypes))
  names(tmp[[2]]) <- NULL
  wgdx(paste0(subtypes, ".gdx"), tmp[[1]], tmp[[2]])
  }

  return(tmp)
}
