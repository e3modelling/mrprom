#' calcEurostat2
#'
#' @param subtype Type of data.
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
#' \item `ALL`:All the available subtypes
#' }
#'
#' @param subset Read part of the data.
#'
#' @param select Select one or all the available subtypes.
#' \itemize{
#' \item `one`:One subtype
#' \item `all`:All the available subtypes
#' }
#'
#' @return Eurostat data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#' @examples
#' \dontrun{
#' calcOutput("Eurostat2")
#' }
#'
#' @importFrom gdx writeGDX
#' @importFrom gdxrrw wgdx
#' @importFrom quitte as.quitte


calcEurostat2 <- function(subtype = "SIOT", subset = "all", select = "one") {

  if (subtype == "SIOT") {
    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "SIOT",
                                               subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "SIOT", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "SIOT"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Symmetric input-output table at basic prices (product by product)"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$stk_flow))
    gdx$uels[[5]] <- unlist(as.character(qx$induse))
    gdx$uels[[6]] <- unlist(as.character(qx$prod_na))

  } else if (subtype == "SUPL") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "SUPL",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "SUPL", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "SUPL"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Supply table at basic prices incl. transformation into purchasers' prices"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                    strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$stk_flow))
    gdx$uels[[5]] <- unlist(as.character(qx$induse))
    gdx$uels[[6]] <- unlist(as.character(qx$prod_na))


  } else if (subtype == "USEp") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "USEp",
                                             subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "USEp", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "USEp"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Use table at purchasers' prices"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$stk_flow))
    gdx$uels[[5]] <- unlist(as.character(qx$induse))
    gdx$uels[[6]] <- unlist(as.character(qx$prod_na))


  } else if (subtype == "TRM") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "TRM",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "TRM", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "TRM"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Table of trade and transport margins"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$stk_flow))
    gdx$uels[[5]] <- unlist(as.character(qx$induse))
    gdx$uels[[6]] <- unlist(as.character(qx$prod_na))


  } else if (subtype == "TAX") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "TAX",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "TAX", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "TAX"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Table of taxes less subsidies on product"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                    strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$stk_flow))
    gdx$uels[[5]] <- unlist(as.character(qx$induse))
    gdx$uels[[6]] <- unlist(as.character(qx$prod_na))


  } else if (subtype == "BTRADE") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "BTRADE",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "BTRADE", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "BTRADE"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "International trade in services"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$currency))
    gdx$uels[[4]] <- unlist(as.character(qx$bop_item))
    gdx$uels[[5]] <- unlist(as.character(qx$stk_flow))
    gdx$uels[[6]] <- unlist(as.character(qx$partner))


  } else if (subtype == "EMP_SEC") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "EMP_SEC",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "EMP_SEC", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 5
    gdx$name <- "EMP_SEC"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "National accounts aggregates by industry"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$nace_r2))
    gdx$uels[[5]] <- unlist(as.character(qx$na_item))


  } else if (subtype == "POP_NAT") {

    x <- readSource("Eurostat2", subtype = "POP_NAT", subset  = "all")
    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        qx$value), nrow = nrow(qx))
    gdx$dim <- 4
    gdx$type <- "parameter"
    gdx$name <- "POP_NAT"
    gdx$form <- "sparse"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$ts <- "Population and employment"
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$na_item))

  }  else if (subtype == "DEMO") {

    x <- readSource("Eurostat2", subtype = "DEMO", subset  = "all")
    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 5
    gdx$type <- "parameter"
    gdx$name <- "DEMO"
    gdx$form <- "sparse"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$ts <- "Population on 1 January by age and sex"
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$age))
    gdx$uels[[5]] <- unlist(as.character(qx$sex))

  }  else if (subtype == "LFS_POP") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "LFS_POP",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "LFS_POP", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), 1:nrow(qx), 1:nrow(qx), qx$value),
                      nrow = nrow(qx))
    gdx$dim <- 7
    gdx$name <- "LFS_POP"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Population by sex, age, citizenship and labour status"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$sex))
    gdx$uels[[5]] <- unlist(as.character(qx$citizen))
    gdx$uels[[6]] <- unlist(as.character(qx$age))
    gdx$uels[[7]] <- unlist(as.character(qx$wstatus))


  } else if (subtype == "LFS_EMP") {

    x <- readSource("Eurostat2", subtype = "LFS_EMP", subset  = "all")
    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), 1:nrow(qx), 1:nrow(qx), qx$value),
                      nrow = nrow(qx))
    gdx$dim <- 7
    gdx$name <- "LFS_EMP"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Employment by sex, age, occupation and economic activity"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$age))
    gdx$uels[[5]] <- unlist(as.character(qx$sex))
    gdx$uels[[6]] <- unlist(as.character(qx$nace_r2))
    gdx$uels[[7]] <- unlist(as.character(qx$isco08))

  } else if (subtype == "LFS_UNEMP") {

    x <- readSource("Eurostat2", subtype = "LFS_UNEMP", subset  = "all")
    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                       1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 5
    gdx$name <- "LFS_UNEMP"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Previous occupations of the unemployed, by sex"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$sex))
    gdx$uels[[5]] <- unlist(as.character(qx$isco08))

  } else if (subtype == "NFTR") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "NFTR",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "NFTR", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), 1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 6
    gdx$name <- "NFTR"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Non-financial transactions - annual data"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$direct))
    gdx$uels[[5]] <- unlist(as.character(qx$na_item))
    gdx$uels[[6]] <- unlist(as.character(qx$sector))


  }  else if (subtype == "FTR") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "FTR",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "FTR", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), 1:nrow(qx), 1:nrow(qx), qx$value),
                      nrow = nrow(qx))
    gdx$dim <- 7
    gdx$name <- "FTR"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Financial transactions - annual data"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$co_nco))
    gdx$uels[[5]] <- unlist(as.character(qx$sector))
    gdx$uels[[6]] <- unlist(as.character(qx$finpos))
    gdx$uels[[7]] <- unlist(as.character(qx$na_item))


  } else if (subtype == "ENV") {

    if (subset == "all") {
      x <- readSource("Eurostat2", subtype = "ENV",
                                          subset  = "all")
    } else {
      x <- readSource("Eurostat2", subtype = "ENV", subset  = subset)
      }

    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 5
    gdx$name <- "ENV"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Air emissions accounts by NACE Rev. 2 activity"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$airpol))
    gdx$uels[[5]] <- unlist(as.character(qx$nace_r2))


  } else if (subtype == "COICOP") {

    x <- readSource("Eurostat2", subtype = "COICOP", subset  = "all")
    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        1:nrow(qx), qx$value), nrow = nrow(qx))
    gdx$dim <- 4
    gdx$name <- "COICOP"
    gdx$type <- "parameter"
    gdx$form <- "sparse"
    gdx$ts <- "Final consumption expenditure of households by consumption purpose"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$coicop))

  }
  if (select == "one") {
    writeGDX(gdx, paste0(subtype, subset, ".gdx"))
  } else if (select == "all") {

    x <- readSource("Eurostat2", subtype = "POP_NAT", subset  = "all")
    qx <- as.quitte(x)
    gdx <- NULL
    gdx$val <- matrix(c(1:nrow(qx), 1:nrow(qx), 1:nrow(qx), 1:nrow(qx),
                        qx$value), nrow = nrow(qx))
    gdx$dim <- 4
    gdx$type <- "parameter"
    gdx$name <- "POP_NAT"
    gdx$form <- "sparse"
    gdx$domains <- c(names(dimnames(x))[1], names(dimnames(x))[2],
                     strsplit(names(dimnames(x))[[3]], "\\.")[[1]])
    gdx$ts <- "Population and employment"
    gdx$uels[[1]] <- unlist(as.character(qx$region))
    gdx$uels[[2]] <- unlist(as.character(qx$period))
    gdx$uels[[3]] <- unlist(as.character(qx$unit))
    gdx$uels[[4]] <- unlist(as.character(qx$na_item))

    x2 <- readSource("Eurostat2", subtype = "LFS_UNEMP", subset  = "all")
    qx2 <- as.quitte(x2)
    gdx2 <- NULL
    gdx2$val <- matrix(c(1:nrow(qx2), 1:nrow(qx2), 1:nrow(qx2), 1:nrow(qx2),
                         1:nrow(qx2), qx2$value), nrow = nrow(qx2))
    gdx2$dim <- 5
    gdx2$name <- "LFS_UNEMP"
    gdx2$type <- "parameter"
    gdx2$form <- "sparse"
    gdx2$ts <- "Previous occupations of the unemployed, by sex"
    gdx2$domains <- c(names(dimnames(x2))[1], names(dimnames(x2))[2],
                      strsplit(names(dimnames(x2))[[3]], "\\.")[[1]])
    gdx2$uels[[1]] <- unlist(as.character(qx2$region))
    gdx2$uels[[2]] <- unlist(as.character(qx2$period))
    gdx2$uels[[3]] <- unlist(as.character(qx2$unit))
    gdx2$uels[[4]] <- unlist(as.character(qx2$sex))
    gdx2$uels[[5]] <- unlist(as.character(qx2$isco08))

    x3 <- readSource("Eurostat2", subtype = "LFS_EMP", subset  = "all")
    qx3 <- as.quitte(x3)
    gdx3 <- NULL
    gdx3$val <- matrix(c(1:nrow(qx3), 1:nrow(qx3), 1:nrow(qx3), 1:nrow(qx3),
                        1:nrow(qx3), 1:nrow(qx3), 1:nrow(qx3), qx3$value),
                       nrow = nrow(qx3))
    gdx3$dim <- 7
    gdx3$name <- "LFS_EMP"
    gdx3$type <- "parameter"
    gdx3$form <- "sparse"
    gdx3$ts <- "Employment by sex, age, occupation and economic activity"
    gdx3$domains <- c(names(dimnames(x3))[1], names(dimnames(x3))[2],
                      strsplit(names(dimnames(x3))[[3]], "\\.")[[1]])
    gdx3$uels[[1]] <- unlist(as.character(qx3$region))
    gdx3$uels[[2]] <- unlist(as.character(qx3$period))
    gdx3$uels[[3]] <- unlist(as.character(qx3$unit))
    gdx3$uels[[4]] <- unlist(as.character(qx3$age))
    gdx3$uels[[5]] <- unlist(as.character(qx3$sex))
    gdx3$uels[[6]] <- unlist(as.character(qx3$nace_r2))
    gdx3$uels[[7]] <- unlist(as.character(qx3$isco08))

    x4 <- readSource("Eurostat2", subtype = "SIOT", subset  = "all")
    qx4 <- as.quitte(x4)
    gdx4 <- NULL
    gdx4$val <- matrix(c(1:nrow(qx4), 1:nrow(qx4), 1:nrow(qx4), 1:nrow(qx4),
                        1:nrow(qx4), 1:nrow(qx4), qx4$value), nrow = nrow(qx4))
    gdx4$dim <- 6
    gdx4$name <- "SIOT"
    gdx4$type <- "parameter"
    gdx4$form <- "sparse"
    gdx4$ts <- "Symmetric input-output table at basic prices (product by product)"
    gdx4$domains <- c(names(dimnames(x4))[1], names(dimnames(x4))[2],
                      strsplit(names(dimnames(x4))[[3]], "\\.")[[1]])
    gdx4$uels[[1]] <- unlist(as.character(qx4$region))
    gdx4$uels[[2]] <- unlist(as.character(qx4$period))
    gdx4$uels[[3]] <- unlist(as.character(qx4$unit))
    gdx4$uels[[4]] <- unlist(as.character(qx4$stk_flow))
    gdx4$uels[[5]] <- unlist(as.character(qx4$induse))
    gdx4$uels[[6]] <- unlist(as.character(qx4$prod_na))

    x5 <- readSource("Eurostat2", subtype = "EMP_SEC", subset  = "all")
    qx5 <- as.quitte(x5)
    gdx5 <- NULL
    gdx5$val <- matrix(c(1:nrow(qx5), 1:nrow(qx5), 1:nrow(qx5), 1:nrow(qx5),
                        1:nrow(qx5), qx5$value), nrow = nrow(qx5))
    gdx5$dim <- 5
    gdx5$name <- "EMP_SEC"
    gdx5$type <- "parameter"
    gdx5$form <- "sparse"
    gdx5$ts <- "National accounts aggregates by industry"
    gdx5$domains <- c(names(dimnames(x5))[1], names(dimnames(x5))[2],
                      strsplit(names(dimnames(x5))[[3]], "\\.")[[1]])
    gdx5$uels[[1]] <- unlist(as.character(qx5$region))
    gdx5$uels[[2]] <- unlist(as.character(qx5$period))
    gdx5$uels[[3]] <- unlist(as.character(qx5$unit))
    gdx5$uels[[4]] <- unlist(as.character(qx5$nace_r2))
    gdx5$uels[[5]] <- unlist(as.character(qx5$na_item))

    x6 <- readSource("Eurostat2", subtype = "SUPL", subset  = "all")
    qx6 <- as.quitte(x6)
    gdx6 <- NULL
    gdx6$val <- matrix(c(1:nrow(qx6), 1:nrow(qx6), 1:nrow(qx6), 1:nrow(qx6),
                         1:nrow(qx6), 1:nrow(qx6), qx6$value), nrow = nrow(qx6))
    gdx6$dim <- 6
    gdx6$name <- "SUPL"
    gdx6$type <- "parameter"
    gdx6$form <- "sparse"
    gdx6$ts <- "Supply table at basic prices incl. transformation into purchasers' prices"
    gdx6$domains <- c(names(dimnames(x6))[1], names(dimnames(x6))[2],
                      strsplit(names(dimnames(x6))[[3]], "\\.")[[1]])
    gdx6$uels[[1]] <- unlist(as.character(qx6$region))
    gdx6$uels[[2]] <- unlist(as.character(qx6$period))
    gdx6$uels[[3]] <- unlist(as.character(qx6$unit))
    gdx6$uels[[4]] <- unlist(as.character(qx6$stk_flow))
    gdx6$uels[[5]] <- unlist(as.character(qx6$induse))
    gdx6$uels[[6]] <- unlist(as.character(qx6$prod_na))

    x7 <- readSource("Eurostat2", subtype = "USEp", subset  = "all")
    qx7 <- as.quitte(x7)
    gdx7 <- NULL
    gdx7$val <- matrix(c(1:nrow(qx7), 1:nrow(qx7), 1:nrow(qx7), 1:nrow(qx7),
                         1:nrow(qx7), 1:nrow(qx7), qx7$value), nrow = nrow(qx7))
    gdx7$dim <- 6
    gdx7$name <- "USEp"
    gdx7$type <- "parameter"
    gdx7$form <- "sparse"
    gdx7$ts <- "Use table at purchasers' prices"
    gdx7$domains <- c(names(dimnames(x7))[1], names(dimnames(x7))[2],
                      strsplit(names(dimnames(x7))[[3]], "\\.")[[1]])
    gdx7$uels[[1]] <- unlist(as.character(qx7$region))
    gdx7$uels[[2]] <- unlist(as.character(qx7$period))
    gdx7$uels[[3]] <- unlist(as.character(qx7$unit))
    gdx7$uels[[4]] <- unlist(as.character(qx7$stk_flow))
    gdx7$uels[[5]] <- unlist(as.character(qx7$induse))
    gdx7$uels[[6]] <- unlist(as.character(qx7$prod_na))

    x8 <- readSource("Eurostat2", subtype = "TRM", subset  = "all")
    qx8 <- as.quitte(x8)
    gdx8 <- NULL
    gdx8$val <- matrix(c(1:nrow(qx8), 1:nrow(qx8), 1:nrow(qx8), 1:nrow(qx8),
                        1:nrow(qx8), 1:nrow(qx8), qx8$value), nrow = nrow(qx8))
    gdx8$dim <- 6
    gdx8$name <- "TRM"
    gdx8$type <- "parameter"
    gdx8$form <- "sparse"
    gdx8$ts <- "Table of trade and transport margins"
    gdx8$domains <- c(names(dimnames(x8))[1], names(dimnames(x8))[2],
                     strsplit(names(dimnames(x8))[[3]], "\\.")[[1]])
    gdx8$uels[[1]] <- unlist(as.character(qx8$region))
    gdx8$uels[[2]] <- unlist(as.character(qx8$period))
    gdx8$uels[[3]] <- unlist(as.character(qx8$unit))
    gdx8$uels[[4]] <- unlist(as.character(qx8$stk_flow))
    gdx8$uels[[5]] <- unlist(as.character(qx8$induse))
    gdx8$uels[[6]] <- unlist(as.character(qx8$prod_na))

    x9 <- readSource("Eurostat2", subtype = "TAX", subset  = "all")
    qx9 <- as.quitte(x9)
    gdx9 <- NULL
    gdx9$val <- matrix(c(1:nrow(qx9), 1:nrow(qx9), 1:nrow(qx9), 1:nrow(qx9),
                        1:nrow(qx9), 1:nrow(qx9), qx9$value), nrow = nrow(qx9))
    gdx9$dim <- 6
    gdx9$name <- "TAX"
    gdx9$type <- "parameter"
    gdx9$form <- "sparse"
    gdx9$ts <- "Table of taxes less subsidies on product"
    gdx9$domains <- c(names(dimnames(x9))[1], names(dimnames(x9))[2],
                      strsplit(names(dimnames(x9))[[3]], "\\.")[[1]])
    gdx9$uels[[1]] <- unlist(as.character(qx9$region))
    gdx9$uels[[2]] <- unlist(as.character(qx9$period))
    gdx9$uels[[3]] <- unlist(as.character(qx9$unit))
    gdx9$uels[[4]] <- unlist(as.character(qx9$stk_flow))
    gdx9$uels[[5]] <- unlist(as.character(qx9$induse))
    gdx9$uels[[6]] <- unlist(as.character(qx9$prod_na))

    x10 <- readSource("Eurostat2", subtype = "BTRADE", subset  = "all")
    qx10 <- as.quitte(x10)
    gdx10 <- NULL
    gdx10$val <- matrix(c(1:nrow(qx10), 1:nrow(qx10), 1:nrow(qx10), 1:nrow(qx10),
                         1:nrow(qx10), 1:nrow(qx10), qx10$value), nrow = nrow(qx10))
    gdx10$dim <- 6
    gdx10$name <- "BTRADE"
    gdx10$type <- "parameter"
    gdx10$form <- "sparse"
    gdx10$ts <- "International trade in services"
    gdx10$domains <- c(names(dimnames(x10))[1], names(dimnames(x10))[2],
                       strsplit(names(dimnames(x10))[[3]], "\\.")[[1]])
    gdx10$uels[[1]] <- unlist(as.character(qx10$region))
    gdx10$uels[[2]] <- unlist(as.character(qx10$period))
    gdx10$uels[[3]] <- unlist(as.character(qx10$currency))
    gdx10$uels[[4]] <- unlist(as.character(qx10$bop_item))
    gdx10$uels[[5]] <- unlist(as.character(qx10$stk_flow))
    gdx10$uels[[6]] <- unlist(as.character(qx10$partner))

    x11 <- readSource("Eurostat2", subtype = "DEMO", subset = "all")
    qx11 <- as.quitte(x11)
    gdx11 <- NULL
    gdx11$val <- matrix(c(1:nrow(qx11), 1:nrow(qx11), 1:nrow(qx11), 1:nrow(qx11),
                          1:nrow(qx11), qx11$value), nrow = nrow(qx11))
    gdx11$dim <- 5
    gdx11$type <- "parameter"
    gdx11$name <- "DEMO"
    gdx11$form <- "sparse"
    gdx11$domains <- c(names(dimnames(x11))[1], names(dimnames(x11))[2],
                       strsplit(names(dimnames(x11))[[3]], "\\.")[[1]])
    gdx11$ts <- "Population on 1 January by age and sex"
    gdx11$uels[[1]] <- unlist(as.character(qx11$region))
    gdx11$uels[[2]] <- unlist(as.character(qx11$period))
    gdx11$uels[[3]] <- unlist(as.character(qx11$unit))
    gdx11$uels[[4]] <- unlist(as.character(qx11$age))
    gdx11$uels[[5]] <- unlist(as.character(qx11$sex))

    x12 <- readSource("Eurostat2", subtype = "LFS_POP", subset  = "all")
    qx12 <- as.quitte(x12)
    gdx12 <- NULL
    gdx12$val <- matrix(c(1:nrow(qx12), 1:nrow(qx12), 1:nrow(qx12), 1:nrow(qx12),
                          1:nrow(qx12), 1:nrow(qx12), 1:nrow(qx12), qx12$value),
                        nrow = nrow(qx12))
    gdx12$dim <- 7
    gdx12$name <- "LFS_POP"
    gdx12$type <- "parameter"
    gdx12$form <- "sparse"
    gdx12$ts <- "Population by sex, age, citizenship and labour status"
    gdx12$domains <- c(names(dimnames(x12))[1], names(dimnames(x12))[2],
                       strsplit(names(dimnames(x12))[[3]], "\\.")[[1]])
    gdx12$uels[[1]] <- unlist(as.character(qx12$region))
    gdx12$uels[[2]] <- unlist(as.character(qx12$period))
    gdx12$uels[[3]] <- unlist(as.character(qx12$unit))
    gdx12$uels[[4]] <- unlist(as.character(qx12$sex))
    gdx12$uels[[5]] <- unlist(as.character(qx12$citizen))
    gdx12$uels[[6]] <- unlist(as.character(qx12$age))
    gdx12$uels[[7]] <- unlist(as.character(qx12$wstatus))

    x13 <- readSource("Eurostat2", subtype = "NFTR", subset  = "all")
    qx13 <- as.quitte(x13)
    gdx13 <- NULL
    gdx13$val <- matrix(c(1:nrow(qx13), 1:nrow(qx13), 1:nrow(qx13), 1:nrow(qx13),
                         1:nrow(qx13), 1:nrow(qx13), qx13$value),
                        nrow = nrow(qx13))
    gdx13$dim <- 6
    gdx13$name <- "NFTR"
    gdx13$type <- "parameter"
    gdx13$form <- "sparse"
    gdx13$ts <- "Non-financial transactions - annual data"
    gdx13$domains <- c(names(dimnames(x13))[1], names(dimnames(x13))[2],
                       strsplit(names(dimnames(x13))[[3]], "\\.")[[1]])
    gdx13$uels[[1]] <- unlist(as.character(qx13$region))
    gdx13$uels[[2]] <- unlist(as.character(qx13$period))
    gdx13$uels[[3]] <- unlist(as.character(qx13$unit))
    gdx13$uels[[4]] <- unlist(as.character(qx13$direct))
    gdx13$uels[[5]] <- unlist(as.character(qx13$na_item))
    gdx13$uels[[6]] <- unlist(as.character(qx13$sector))

    x14 <- readSource("Eurostat2", subtype = "FTR", subset = "all")
    qx14 <- as.quitte(x14)
    gdx14 <- NULL
    gdx14$val <- matrix(c(1:nrow(qx14), 1:nrow(qx14), 1:nrow(qx14), 1:nrow(qx14),
                         1:nrow(qx14), 1:nrow(qx14), 1:nrow(qx14), qx14$value),
                        nrow = nrow(qx14))
    gdx14$dim <- 7
    gdx14$name <- "FTR"
    gdx14$type <- "parameter"
    gdx14$form <- "sparse"
    gdx14$ts <- "Financial transactions - annual data"
    gdx14$domains <- c(names(dimnames(x14))[1], names(dimnames(x14))[2],
                       strsplit(names(dimnames(x14))[[3]], "\\.")[[1]])
    gdx14$uels[[1]] <- unlist(as.character(qx14$region))
    gdx14$uels[[2]] <- unlist(as.character(qx14$period))
    gdx14$uels[[3]] <- unlist(as.character(qx14$unit))
    gdx14$uels[[4]] <- unlist(as.character(qx14$co_nco))
    gdx14$uels[[5]] <- unlist(as.character(qx14$sector))
    gdx14$uels[[6]] <- unlist(as.character(qx14$finpos))
    gdx14$uels[[7]] <- unlist(as.character(qx14$na_item))

    x15 <- readSource("Eurostat2", subtype = "ENV", subset = "all")
    qx15 <- as.quitte(x15)
    gdx15 <- NULL
    gdx15$val <- matrix(c(1:nrow(qx15), 1:nrow(qx15), 1:nrow(qx15), 1:nrow(qx15),
                         1:nrow(qx15), qx15$value), nrow = nrow(qx15))
    gdx15$dim <- 5
    gdx15$name <- "ENV"
    gdx15$type <- "parameter"
    gdx15$form <- "sparse"
    gdx15$ts <- "Air emissions accounts by NACE Rev. 2 activity"
    gdx15$domains <- c(names(dimnames(x15))[1], names(dimnames(x15))[2],
                       strsplit(names(dimnames(x15))[[3]], "\\.")[[1]])
    gdx15$uels[[1]] <- unlist(as.character(qx15$region))
    gdx15$uels[[2]] <- unlist(as.character(qx15$period))
    gdx15$uels[[3]] <- unlist(as.character(qx15$unit))
    gdx15$uels[[4]] <- unlist(as.character(qx15$airpol))
    gdx15$uels[[5]] <- unlist(as.character(qx15$nace_r2))

    x16 <- readSource("Eurostat2", subtype = "COICOP", subset = "all")
    qx16 <- as.quitte(x16)
    gdx16 <- NULL
    gdx16$val <- matrix(c(1:nrow(qx16), 1:nrow(qx16), 1:nrow(qx16),
                          1:nrow(qx16), qx16$value), nrow = nrow(qx16))
    gdx16$dim <- 4
    gdx16$name <- "COICOP"
    gdx16$type <- "parameter"
    gdx16$form <- "sparse"
    gdx16$ts <- "Final consumption expenditure of households by consumption purpose"
    gdx16$domains <- c(names(dimnames(x16))[1], names(dimnames(x16))[2],
                       strsplit(names(dimnames(x16))[[3]], "\\.")[[1]])
    gdx16$uels[[1]] <- unlist(as.character(qx16$region))
    gdx16$uels[[2]] <- unlist(as.character(qx16$period))
    gdx16$uels[[3]] <- unlist(as.character(qx16$unit))
    gdx16$uels[[4]] <- unlist(as.character(qx16$coicop))

    wgdx("ALL.gdx", gdx, gdx2, gdx3, gdx4, gdx5, gdx6, gdx7, gdx8, gdx9, gdx10,
         gdx11, gdx12, gdx13, gdx14, gdx15, gdx16)
  }


  return(list(x = x,
              unit = "1",
              description = "Eurostat data"))

}
