#' calcIOEdgeBuildings
#'
#' Calculates buildings-related energy flows from the IEA energy balances. 'output_EDGE_buildings' is a key input
#' to EDGE-Buildings providing the historic final energy demand from buildings. 'output_EDGE' does the same for
#' buildings and industry together.
#'
#' @param subtype Data subtype. See default argument for possible values.
#' @return IEA data as MAgPIE object aggregated to country level
#'
#' @author Pascal Führlich, Anastasis Giannousakis
#' @examples
#' \dontrun{
#' a <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings")
#' }
#'
#' @importFrom dplyr %>% all_of filter sym
#' @importFrom tidyr unite
#' 

calcIOEdgeBuildings <- function(subtype = c("output_EDGE", "output_EDGE_buildings")) {
  subtype <- match.arg(subtype)
  switch(subtype,
         output_EDGE = {
           mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                                     returnPathOnly = TRUE, where = "mrprom")
           target <- "EDGEitems"
         },
         output_EDGE_buildings = {
           mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                                     returnPathOnly = TRUE, where = "mrprom")
           target <- "EDGE_buildings"
         })
  
  data <- readSource("IEA", subtype = "all")
  
  ieamatch <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")
  
  # delete NAs rows
  ieamatch <- ieamatch[c("iea_product", "iea_flows", target, "Weight")] %>%
    na.omit() %>%
    unite("target", all_of(target), sep = ".") %>%
    unite("product.flow", c("iea_product", "iea_flows"), sep = ".", remove = FALSE)
  
  magpieNames <- ieamatch[["target"]] %>% unique()
  
  # in case we include IEA categories in the output, iea categories in `ieamatch` got renamed
  ieapname <- "iea_product"
  ieafname <- "iea_flows"
  
  reminditems <-  do.call(mbind,
                          lapply(magpieNames, function(item) {
                            testdf <- ieamatch[ieamatch$target == item, c(ieapname, ieafname, "Weight")]
                            prfl <- paste(testdf[, ieapname], testdf[, ieafname], sep = ".")
                            vec <- as.numeric(ieamatch[rownames(testdf), "Weight"])
                            names(vec) <- prfl
                            tmp <- data[, , prfl] * as.magpie(vec)
                            tmp <- dimSums(tmp, dim = 3, na.rm = TRUE)
                            getNames(tmp) <- item
                            return(tmp)
                          }))
  
  # Split residential Biomass into traditional and modern biomass depending upon the income per capita
  if (subtype ==  "output_EDGE") {
    nBiotrad <- "feresbiotrad"
    nBiomod <- "feresbiomod"
    nBioshare <- "feresbioshare"
  } else if (subtype == "output_EDGE_buildings") {
    nBiotrad <- "biotrad"
    nBiomod <- "biomod"
    nBioshare <- "bioshare"
  }
  # Read-in data to compute income per capita
  gdp <- calcOutput("iGDP", aggregate = FALSE)
  pop <- calcOutput("POP", aggregate = FALSE)
  gdppop <- gdp[, intersect(getYears(gdp), getYears(pop)), ] / pop[, intersect(getYears(gdp), getYears(pop)), ]
  # Create a lambda which is 1 for income per capita <= 10000, and 0 above 15000
  # the multiplication by gdppop was necessary to avoid error from vector length.
  lambda <- pmin(gdppop * 0 + 1, pmax(0 * gdppop, (15000 - gdppop) / (15000 - 10000)))
  lambda <- time_interpolate(lambda, getYears(reminditems), extrapolation_type = "constant")
  
  reminditems <- reminditems[Reduce(intersect, list(getRegions(reminditems), getRegions(lambda))),, ]
  lambda <- lambda[Reduce(intersect, list(getRegions(reminditems), getRegions(lambda))),, ]
  
  # Split Bioshare (residential PRIMSBIO) between traditional and modern biomass according to lambda
  bioshareTrad <- setNames(reminditems[, , nBioshare] * lambda, nBiotrad)
  bioshareMod <- setNames(reminditems[, , nBioshare] - bioshareTrad, nBiomod)
  
  # In case biomod and biotrad do not exist yet in the data set, create dummy items
  if (!any(nBiomod %in% getNames(reminditems))) {
    reminditems <- mbind(reminditems,
                         setNames(reminditems[, , nBioshare] * 0, nBiomod))
  }
  if (!any(nBiotrad %in% getNames(reminditems))) {
    reminditems <- mbind(reminditems,
                         setNames(reminditems[, , nBioshare] * 0, nBiotrad))
  }
  
  # Add the values from bioshare to the other modern and traditional biomass
  reminditems[, , nBiotrad] <- reminditems[, , nBiotrad] + bioshareTrad
  reminditems[, , nBiomod] <- reminditems[, , nBiomod] + bioshareMod
  
  # Remove the bioshare item
  reminditems <- reminditems[, , nBioshare, invert = TRUE]
  reminditems <- toolCountryFill(reminditems)
  reminditems <- toolISOhistorical(reminditems)
  
  return(list(x = reminditems[as.character(getISOlist()), , ], weight = NULL, unit = "EJ",
              description = paste("Historic final energy demand from buildings (and industry)",
                                  "based on IEA World Energy Balances")))
}