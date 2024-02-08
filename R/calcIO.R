#' calcIO
#'
#' Computes IEA-based model data for "output" and "output_biomass" "subtypes" by use of raw IEA "Energy Balances" data
#' and a mapping that corresponds to the structure of "products" and "flows" of IEA.
#'
#' Mapping structure example: IEA product ANTCOAL used for IEA flow TPATFUEL, contributes via REMIND technology
#' coaltr for generating sesofos from pecoal (REMIND names)
#'
#' @param subtype Data subtype.
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis
#' @examples
#' \dontrun{
#' a <- calcOutput("IO", subtype = "output")
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% filter mutate select sym pull
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble as_tibble
#'

calcIO <- function(subtype = c("output", "output_biomass")) {
  subtype <- match.arg(subtype)
  switch(
    subtype,
    output = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE,
                                where = "mrprom")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    },
    output_biomass = {
      mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", returnPathOnly = TRUE,
                                where = "mrprom")
      target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")
    }
  )

  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = "all") * 4.1868e-5

  ieamatch <- read.csv2(mapping, stringsAsFactors = FALSE, na.strings = "")

  # add total buildings electricity demand (feelb = feelcb + feelhpb + feelrhb)
  if (subtype %in% c("output")) {
    ieamatch <- rbind(ieamatch,
                      ieamatch %>%
                        filter(.data$REMINDitems_out %in% c("feelcb", "feelhpb", "feelrhb")) %>%
                        mutate(REMINDitems_out = "feelb"))
  }

  # delete NAs rows
  ieamatch <- ieamatch %>%
    as_tibble() %>%
    select(all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    na.omit() %>%
    unite("target", all_of(target), sep = ".", remove = FALSE) %>%
    unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
    filter(!!sym("product.flow") %in% getNames(data))
  magpieNames <- ieamatch[["target"]] %>% unique()

  if (subtype == "output_biomass") {
    magpieNames <- grep("(fesob|fesoi)", magpieNames, value = TRUE)
    if (is.null(magpieNames)) {
      stop("Please consider the split between traditional and modern biomass when changing the IEA mappings.calcIO, ",
           "subtypes = output_biomass and output_EDGE_buildings")
    }
  }

  reminditems <-  do.call(
    mbind,
    lapply(magpieNames,
           function(item) {
             product_flow <- ieamatch %>%
               filter(item == .data$target) %>%
               pull("product.flow")
 
             weights <- ieamatch %>%
               filter(item == .data$target) %>%
               pull("Weight") %>%
               as.numeric()

             tmp <- dimSums(  data[, , product_flow]
                              * setNames(as.magpie(weights), product_flow),
                              dim = 3, na.rm = TRUE)
             getNames(tmp) <- item

             return(tmp)
           })
  )

  # Split residential Biomass into traditional and modern biomass depending upon the income per capita
  if (subtype %in% c("output")) {
    # In order to split the REMIND technology biotr between biotr and biotrmod,
    # We use the traditional biomass split for EDGE buildings and divide by the total quantity of FE biomass

    edgeBio <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)
    feBio <- calcOutput("IO", subtype = "output_biomass", aggregate = FALSE)
    shareBiotrad <- edgeBio[, , "biotrad"] / (feBio[, , "sesobio.fesob.tdbiosob"] + feBio[, , "sesobio.fesoi.tdbiosoi"])
    shareBiotrad[is.na(shareBiotrad)] <- 0

    reminditems <- reminditems[Reduce(intersect, list(getRegions(reminditems), getRegions(shareBiotrad))), , ]
    shareBiotrad <- shareBiotrad[Reduce(intersect, list(getRegions(reminditems), getRegions(shareBiotrad))), , ]

    reminditems <- mbind(reminditems,
                         setNames(reminditems[, , "pebiolc.sesobio.biotr"] * (1 - shareBiotrad),
                                  "pebiolc.sesobio.biotrmod"))
    reminditems[, , "pebiolc.sesobio.biotr"] <- reminditems[, , "pebiolc.sesobio.biotr"] * (shareBiotrad)
  }
  reminditems <- toolCountryFill(reminditems)
  reminditems <- toolISOhistorical(reminditems)

  return(list(x = reminditems[as.character(getISOlist()), , ], weight = NULL, unit = "EJ",
              description = "IEA SE Output Data of IEA World Energy Balances"))

}
