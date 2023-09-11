#' calcACTV
#'
#' Derive economic activity data for OPENPROM sectors based on two data sources:
#' transport, traffic, air transport passengers per country and per year (IRF)
#' and Production Level and Unit Cost (GEME3).
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ACTV", file = "iACTV.csv", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select

calcACTV <- function() {

  x <- readSource("GEME3", convert = TRUE) #nolint
  map <- toolGetMapping("prom-gem-mappingNEW.csv", type = "sectoral") # nolint
  map <- filter(map, map[["PROM.Code"]] != "")
  tmp <- as.quitte(x[, , "Unit Cost"][, , map[["GEME3.Name"]]] * x[, , "Production Level"][, , map[["GEME3.Name"]]]) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie %>% # nolint
    collapseNames # nolint


  # For HOU (PROM sector) use from GEME3: SUM(GEME3_SECTORS, P_HC * A_PC)
  tmp2 <- as.quitte(dimSums(x[, , "Household Consumption"]) * dimSums(x[, , "End-Use Price (Consumption Products)"])) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie %>% # nolint
    collapseNames %>% # nolint
    magclass::setNames(nm = "HOU") # nolint

  # TODO for all values after 2015, compute Production Level (and A_PC) growth rates
  # and apply to 2014 PL*UC (and P_HC * A_PC) value

  # aggregate to OPEN-PROM sectors (from GEM sectors)
  rel <- select(map, c("GEME3.Name", "PROM.Code")) # gem-prom sectoral mapping
  tmp <- toolAggregate(tmp, rel = rel, weight = NULL, from = "GEME3.Name", to = "PROM.Code", dim = 3) # nolint
  x <- mbind(tmp, tmp2)

  # TODO add BU&NEN (= PCH?)
  x <- add_columns(x, addnm = c("BU", "NEN", "PCH"), dim = 3.1)
  x[, , c("BU", "NEN", "PCH")] <- x[, , "CH"] # TODO: equal to PCH, not CH!
  getSets(x)[3] <- "variable"

  # add units
  x <- add_dimension(x, dim = 3.2, nm = "billion US$2014", add = "unit")

  # add transport
  period <- NULL
  pc <- as.quitte(readSource("IRF", subtype = "passenger-cars-in-use")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  pc[["value"]] <- pc[["value"]] / 10^6
  pc[["unit"]] <- paste0("million ", pc[["unit"]])
  #    pc <- pc[intersect(getRegions(x), getRegions(pc)), intersect(getYears(x), getYears(pc)), ] / 10^6
  pt <- as.quitte(readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  #    pt <- pt[intersect(getRegions(pc), getRegions(pt)), intersect(getYears(pc), getYears(pt)), ]
  pa <- as.quitte(readSource("WDI_PA", convert = TRUE)) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  #    pa <- pa[intersect(getRegions(pt), getRegions(pa)), intersect(getYears(pt), getYears(pa)), ]
  gu <- as.quitte(readSource("IRF", subtype = "lorry-and-road-tractor-traffic")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  #    gu <- gu[intersect(getRegions(pa), getRegions(gu)), intersect(getYears(pa), getYears(gu)), ]
  gt <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-road")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  #    gt <- gt[intersect(getRegions(gu), getRegions(gt)), intersect(getYears(gu), getYears(gt)), ]
  gn <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  #    gn <- gn[intersect(getRegions(gt), getRegions(gn)), intersect(getYears(gt), getYears(gn)), ]
  #    pc <- pc[intersect(getRegions(gn), getRegions(pc)), intersect(getYears(gn), getYears(pc)), ]
  #    pt <- pt[intersect(getRegions(pc), getRegions(pt)), intersect(getYears(pc), getYears(pt)), ]
  tr <- rbind(pc, pt, pa, gu, gt, gn)
  #    x <- mbind(x, mbind(tr, new.magpie(getRegions(tr), setdiff(getYears(x), getYears(tr)), getNames(tr), fill = NA)))
  levels(tr[["variable"]]) <- sub("passenger-cars-in-use", "PC", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-passenger-transport-by-rail", "PT", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("Air transport, passengers carried", "PA", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("lorry-and-road-tractor-traffic", "GU", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-road", "GT", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-inland-waterway", "GN", levels(tr[["variable"]])) # nolint
  x <- rbind(as.quitte(x), filter(tr, tr[["region"]] %in% getRegions(x)))
  x <- as.magpie(x)
  getNames(x) <- sub("\\..*$", "", getNames(x)) # remove units in the file read by GAMS

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "economic activity data for OPENPROM sectors")
}