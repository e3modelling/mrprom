#' calcIMatureFacPlaDisp
#'
#' Maturity factor related to plant dispatching to derive OPENPROM input
#' parameter iMatureFacPlaDisp
#'
#' @return  OPENPROM input data iIMatureFacPlaDisp.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IMatureFacPlaDisp", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate filter select distinct left_join
#' @importFrom tidyr drop_na nesting expand complete
#'

calcIMatureFacPlaDisp <- function() {
  
  a <- readSource("Landlocked_Countries")
  
  IMatureFacPlaDisp <- data.frame(variable = c("*CTHLGN","*CTHHCL","*CTHRFO",
                                              "*CTHNGS","CTHBMSWAS","ATHLGN",
                                              "ATHHCL","ATHRFO","ATHNGS",
                                              "ATHBMSWAS","SUPCRL","SUPCR",
                                              "FBCLGN","FBCHCL","IGCCLGN",
                                              "IGCCHCL","IGCCBMS","CCCGT",
                                              "*ACCHT","ACCGT","*CGTGDO",
                                              "*CGTNGS","AGTGDO","AGTNGS",
                                              "*ICEH2","*FC1","*FC2",
                                              "*PGNUC","PGLHYD","PGSHYD",
                                              "PGWND","PGSOL","*PGOTHREN",
                                              "PGASHYD","PGAWND","PGASOL",
                                              "PGADPV","PGAOTHREN","PGANUC",
                                              "PGAPSS","PGAPSSL","PGACGSL",
                                              "PGACGS","PGAGGS","PGAWNO"),
                                  value =	c(20.00000,20.00000,20.00000,
                                            20.00000,20.00000,20.00000,
                                            20.00000,60.00000,40.00000,
                                            20.00000,20.00000,20.00000,
                                            20.00000,20.00000,20.00000,
                                            20.00000,20.00000,40.00000,
                                            0.00000010,50.00000,20.00000,
                                            20.00000,40.00000,40.00000,
                                            20.00000,20.00000,20.00000,
                                            1.00000,0.20000,0.00100,
                                            0.60000,0.00050,0.00000,
                                            0.00050,0.60000,0.00050,
                                            0.00010,0.0000001,1.00000,
                                            20.00000,20.00000,20.00000,
                                            20.00000,20.00000,0.60000000))
  
  IMatureFacPlaDisp <- IMatureFacPlaDisp[!grepl("^[*]", IMatureFacPlaDisp[["variable"]]), ]

  rownames(IMatureFacPlaDisp) <- 1 : length(rownames(IMatureFacPlaDisp))
  
  df <- expand(IMatureFacPlaDisp, nesting(variable, value), region = getISOlist())
  
  q <- as.quitte(a)
  q <- q[, c(3,7)]
  
  x <- left_join(df, q, by = c("region"))
  
  x[which(x[["variable"]] == "PGAWNO" & x[["value.y"]] == "1"), 2] <- 0
  
  x <- x[, 1:3]
  
  names(x) <- sub("value.x", "value", names(x))
  
  x <- as.quitte(x) %>% as.magpie()
  
  # Only keeping the ISO countries
  x <- x[getISOlist(), , ]
  
  return(list(x = x,
              weight = NULL,
              unit = "no",
              class = "magpie",
              description = "Maturity factor related to plant dispatching"))
  
}
