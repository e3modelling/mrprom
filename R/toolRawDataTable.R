#' toolRawDataTable
#'
#' Description of the sources of mrprom.
#' 
#' @param file The input.gms file path.
#'
#' @return Info about sources of mrprom.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolRawDataTable(file)
#' }
#'
#' @importFrom data.table transpose
#' 
#' @export

toolRawDataTable <- function(file) {

  a <- input_data_overview(file)
  x <- getSources(packages = "mrprom")
  dr <- c("EUC", "Eurostat2", "EvalGlobal", "IAMCOMPACT", "toolSets")
  full_table <- NULL
  x <- x[!(x[["source"]] %in% dr), ]
  
  for (i in 1 : nrow(x)) {
    z <- x[i,1]
    if (z %in% c("Sheet", "Sheet2", "MACC", "MACCSheets", "NDC_LTT_NECP",
                 "Sheet3", "Sheet4", "AvailRate", "EmissionsBaselineToMagpie",
                 "FgasesToMagpie", "MacToMagpie")) {
      next  
    }
    z <- x[i,1]
    print(z)
    y <- readSource(z, convert = FALSE, supplementary = TRUE)
    description <- y[["description"]]
    y <- readSource(z, convert = FALSE)
    years <- getYears(y, as.integer = TRUE)
    countries <- length(getRegions(y))
    description_mrprom <- a[a[,"name"] == paste0("     ","read",z), 2]
    table2 <- as.data.frame(description)
    table <- transpose(table2)
    colnames(table) <- rownames(table2)
    folder <- paste0("https://ricardogroup.sharepoint.com/:f:/s/GlobalIntegratedAssessmentModels/Es3qJ4glKbdMgEMfabMTbIYB7pQYvxLbJChfHjYo33MQvQ?e=dF8iQu/",z)
    table <- cbind(as.data.frame(z), as.data.frame(folder), table, as.data.frame(description_mrprom), as.data.frame(paste0(years[1],":",years[length(years)])), as.data.frame(countries))
    colnames(table)[1] <- "Source"
    colnames(table)[2] <- "folder"
    colnames(table)[11] <- "Temporal resolution"
    full_table <- rbind(full_table, table)
  }
  
  return(full_table)
}
