correctIEA2025 <- function(x) {
  if ("AUTOHEAT" %in% getItems(x, 3.3)) {
    print("ENTER CORRECT IEA2025")
    corr <- readSource("IEA2025", subset = "HEAUTOH", convert = FALSE)
    years <- getYears(corr)
    regions <- c("RUSSIA", "JAPAN")
    x[regions, years, "KTOE.OTH_ENSOURC.AUTOHEAT"] <- -corr[regions, years, "KTOE.OTH_ENSOURC.HEAUTOH"]
  }
  return(x)
}
