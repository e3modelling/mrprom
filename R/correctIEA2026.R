correctIEA2026 <- function(x) {
  if ("AUTOHEAT" %in% getItems(x, 3.3)) {
    print("ENTER CORRECT IEA2026")
    corr <- readSource("IEA2026", subset = "HEAUTOH", convert = FALSE)
    years <- getYears(corr)
    regions <- c("RUSSIA", "JAPAN")
    x[regions, years, "KTOE.OTH_ENSOURC.AUTOHEAT"] <- -corr[regions, years, "KTOE.OTH_ENSOURC.HEAUTOH"]
  }
  return(x)
}
