# GLOBIOM's lookup workbook has no EU28 agriculture CH4/N2O rows. The GLOBIOM
# emulator therefore has a dedicated annual supplement derived from one MAgPIE
# run. Keeping this adapter in the GLOBIOM pipeline makes the exceptional data
# dependency explicit; the new MAgPIE emulator does not read or reuse it.

.toolBmswasLoadAgriSupplementGLOBIOM <- function() {
  readSource(
    "GLOBIOM_LookupTable",
    subtype = "euAgricultureSupplement",
    convert = FALSE
  )
}

.toolBmswasAgriSupplementSeriesGLOBIOM <- function(x, region, emtype) {
  if (!(region %in% getItems(x, 1)) || !(emtype %in% getItems(x, 3))) {
    return(NULL)
  }
  years <- paste0("y", .toolBmswasOutYearsGLOBIOM)
  as.numeric(x[region, years, emtype])
}
