toolReadExcelWindow <- function(file, sheet, range) {
    x <- readxl::read_excel(path = file, sheet = sheet,range = range)
    x <- filter(x, x[[names(x)[2]]] != "NA")
    x["variable"] <- names(x)[2]
    names(x) <- c(names(x)[1], as.character(x[1, grep("[a-z,A-Z]", as.character(x[1, ]), invert = TRUE)]), names(x)[length(x[1, ])])
    x <- filter(x, x[[1]] != "NA")
    x <- pivot_longer(x, cols = grep("[a-z,A-Z]", names(x), invert = TRUE), names_to = "period")
    return(x)
}