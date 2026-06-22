#' descriptionmrprom
#'
#' Description and in which functions are used the functions of mrprom.
#' 
#' @param file The input.gms file path.
#'
#' @return The description of mrprom functions
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- descriptionmrprom(file)
#' }
#' 
#' @import tools
#' @import gms
#' @importFrom pagedown chrome_print
#' 
#' @export
#' 

descriptionmrprom <- function(file) {
  
  a <- tools::Rd_db("mrprom")
  x <- NULL
  z <- NULL
  y <- getMadratGraph("mrprom")
  rownames(y) <- 1: nrow(y)
  
  setNames <- gms::readDeclarations(file)
  setNames <- as.data.frame(setNames)
  
  for (i in a) {
    f <- as.character(i[[1]][[1]])
    if (f == "fullOPEN-PROM") {
      f = "fullOPEN_PROM"
    }
    k <- tools:::.Rd_get_text(a[[paste0(f, ".Rd")]])
    index_of_descr <- which(k == "Description:") : which(k == "Usage:")
    description_mrprom <- k[(min(index_of_descr) + 4) : max(index_of_descr) - 2]
    description_mrprom <- paste(unlist(description_mrprom), collapse = " ")
    name <- as.character(k[3])
    used_from <- which(y[, 1] == f)
    used_from <- paste(unlist(y[used_from, 2]), collapse = ",")
    uses_the <- which(y[, 2] == f)
    uses_the <- paste(unlist(y[uses_the, 1]), collapse = ",")
    description <- setNames[which(grepl(substring(f, first = 5), setNames[, 1], ignore.case = TRUE)), ]
    description_open_prom <- description[, 3]
    
    if (length(description_open_prom) == 0) {
      description_open_prom = "No description"
    }
    if (length(description_open_prom) == 2) {
      description_open_prom <- description_open_prom[1]
    }
    
    z <- data.frame(name, description_mrprom, used_from, uses_the, description_open_prom)
    x <- rbind(x, z)
  }
  
  calc <- x[startsWith(x$name, "     calc"), ]
  calc[["name"]] <- trimws(calc[["name"]])
  calc <- filter(calc, !(used_from %in% ""))
  calc <- select(calc, -description_open_prom)
  
  calc <- calc[order(calc$name), ]
  
  html <- tempfile(fileext = ".html")
  
  cat(
    '<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">

<style>

body {
  font-family: Arial, sans-serif;
  margin: 40px;
  line-height: 1.4;
}

h1 {
  text-align: center;
}

h2 {
  margin-top: 35px;
  border-bottom: 1px solid #cccccc;
}

table {
  border-collapse: collapse;
  width: 100%;
  margin-top: 10px;
  margin-bottom: 20px;
}

th, td {
  border: 1px solid black;
  padding: 6px;
  text-align: left;
}

.toc ul {
  columns: 2;
}

.toc li {
  margin-bottom: 4px;
}

.pagebreak {
  page-break-after: always;
}

</style>

</head>
<body>
',
file = html
  )
  
  # ======================================================
  # Cover page
  # ======================================================
  
  cat(
    sprintf(
      "<h1>OPENPROM Function Documentation</h1>
<p><b>Total functions:</b> %s</p>",
      nrow(calc)
    ),
    file = html,
    append = TRUE
  )
  
  # ======================================================
  # Table of contents
  # ======================================================
  
  cat(
    "<div class='toc'>
<h2>Contents</h2>
<ul>",
    file = html,
    append = TRUE
  )
  
  for(i in seq_len(nrow(calc))) {
    
    cat(
      sprintf(
        "<li><a href='#%s'>%s</a></li>",
        calc$name[i],
        calc$name[i]
      ),
      file = html,
      append = TRUE
    )
    
  }
  
  cat(
    "</ul>
</div>

<div class='pagebreak'></div>",
    file = html,
    append = TRUE
  )
  
  # ======================================================
  # Function documentation
  # ======================================================
  
  for(i in seq_len(nrow(calc))) {
    
    desc <- ifelse(
      is.na(calc$description_mrprom[i]),
      "",
      calc$description_mrprom[i]
    )
    
    used_from <- ifelse(
      is.na(calc$used_from[i]),
      "",
      calc$used_from[i]
    )
    
    uses_the <- ifelse(
      is.na(calc$uses_the[i]),
      "",
      calc$uses_the[i]
    )
    
    cat(
      sprintf(
        '
<h2 id="%s">%s</h2>

<p>%s</p>

<table>
<tr>
  <th>Field</th>
  <th>Value</th>
</tr>

<tr>
  <td>used_from</td>
  <td>%s</td>
</tr>

<tr>
  <td>uses_the</td>
  <td>%s</td>
</tr>

</table>
',
        calc$name[i],
        calc$name[i],
        desc,
        used_from,
        uses_the
      ),
      file = html,
      append = TRUE
    )
  }
  
  cat(
    "</body></html>",
    file = html,
    append = TRUE
  )
  
  pagedown::chrome_print(
    input = html,
    output = "OPENPROM_Function_Documentation.pdf",
    wait = 5
  )
  
  return(calc)
  
}
