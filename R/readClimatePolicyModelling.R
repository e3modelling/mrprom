#' Read Climate Policy Modelling data
#'
#' Reads a selected sheet from the Climate Policy Modelling Protocol workbook,
#' removes rows marked as deleted using strikethrough formatting, and converts
#' the result into a quitte object.
#'
#' @param subtype Character. Available subtypes are:
#' \itemize{
#'   \item `High impact policies_update`
#'   \item `Translation to model target`
#'   \item `Archived policies`
#' }
#'
#' @return A list containing the data as a quitte object and its metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ClimatePolicyModelling",subtype = "Translation to model target")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyxl xlsx_cells xlsx_formats
#' @importFrom dplyr filter mutate select distinct pull case_when row_number
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom stringr str_extract str_trim
#' @importFrom lubridate year
#'
readClimatePolicyModelling <- function(subtype) {
  
  file <- "Climate Policy Modelling Protocol_2026_v5.1.xlsx"
  
  valid_subtypes <- c(
    "High impact policies_update",
    "Translation to model target",
    "Archived policies"
  )
  
  if (!subtype %in% valid_subtypes) {
    stop(
      "Unknown subtype: ", subtype,
      ". Available subtypes are: ",
      paste(valid_subtypes, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Identify Excel rows containing strikethrough formatting.
  # ---------------------------------------------------------------------------
  getDeletedRows <- function(file, sheet) {
    
    cells <- tidyxl::xlsx_cells(
      path = file,
      sheets = sheet
    )
    
    formats <- tidyxl::xlsx_formats(file)
    
    strike_formats <- formats$local$font$strike
    strike_formats[is.na(strike_formats)] <- FALSE
    
    cells |>
      dplyr::mutate(
        is_strikethrough = dplyr::case_when(
          is.na(local_format_id) ~ FALSE,
          
          # local_format_id is zero-based, while R indexing is one-based.
          local_format_id + 1L <= length(strike_formats) ~
            strike_formats[local_format_id + 1L],
          
          TRUE ~ FALSE
        )
      ) |>
      dplyr::filter(is_strikethrough) |>
      dplyr::distinct(row) |>
      dplyr::pull(row)
  }
  
  # ---------------------------------------------------------------------------
  # Read a sheet and remove rows containing strikethrough formatting.
  # ---------------------------------------------------------------------------
  readSheetWithoutDeleted <- function(file, sheet, skip = 0L) {
    
    deleted_excel_rows <- getDeletedRows(
      file = file,
      sheet = sheet
    )
    
    x <- readxl::read_excel(
      path = file,
      sheet = sheet,
      skip = skip
    )
    
    # With skip = 0:
    # Excel row 1 contains headers and dataframe row 1 is Excel row 2.
    #
    # With skip = 1:
    # Excel row 2 contains headers and dataframe row 1 is Excel row 3.
    x |>
      dplyr::mutate(
        .excel_row = dplyr::row_number() + skip + 1L
      ) |>
      dplyr::filter(
        !.excel_row %in% deleted_excel_rows
      ) |>
      dplyr::select(-.excel_row)
  }
  
  # ---------------------------------------------------------------------------
  # High impact policies
  # ---------------------------------------------------------------------------
  if (subtype == "High impact policies_update") {
    
    x <- readSheetWithoutDeleted(
      file = file,
      sheet = subtype,
      skip = 1L
    )
    
    # The first column contains the policy code, but its header is Argentina.
    names(x)[1] <- "Policy"
    
    x <- x |>
      dplyr::mutate(
        # Extract ISO3 code from identifiers such as:
        # 1a-ARG-TRA-BIO-26
        region = stringr::str_extract(
          Policy,
          "(?<=-)[A-Z]{3}(?=-)"
        ),
        
        # Extract a four-digit year from numeric or character values.
        period = as.integer(
          stringr::str_extract(
            as.character(`End date of implementation`),
            "[0-9]{4}"
          )
        ),
        
        variable = paste0(
          "Climate Policy|High Impact|",
          Policy
        ),
        
        unit = "policy",
        
        # One indicates that the policy exists.
        value = 1
      ) |>
      dplyr::filter(
        !is.na(region)
      ) |>
      dplyr::select(
        region,
        period,
        variable,
        unit,
        value,
        Policy,
        `Policy database ID`,
        Sector,
        `Policy instrument/Policy target`,
        `Type of policy instrument`,
        `Policy type`,
        `Name of policy (+ link to Climate Policy database or policy document)`,
        `Policy status`,
        Impact,
        Credibility,
        Quantifiable,
        `IMAGE CurPol scenario inclusion`,
        `Start date of implementation`,
        `End date of implementation`,
        `Inclusion date / project`,
        `2026 review`
      )
    
    # ---------------------------------------------------------------------------
    # Translation to model target
    # ---------------------------------------------------------------------------
  } else if (subtype == "Translation to model target") {
    
    x <- readSheetWithoutDeleted(
      file = file,
      sheet = subtype,
      skip = 0L
    )
    
    # Remove spaces at the beginning or end of column names.
    names(x) <- stringr::str_trim(names(x))
    
    x <- x |>
      dplyr::mutate(
        model_value_min_original = `Model Target Value Min`,
        model_value_max_original = `Model Target Value Max`
      ) |>
      tidyr::pivot_longer(
        cols = c(
          "Model Target Value Min",
          "Model Target Value Max"
        ),
        names_to = "value_type",
        values_to = "value_raw"
      ) |>
      dplyr::mutate(
        region = `ISO-3`,
        
        period = as.integer(
          stringr::str_extract(
            as.character(`Target Year`),
            "[0-9]{4}"
          )
        ),
        
        variable = paste0(
          `Model Target Indicator`,
          "|",
          dplyr::case_when(
            value_type == "Model Target Value Min" ~ "Minimum",
            value_type == "Model Target Value Max" ~ "Maximum",
            TRUE ~ value_type
          )
        ),
        
        unit = `Model Target Unit`,
        
        # Convert numeric text and preserve negative signs such as "-9".
        value = suppressWarnings(
          as.numeric(value_raw)
        )
      ) |>
      dplyr::filter(
        !is.na(region),
        region != ""
      ) |>
      dplyr::select(
        region,
        period,
        variable,
        unit,
        value,
        value_type,
        value_raw,
        `Policy ID`,
        Country,
        `Policy Type`,
        Sector,
        `Policy status`,
        `Original Target Indicator`,
        `Original Target Value Min`,
        `Original Target Value Max`,
        `Original Target Unit`,
        `Model Target Indicator`,
        model_value_min_original,
        model_value_max_original,
        `Model Target Unit`,
        `Target type`,
        `Base Year`,
        `Quantification based on`,
        Comments
      )
    
    # ---------------------------------------------------------------------------
    # Archived policies
    # ---------------------------------------------------------------------------
  } else if (subtype == "Archived policies") {
    
    x <- readSheetWithoutDeleted(
      file = file,
      sheet = subtype,
      skip = 0L
    )
    
    # Remove completely empty columns.
    x <- x[
      ,
      colSums(!is.na(x)) > 0,
      drop = FALSE
    ]
    
    x <- x |>
      dplyr::filter(
        !is.na(Country),
        Country != ""
      ) |>
      dplyr::mutate(
        region = Country,
        
        period = lubridate::year(`Date deleted`),
        
        variable = paste0(
          "Climate Policy|Archived|",
          Policy
        ),
        
        unit = "policy",
        
        # One indicates that the policy was archived.
        value = 1
      ) |>
      dplyr::filter(
        !is.na(region)
      ) |>
      dplyr::select(
        region,
        period,
        variable,
        unit,
        value,
        Country,
        Policy,
        `Policy ID`,
        Sector,
        `Policy target/policy instrument`,
        `Name of policy`,
        `Date deleted`,
        Institute,
        Comment
      )
  }
  
  # Convert the dataframe into a quitte object.
  x <- quitte::as.quitte(x)
  
  list(
    x = x,
    weight = NULL,
    class = "quitte",
    description = c(
      category = "Climate Policy Modelling",
      type = subtype,
      filename = file,
      `Indicative size (MB)` = 1.2,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}