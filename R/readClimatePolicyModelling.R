#' Read Climate Policy Modelling data
#'
#' Reads a selected sheet from the Climate Policy Modelling Protocol workbook
#' and converts it into a quitte object.
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
#' @importFrom dplyr filter mutate rename select
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
  
  if (subtype == "High impact policies_update") {
    
    x <- readxl::read_excel(
      path = file,
      sheet = subtype,
      skip = 1
    )
    
    # The first column contains the policy code, but its header is Argentina.
    names(x)[1] <- "Policy"
    
    x <- x |>
      dplyr::filter(!is.na(Policy)) |>
      dplyr::mutate(
        # Extract ISO3 code from policy codes such as:
        # 1a-ARG-TRA-BIO-26
        region = stringr::str_extract(Policy, "(?<=-)[A-Z]{3}(?=-)"),
        
        period = as.integer(`End date of implementation`),
        
        variable = paste0(
          "Climate Policy|High Impact|",
          Policy
        ),
        
        unit = "policy",
        
        # One means that the policy exists for this region and year.
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
    
  } else if (subtype == "Translation to model target") {
    
    x <- readxl::read_excel(
      path = file,
      sheet = subtype
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
        values_to = "value"
      ) |>
      dplyr::mutate(
        region = `ISO-3`,
        period = as.integer(`Target Year`),
        
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
        value = suppressWarnings(as.numeric(value))) |>
      dplyr::filter(
        !is.na(region)
      ) |>
      dplyr::select(
        region,
        period,
        variable,
        unit,
        value,
        value_type,
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
    
  } else if (subtype == "Archived policies") {
    
    x <- readxl::read_excel(
      path = file,
      sheet = subtype
    )
    
    # Remove completely empty columns.
    x <- x[, colSums(!is.na(x)) > 0, drop = FALSE]
    
    x <- x |>
      dplyr::filter(
        !is.na(Country)
      ) |>
      dplyr::mutate(
        region = Country,
        
        period = lubridate::year(`Date deleted`),
        
        variable = paste0(
          "Climate Policy|Archived|",
          Policy
        ),
        
        unit = "policy",
        
        # One means that the policy was archived in this year.
        value = 1
      ) |>
      dplyr::filter(
        !is.na(region),
        !is.na(period)
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