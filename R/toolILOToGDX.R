#' toolILOToGDX
#'
#' Read in data from ILO and convert it to gdx file.
#'
#' @param subtypes Type of data.
#'
#' @return The read-in data into a gdx file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolILOToGDX(subtypes = c("POP_XWAP_SEX_AGE_NB_A", "EMP_TEMP_SEX_AGE_OCU_NB_A"))
#' }
#'
#' @importFrom Rilostat get_ilostat
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
#'
#' @export

toolILOToGDX <- function(subtypes = "POP_XWAP_SEX_AGE_NB_A") {
  note_source <- NULL
  obs_status <- NULL
  note_classif <- NULL
  note_indicator <- NULL

  if (length(subtypes) == 1) {
    tmp <- NULL
    x <- NULL
    x <- get_ilostat(subtypes, cache = FALSE)
    if ("note_source" %in% names(x))  {x <- select((x), -c(note_source))}
    if ("obs_status" %in% names(x))  {x <- select((x), -c(obs_status))}
    if ("note_classif" %in% names(x))  {x <- select((x), -c(note_classif))}
    if ("note_indicator" %in% names(x))  {x <- select((x), -c(note_indicator))}
    names(x) <- sub("obs_value", "values", names(x))
    type <- "ILO"
    tmp <- c(toolSubtype(x, subtypes, type))
    names(tmp[[2]]) <- NULL
    wgdx(paste0(subtypes, ".gdx"), tmp[[1]], tmp[[2]])
  }

  if (length(subtypes) > 1) {
    tmp2 <- NULL
    for (i in subtypes) {
      x <- NULL
      x <- get_ilostat(i,  cache = FALSE)
      if ("note_source" %in% names(x))  {x <- select((x), -c(note_source))}
      if ("obs_status" %in% names(x))  {x <- select((x), -c(obs_status))}
      if ("note_classif" %in% names(x))  {x <- select((x), -c(note_classif))}
      if ("note_indicator" %in% names(x))  {x <- select((x), -c(note_indicator))}
      names(x) <- sub("obs_value", "values", names(x))
      type <- "ILO"
      tmp2 <- c(tmp2, toolSubtype(x, i, type))
    }

    n <- names(tmp2[[2]])

    for (l in seq(4, length(tmp2), 2)){
      ng <- names(tmp2[[l]])
      for (i in 1:length(n)) {
        for (j in 1:length(ng)) {
          if (n[i] == ng[j]) {
            tmp2[[2]][[n[i]]]$uels[[1]] <- union(tmp2[[2]][[n[i]]]$uels[[1]],
                                                 tmp2[[l]][[ng[j]]]$uels[[1]])
            tmp2[[2]][[n[i]]]$val <- matrix(1:length(tmp2[[2]][[n[i]]]$uels[[1]]),
                                            nrow = length(tmp2[[2]][[n[i]]]$uels[[1]]))
          } else if (!(ng[j] %in% n))  {
            tmp2[[2]][[ng[j]]] <- tmp2[[l]][[ng[j]]]
            n <- c(n, ng[j])
          }
        }
      }
    }

    tmp <- list()
    tmp <- tmp2[seq(1, length(tmp2), 2)]

    names(tmp2[[2]]) <- NULL
    
    wgdx(paste0("ILO_ALL.gdx"), tmp, tmp2[[2]])
  }
  return(tmp)
}


