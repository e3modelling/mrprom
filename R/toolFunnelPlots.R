#' toolFunnelPlots
#'
#' Make FunnelPlots from magpie object for specific regions and variables
#' with many models and scenarios and years.
#'
#' @param magpie_object
#'
#' @return The read-in data to plots
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolFunnelPlots(magpie_object = x)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tidyr drop_na
#' @import ggplot2
#'
#' @export

toolFunnelPlots <- function(magpie_object = x) {
  
  df <- as.quitte(x)
  df <- drop_na(df)
  model <- df["model"]
  names(model) <- "temp"
  df[["model"]] <- paste0(df[["model"]], ".", df[["scenario"]])
  df <- cbind(df, model)
  statistics <- mutate(df, max = max(value, na.rm = TRUE), .by = c("region", "period"))
  statistics <- mutate(statistics, min = min(value, na.rm = TRUE), .by = c("region", "period"))
  statistics <- mutate(statistics, mean = mean(value, na.rm = TRUE), .by = c("region", "period"))
  statistics <- mutate(statistics, min75 = min(value, na.rm = TRUE)+0.75*(mean(value, na.rm = TRUE)-min(value, na.rm = TRUE)), .by = c("region", "period"))
  statistics <- mutate(statistics, max75 = max(value, na.rm = TRUE)-0.75*(max(value, na.rm = TRUE)-mean(value, na.rm = TRUE)), .by = c("region", "period"))
  df <- cbind.data.frame(df, max = statistics[,"max"],  min = statistics[,"min"],  mean = statistics[,"mean"],
                         max75 = statistics[,"max75"],  min75 = statistics[,"min75"])
  variable_names <- paste(unique(df[,"scenario"]), collapse = "\n")
  
  p <- ggplot(df, aes(period, value, colour = temp)) + 
    geom_point()
    
    if (length(df) < 12) {
      p <- p + ggtitle(paste0(df[,"region"]," ",df[,"variable"]))
    } else {
      p <- p + ggtitle(paste0(df[,"region"][1]," ", df[1,8]," ",df[,"variable"][1]))
    }
  
  p <- p  +labs(x="period", y=df[,"unit"][1]) + 
    theme_bw() + theme(legend.title=element_blank())
  check <- (length(unique(df[,"temp"])[unique(df[,"temp"])>0 & (is.finite(unique(df[,"temp"])))]))
  check2 <- (length(unique(df[,"value"])[unique(df[,"value"])>0 & (is.finite(unique(df[,"value"])))]))
  if ((check > 1) & (check2 > check)) {
    p <- p + geom_line(aes(y = mean, color="mean"))
    p <- p + geom_line(aes(y = max, color="max"))
    p <- p + geom_line(aes(y = min, color="min"))
    p <- p + geom_ribbon(aes(x = period, ymax = max, ymin = min), alpha = 0.6,
                          color = "red", linetype = 0, fill = "grey")
    p <- p + geom_ribbon(aes(x = period, ymax = max75, ymin = min75), alpha = 0.6,
                           color = "red", linetype = 0, fill = "skyblue")
  }
  p <- p  + annotate("text", x = min(df[["period"]],na.rm = TRUE), y = max(df[["value"]],na.rm = TRUE), label = paste("Scenario:\n", variable_names), hjust = 0, vjust = 1, size = 3)
  p <- p +   coord_cartesian(ylim = c(0, NA))
  
  return(p)
}


