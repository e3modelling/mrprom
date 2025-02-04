#' toolFunnelPlots
#'
#' Make FunnelPlots from quitte object for specific years, regions and variables.
#'
#' @param subtypes magpie object
#'
#' @return The read-in data to plots
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolFunnelPlots(subtype = df)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @import ggplot2
#'
#' @export

toolFunnelPlots <- function(subtypes = x) {
  
  df <- as.quitte(x)
  
  for (ii in unique(x[["variable"]])) {
    
    df <- df[which(df[,"variable"] == ii), ]
    model <- df["model"]
    names(model) <- "r"
    df[["model"]] <- paste0(df[["model"]], ".", df[["scenario"]])
    df <- cbind(df, model)
    fit95 <- mutate(df, max = max(value, na.rm = TRUE), .by = c("region", "period"))
    fit95 <- mutate(fit95, min = min(value, na.rm = TRUE), .by = c("region", "period"))
    fit95 <- mutate(fit95, mean = mean(value, na.rm = TRUE), .by = c("region", "period"))
    df <- cbind.data.frame(df, 
                           lwr95=fit95[,"max"],  upr95=fit95[,"min"],  upr=fit95[,"mean"])
    names(df)[10] <- "max"
    names(df)[11] <- "min"
    names(df)[12] <- "mean"
    variable_names <- paste(unique(df[,"scenario"]), collapse = "\n")
    
    p <- ggplot(df, aes(period, value, colour = r)) + 
      geom_point()
      
      if (length(df) < 12) {
        p <- p + ggtitle(paste0(df[,"region"]," ",df[,"variable"]))
      } else {
        p <- p + ggtitle(paste0(df[,"region"][1]," ", df[1,8]," ",df[,"variable"][1]))
      }
    
    p <- p  +labs(x="period", y=df[,"unit"][1]) + 
      theme_bw() + theme(legend.title=element_blank())
    check <- (length(unique(df[,"r"])[unique(df[,"r"])>0 & (is.finite(unique(df[,"r"])))]))
    check2 <- (length(unique(df[,"value"])[unique(df[,"value"])>0 & (is.finite(unique(df[,"value"])))]))
    if ((check > 1) & (check2 > check)) {
      p <- p + geom_line(aes(y = mean, color="mean"))
      p <- p + geom_line(aes(y = max, color="max"))
      p <- p + geom_line(aes(y = min, color="min"))
      p <- p + geom_ribbon(aes(x = period, ymax = max, ymin = min), alpha = 0.6,
                           fill = "grey70", color = "red", linetype = 0)
    }
    p <- p  + annotate("text", x = min(df[["period"]],na.rm = TRUE), y = max(df[["value"]],na.rm = TRUE), label = paste("Scenario:\n", variable_names), hjust = 0, vjust = 1, size = 3)
    
    ggsave(filename = paste0(df[,"region"][1]," ", df[1,8]," ",gsub("[|]", " ", ii),'.png'), plot = p,width = 7,height = 7)
    
  }
}


