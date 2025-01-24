#' toolFunnelPlots
#'
#' Read in data XLSX file from IAMCOMPACT and make plots for "Final Energy" and
#' "Emissions|CO2" for all the studies. ENERDATA and NAVIGATE are used for validation.
#'
#' @param subtypes Region
#'
#' @return The read-in data to plots
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolFunnelPlots(subtype = "World")
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @import ggplot2
#'
#' @export

toolFunnelPlots <- function(subtypes = "World") {
  
  a <- readSource("IAMCOMPACT", subtype = "all")
  years <- c(2005, 2010, 2015, 2020, 2025)
  k<-a[, years, ][,,c("Emissions|CO2","Final Energy")][subtypes,,]
  k[,,"Final Energy"] <- k[,,"Final Energy"] * 23.8846
  getItems(k,3.4) <- c("Mt CO2/yr", "Mtoe")
  qk <- as.quitte(k)
  
  consumption_ENERDATA <- readSource("ENERDATA", subtype =  "consumption", convert = TRUE)
  consumption_ENERDATA <- consumption_ENERDATA[,,"Total final consumption.Mtoe"]
  number_2 <- readSource("ENERDATA", "2", convert = TRUE)
  CO2_emissions_ENERDATA <- number_2[, , "CO2 emissions from fuel combustion (sectoral approach).MtCO2"]
  year <-  c(2005:2021)
  ENERDATA <- mbind(consumption_ENERDATA[, year, ], CO2_emissions_ENERDATA[, year, ])
  
  if (subtypes == "World") {
    ENERDATA <- dimSums(ENERDATA, 1, na.rm = TRUE)
    getItems(ENERDATA, 1) <- "World"
    getItems(ENERDATA,3.1) <- c("Final Energy", "Emissions|CO2")
  } else {
    ENERDATA <- ENERDATA[subtypes,,]
    getItems(ENERDATA,3.1) <- c("Final Energy", "Emissions|CO2")
  }
  
  ENERDATA <- as.quitte(ENERDATA)
  ENERDATA[,8] <- "ENERDATA"
  names(ENERDATA)[8] <- "study"
  ENERDATA <- ENERDATA
  
  if (subtypes == "World") {
    Navigate_s <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
    Navigate_s <- Navigate_s["World",years,][,,c("Emissions|CO2","Final Energy")][,,"REMIND-MAgPIE 3_2-4_6"]
    Navigate_s[,,"Final Energy"] <- Navigate_s[,,"Final Energy"] * 23.8846
    Navigate_s <- as.quitte(Navigate_s) %>%
      interpolate_missing_periods(period = getYears(Navigate_s,as.integer=TRUE)[1]:getYears(Navigate_s,as.integer=TRUE)[length(getYears(Navigate_s))], expand.values = TRUE)
  } else {
    Navigate_s <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
    years <- years[years %in% getYears(Navigate_s,as.integer = TRUE)]
    Navigate_s <- Navigate_s[subtypes,years,][,,c("Emissions|CO2","Final Energy")][,,"REMIND-MAgPIE 3_2-4_6"]
    Navigate_s[,,"Final Energy"] <- Navigate_s[,,"Final Energy"] * 23.8846
    Navigate_s <- as.quitte(Navigate_s) %>%
      interpolate_missing_periods(period = getYears(Navigate_s,as.integer=TRUE)[1]:getYears(Navigate_s,as.integer=TRUE)[length(getYears(Navigate_s))], expand.values = TRUE)
  }
  
  Navigate <- as.quitte(Navigate_s)
  Navigate[,8] <- "Navigate"
  names(Navigate)[8] <- "study"
  REMIND_MAgPIE <- Navigate
  
  x <- rbind(qk, ENERDATA, REMIND_MAgPIE)
  
  for (ii in unique(x[["variable"]])) {
    for (i in getItems(a,3.5)) {
      df <- x
      ENERDATA <- x[which(x[, 8] == "ENERDATA" & x[, 4] == ii), ]
      REMIND_MAgPIE <- x[which(x[, 8] == "Navigate" & x[, 4] == ii), ]
      df <- df[which(df[, 8] == i & df[, 4] == ii), ]
      model <- df["model"]
      names(model) <- "r"
      df[["model"]] <- paste0(df[["model"]], ".", df[["scenario"]])
      df <- cbind(df, model)
      fit95 <- mutate(df, max = max(value, na.rm = TRUE), .by = c("region", "period"))
      fit95 <- mutate(fit95, min = min(value, na.rm = TRUE), .by = c("region", "period"))
      fit95 <- mutate(fit95, mean = mean(value, na.rm = TRUE), .by = c("region", "period"))
      df <- cbind.data.frame(df, 
                             lwr95=fit95[,"max"],  upr95=fit95[,"min"],  upr=fit95[,"mean"])
      WITCH <- REMIND_MAgPIE
      names(df)[10] <- "max"
      names(df)[11] <- "min"
      names(df)[12] <- "mean"
      df <- df[, c(1, 2, 3, 4, 5 , 6 , 7, 8 , 10, 11, 12, 9)]
      variable_names <- paste(unique(df[,2]), collapse = "\n")
      
      p <- ggplot(df, aes(period, value, colour = r)) + 
        geom_point() +  
        ggtitle(paste0(df[1,3]," ", df[1,8]," ",df[1,4]))+
        labs(x="period", y=df[1,5]) +    
        theme_bw() +  
        theme(legend.title=element_blank())
      check <- (length(unique(df[,9])[unique(df[,9])>0 & (is.finite(unique(df[,9])))]))
      check2 <- (length(unique(df[,7])[unique(df[,7])>0 & (is.finite(unique(df[,7])))]))
      if ((check > 1) & (check2 > check)) {
        p <- p + geom_line(aes(y = mean, color="mean"))
        p <- p + geom_line(aes(y = max, color="max"))
        p <- p + geom_line(aes(y = min, color="min"))
        p <- p + geom_ribbon(aes(x = period, ymax = max, ymin = min), alpha = 0.6,
                             fill = "grey70", color = "red", linetype = 0)
      }
      p <- p + geom_line(data=ENERDATA, aes(x=period, y=value, color="ENERDATA")) +
        geom_line(data=REMIND_MAgPIE, aes(x=period, y=value, color="REMIND_MAgPIE")) +
        annotate("text", x = min(df[["period"]],na.rm = TRUE), y = max(df[["value"]],na.rm = TRUE), label = paste("Scenario:\n", variable_names), hjust = 0, vjust = 1, size = 3)
      
      ggsave(filename = paste0(df[1,3]," ", df[1,8]," ",gsub("[|]", " ", ii),'.png'), plot = p,width = 7,height = 7)
      
    }
  }
}


