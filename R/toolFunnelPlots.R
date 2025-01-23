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
#' @importFrom Rilostat get_ilostat
#' @importFrom gdxrrw wgdx
#' @importFrom dplyr select filter
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
  ENERDATA <- mbind(consumption_ENERDATA, CO2_emissions_ENERDATA)
  
  if (subtypes = "World") {
    ENERDATA <- dimSums(ENERDATA, 1, na.rm = TRUE)
    getItems(ENERDATA, 1) <- "World"
  } else
    ENERDATA <- ENERDATA[subtypes,,]
  }


  getItems(consumption_ENERDATA, 3.1) <- "Final Energy"
  consumption_ENERDATA <- consumption_ENERDATA[,c(2005:2021),]
  qFE <- as.quitte(consumption_ENERDATA)
  qFE[,8] <- "ENERDATA"
  names(qFE)[8] <- "study"
  qFE[["model"]] <- "ENERDATA"
  qFE[["scenario"]] <- "VALIDATION"
  
  
 qEM_ENERDATA <- dimSums(CO2_emissions_ENERDATA, 1, na.rm = TRUE)
  getItems(qEM_ENERDATA, 1) <- "World"
  CO2_emissions_ENERDATA <- CO2_emissions_ENERDATA["CHN",,]
  getItems(CO2_emissions_ENERDATA, 1) <- "CHN"
  getItems(qEM_ENERDATA, 3.1) <- "Emissions|CO2"
  qEM_ENERDATA <- qEM_ENERDATA[,c(2005:2021),]
  qEM <- as.quitte(qEM_ENERDATA)
  qEM[,8] <- "ENERDATA"
  names(qEM)[8] <- "study"
  qEM[["model"]] <- "ENERDATA"
  qEM[["scenario"]] <- "VALIDATION"
  qEM[["unit"]] <- "Mt CO2/yr"
  
  Navigate_s <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
  Navigate <- Navigate_s["European Union (28 member countries)",years,][,,c("Emissions|CO2","Final Energy")][,,"WITCH 5_0"]
  Navigate <- Navigate_s["World",years,][,,c("Emissions|CO2","Final Energy")][,,"REMIND-MAgPIE 3_2-4_6"]
  Navigate <- as.quitte(Navigate)
  Navigate[,8] <- "Navigate"
  names(Navigate)[8] <- "study"
  REMIND_MAgPIE <- Navigate
  
  x <- rbind(qk, qFE, REMIND_MAgPIE)
  # df <- as.magpie(x)
  # write.report(df, file = "IAMCOMPACT.mif")
  
  for (i in getItems(a,3.5)) {
    print(i)
    df <- x
    ENERDATA <- x[which(x[, 8] == "ENERDATA" & x[, 4] == "Final Energy"), ]
    REMIND_MAgPIE <- x[which(x[, 8] == "Navigate" & x[, 4] == "Final Energy"), ]
    REMIND_MAgPIE[,7] <- REMIND_MAgPIE[,7]*23.8846
    df <- df[which(df[, 8] == i & df[, 4] == "Final Energy"), ]
    #df <- rbind(df1, df2)
    r <- df["model"]
    names(r) <- "r"
    df[["model"]] <- paste0(df[["model"]], ".", df[["scenario"]])
    df <- cbind(df, r)
    
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
      #geom_line(aes(linetype = model))+ 
      ggtitle(paste0(df[1,3]," ", df[1,8]," ",df[1,4]))+
      labs(x="period", y="Mtoe") +    
      theme_bw() +
      # theme(legend.position="none")+  
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
    
    ggsave(filename = paste0(df[1,3]," ", df[1,8]," ","Final Energy",'.png'), plot = p,width = 7,height = 7)
    
  }
  
}


