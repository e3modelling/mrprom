readTechCosts <- function(subtype = "PowerAndHeat") {



# categories and subcategories:
# demand / supply
# sectors: industry, transport, buildings, power&heat, etc.
# categories specific to each sector
  if (subtype == "PowerAndHeat") {
    x <- read.csv("power_and_heat_energyf.csv")
    names(x) <- c("tech", "value", "year", "variable", "unit")
    x <- filter(x, x[["tech"]] != "Refurbishment of existing nuclear reactors")
    x[["value"]] <- as.numeric(x[["value"]])
    x <- as.quitte(x)

  } else if (subtype == "DomesticEnergy"){
    x <- read.csv("domestic_energyf2.csv")
    names(x) <- c("tech", "appliances", "sector", "value", "category", "measurement", "variant", "unit")
    x[["value"]] <- as.numeric(x[["value"]])
    x <- as.quitte(x)
  }
  else if (subtype == "IndustryEnergy"){
  x <- read.csv("industry_energyf2.csv")
  names(x) <- c("tech", "category", "value", "type", "measurement", "variant", "units")
  x[["value"]] <- as.numeric(x[["value"]])
  x <- as.quitte(x)
  }
  else if (subtype == "infrastructure"){
    x <- read.csv("Infrastructuref2.csv")
    names(x) <- c("tech", "value", "measurement","variable", "unit", "title")
    x[["value"]] <- as.numeric(x[["value"]])
    x <- as.quitte(x)
  }
  else if (subtype == "new_fuels_energy"){
    x <- read.csv("new_fuels_energy.csv")
    x <- as.quitte(x)
  }

  else if (subtype == "maritime"){
    x <- read.csv("maritime.csv")
    x[["Reference_energy_consumption"]] <- as.character(x[["Reference_energy_consumption"]])
    x[["Reference_capital_cost"]] <- as.character(x[["Reference_capital_cost"]])
    x[["Reduction_or_electric_range_value"]] <- as.character(x[["Reduction_or_electric_range_value"]])
    x <- as.quitte(x)
  }

return(as.magpie(x))
}
