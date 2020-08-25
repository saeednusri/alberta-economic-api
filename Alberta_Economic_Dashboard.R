# List packages here if more are required. Do not forget to load them later. 
list.of.packages <- c("dplyr", 
                      "httr", 
                      "jsonlite")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# dplyr only required for inner joins of multiple tables.
library(dplyr)


###########################
download_decision = TRUE # Default method
##########################


# Downloading through screen scraped url from Alberta Economic Dashboard
download <- function(dashboad_element){
  # Path to elements to
  path_1 <- "https://economicdashboard.alberta.ca/Download/DownloadFile?extension=csv&requestUrl=https%3A%2F%2Feconomicdashboard.alberta.ca%2Fapi%2F"
  path <- paste0(path_1, dashboad_element)
  download.file(path, "load_file.csv")
  load_file <- read.csv("load_file.csv")
  load_file
}


# Add or remove elements of the dashboard. In order to remove any elements just comment the elements that are not required
# Some element don't work with the scraping method.
## Important Note: Wells Drilled, Active Drilling Rigs, MLS Sales Value are not compatible with this method of scraping. 

list <- c("NaturalGasProduction", 
          "OilProduction", 
          "NaturalGasPrice", 
          "OilPrice",
          "RetailTrade",
          "ConsumerPriceIndexChange",
          "MotorVehicleSales",
          "WholesaleTrade",
          "ManufacturingShipments",
          "AirPassengers",
          "ProductivityGrowth",
          "GrossDomesticProduct",
          "HousingStarts",
          "BuildingPermits",
          "Investment",
          "LivestockPrices",
          "GrainPrices",
          "LumberProduction",
          "GrainDeliveries",
          "FarmCashReceipts",
          "Unemployment",
          "AverageWeeklyEarnings",
          "EmploymentInsurance",
          "MerchandiseExports",
          "Bankruptcies",
          "BusinessIncorporations",
          "NetMigration",
          "Population")

# Preferred method of function is download, i.e. download = T. If download = F, need bigger memory allocation. 
download_or_aggregate <- function(download_decision = TRUE, list){
  # dir.create("data")
  selection = download_decision[1]
  if(selection == FALSE){
    # Loop initiator for consistent inner joins
    aggregate <- data.frame()
    df <- download(list[1])
    df <- df[, colnames(df) %in% c("When", "Type" ,"Alberta", "Id")]
    names(df)[names(df) == "Type"] <- paste0(list[1], "_Type")
    names(df)[names(df) == "Id"] <- paste0(list[1], "_Id")
    names(df)[names(df) == "Alberta"] <- list[1]
    aggregate <- df
    
    # Loop though the remaining list and aggregate alL into single data frame
    for(i in 2:length(list)){
      tryCatch({
        df <- download(list[i])
        df <- df[, colnames(df) %in% c("When", "Type" ,"Alberta", "Id")]
        names(df)[names(df) == "Type"] <- paste0(list[i], "_Type")
        names(df)[names(df) == "Id"] <- paste0(list[i], "_Id")
        names(df)[names(df) == "Alberta"] <- list[i]
        aggregate <- inner_join(aggregate, df, by = "When")
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    
  } else{
    for(i in 1:length(list)){
      df <- download(list[i])
      write.csv(df, paste0(list[i], ".csv"))
      # For windows use the following. Comment the above line.
      # write.csv(df, paste0("data\", list[i], ".csv"))
    }
    
    
  }
  
}

# Default method download_decision = T = True
download_or_aggregate(download = TRUE, list)

# Use following if download_decision = False
#write.csv(aggregate, "final_extract.csv")
