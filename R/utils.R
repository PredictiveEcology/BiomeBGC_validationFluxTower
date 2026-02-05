mergeGPPdata <- function(towerData, BiomeBGCData, timescale, NEEpartitioningMethod = "DT", ustarThresMethod = "VUT", centralValue = "REF", confInt = NA, NAvalues = c(-9999, 0)){
  
  # Define which columns of towerData we need
  centralValueCol <- paste("GPP", NEEpartitioningMethod, ustarThresMethod, centralValue, sep = "_")
  colToKeep <- c("TIMESTAMP", centralValueCol)
  
  if (!is.na(confInt)){
    lowerBoundCol <- paste("GPP", NEEpartitioningMethod, ustarThresMethod, formatC(100-confInt, width = 2, flag = 0), sep = "_")
    upperBoundCol <- paste("GPP", NEEpartitioningMethod, ustarThresMethod, formatC(confInt, width = 2, flag = 0), sep = "_")
    colToKeep <- c(colToKeep, lowerBoundCol, upperBoundCol)
  }
  
  # format towerData
  towerData <- towerData[, colToKeep]
  towerData[towerData %in% NAvalues] <- NA
  if (0 %in% NAvalues){
    towerData[towerData <= 0] <- NA
  }
  
  if (timescale == "day"){
    # format dates
    dates <- as.Date(as.character(towerData[,"TIMESTAMP"]), format = "%Y%m%d")
    
    # If daily, removes february 29th as it is not simulated by Biome-BGC
    feb29 <- format(dates, "%d") == "29" & format(dates, "%m") == "02"
    towerData <- towerData[!feb29,]
    dates <- dates[!feb29]
    
    # Prepare tower data for merging
    years <- format(dates, "%Y")
    nyear <- length(unique(years))
    if(is.na(confInt)){
      towerData <- data.table(
        year = as.integer(years),
        day = rep(c(1:365), nyear),
        tower_GPP = towerData[, centralValueCol]
      )
      # prepare output
      out <- merge(towerData, BiomeBGCData[,.(year, timestep, day, daily_gpp)])
      # put in the same units (gC/m2/day) and rename columns
      out <- out[, .(timestep, year, day, tower_GPP, BGC_GPP = daily_gpp*1000)]
      
    } else {
      
      towerData <- data.table(
        year = as.integer(years),
        day = rep(c(1:365), nyear),
        tower_GPP = towerData[, centralValueCol],
        tower_GPP_min = towerData[, lowerBoundCol],
        tower_GPP_max = towerData[, upperBoundCol]
      )
      # prepare output
      out <- merge(towerData, BiomeBGCData[,.(year, timestep, day, daily_gpp)])
      # put in the same units (gC/m2/day) and rename columns
      out <- out[, .(timestep, year, day, tower_GPP, tower_GPP_min, tower_GPP_max, BGC_GPP = daily_gpp*1000)]
    }

  } else if (timescale == "month") {
    
    # format dates
    dates <- as.Date(paste0(as.character(towerData[,"TIMESTAMP"]), "01"), format = "%Y%m%d")
    
    # Prepare tower data for merging
    years <- format(dates, "%Y")
    nyear <- length(unique(years))
    
    if(is.na(confInt)){
      towerData <- data.table(
        year = as.integer(years),
        month = rep(c(1:12), nyear),
        tower_GPP = towerData[, centralValueCol]
      )
      
      out <- merge(towerData, BiomeBGCData[,.(year, month, daily_gpp)])
      # put in the same units (gC/m2/day) and rename columns
      out <- out[, .(year, month, tower_GPP, BGC_GPP = daily_gpp*1000)]
    } else {
      towerData <- data.table(
        year = as.integer(years),
        month = rep(c(1:12), nyear),
        tower_GPP = towerData[, centralValueCol],
        tower_GPP_min = towerData[, lowerBoundCol],
        tower_GPP_max = towerData[, upperBoundCol]
      )
      
      out <- merge(towerData, BiomeBGCData[,.(year, month, daily_gpp)])
      # put in the same units (gC/m2/day) and rename columns
      out <- out[, .(year, month, tower_GPP, tower_GPP_min, tower_GPP_max, BGC_GPP = daily_gpp*1000)]
    }
    
  } else {
    
    # Prepare tower data for merging
    years <- as.numeric(towerData[,"TIMESTAMP"])
    
    if(is.na(confInt)){
      towerData <- data.table(
        year = as.integer(years),
        tower_GPP = towerData[,centralValueCol]
      )
      
      out <- merge(towerData, BiomeBGCData[,.(year, daily_gpp)]) |> na.omit()
      # put in the same units (gC/m2/yr) and rename columns
      out <- out[, .(year, tower_GPP, BGC_GPP = daily_gpp*1000*365)]
    } else {
      towerData <- data.table(
        year = as.integer(years),
        tower_GPP = towerData[, centralValueCol],
        tower_GPP_min = towerData[, lowerBoundCol],
        tower_GPP_max = towerData[, upperBoundCol]
      )
      
      out <- merge(towerData, BiomeBGCData[,.(year, daily_gpp)]) |> na.omit()
      # put in the same units (gC/m2/yr) and rename columns
      out <- out[, .(year, tower_GPP, tower_GPP_min, tower_GPP_max, BGC_GPP = daily_gpp*1000*365)]
    }
  }
  
  return(out)
  
}
